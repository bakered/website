#!/bin/bash
# Usage:
#   publish html  <project-name> <file-path>   — static HTML output (plot, chart, etc.)
#   publish post  <project-name> <file-path>   — blog post (appears in Posts section)
#   publish graph <project-name> <file-path>   — economist graph (appears in Economist Graphs section)

set -e

REPO="/Users/edbaker/website2point0"
DOMAIN="bakered91.com"

TYPE="$1"
PROJECT="$2"
FILE="$3"

if [ -z "$TYPE" ] || [ -z "$PROJECT" ] || [ -z "$FILE" ]; then
  echo "Usage: publish <html|post|graph> <project-name> <file-path>"
  exit 1
fi

if [ ! -f "$FILE" ]; then
  echo "Error: file not found: $FILE"
  exit 1
fi

FILENAME=$(basename "$FILE")
TODAY=$(date +%Y-%m-%d)

# Extract title from <title> tag, fall back to project name
extract_title() {
  local f="$1"
  local fallback="$2"
  local t
  t=$(grep -i '<title>' "$f" | sed 's/.*<title>\(.*\)<\/title>.*/\1/' | tr -d '\r\n' | head -1)
  echo "${t:-$fallback}"
}

# Copy HTML to static dir, rewriting local img src paths and bundling images
copy_html() {
  local src="$1"
  local dest_dir="$2"
  mkdir -p "$dest_dir"
  local abs_src
  abs_src=$(cd "$(dirname "$src")" && pwd)/$(basename "$src")

  python3 - <<PYEOF
import re, shutil
from pathlib import Path

html_file = Path("$abs_src")
dest_dir  = Path("$dest_dir")
html_dir  = html_file.parent
content   = html_file.read_text()
copied, missing = [], []

def replace_src(m):
    src = m.group(1)
    if src.startswith(('http://', 'https://', 'data:', '//', '#', '/')):
        return m.group(0)
    img_path = (html_dir / src).resolve()
    if img_path.exists():
        shutil.copy2(img_path, dest_dir / img_path.name)
        copied.append(img_path.name)
        return f'src="{img_path.name}"'
    missing.append(src)
    return m.group(0)

content = re.sub(r'src="([^"]*)"', replace_src, content)
(dest_dir / html_file.name).write_text(content)

if copied:
    print(f"  Copied {len(copied)} image(s): {', '.join(copied)}")
if missing:
    print(f"  Warning — image not found: {', '.join(missing)}")
PYEOF
}

case "$TYPE" in
  html)
    DEST="$REPO/static/outputs/$PROJECT"
    URL="https://$DOMAIN/outputs/$PROJECT/$FILENAME"

    copy_html "$FILE" "$DEST"
    ;;

  post)
    TITLE=$(extract_title "$FILE" "$PROJECT")

    copy_html "$FILE" "$REPO/static/blog/$PROJECT"

    POST_DIR="$REPO/content/post/$PROJECT"
    mkdir -p "$POST_DIR"
    cat > "$POST_DIR/index.md" <<HUGO
---
title: "$TITLE"
date: $TODAY
show_date: false
---

<meta http-equiv="refresh" content="0; url=/blog/$PROJECT/$FILENAME">

[Click here if not redirected automatically](/blog/$PROJECT/$FILENAME)
HUGO

    URL="https://$DOMAIN/blog/$PROJECT/$FILENAME"
    ;;

  graph)
    TITLE=$(extract_title "$FILE" "$PROJECT")

    copy_html "$FILE" "$REPO/static/graphs/$PROJECT"

    GRAPH_DIR="$REPO/content/graphs/$TODAY-$PROJECT"
    mkdir -p "$GRAPH_DIR"
    cat > "$GRAPH_DIR/index.md" <<HUGO
---
title: "$TITLE"
date: '$TODAY'
slug: $TODAY-$PROJECT
categories: []
tags: []
summary: ''
---

<meta http-equiv="refresh" content="0; url=/graphs/$PROJECT/$FILENAME">

[Click here if not redirected automatically](/graphs/$PROJECT/$FILENAME)
HUGO

    URL="https://$DOMAIN/graphs/$PROJECT/$FILENAME"
    ;;

  *)
    echo "Error: type must be 'html', 'post', or 'graph'"
    exit 1
    ;;
esac

cd "$REPO"
git add static/ content/
git commit -m "publish $TYPE: $PROJECT/$FILENAME"
git push

echo ""
echo "Live in ~1-2 min at: $URL"
