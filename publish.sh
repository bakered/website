#!/bin/bash
# Usage:
#   publish plot <project-name> <file-path>
#   publish blog <project-name> <file-path>

set -e

REPO="/Users/edbaker/website2point0"
DOMAIN="bakered91.com"

TYPE="$1"
PROJECT="$2"
FILE="$3"

if [ -z "$TYPE" ] || [ -z "$PROJECT" ] || [ -z "$FILE" ]; then
  echo "Usage: publish <plot|blog> <project-name> <file-path>"
  exit 1
fi

if [ ! -f "$FILE" ]; then
  echo "Error: file not found: $FILE"
  exit 1
fi

FILENAME=$(basename "$FILE")

case "$TYPE" in
  plot)
    DEST="$REPO/static/outputs/$PROJECT"
    URL="https://$DOMAIN/outputs/$PROJECT/$FILENAME"

    mkdir -p "$DEST"
    cp "$FILE" "$DEST/$FILENAME"
    ;;

  blog)
    # Extract title from <title> tag, fall back to project name
    TITLE=$(grep -i '<title>' "$FILE" | sed 's/.*<title>\(.*\)<\/title>.*/\1/' | tr -d '\r\n' | head -1)
    if [ -z "$TITLE" ]; then
      TITLE="$PROJECT"
    fi

    # Copy HTML to static (also accessible as a standalone URL)
    STATIC_DEST="$REPO/static/blog/$PROJECT"
    mkdir -p "$STATIC_DEST"
    cp "$FILE" "$STATIC_DEST/$FILENAME"

    # Create/overwrite Hugo post entry so it appears in the blog listing
    POST_DIR="$REPO/content/post/$PROJECT"
    mkdir -p "$POST_DIR"
    cat > "$POST_DIR/index.md" <<HUGO
---
title: "$TITLE"
date: $(date +%Y-%m-%d)
show_date: false
---

<iframe src="/blog/$PROJECT/$FILENAME"
        width="100%"
        height="100%"
        style="min-height:90vh; border:none;">
</iframe>
HUGO

    URL="https://$DOMAIN/blog/$PROJECT/$FILENAME"
    ;;

  *)
    echo "Error: type must be 'plot' or 'blog'"
    exit 1
    ;;
esac

cd "$REPO"
git add static/ content/
git commit -m "publish $TYPE: $PROJECT/$FILENAME"
git push

echo ""
echo "Live in ~1-2 min at: $URL"
