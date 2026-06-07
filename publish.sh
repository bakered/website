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
    ;;
  blog)
    DEST="$REPO/static/blog/$PROJECT"
    URL="https://$DOMAIN/blog/$PROJECT/$FILENAME"
    ;;
  *)
    echo "Error: type must be 'plot' or 'blog'"
    exit 1
    ;;
esac

mkdir -p "$DEST"
cp "$FILE" "$DEST/$FILENAME"

cd "$REPO"
git add "static/"
git commit -m "publish $TYPE: $PROJECT/$FILENAME"
git push

echo ""
echo "Live in ~1-2 min at: $URL"
