#!/bin/bash
set -e

# Save current branch
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
echo "Current branch: $CURRENT_BRANCH"

BRANCH="gh-pages"
BUILD_DIR="_book"

# Switch to gh-pages
git fetch origin $BRANCH || git checkout --orphan $BRANCH
git checkout $BRANCH

# Copy compiled book files
cp -r ../${BUILD_DIR}/* .

# Commit and push
git add .
git commit -m "Update bookdown site $(date +'%Y-%m-%d %H:%M:%S')" || echo "Nothing to commit"
git push origin $BRANCH

# Return to the original branch
git checkout $CURRENT_BRANCH
echo "Returned to branch: $CURRENT_BRANCH"

echo "Deployment complete!"
