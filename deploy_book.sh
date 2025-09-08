#!/bin/bash
set -e

CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
echo "Current branch: $CURRENT_BRANCH"

BRANCH="gh-pages"
BUILD_DIR="_book"

# Remove any existing gh-pages worktree
if git worktree list | grep -q "$BRANCH"; then
    EXISTING_PATH=$(git worktree list | grep "$BRANCH" | awk '{print $1}')
    echo "Removing existing worktree at $EXISTING_PATH"
    git worktree remove "$EXISTING_PATH"
fi

# Create a new temporary directory for worktree
TMP_DIR=$(mktemp -d)
echo "Adding worktree in $TMP_DIR"
git worktree add $TMP_DIR $BRANCH

# Copy compiled book files into worktree
cp -r $BUILD_DIR/* $TMP_DIR/

# Commit and force push
pushd $TMP_DIR
git add .
git commit -m "Update bookdown site $(date +'%Y-%m-%d %H:%M:%S')" || echo "Nothing to commit"
git push origin $BRANCH --force
popd

# Remove temporary worktree
git worktree remove $TMP_DIR

# Return to original branch
git checkout $CURRENT_BRANCH
echo "Deployment complete! Back on branch: $CURRENT_BRANCH"
