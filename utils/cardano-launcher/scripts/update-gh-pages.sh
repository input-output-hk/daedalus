#!/usr/bin/env bash

set -euo pipefail

rev=$(git rev-parse --short HEAD)
old_head=$(git symbolic-ref HEAD)
cd $(git rev-parse --show-toplevel)
out="$(pwd)/site"

if ! git diff-index --quiet HEAD --; then
    echo "There are uncommitted changes - aborting!"
    exit 1
fi

echo "Fetching remote branches"
git fetch origin || true

echo "Building..."
rm -rf "$out"
npm run typedoc
touch "$out/.nojekyll"
mkdir -p "$out/docs"
make -C docs
cp docs/*.png docs/*.html docs/*.svg docs/*.js "$out/docs"

echo "Updating git index..."
git checkout gh-pages
trap "git symbolic-ref HEAD $old_head && git reset --hard" EXIT
git reset --hard origin/gh-pages
GIT_WORK_TREE="$out" git add -A

if git diff-index --cached --quiet HEAD --; then
  echo "No changes to commit, exiting."
  exit 0
fi

echo "Committing changes..."
GIT_WORK_TREE="$out" git commit --no-gpg-sign --message "Update gh-pages for $rev"
