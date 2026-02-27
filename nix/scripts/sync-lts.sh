#!/usr/bin/env bash
#
# Sync stack.yaml resolver with the LTS version used by nixpkgs.
#
# Usage: nix run .#sync-lts
#
# This script:
# 1. Reads the nixpkgs rev from flake.lock
# 2. Searches nixpkgs commit history for the haskellPackages LTS update
# 3. Updates stack.yaml with the matching LTS version

set -Eeuo pipefail

# Extract nixpkgs rev from flake.lock
nixpkgs_rev=$(jq -r '.nodes.nixpkgs.locked.rev' flake.lock)
nixpkgs_date=$(jq -r '.nodes.nixpkgs.locked.lastModified' flake.lock)
# Convert epoch to ISO date
nixpkgs_date_iso=$(date -d "@$nixpkgs_date" -u +%Y-%m-%dT%H:%M:%SZ)

echo "nixpkgs rev: $nixpkgs_rev"
echo "nixpkgs date: $nixpkgs_date_iso"

# Search for LTS commits in nixpkgs using GitHub search API
# We search for commits before the pinned date
echo "Searching for LTS version in nixpkgs history..."

search_result=$(gh api "search/commits" \
    -X GET \
    -f q="repo:NixOS/nixpkgs haskellPackages stackage LTS committer-date:<=$nixpkgs_date_iso" \
    -f sort="committer-date" \
    -f order="desc" \
    -f per_page=1 \
    --jq '.items[0].commit.message' 2>&1)

if [[ -z "$search_result" || "$search_result" == "null" ]]; then
    echo "Error: Could not find LTS commit via search" >&2
    exit 1
fi

echo "Found commit: $search_result"

# Extract the target LTS version (the one after "->")
lts_version=$(echo "$search_result" | grep -oP 'LTS \K[0-9]+\.[0-9]+' | tail -1)

if [[ -z "$lts_version" ]]; then
    echo "Error: Could not parse LTS version from commit message" >&2
    echo "Commit message: $search_result" >&2
    exit 1
fi

echo "LTS version: $lts_version"

# Update stack.yaml
current_snapshot=$(grep -oP '^snapshot: lts-\K[0-9]+\.[0-9]+' stack.yaml || echo "")

if [[ "$current_snapshot" == "$lts_version" ]]; then
    echo "stack.yaml already uses lts-$lts_version"
else
    echo "Updating stack.yaml: lts-$current_snapshot -> lts-$lts_version"
    sed -i "s/^snapshot: lts-.*/snapshot: lts-$lts_version/" stack.yaml
fi

echo "Done!"
