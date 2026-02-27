#!/usr/bin/env bash
#
# Check for new Cabal/Cabal-syntax versions on Hackage and update if found.
#
# Usage: nix run .#update-cabal
#
# This script:
# 1. Queries Hackage for latest Cabal and Cabal-syntax versions
# 2. Updates stack.yaml extra-deps if newer versions exist
# 3. Regenerates nix package definitions

set -Eeuo pipefail

# Retry a command with exponential backoff
retry() {
    local max_attempts=3
    local delay=2
    local attempt=1
    local output

    while (( attempt <= max_attempts )); do
        if output=$("$@" 2>&1); then
            echo "$output"
            return 0
        fi
        echo "Attempt $attempt/$max_attempts failed, retrying in ${delay}s..." >&2
        sleep "$delay"
        (( attempt++ ))
        (( delay *= 2 ))
    done

    echo "All $max_attempts attempts failed" >&2
    return 1
}

# Get latest version from Hackage
get_latest_version() {
    local pkg="$1"
    retry curl -sH "Accept: application/json" "https://hackage.haskell.org/package/$pkg/preferred" | jq -r '.["normal-version"][0]'
}

# Get current version from stack.yaml
get_current_version() {
    local pkg="$1"
    grep -oP "^  - ${pkg}-\\K[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" stack.yaml || echo ""
}

echo "Checking Hackage for latest Cabal versions..."

cabal_latest=$(get_latest_version "Cabal")
cabal_syntax_latest=$(get_latest_version "Cabal-syntax")

echo "Latest on Hackage:"
echo "  Cabal: $cabal_latest"
echo "  Cabal-syntax: $cabal_syntax_latest"

cabal_current=$(get_current_version "Cabal")
cabal_syntax_current=$(get_current_version "Cabal-syntax")

echo "Current in stack.yaml:"
echo "  Cabal: $cabal_current"
echo "  Cabal-syntax: $cabal_syntax_current"

updated=false

if [[ "$cabal_current" != "$cabal_latest" ]]; then
    echo "Updating Cabal: $cabal_current -> $cabal_latest"
    sed -i "s/- Cabal-${cabal_current}/- Cabal-${cabal_latest}/" stack.yaml
    updated=true
fi

if [[ "$cabal_syntax_current" != "$cabal_syntax_latest" ]]; then
    echo "Updating Cabal-syntax: $cabal_syntax_current -> $cabal_syntax_latest"
    sed -i "s/- Cabal-syntax-${cabal_syntax_current}/- Cabal-syntax-${cabal_syntax_latest}/" stack.yaml
    updated=true
fi

if [[ "$updated" == "true" ]]; then
    echo "Regenerating nix package definitions..."
    # Regenerate just the Cabal packages (with retries for Hackage fetches)
    retry cabal2nix "cabal://Cabal-${cabal_latest}" > nix/packages/Cabal.nix
    retry cabal2nix "cabal://Cabal-syntax-${cabal_syntax_latest}" > nix/packages/Cabal-syntax.nix
    echo "Done! Cabal packages updated."
else
    echo "Already up to date."
fi
