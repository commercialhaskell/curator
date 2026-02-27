# Update all dependencies: nixpkgs, LTS snapshot, and nix package definitions
update-deps:
    nix flake update
    nix run .#sync-lts
    nix run .#gen-packages

# Update just the nixpkgs flake input
update-nixpkgs:
    nix flake update

# Sync stack.yaml snapshot with nixpkgs LTS version
sync-lts:
    nix run .#sync-lts

# Regenerate nix package definitions from cabal files
gen-packages:
    nix run .#gen-packages

# Build with nix
build:
    nix build

# Push build outputs to cachix
push-cachix:
    # Outputs
    nix build --json | jq -r '.[].outputs | to_entries[].value' | cachix push stackage-infrastructure
    # Inputs
    nix flake archive --json | jq -r '.path,(.inputs|to_entries[].value.path)' | cachix push stackage-infrastructure
    # Shell
    nix develop --profile .dev-profile -c true
    cachix push stackage-infrastructure .dev-profile

# Build with stack
build-stack:
    stack build

# Run tests with stack
test-stack:
    stack test

# Enter development shell
dev:
    nix develop
