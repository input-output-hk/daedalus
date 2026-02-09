---
description: Nix development environment setup
---

# Nix Workflow

This workflow guides setting up and using the Nix development environment for Daedalus.

## Overview

Daedalus uses **Nix** for reproducible builds. Nix provides:
- Exact versions of cardano-node and cardano-wallet
- Correct Node.js version
- Native build dependencies
- Cross-platform consistency

---

## Quick Commands

### Enter Development Shell

```bash
# Choose your network
yarn nix:mainnet     # Mainnet
yarn nix:preprod     # Preprod testnet
yarn nix:preview     # Preview testnet
yarn nix:selfnode    # Local self-node (testing)
```

### Verify Nix Shell

```bash
# Check cardano binaries
cardano-node --version
cardano-wallet version

# Check Node.js
node --version
```

---

## Installation

### Step 1: Install Nix

```bash
# Multi-user installation (recommended)
sh <(curl -L https://nixos.org/nix/install) --daemon
```

After installation:
```bash
# Restart terminal or source profile
source ~/.nix-profile/etc/profile.d/nix.sh

# Verify
nix --version
```

### Step 2: Enable Flakes

Add to `~/.config/nix/nix.conf` (create if needed):

```
experimental-features = nix-command flakes
```

### Step 3: Configure IOHK Binary Cache

Add to `~/.config/nix/nix.conf`:

```
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

**Important:** Without the IOHK cache, cardano-node will build from source (hours).

### Step 4: Verify Setup

```bash
# Check Nix health
nix doctor

# Test flake (from Daedalus root)
nix develop .#preprod --command echo "Nix works!"
```

---

## Development Shells

### Available Shells

| Shell    | Command             | Network            |
|----------|---------------------|--------------------|
| Mainnet  | `yarn nix:mainnet`   | Production Cardano |
| Preprod  | `yarn nix:preprod`   | Preprod testnet    |
| Preview  | `yarn nix:preview`   | Preview testnet    |
| Selfnode | `yarn nix:selfnode`  | Local testing node |

### What Each Shell Provides

```bash
# In any Nix shell, these are available:
cardano-node      # Full Cardano node
cardano-wallet    # Wallet backend
cardano-cli       # CLI tools
node              # Node.js (correct version)
yarn              # Yarn package manager
```

### Shell Environment Variables

Each shell sets network-specific variables:
- `NETWORK` - Network name
- `CARDANO_NODE_SOCKET_PATH` - Node socket path
- Various cardano configuration paths

---

## Nix Flake Configuration

### flake.nix

The `flake.nix` in the repository root defines development shells:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    cardano.url = "github:input-output-hk/cardano-node";
  };
  
  outputs = { self, nixpkgs, cardano }:
    let
      # Shell definitions
    in {
      devShells.x86_64-linux.mainnet = ...;
      devShells.x86_64-linux.preprod = ...;
    };
}
```

### flake.lock

The `flake.lock` pins exact versions of all dependencies.

---

## Common Operations

### Update Flake Inputs

```bash
# Update all inputs
nix flake update

# Update specific input
nix flake lock --update-input cardano
```

### Check What's Provided

```bash
# List available shells
nix flake show

# Check specific shell
nix develop .#preprod --command which cardano-node
```

### Clean Nix Store

```bash
# Remove old generations
nix-collect-garbage -d

# Deep clean (recovers disk space)
nix store gc
```

---

## Troubleshooting

### Nix Command Not Found

**Problem:** `nix: command not found` after installation
**Solution:**
```bash
# Add to shell profile
source ~/.nix-profile/etc/profile.d/nix.sh

# Or restart terminal
```

### Flakes Not Enabled

**Problem:** `error: experimental Nix feature 'flakes' is disabled`
**Solution:** Add to `~/.config/nix/nix.conf`:
```
experimental-features = nix-command flakes
```

### Slow Downloads

**Problem:** cardano-node takes hours to download/build
**Solution:** Verify IOHK cache:
```bash
nix config show | grep substituters
# Should include: https://cache.iog.io
```

If not configured, add to `~/.config/nix/nix.conf`:
```
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

### Permission Denied

**Problem:** Nix can't access store
**Solution:**
```bash
# For multi-user install, ensure nix-daemon is running
sudo systemctl start nix-daemon
sudo systemctl enable nix-daemon
```

### Disk Space Issues

**Problem:** Nix store taking too much space
**Solution:**
```bash
# Garbage collect
nix-collect-garbage -d

# Check store size
du -sh /nix/store
```

### Shell Won't Start

**Problem:** `yarn nix:*` hangs or fails
**Solution:**
1. Check network connectivity
2. Verify `GC_DONT_GC=1` is set (prevents GC during build)
3. Try with verbose output: `nix develop .#preprod --verbose`

---

## Platform-Specific Notes

### macOS

```bash
# May need Xcode command line tools
xcode-select --install

# For M1/M2 Macs, ensure Rosetta is installed
softwareupdate --install-rosetta
```

### Linux

```bash
# Ensure user is in correct groups
sudo usermod -aG nix-users $USER
```

### Windows (WSL2)

```bash
# Install Nix in WSL2
sh <(curl -L https://nixos.org/nix/install) --no-daemon

# Note: Use --no-daemon for WSL2
```

---

## Advanced Usage

### Custom Shell

```bash
# Enter shell with additional packages
nix develop .#preprod --command bash -c "export MY_VAR=value && bash"
```

### Check Derivation

```bash
# See what a shell provides
nix develop .#preprod --command printenv
```

### Build Without Entering Shell

```bash
# Just build dependencies
nix build .#preprod
```
