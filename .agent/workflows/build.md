---
description: Build Daedalus with Nix and Yarn
---

# Build Workflow

This workflow guides building all components of the Daedalus wallet.

## Prerequisites

### Nix Installation

Daedalus uses Nix for reproducible builds. Install Nix with IOHK binary cache:

```bash
# Install Nix
sh <(curl -L https://nixos.org/nix/install) --daemon

# Configure IOHK binary cache (add to ~/.config/nix/nix.conf)
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

### Node.js

Node.js 14.18.1+ is required. Nix shell provides the correct version.

---

## Quick Build Commands

### Enter Nix Shell (Required First)

```bash
# Choose your network
yarn nix:mainnet     # Mainnet
yarn nix:preprod     # Preprod testnet
yarn nix:preview     # Preview testnet
yarn nix:selfnode    # Local self-node
```

This provides:
- `cardano-node` and `cardano-wallet` binaries
- Correct Node.js version
- Native dependencies

### Development Build

```bash
# Inside Nix shell
yarn dev
```

This runs:
- Webpack dev server for renderer (port 8080)
- Webpack watch for main process
- Hot reloading for React components

### Production Build

```bash
# Inside Nix shell
yarn build
```

This creates optimized bundles in `dist/`:
- `dist/main/index.js` - Main process bundle
- `dist/renderer/` - Renderer process bundle

### Create a Yarn package

```bash
# Inside Nix shell
yarn package
```

This produces Electron packaging output in `release/` for development. It is
not the Linux product-package or release pipeline. Linux release artifacts must
be built through the `.deb`/`.rpm` Nix outputs below; `yarn package` does not
create a supported portable Linux channel.

---

## Detailed Build Steps

### Step 1: Enter Nix Development Shell

```bash
yarn nix:mainnet
```

**What this does:**
- Loads `flake.nix` configuration
- Downloads cardano-node and cardano-wallet from IOHK cache
- Sets up environment variables
- Provides native build tools

**Troubleshooting:**
- If Nix download is slow, verify IOHK cache is configured
- Run `nix doctor` to check Nix installation
- Ensure `GC_DONT_GC=1` is set (prevents garbage collection during build)

### Step 2: Install Node Dependencies

```bash
yarn install
```

**Note:** This installs npm packages. Native modules are rebuilt for Electron.

### Step 3: Build Main Process

```bash
yarn build:main
```

**Output:** `dist/main/index.js`

This bundles:
- `source/main/` - Electron main process
- `source/common/` - Shared code

### Step 4: Build Renderer Process

```bash
yarn build:renderer
```

**Output:** `dist/renderer/`

This bundles:
- `source/renderer/` - React application
- `source/common/` - Shared code
- SCSS styles and assets

### Step 5: Rebuild Native Modules (if needed)

```bash
yarn build:electron
```

This rebuilds native Node modules for the Electron version.

---

## Build Outputs

| Output       | Location             | Description             |
|--------------|----------------------|-------------------------|
| Main process | `dist/main/index.js` | Electron main bundle    |
| Renderer     | `dist/renderer/`     | React app bundle        |
| Storybook    | `dist/storybook/`    | Component documentation |
| Installer    | `release/`           | Platform installers     |

---

## Webpack Configuration

Daedalus uses separate Webpack configs for main and renderer:

| Config   | Location                            | Purpose               |
|----------|-------------------------------------|-----------------------|
| Main     | `source/main/webpack.config.js`     | Node.js target        |
| Renderer | `source/renderer/webpack.config.js` | Web target with React |

### Key Webpack Features

- **SWC Loader**: Fast TypeScript/JSX compilation
- **SCSS Modules**: Scoped CSS with type definitions
- **Hot Reload**: Development mode React refresh
- **Source Maps**: Enabled in development

---

## Development Mode

```bash
yarn dev
```

This runs concurrently:
1. **Renderer dev server** on `localhost:8080`
2. **Main process watcher** rebuilds on changes

### Development Features

- Hot module replacement for React
- TypeScript type checking
- Automatic Electron restart on main changes
- Source maps for debugging

---

## Code Quality Checks

Run before committing:

```bash
yarn check:all
```

This runs:
- `yarn prettier:check` - Code formatting
- `yarn lint` - ESLint
- `yarn compile` - TypeScript type check
- `yarn stylelint` - SCSS linting
- `yarn i18n:manage` - Translation validation
- `yarn storybook:build` - Storybook compilation

### Individual Checks

```bash
yarn lint              # ESLint
yarn lint:fix          # Fix ESLint issues
yarn compile           # TypeScript check
yarn prettier:check    # Check formatting
yarn prettier:format   # Fix formatting
yarn stylelint         # SCSS linting
yarn stylelint:fix     # Fix SCSS issues
```

---

## Platform-Specific Builds

### Windows

```bash
# In Nix shell
yarn package
```

Creates `.exe` installer.

### macOS

```bash
# In Nix shell
yarn package
```

Creates `.dmg` installer.

### Linux

```bash
# In Nix shell; choose the required cluster suffix
nix build -L .#deb-installer-mainnet
nix build -L .#rpm-installer-mainnet
```

These are the complete native Linux release output set. They create the
root-installable system `.deb` and `.rpm` outputs under `result/`; cluster
variants use `deb-installer-{mainnet,preprod,preview,selfnode}` and
`rpm-installer-{mainnet,preprod,preview,selfnode}`. Linux has no
`installer-<cluster>` or `makeSignedInstaller-<cluster>` output. The
`daedalus-<cluster>` Nix package remains a wallet-only developer artifact, not a
shipping channel.

The `.deb`/`.rpm` packages install under fixed
`/opt/daedalus/<cluster>` paths. Only those installed fixed-path packages can
satisfy the Linux dApp package-identity and sandbox gates. Portable `.bin`,
AppImage, Flatpak, and Snap are rejected product formats.

---

## CI and release flow

Buildkite invokes the Linux artifact pipeline with:

```bash
nix </dev/null run --no-accept-flake-config -L .#packages.x86_64-linux.buildkitePipeline
```

For each cluster, that pipeline builds and uploads only
`deb-installer-<cluster>` and `rpm-installer-<cluster>` on Linux. Hydra exposes
the same artifacts under these exact job families:

```text
deb-installer.x86_64-linux.<cluster>
rpm-installer.x86_64-linux.<cluster>
```

Non-Linux Hydra artifacts continue to use
`installer.<system>.<cluster>`. A release operator first fetches, signs, tests,
and publishes the combined installer manifest with the existing `drt` flow:

```bash
nix develop .#ops
drt fetch-installers --url https://ci.iog.io/eval/<ID> --env <cluster> -o installers/<cluster>/
drt sign installers/<cluster>/
drt serve --installers installers/<cluster>/
drt release installers/<cluster>/ --bucket <bucket> --bucket-url <public-host>
```

The release manifest records the two Linux packages independently as
`linux-deb` and `linux-rpm`; the pair must be present together and all artifacts
must report one release version. Neither package is emitted in
`softwareUpdate`. Both map to target OS `linux`.

**`drt release` alone does not notify legacy Linux users.** After the published
`daedalus-latest-version.json` is reachable, generate the production newsfeed
changes in both newsfeed repositories:

```bash
drt newsfeed release \
  --env <cluster> \
  --newsfeed-repo ../daedalus-newsfeed-content \
  --verification-repo ../daedalus-newsfeed-verification \
  --installer-json https://<public-host>/daedalus-latest-version.json \
  --release-notes https://github.com/input-output-hk/daedalus/releases/tag/<version>
```

`drt newsfeed release` opens the operator's editor before writing. Review the
resulting content and verification files together: macOS/Windows keep their
app-managed software-update entries, Linux has no `softwareUpdate` URL/hash,
and the deb/rpm pair is deduplicated into one ordinary Linux
release/migration announcement linking the release notes and manual
package-manager instructions. Verify target versions/platforms and the
verification file before publication.

Optionally exercise those reviewed repository files against the test bucket:

```bash
drt newsfeed publish \
  --env <cluster> \
  --newsfeed-repo ../daedalus-newsfeed-content \
  --verification-repo ../daedalus-newsfeed-verification \
  --bucket <test-bucket> \
  --bucket-url <test-public-host> \
  --dry-run
# Remove --dry-run only for the approved test publication.
```

Finally commit the reviewed generated file in
`daedalus-newsfeed-content` and its matching verification file in
`daedalus-newsfeed-verification`, then publish/push both through the approved
newsfeed repository release process. Only that newsfeed publication makes the
ordinary announcement visible to legacy portable users.

The Linux application never executes package bytes or invokes a package
manager. Users close Daedalus and manually install each newer local package
with `apt` or `dnf`; legacy `.bin` users receive the published announcement but
no new portable artifact.

### Build Caching

- Nix uses IOHK binary cache for cardano binaries
- Webpack uses `node_modules/.cache` for incremental builds
- Clear cache with `yarn clear:cache`

---

## Troubleshooting

### Nix Shell Won't Start

**Problem:** `nix develop` fails
**Solution:**
1. Check Nix installation: `nix --version`
2. Verify flake support: ensure `experimental-features = nix-command flakes` in nix.conf
3. Check IOHK cache configuration

### Native Module Errors

**Problem:** `node-hid` or other native module fails
**Solution:**
```bash
yarn build:electron
```

### Out of Memory

**Problem:** Webpack runs out of memory
**Solution:**
```bash
export NODE_OPTIONS="--max-old-space-size=4096"
yarn build
```

### Slow Nix Downloads

**Problem:** cardano-node download is slow
**Solution:** Verify IOHK binary cache is configured:
```bash
nix config show | grep substituters
# Should include https://cache.iog.io
```

---

## Environment Variables

| Variable                     | Purpose              | Default           |
|------------------------------|----------------------|-------------------|
| `NODE_ENV`                   | Build mode           | `development`     |
| `NETWORK`                    | Cardano network      | From Nix shell    |
| `DAEDALUS_INSTALL_DIRECTORY` | Install path         | Platform-specific |
| `LAUNCHER_CONFIG`            | Launcher config path | Platform-specific |
