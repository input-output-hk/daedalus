---
description: Run Jest unit tests and Cucumber E2E tests
---

# Test Workflow

This workflow guides running tests for the Daedalus wallet.

## Quick Test Commands

### All Tests
```bash
yarn test           # Build + unit tests + E2E tests
```

### Unit Tests Only
```bash
yarn test:unit      # Cucumber unit tests
yarn test:jest      # Jest tests
```

### E2E Tests Only
```bash
yarn test:e2e       # Full E2E test suite
```

---

## Test Framework Overview

Daedalus uses multiple testing frameworks:

| Framework     | Purpose                  | Location                 |
|---------------|--------------------------|--------------------------|
| **Cucumber**  | BDD unit and E2E tests   | `tests/`                 |
| **Jest**      | Component unit tests     | `*.test.ts`, `*.spec.ts` |
| **Storybook** | Visual component testing | `storybook/`             |

---

## Cucumber Tests

### Unit Tests

```bash
yarn test:unit
```

This runs Cucumber tests tagged with `@unit`:
- Fast, isolated tests
- No Electron app required
- Tests business logic and utilities

**Watch mode:**
```bash
yarn test:unit:watch
```

**Run specific tag:**
```bash
yarn test:unit --tags '@unit and @wallets'
```

### E2E Tests

```bash
yarn test:e2e
```

This runs Cucumber tests tagged with `@e2e`:
- Launches full Electron app
- Tests user workflows
- Requires built app (`yarn build` first)

**Fail-fast mode:**
```bash
yarn test:e2e:fail-fast
```

**Rerun failed tests:**
```bash
yarn test:e2e:rerun
```

### Cucumber Configuration

Test setup is in:
- `tests/setup-common.ts` - Common setup for all tests
- `tests/setup-e2e.ts` - E2E-specific setup

### Cucumber Tags

| Tag        | Purpose                      |
|------------|------------------------------|
| `@unit`    | Unit tests                   |
| `@e2e`     | End-to-end tests             |
| `@skip`    | Skip test                    |
| `@wip`     | Work in progress (skipped)   |
| `@watch`   | Run in watch mode            |
| `@unbound` | Unbound/long-running tests   |

### Run Specific Tests

```bash
# By tag
yarn test:unit --tags '@unit and @wallets'

# Specific feature file
yarn cucumber:run tests/wallets/unit/wallet-creation.feature
```

---

## Jest Tests

### Run All Jest Tests

```bash
yarn test:jest
```

### Run Specific Test File

```bash
yarn test:jest source/renderer/app/stores/SidebarStore.spec.ts
```

### Watch Mode

```bash
yarn test:jest --watch
```

### Jest Configuration

Jest config is in `jest.config.js`:
- Uses SWC for fast TypeScript compilation
- CSS modules mocked with `identity-obj-proxy`
- jsdom environment for React components

---

## Test Directory Structure

```
tests/
├── setup-common.ts          # Common test setup
├── setup-e2e.ts             # E2E test setup
├── reporter.ts              # Test report generator
├── wallets/
│   ├── unit/                # Wallet unit tests
│   │   └── *.ts
│   └── e2e/                 # Wallet E2E tests
│       └── *.ts
├── transactions/
│   ├── unit/
│   └── e2e/
├── staking/
│   ├── unit/
│   └── e2e/
├── settings/
│   ├── unit/
│   └── e2e/
└── ...
```

---

## Hardware Wallet Tests

```bash
yarn test:hardware-wallets
```

This runs the legacy interactive Ledger diagnostic in
`hardware-wallet-tests/index.ts`. It currently checks connection, public-key and
disconnect behavior only; it does not test Trezor, exact transactions, CIP-8,
cancellation or the task-607 physical certification matrix.

**Requirements:**
- Connected Ledger device
- Cardano app installed on the Ledger device

Static capability-contract validation requires no device:

```bash
yarn test:jest source/common/hardware/hardwareCapabilityMatrix.spec.ts --runInBand --coverage=false
```

---

## Storybook (Visual Testing)

```bash
yarn storybook
```

Opens Storybook at `http://localhost:6006` for visual component testing.

### Build Storybook

```bash
yarn storybook:build
```

Output: `dist/storybook/`

---

## Linux release and migration checks

Run the focused reproducible checks after Linux package, launcher, CI, or
release-tool changes:

```bash
nix build -L .#checks.x86_64-linux.linux-release-artifacts-contract
nix build -L .#checks.x86_64-linux.linux-deb-package-contract
nix build -L .#checks.x86_64-linux.linux-rpm-package-contract
nix build -L .#checks.x86_64-linux.linux-remaining-launcher-contract
nix build -L .#checks.x86_64-linux.drt-test
```

These checks cover the declared Linux artifact surface (`.deb`/`.rpm` only),
package archive/layout contracts, the wallet-only developer launcher, and the
release CLI's `linux-deb`/`linux-rpm` manifest plus newsfeed behavior. They must
also ensure no Linux package appears in `softwareUpdate`; Linux receives one
ordinary release/migration announcement.

Archive and build checks do **not** prove privileged package lifecycle,
installed Chromium containment, or non-destructive migration. Those require
approved disposable-host scenarios using exact built artifacts:

1. Fresh-install the `.deb` with `apt` and the `.rpm` with `dnf`, then launch
   the exact `/usr/bin/daedalus-<cluster>`.
2. Upgrade from the prior package with the same package manager while Daedalus
   is fully stopped; verify package files, policy state, and launcher identity.
3. Seed non-sensitive fixture state under
   `${XDG_DATA_HOME:-$HOME/.local/share}/Daedalus`, preserve its before/after
   identity, and migrate from a legacy `$HOME/.daedalus/<cluster>` executable
   without moving or deleting that state.
4. Repeat migration with a custom `XDG_DATA_HOME`, passing the same value on
   first launch, and prove the expected fixture state opens.
5. Create a legacy user desktop symlink, verify that it shadows the system
   desktop entry, move aside only that verified symlink, and prove the system
   package launches. Separately verify that a stale `namespaceHelper` symlink
   can remain without affecting startup.

Never run package lifecycle or destructive rollback experiments on a real
wallet-bearing host. Record disposable-host install, upgrade, rollback,
AppArmor/SELinux, exact-renderer, and migration evidence separately from the
Nix build result.

---

## Writing New Tests

For guidance on creating new Cucumber BDD tests (feature files, step definitions, helpers), see the [e2e-test-creation skill](../skills/e2e-test-creation/SKILL.md).

---

## Test Reports

### Generate Report

```bash
yarn test:generate:report
```

Generates HTML report from Cucumber results.

### Report Location

| Report            | Location                         |
|-------------------|----------------------------------|
| Cucumber JSON     | `tests-report/report-data.json`  |
| Cucumber Summary  | `tests-report/summary.log`       |
| Cucumber Results  | `tests-report/results.log`       |
| Failed Tests      | `tests-report/@rerun.txt`        |

---

## Wallet Importers (Test Data)

For testing with real wallet data:

```bash
# Shelley era wallets
yarn shelley:wallet:importer

# Byron era wallets
yarn byron:wallet:importer

# Mary era wallets (native tokens)
yarn mary:wallet:importer

# Yoroi wallets
yarn yoroi:wallet:importer

# ITN (Incentivized Testnet) wallets
yarn itn:byron:wallet:importer
yarn itn:shelley:wallet:importer
```

These import test wallets from `utils/api-importer/`.

---

## CI Test Pipeline

### Test Command

```bash
yarn test
```

This runs:
1. `yarn build` - Production build
2. `yarn test:unit` - Unit tests
3. `yarn test:e2e:fail-fast` - E2E tests (stops on first failure)

### Windows Jest

`.github/workflows/windows-tests.yml` runs Node 24 with:

```bash
yarn install --frozen-lockfile --ignore-scripts
yarn test:jest --coverage=false --ci
```

Node-based specs must explicitly mock unused Electron integration boundaries:
the install intentionally omits Electron's binary download and native builds.
Platform simulations must supply the target platform's filesystem constants and
path semantics, rather than inheriting them from the test host. POSIX file-mode
assertions apply only outside Windows; persistence checks run on every platform.
TypeScript audit paths must use consistent separators, including virtual files.

The hardware case generator explicitly buffers complete CBOR candidates and
canonical round trips, so the 65,536/65,537-byte boundary fixtures do not depend
on the host's default stream buffer size.

The hardware provenance probe awaits asynchronous Prettier formatting before
writing evidence. Live dependency identities are checked against the current
lockfile rather than historical installation layout or whole-lock snapshots.
Archived evidence hashes remain unchanged. `.gitattributes` pins hash-sensitive
lockfiles and fixture JSON to LF so Windows checkout conversion cannot alter
the evidence bytes.
The probe discovers Yarn's cache with `yarn cache dir --json`, respecting
Windows locations, `.yarnrc`, and `YARN_CACHE_FOLDER` without interpreting
terminal control sequences as path bytes.

### Hermetic Hydra checks

Nix checks reuse prebuilt `node_modules`, not the installer's home-directory
cache. `perSystem/checks.nix` declares the pinned offline mirror through
`DAEDALUS_YARN_OFFLINE_CACHE`; the probe verifies the tarball against both the
original lock's SHA-1 and SRI before loading vendor code. No network access or
registry-cache metadata is required in this mode.

Reviewed JSON evidence is excluded consistently from `.prettierignore` and
`perSystem/formatter.nix`. Do not reformat or re-pin historical observations to
make current-contract checks pass. Conformance scenarios execute the current
request/result validators against independent normative examples; unchanged
adapter byte fixtures remain bound to their historical hashes.

Use the actual Linux derivations to verify changes to this environment:

```bash
nix build --no-link -L \
  .#checks.x86_64-linux.compile .#checks.x86_64-linux.jest \
  .#checks.x86_64-linux.cucumber-unit .#checks.x86_64-linux.crypto-vectors \
  .#checks.x86_64-linux.lint .#checks.x86_64-linux.stylelint \
  .#checks.x86_64-linux.treefmt
```

### Before pushing

There are no git hooks in this repository, so nothing runs automatically on
commit or push. Run the checks yourself:

```bash
yarn check:all
```

---

## Troubleshooting

### E2E Tests Fail to Start

**Problem:** Electron app doesn't launch
**Solution:**
1. Ensure app is built: `yarn build`
2. Check if another instance is running
3. Verify Nix shell is active

### Tests Hang

**Problem:** Tests don't complete
**Solution:**
```bash
# Kill any stuck processes
pkill -f electron
pkill -f cardano

# Run with timeout
timeout 300 yarn test:e2e
```

### Cucumber Tests Not Found

**Problem:** "No scenarios found"
**Solution:**
- Check tag syntax: `--tags '@unit and not @skip and not @wip'`
- Verify feature files exist in `tests/`
- Check file patterns in `yarn cucumber:run`

### Jest Module Resolution

**Problem:** Cannot find module
**Solution:**
- Run `yarn install` to ensure dependencies
- Check `jest.config.js` module mappings
- Verify TypeScript paths in `tsconfig.json`

### Mock IPC in Tests

For unit testing renderer components that use IPC:

```typescript
import { ipcRenderer } from 'electron-mock-ipc';

// Mock IPC responses
ipcRenderer.on('MY_CHANNEL', (event, data) => {
  event.sender.send('MY_CHANNEL_RESPONSE', mockData);
});
```

---

## Environment Variables

| Variable                | Purpose                              |
|-------------------------|--------------------------------------|
| `NODE_ENV=test`         | Test mode                            |
| `KEEP_APP_AFTER_TESTS`  | Don't close app after E2E tests      |
| `DEBUG`                 | Enable debug logging                 |
