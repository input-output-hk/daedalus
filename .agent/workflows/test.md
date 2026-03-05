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

This runs hardware wallet integration tests in `hardware-wallet-tests/`.

**Requirements:**
- Connected Ledger or Trezor device
- Cardano app installed on device

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

### Pre-commit Checks

The pre-commit hook runs:
```bash
pretty-quick --staged
```

### Pre-push Checks

The pre-push hook runs:
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
