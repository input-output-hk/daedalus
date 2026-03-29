# Daedalus AI Agent Instructions

> **For all AI coding assistants.** This file provides a starting point for understanding the Daedalus codebase.

---

## Quick Start

**Read the full documentation:** [`.agent/readme.md`](./.agent/readme.md)

The `.agent/` directory contains comprehensive documentation organized for AI agents:

| Folder              | Purpose                                  | When to Read                     |
|---------------------|------------------------------------------|----------------------------------|
| `.agent/system/`    | Architecture, APIs, state management     | Understanding system design      |
| `.agent/plans/`     | Past PRDs and implementation plans       | Before implementing new features |
| `.agent/SOPs/`      | Standard operating procedures            | When encountering known issues   |
| `.agent/workflows/` | Step-by-step guides                      | When executing specific tasks    |
| `.agent/skills/`    | Reusable task-specific playbooks         | When making domain-specific changes |

---

## Available Workflows

Use these slash commands to access workflows:

| Command       | Description                                  |
|---------------|----------------------------------------------|
| `/build`      | Build with Nix and Yarn                      |
| `/test`       | Run Jest unit tests and Cucumber E2E tests   |
| `/electron`   | Electron main process development            |
| `/frontend`   | React/MobX renderer development              |
| `/storybook`  | Component development with Storybook         |
| `/hardware-wallets` | Ledger/Trezor hardware wallet development |
| `/ipc`        | IPC channel development                       |
| `/nix`        | Nix environment setup                         |
| `/agentic-kb` | KB workflow for Compose boot, sync, snapshots, and read-only MCP |
| `/update-doc` | Update this documentation                    |

For KB operations, use [`.agent/workflows/agentic-kb.md`](./.agent/workflows/agentic-kb.md) as the workflow source of truth. Use [`agentic/README.md`](./agentic/README.md) for copy-paste OpenCode, Claude Code, and local `.mcp.json` MCP client setup examples.

---

## Project Overview

**Daedalus** is the official full-node cryptocurrency wallet for Cardano, built with Electron. It runs a full cardano-node and cardano-wallet backend, providing maximum security and decentralization.

### Tech Stack

| Layer              | Technology                  |
|--------------------|-----------------------------|
| Desktop Framework  | Electron 24.2.0             |
| UI Library         | React 16.14.0               |
| State Management   | MobX 5.15.7                 |
| Component Library  | react-polymorph 1.0.4       |
| Styling            | SCSS Modules                |
| Routing            | React Router 5.2.0          |
| i18n               | react-intl 2.9.0 (EN, JA)   |
| Build System       | Nix + Webpack 5             |
| Testing            | Jest + Cucumber             |
| Hardware Wallets   | Ledger, Trezor              |

### Key Directories

| Directory          | Purpose                           |
|--------------------|-----------------------------------|
| `source/main/`     | Electron main process (Node.js)   |
| `source/renderer/` | React UI (browser context)        |
| `source/common/`   | Shared code and IPC contracts     |
| `storybook/`       | Component stories                 |
| `tests/`           | Cucumber E2E and unit tests       |
| `installers/`      | Platform-specific installers      |
| `nix/`             | Nix build configuration           |
| `utils/`           | API importers, test wallets       |

---

## Important Patterns

### Electron Main Process (`source/main/`)
- Manages cardano-node and cardano-wallet lifecycle
- IPC handlers in `source/main/ipc/`
- Hardware wallet integration (Ledger, Trezor)
- Window management and native menus

### React Renderer (`source/renderer/app/`)
- MobX stores in `stores/` for state management
- React components in `components/` (23 categories)
- Container components in `containers/`
- API client in `api/` for cardano-wallet REST calls
- IPC client in `ipc/` for main process communication

### IPC Communication (`source/common/ipc/`)
- Type-safe IPC channels between main and renderer
- Channel definitions in `source/common/ipc/api.ts`
- Main-side handlers in `source/main/ipc/`
- Renderer-side clients in `source/renderer/app/ipc/`

---

## Common Commands

### Development
```bash
yarn nix:mainnet          # Enter Nix shell (mainnet)
yarn nix:preprod          # Enter Nix shell (preprod testnet)
yarn dev                  # Start development mode
```

### Building
```bash
yarn build                # Production build
yarn package              # Create installer
```

### Testing
```bash
yarn test:unit            # Run unit tests
yarn test:e2e             # Run E2E tests
yarn test:jest            # Run Jest tests
yarn storybook            # Run Storybook
```

### Code Quality
```bash
yarn lint                 # ESLint
yarn compile              # TypeScript check
yarn prettier:check       # Prettier check
```

---

## Before You Start

1. Read [`.agent/readme.md`](./.agent/readme.md) for full documentation index
2. Check [`.agent/system/architecture.md`](./.agent/system/architecture.md) for system overview
3. Review relevant workflow in [`.agent/workflows/`](./.agent/workflows/)

---

## Security Notes

- Never commit secrets or private keys
- Daedalus stores wallet data in user's app data directory
- Hardware wallet signing happens on device, not in Daedalus
