# Daedalus Agent Documentation Index

> **For AI Agents:** Read this file first to understand available documentation and when to reference each resource.

This folder contains all the documentation needed for AI agents to effectively assist with development on the Daedalus project. Use this index to quickly navigate to the relevant documentation for your current task.

---

## Quick Navigation

| Folder                        | Purpose                                | When to Read                                                               |
|-------------------------------|----------------------------------------|----------------------------------------------------------------------------|
| [`/system`](./system/)        | Architecture, APIs, state management   | **First**, for any architectural decisions or understanding system design  |
| [`/plans`](./plans/)          | PRDs and implementation plans history  | When implementing new features similar to past work                        |
| [`/SOPs`](./SOPs/)            | Standard Operating Procedures          | When encountering known issues or following established patterns           |
| [`/skills`](./skills/)        | Reusable task-specific playbooks       | When making domain-specific changes (CLI, frontend, testing)               |
| [`/workflows`](./workflows/)  | Step-by-step development workflows     | When executing specific development tasks                                  |

---

## Folder Details

### `/system` — Architecture & APIs

**The source of truth for major architectural decisions.**

Read these files to understand:
- Electron main/renderer process architecture
- IPC communication patterns
- MobX state management
- cardano-wallet REST API integration

Files:
- `architecture.md` — System overview, Electron architecture, IPC patterns, data flow
- `api-endpoints.md` — cardano-wallet REST API, IPC channels
- `state-management.md` — MobX store architecture and patterns

---

### `/plans` — Implementation History

**Successful PRDs and implementation plans for reference.**

Before implementing a feature:
1. Check if a similar feature was implemented before
2. Use past plans as templates for consistency
3. Follow established patterns from successful implementations

This folder uses subdirectories per major feature/task.

---

### `/SOPs` — Standard Operating Procedures

**Learnings from resolved issues and best practices.**

When an issue is resolved or a complex integration succeeds:
1. Document the step-by-step solution
2. Include common pitfalls and how to avoid them
3. Reference related code or configuration

**To create a new SOP**, ask the agent:
> "Generate SOP for [task/integration name]"

---

### `/skills` — Reusable Skills

**Reusable, task-specific playbooks organized by domain.**

#### Cardano CLI

| Skill                                                                  | Description                             |
|------------------------------------------------------------------------|-----------------------------------------|
| [`bech32-encoding-decoding`](./skills/bech32-encoding-decoding/SKILL.md) | Encode/decode bech32 strings            |
| [`cardano-cli-doctor`](./skills/cardano-cli-doctor/SKILL.md)          | Diagnose cardano-cli versions and flags |
| [`cardano-cli-plutus-scripts`](./skills/cardano-cli-plutus-scripts/SKILL.md) | Plutus script guidance              |
| [`cardano-cli-plutus-scripts-operator`](./skills/cardano-cli-plutus-scripts-operator/SKILL.md) | Plutus script execution (operator) |
| [`cardano-cli-staking`](./skills/cardano-cli-staking/SKILL.md)        | Staking operations guidance             |
| [`cardano-cli-staking-operator`](./skills/cardano-cli-staking-operator/SKILL.md) | Staking execution (operator)        |
| [`cardano-cli-transactions`](./skills/cardano-cli-transactions/SKILL.md) | Transaction building guidance      |
| [`cardano-cli-transactions-operator`](./skills/cardano-cli-transactions-operator/SKILL.md) | Transaction execution (operator)   |
| [`cardano-cli-wallets`](./skills/cardano-cli-wallets/SKILL.md)        | Wallet operations guidance              |
| [`cardano-cli-wallets-operator`](./skills/cardano-cli-wallets-operator/SKILL.md) | Wallet execution (operator)       |
| [`cardano-protocol-params`](./skills/cardano-protocol-params/SKILL.md) | Protocol parameters diagnostics     |
| [`cbor-encoding-decoding`](./skills/cbor-encoding-decoding/SKILL.md)  | CBOR encoding/decoding guidance          |

#### Frontend

| Skill                                                                  | Description                             |
|------------------------------------------------------------------------|-----------------------------------------|
| [`i18n-messaging`](./skills/frontend/i18n-messaging/SKILL.md)         | Manage react-intl i18n messaging        |
| [`theme-management`](./skills/frontend/theme-management/SKILL.md)     | CSS variables theme system              |
| [`storybook-creation`](../.claude/skills/storybook-creation/SKILL.md)  | Create/update Storybook stories         |

#### General

| Skill                                                                  | Description                             |
|------------------------------------------------------------------------|-----------------------------------------|
| [`e2e-test-creation`](./skills/e2e-test-creation/SKILL.md)            | Create Cucumber BDD e2e tests           |
| [`git-commit-formatter`](./skills/git-commit-formatter/SKILL.md)      | Conventional commit formatting          |

---

### `/workflows` — Development Workflows

**Step-by-step guides for common development tasks.**

Available workflows:

| Workflow                                  | Description                        | Trigger       |
|-------------------------------------------|------------------------------------|---------------|
| [`build.md`](./workflows/build.md)        | Nix shells and Yarn builds         | `/build`      |
| [`test.md`](./workflows/test.md)          | Jest and Cucumber testing          | `/test`       |
| [`electron.md`](./workflows/electron.md)  | Electron main process development  | `/electron`   |
| [`frontend.md`](./workflows/frontend.md)  | React/MobX development             | `/frontend`   |
| [`hardware-wallets.md`](./workflows/hardware-wallets.md) | Ledger/Trezor development | `/hardware-wallets` |
| [`ipc.md`](./workflows/ipc.md)            | IPC channel development            | `/ipc`        |
| [`nix.md`](./workflows/nix.md)            | Nix environment setup              | `/nix`        |
| [`storybook.md`](./workflows/storybook.md) | Storybook component work          | `/storybook`  |
| [`update-doc.md`](./workflows/update-doc.md) | Update documentation            | `/update-doc` |

---

## Project Overview

**Daedalus** is the official full-node cryptocurrency wallet for Cardano, built with Electron. It runs a full cardano-node and cardano-wallet backend, providing maximum security and decentralization.

### Architecture Overview

```
+-----------------------------------------------------------------------------+
|                          DAEDALUS WALLET (Electron)                         |
+-----------------------------------------------------------------------------+
|                                                                             |
|  +-------------------------------+    +-----------------------------------+ |
|  |      Renderer Process         |    |          Main Process             | |
|  |      (React/MobX UI)          |    |      (Node.js/Electron)           | |
|  +-------------------------------+    +-----------------------------------+ |
|  | source/renderer/app/          |    | source/main/                      | |
|  | +-- components/ (React)       |    | +-- cardano/                      | |
|  | +-- stores/ (MobX)            |    | |   +-- CardanoNode.ts            | |
|  | +-- api/ (HTTP client)        |    | |   +-- CardanoWalletLauncher.ts  | |
|  | +-- containers/               |    | |   +-- setup.ts                  | |
|  | +-- themes/                   |    | +-- ipc/ (IPC handlers)           | |
|  | +-- i18n/                     |    | +-- menus/ (native menus)         | |
|  +--------------+----------------+    | +-- trezor/ (Trezor Connect)      | |
|                 |                     | +-- windows/ (window mgmt)        | |
|                 |                     | +-- utils/ (logging, etc)         | |
|                 |                     +----------------+------------------+ |
|                 |                                      |                    |
|                 |         Electron IPC                |                     |
|                 +----------------+--------------------+                     |
|                                  |                                          |
+----------------------------------+------------------------------------------+
|                      REST API (localhost:8090)                              |
|                                  |                                          |
|  +-------------------------------+--------------------------------------+   |
|  |                        cardano-wallet (Haskell)                      |   |
|  |                    External process managed by Daedalus              |   |
|  +----------------------------------------------------------------------+   |
|                                  |                                          |
|  +-------------------------------+--------------------------------------+   |
|  |                         cardano-node (Haskell)                       |   |
|  |                    Full node syncing Cardano blockchain              |   |
|  +----------------------------------------------------------------------+   |
+-----------------------------------------------------------------------------+
```

### Technology Stack

| Layer                 | Technology            | Location                                      |
|-----------------------|-----------------------|-----------------------------------------------|
| **Desktop Framework** | Electron 24.2.0       | Root package.json                             |
| **UI Library**        | React 16.14.0         | source/renderer/                              |
| **State Management**  | MobX 5.15.7           | source/renderer/app/stores/                   |
| **Component Library** | react-polymorph 1.0.4 | Widgets and forms                             |
| **Styling**           | SCSS Modules          | Co-located with components                    |
| **Routing**           | React Router 5.2.0    | Hash history                                  |
| **i18n**              | react-intl 2.9.0      | EN, JA locales                                |
| **Build System**      | Nix + Webpack 5       | flake.nix, webpack configs                    |
| **Testing**           | Jest + Cucumber       | tests/, *.test.tsx                            |
| **Hardware Wallets**  | Ledger, Trezor        | @cardano-foundation/ledgerjs, @trezor/connect |

### Key Directories

```
daedalus/
├── source/
│   ├── main/                    # Electron main process
│   │   ├── cardano/             # Node/wallet management
│   │   ├── ipc/                 # IPC handlers
│   │   ├── menus/               # Native menus
│   │   ├── trezor/              # Trezor integration
│   │   ├── utils/               # Logging, processes
│   │   └── windows/             # Window management
│   ├── renderer/                # Electron renderer (React)
│   │   └── app/
│   │       ├── actions/         # MobX actions
│   │       ├── api/             # cardano-wallet API client
│   │       ├── components/      # React components (23 categories)
│   │       ├── containers/      # Container components
│   │       ├── stores/          # MobX stores
│   │       ├── themes/          # Theme definitions
│   │       └── i18n/            # Internationalization
│   └── common/                  # Shared code
│       ├── ipc/                 # IPC API contracts
│       └── types/               # TypeScript types
├── storybook/                   # Component stories
├── tests/                       # Cucumber E2E tests
├── installers/                  # Platform installers
├── nix/                         # Nix configuration
├── translations/                # i18n translation files
└── utils/                       # API importers, test wallets
```

---

## Quick Commands

### Development Environment
```bash
# Enter Nix development shell (required first)
yarn nix:mainnet          # Mainnet
yarn nix:preprod          # Preprod testnet
yarn nix:preview          # Preview testnet

# Start development mode
yarn dev                  # Runs both main and renderer
```

### Building
```bash
yarn build                # Production build
yarn build:main           # Build main process only
yarn build:renderer       # Build renderer only
yarn package              # Create installer
```

### Testing
```bash
yarn test:unit            # Cucumber unit tests
yarn test:e2e             # Cucumber E2E tests
yarn test:jest            # Jest tests
yarn storybook            # Component development
```

### Code Quality
```bash
yarn lint                 # ESLint
yarn compile              # TypeScript check
yarn prettier:check       # Prettier check
yarn check:all            # Run all checks
```

### Wallet Importers (Testing)
```bash
yarn shelley:wallet:importer    # Import Shelley test wallets
yarn byron:wallet:importer      # Import Byron test wallets
yarn mary:wallet:importer       # Import Mary test wallets
```

---

## Security Notes

- Never commit secrets or private keys
- Daedalus stores wallet data in user's app data directory
- Hardware wallet signing happens on device, not in Daedalus
- Recovery phrases are never stored unencrypted
