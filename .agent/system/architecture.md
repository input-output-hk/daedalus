# Daedalus System Architecture

> **Source of truth for system-wide architectural decisions.**

This document provides the definitive overview of the Daedalus system architecture, process relationships, and core design patterns.

---

## System Overview

Daedalus is the official full-node cryptocurrency wallet for Cardano, built with Electron. It runs a complete cardano-node and cardano-wallet backend locally, providing maximum security and decentralization.

```
┌──────────────────────────────────────────────────────────────────────────────┐
│                          DAEDALUS WALLET (Electron)                          │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────────────────────┐    ┌───────────────────────────────────┐    │
│  │      Renderer Process       │    │          Main Process             │    │
│  │      (React/MobX UI)        │    │      (Node.js/Electron)           │    │
│  ├─────────────────────────────┤    ├───────────────────────────────────┤    │
│  │ source/renderer/app/        │    │ source/main/                      │    │
│  │ ├── components/ (React)     │    │ ├── cardano/                      │    │
│  │ ├── stores/ (MobX)          │    │ │   ├── CardanoNode.ts            │    │
│  │ ├── api/ (REST client)      │    │ │   ├── CardanoWalletLauncher.ts  │    │
│  │ ├── containers/             │    │ │   └── setup.ts                  │    │
│  │ ├── themes/                 │    │ ├── ipc/ (40+ channels)           │    │
│  │ └── i18n/ (EN, JA)          │    │ ├── menus/ (native menus)         │    │
│  └──────────────┬──────────────┘    │ ├── trezor/ (Trezor Connect)      │    │
│                 │                   │ ├── windows/ (window mgmt)        │    │
│                 │                   │ └── utils/ (logging, etc)         │    │
│                 │                   └────────────────┬──────────────────┘    │
│                 │                                    │                       │
│                 │         Electron IPC               │                       │
│                 └──────────────────┬─────────────────┘                       │
│                                    │                                         │
│  ┌─────────────────────────────────┴─────────────────────────────────────┐   │
│  │                    source/common/                                     │   │
│  │                 (Shared code & IPC contracts)                         │   │
│  │  ├── ipc/api.ts (Channel definitions & types)                         │   │
│  │  ├── types/ (Shared TypeScript types)                                 │   │
│  │  └── config/ (Shared configuration)                                   │   │
│  └───────────────────────────────────────────────────────────────────────┘   │
│                                    │                                         │
├────────────────────────────────────┼─────────────────────────────────────────┤
│                      REST API (localhost:8090)                               │
│                                    │                                         │
│  ┌─────────────────────────────────┴─────────────────────────────────────┐   │
│  │                        cardano-wallet (Haskell)                       │   │
│  │                  External process managed by Daedalus                 │   │
│  │  • Wallet creation, restoration, deletion                             │   │
│  │  • Transaction building and signing                                   │   │
│  │  • Address derivation                                                 │   │
│  │  • Balance and UTXO management                                        │   │
│  │  • Staking operations                                                 │   │
│  └───────────────────────────────────────────────────────────────────────┘   │
│                                    │                                         │
│  ┌─────────────────────────────────┴─────────────────────────────────────┐   │
│  │                         cardano-node (Haskell)                        │   │
│  │                  Full node syncing Cardano blockchain                 │   │
│  │  • Block validation and chain sync                                    │   │
│  │  • Network participation                                              │   │
│  │  • Transaction submission                                             │   │
│  └───────────────────────────────────────────────────────────────────────┘   │
└──────────────────────────────────────────────────────────────────────────────┘
```

---

## Process Architecture

### Main Process (`source/main/`)

The Electron main process runs in Node.js and handles:

| Component             | File                               | Responsibility                      |
|-----------------------|------------------------------------|-------------------------------------|
| **Entry Point**       | `index.ts`                         | App initialization, window creation |
| **Cardano Node**      | `cardano/CardanoNode.ts`           | Node process lifecycle management   |
| **Cardano Wallet**    | `cardano/CardanoWalletLauncher.ts` | Wallet backend lifecycle            |
| **IPC Handlers**      | `ipc/*.ts`                         | 40+ IPC channel handlers            |
| **Hardware Wallets**  | `ipc/hardwareWallets/`             | Ledger/Trezor integration           |
| **Native Menus**      | `menus/`                           | Platform-specific menus             |
| **Window Management** | `windows/`                         | Main window, bounds persistence     |

### Renderer Process (`source/renderer/app/`)

The Electron renderer process runs React/MobX in a browser context:

| Component            | Directory     | Responsibility                   |
|----------------------|---------------|----------------------------------|
| **MobX Stores**      | `stores/`     | Application state management     |
| **React Components** | `components/` | 23 component categories          |
| **Containers**       | `containers/` | Connected components             |
| **API Client**       | `api/`        | cardano-wallet REST client       |
| **IPC Client**       | `ipc/`        | Main process communication       |
| **Themes**           | `themes/`     | Theme definitions (dark/light)   |
| **i18n**             | `i18n/`       | Internationalization (EN, JA)    |

### Shared Code (`source/common/`)

Code shared between main and renderer processes:

| Component        | File          | Responsibility                      |
|------------------|---------------|-------------------------------------|
| **IPC API**      | `ipc/api.ts`  | Channel names and type definitions  |
| **IPC Library**  | `ipc/lib/`    | IpcChannel, IpcConversation classes |
| **Types**        | `types/`      | Shared TypeScript type definitions  |
| **Config**       | `config/`     | Shared configuration                |

---

## IPC Communication

Daedalus uses type-safe IPC channels for main/renderer communication. All channels are defined in `source/common/ipc/api.ts`.

### Channel Categories

| Category             | Example Channels                                   | Purpose                   |
|----------------------|----------------------------------------------------|---------------------------|
| **Cardano**          | `CARDANO_STATE_CHANNEL`, `CARDANO_RESTART_CHANNEL` | Node/wallet lifecycle     |
| **Hardware Wallets** | `GET_HARDWARE_WALLET_TRANSPORT_CHANNEL`            | Ledger/Trezor operations  |
| **File Operations**  | `SHOW_OPEN_DIALOG_CHANNEL`, `GENERATE_PDF_CHANNEL` | File system access        |
| **Logging**          | `GET_LOGS_CHANNEL`, `COMPRESS_LOGS_CHANNEL`        | Log management            |
| **Downloads**        | `REQUEST_DOWNLOAD`, `RESUME_DOWNLOAD`              | App updates               |
| **UI Control**       | `SHOW_UI_PART_CHANNEL`, `TOGGLE_UI_PART_CHANNEL`   | Menu-triggered UI changes |

### IPC Pattern

```typescript
// 1. Define channel in source/common/ipc/api.ts
export const MY_CHANNEL = 'MY_CHANNEL';
export type MyChannelRequest = { param: string };
export type MyChannelResponse = { result: string };

// 2. Create handler in source/main/ipc/
const myChannel = new MainIpcChannel<MyChannelRequest, MyChannelResponse>(MY_CHANNEL);
myChannel.onReceive((request) => {
  return { result: `Processed: ${request.param}` };
});

// 3. Create client in source/renderer/app/ipc/
const myChannel = new RendererIpcChannel<MyChannelRequest, MyChannelResponse>(MY_CHANNEL);
const response = await myChannel.send({ param: 'test' });
```

### Key IPC Channels

| Channel                                 | Direction       | Purpose                    |
|-----------------------------------------|-----------------|----------------------------|
| `CARDANO_STATE_CHANNEL`                 | Main → Renderer | Node state updates         |
| `CARDANO_TLS_CONFIG_CHANNEL`            | Renderer → Main | Get TLS config for API     |
| `GET_HARDWARE_WALLET_TRANSPORT_CHANNEL` | Renderer → Main | Connect to hardware wallet |
| `SIGN_TRANSACTION_LEDGER_CHANNEL`       | Renderer → Main | Sign TX with Ledger        |
| `SIGN_TRANSACTION_TREZOR_CHANNEL`       | Renderer → Main | Sign TX with Trezor        |
| `SHOW_OPEN_DIALOG_CHANNEL`              | Renderer → Main | Native file picker         |
| `GET_LOGS_CHANNEL`                      | Renderer → Main | Retrieve log files         |

---

## State Management (MobX)

Daedalus uses MobX for reactive state management in the renderer process.

### Store Architecture

```
source/renderer/app/stores/
├── index.ts                 # Store initialization and injection
├── AppStore.ts              # App-level state
├── NetworkStatusStore.ts    # Node sync status (29KB)
├── WalletsStore.ts          # Wallet management (55KB)
├── HardwareWalletsStore.ts  # Hardware wallet state (120KB)
├── StakingStore.ts          # Staking operations (36KB)
├── TransactionsStore.ts     # Transaction history
├── AddressesStore.ts        # Address management
├── AssetsStore.ts           # Native token assets
├── ProfileStore.ts          # User preferences (24KB)
├── VotingStore.ts           # Catalyst voting
├── SidebarStore.ts          # UI sidebar state
├── UiDialogsStore.ts        # Modal dialog state
├── UiNotificationsStore.ts  # Toast notifications
├── NewsFeedStore.ts         # News feed
├── CurrencyStore.ts         # Fiat currency conversion
├── WalletSettingsStore.ts   # Per-wallet settings
├── WalletBackupStore.ts     # Backup flow state
├── WalletMigrationStore.ts  # Byron to Shelley migration
├── AppUpdateStore.ts        # App update management
└── WindowStore.ts           # Window state
```

### Store Pattern

```typescript
import { observable, action, computed, runInAction } from 'mobx';

export default class WalletsStore extends Store {
  @observable wallets: Wallet[] = [];
  @observable isLoading = false;
  
  @computed get activeWallet(): Wallet | null {
    return this.wallets.find(w => w.isActive) || null;
  }
  
  @action async loadWallets(): Promise<void> {
    this.isLoading = true;
    const wallets = await this.api.wallets.getWallets();
    runInAction(() => {
      this.wallets = wallets;
      this.isLoading = false;
    });
  }
}
```

---

## API Layer

### cardano-wallet REST API

Daedalus communicates with the cardano-wallet backend via REST API at `localhost:8090`.

The API client is in `source/renderer/app/api/api.ts` (104KB).

| Domain           | Endpoints                       | Purpose                  |
|------------------|---------------------------------|--------------------------|
| **Wallets**      | `/v2/wallets`                   | CRUD operations          |
| **Addresses**    | `/v2/wallets/{id}/addresses`    | Address derivation       |
| **Transactions** | `/v2/wallets/{id}/transactions` | TX history, creation     |
| **Staking**      | `/v2/stake-pools`               | Pool listing, delegation |
| **Network**      | `/v2/network/information`       | Sync status              |
| **Assets**       | `/v2/wallets/{id}/assets`       | Native tokens            |

### API Client Structure

```
source/renderer/app/api/
├── api.ts           # Main API class (104KB)
├── index.ts         # Exports
├── errors.ts        # Error types
├── utils/           # API utilities
├── wallets/         # Wallet API types
├── transactions/    # Transaction API types
├── addresses/       # Address API types
├── staking/         # Staking API types
├── assets/          # Asset API types
├── network/         # Network API types
├── news/            # News feed API
└── voting/          # Catalyst voting API
```

---

## Hardware Wallet Integration

Daedalus supports Ledger and Trezor hardware wallets for secure transaction signing.

### Architecture

```
Main Process                              Hardware Device
┌────────────────────────────────────┐   ┌─────────────────┐
│ source/main/ipc/hardwareWallets/   │   │                 │
│ ├── ledger/                        │   │   Ledger Nano   │
│ │   ├── deviceDetection/           │◄──┤   (USB HID)     │
│ │   └── api.ts                     │   │                 │
│ └── trezor/ (via Trezor Connect)   │   ├─────────────────┤
│                                    │   │                 │
│ source/main/trezor/                │◄──┤   Trezor        │
│ ├── connection.ts                  │   │   (USB HID)     │
│ └── manifest.ts                    │   │                 │
└────────────────────────────────────┘   └─────────────────┘
         │
         │ IPC
         ▼
┌────────────────────────────────────┐
│ Renderer Process                   │
│ HardwareWalletsStore.ts (120KB)    │
│ • Device connection state          │
│ • Extended public key caching      │
│ • Transaction signing flow         │
│ • Address verification             │
└────────────────────────────────────┘
```

### Supported Operations

| Operation               | Ledger | Trezor | IPC Channel                             |
|-------------------------|--------|--------|------------------------------------------|
| Device detection        | Yes    | Yes    | `GET_HARDWARE_WALLET_CONNECTION_CHANNEL` |
| Get extended public key | Yes    | Yes    | `GET_EXTENDED_PUBLIC_KEY_CHANNEL`        |
| Sign transaction        | Yes    | Yes    | `SIGN_TRANSACTION_*_CHANNEL`             |
| Verify address          | Yes    | Yes    | `SHOW_ADDRESS_CHANNEL`                   |
| Derive address          | Yes    | Yes    | `DERIVE_ADDRESS_CHANNEL`                 |

---

## Component Library

Daedalus uses `react-polymorph` for form components and custom components for wallet-specific UI.

### Component Categories

```
source/renderer/app/components/
├── wallet/              # Wallet UI (send, receive, settings)
├── staking/             # Staking pools, delegation
├── transactions/        # Transaction list, details
├── hardware-wallet/     # Hardware wallet pairing, signing
├── voting/              # Catalyst voting
├── assets/              # Native token display
├── sidebar/             # Navigation sidebar
├── navigation/          # Top navigation
├── notifications/       # Toast notifications
├── dialogs/             # Modal dialogs
├── loading/             # Loading states
├── splash/              # Initial sync splash
├── settings/            # App settings
├── profile/             # User profile
├── status/              # Network status
├── news/                # News feed
├── widgets/             # Reusable widgets
├── layout/              # Layout components
├── static/              # Static pages (about, etc)
├── knownIssues/         # Known issues display
├── legacy/              # Legacy Byron components
├── appUpdate/           # App update UI
├── environment/         # Environment info
└── analytics/           # Analytics consent
```

### Styling

- **SCSS Modules**: Co-located with components (`Component.scss`)
- **Themes**: Defined in `source/renderer/app/themes/`
- **CSS Variables**: Used for theming

---

## Testing Architecture

### Test Types

| Type         | Framework           | Location                 | Command             |
|--------------|---------------------|--------------------------|---------------------|
| Unit Tests   | Cucumber            | `tests/**/unit/`         | `yarn test:unit`    |
| E2E Tests    | Cucumber + Spectron | `tests/**/e2e/`          | `yarn test:e2e`     |
| Jest Tests   | Jest                | `*.test.ts`, `*.spec.ts` | `yarn test:jest`    |
| Visual Tests | Storybook           | `storybook/`             | `yarn storybook`    |

### Test Structure

```
tests/
├── setup-common.ts      # Common test setup
├── setup-e2e.ts         # E2E-specific setup
├── wallets/
│   ├── unit/            # Unit tests
│   └── e2e/             # E2E tests
├── transactions/
│   ├── unit/
│   └── e2e/
├── staking/
│   ├── unit/
│   └── e2e/
└── ...
```

---

## Build System

### Nix + Webpack

Daedalus uses Nix for reproducible builds and Webpack for bundling.

```
Build Pipeline:
1. Nix Shell → Provides cardano-node, cardano-wallet, system dependencies
2. Webpack   → Bundles main and renderer processes
3. Electron  → Packages into platform installer
```

### Build Outputs

```
dist/
├── main/           # Bundled main process
│   └── index.js
├── renderer/       # Bundled renderer process
│   ├── index.html
│   └── *.js
└── storybook/      # Built Storybook (optional)
```

---

## Security Considerations

1. **Sensitive Data**: Recovery phrases never stored unencrypted
2. **Hardware Wallets**: All signing happens on device
3. **IPC Security**: Preload script limits exposed APIs
4. **Local Node**: Full validation, no trust required
5. **TLS**: cardano-wallet API uses mutual TLS

---

## Key Design Decisions

1. **Full Node**: Maximum security via local blockchain validation
2. **Electron**: Cross-platform desktop support (Windows, macOS, Linux)
3. **MobX**: Reactive state management with minimal boilerplate
4. **Type-safe IPC**: All IPC channels have TypeScript type definitions
5. **Modular Stores**: Each domain has its own MobX store
6. **Hardware Wallet Support**: First-class Ledger and Trezor integration
7. **Nix Builds**: Reproducible builds across platforms
