# MobX State Management

> **Source of truth for Daedalus state management architecture.**

This document describes how Daedalus manages application state using MobX.

---

## Overview

Daedalus uses **MobX 5.15.7** for reactive state management in the renderer process. State is organized into domain-specific stores that are injected into React components.

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          React Components                               │
│                     (observer, inject stores)                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                    │                                    │
│                                    │ @inject('stores')                  │
│                                    ▼                                    │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │                         MobX Stores                             │    │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌───────────┐  │    │
│  │  │WalletsStore │ │StakingStore │ │ProfileStore │ │    ...    │  │    │
│  │  └──────┬──────┘ └──────┬──────┘ └──────┬──────┘ └─────┬─────┘  │    │
│  │         │               │               │              │        │    │
│  │         └───────────────┼───────────────┼──────────────┘        │    │
│  │                         │               │                       │    │
│  └─────────────────────────┼───────────────┼───────────────────────┘    │
│                            ▼               ▼                            │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │                      API / IPC Layer                            │    │
│  │           (cardano-wallet REST, Electron IPC)                   │    │
│  └─────────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Store Architecture

### Store Location

All stores are in `source/renderer/app/stores/`:

```
stores/
├── index.ts                    # Store initialization
├── lib/
│   └── Store.ts                # Base store class
├── WalletsStore.ts             # Wallet management
├── HardwareWalletsStore.ts     # Hardware wallet state
├── TransactionsStore.ts        # Transaction history
├── StakingStore.ts             # Staking operations
├── NetworkStatusStore.ts       # Node sync status
├── ProfileStore.ts             # User preferences
├── VotingStore.ts              # Catalyst voting
├── AddressesStore.ts           # Address management
├── AssetsStore.ts              # Native tokens
├── AppStore.ts                 # App-level state
├── AppUpdateStore.ts           # App updates
├── SidebarStore.ts             # Sidebar UI state
├── UiDialogsStore.ts           # Modal dialogs
├── UiNotificationsStore.ts     # Toast notifications
├── NewsFeedStore.ts            # News feed
├── CurrencyStore.ts            # Fiat conversion
├── WalletSettingsStore.ts      # Per-wallet settings
├── WalletBackupStore.ts        # Backup flow
├── WalletMigrationStore.ts     # Byron to Shelley migration
└── WindowStore.ts              # Window state
```

### Store Reference

| Store                  | Size (KB) | Purpose                             |
|------------------------|-----------|-------------------------------------|
| `WalletsStore`         | 55KB      | Wallet CRUD, active wallet, balance |
| `HardwareWalletsStore` | 120KB     | Ledger/Trezor pairing, signing      |
| `TransactionsStore`    | 15KB      | Transaction history, filtering      |
| `StakingStore`         | 36KB      | Stake pools, delegation             |
| `NetworkStatusStore`   | 29KB      | Node sync, block height             |
| `ProfileStore`         | 24KB      | Locale, theme, settings             |
| `VotingStore`          | 22KB      | Catalyst voting registration        |
| `AddressesStore`       | 7KB       | Address derivation, display         |
| `AssetsStore`          | 7KB       | Native token management             |
| `AppStore`             | 9KB       | App lifecycle state                 |
| `AppUpdateStore`       | 15KB      | Update download, installation       |
| `SidebarStore`         | 7KB       | Sidebar navigation state            |

---

## Store Base Class

All stores extend the base `Store` class:

```typescript
// source/renderer/app/stores/lib/Store.ts

export default class Store {
  api: Api;
  stores: StoresMap;
  actions: ActionsMap;

  constructor(api: Api, stores: StoresMap, actions: ActionsMap) {
    this.api = api;
    this.stores = stores;
    this.actions = actions;
  }

  // Called after all stores initialized
  setup(): void {}

  // Called on app teardown
  teardown(): void {}
}
```

---

## Creating a Store

### Basic Store Pattern

```typescript
import { observable, action, computed, runInAction } from 'mobx';
import Store from './lib/Store';
import type { Api } from '../api';

export type Widget = {
  id: string;
  name: string;
  value: number;
};

export default class WidgetsStore extends Store {
  // Observable state
  @observable widgets: Widget[] = [];
  @observable activeWidgetId: string | null = null;
  @observable isLoading = false;
  @observable error: Error | null = null;

  // Computed values
  @computed get activeWidget(): Widget | null {
    if (!this.activeWidgetId) return null;
    return this.widgets.find(w => w.id === this.activeWidgetId) || null;
  }

  @computed get totalValue(): number {
    return this.widgets.reduce((sum, w) => sum + w.value, 0);
  }

  // Actions
  @action async loadWidgets(): Promise<void> {
    this.isLoading = true;
    this.error = null;
    
    try {
      const widgets = await this.api.widgets.getWidgets();
      runInAction(() => {
        this.widgets = widgets;
      });
    } catch (error) {
      runInAction(() => {
        this.error = error as Error;
      });
    } finally {
      runInAction(() => {
        this.isLoading = false;
      });
    }
  }

  @action setActiveWidget(id: string): void {
    this.activeWidgetId = id;
  }

  @action async createWidget(name: string): Promise<Widget> {
    const widget = await this.api.widgets.createWidget({ name });
    runInAction(() => {
      this.widgets.push(widget);
    });
    return widget;
  }

  @action async deleteWidget(id: string): Promise<void> {
    await this.api.widgets.deleteWidget(id);
    runInAction(() => {
      this.widgets = this.widgets.filter(w => w.id !== id);
      if (this.activeWidgetId === id) {
        this.activeWidgetId = null;
      }
    });
  }

  // Lifecycle
  setup(): void {
    // Called after all stores initialized
    this.loadWidgets();
  }

  teardown(): void {
    // Cleanup subscriptions, timers, etc.
  }
}
```

### Registering a Store

```typescript
// source/renderer/app/stores/index.ts

import WidgetsStore from './WidgetsStore';

export type StoresMap = {
  widgets: WidgetsStore;
  // ... other stores
};

export const setupStores = (api: Api, actions: ActionsMap): StoresMap => {
  const stores: Partial<StoresMap> = {};
  
  stores.widgets = new WidgetsStore(api, stores as StoresMap, actions);
  // ... other stores
  
  // Setup all stores
  Object.values(stores).forEach(store => store.setup?.());
  
  return stores as StoresMap;
};
```

---

## MobX Decorators

### @observable

Mark properties as reactive:

```typescript
@observable widgets: Widget[] = [];
@observable isLoading = false;
@observable.ref largeObject = {}; // Shallow observation
```

### @computed

Derived values that update automatically:

```typescript
@computed get activeWallet(): Wallet | null {
  return this.wallets.find(w => w.id === this.activeWalletId) || null;
}

@computed get formattedBalance(): string {
  return formatAda(this.activeWallet?.balance || 0);
}
```

### @action

Methods that modify state:

```typescript
@action setActiveWallet(id: string): void {
  this.activeWalletId = id;
}

// Async actions need runInAction for state updates
@action async loadData(): Promise<void> {
  const data = await fetchData();
  runInAction(() => {
    this.data = data;
  });
}
```

---

## Using Stores in Components

### Inject and Observe

```typescript
import { observer, inject } from 'mobx-react';
import type { StoresMap } from '../stores';

type Props = {
  stores?: StoresMap;
};

const WalletList = inject('stores')(observer(({ stores }: Props) => {
  const { wallets, isLoading, activeWalletId } = stores!.wallets;
  
  if (isLoading) {
    return <LoadingSpinner />;
  }
  
  return (
    <ul>
      {wallets.map(wallet => (
        <li 
          key={wallet.id}
          className={wallet.id === activeWalletId ? 'active' : ''}
        >
          {wallet.name}
        </li>
      ))}
    </ul>
  );
}));

export default WalletList;
```

### Accessing Multiple Stores

```typescript
const TransactionSender = inject('stores', 'actions')(
  observer(({ stores, actions }: Props) => {
    const { activeWallet } = stores.wallets;
    const { createTransaction } = actions.transactions;
    
    const handleSend = async () => {
      await createTransaction({
        walletId: activeWallet.id,
        // ...
      });
    };
    
    return <SendForm onSubmit={handleSend} />;
  })
);
```

---

## Actions Architecture

Actions are defined separately from stores in `source/renderer/app/actions/`:

```typescript
// source/renderer/app/actions/transactions-actions.ts

import Action from './lib/Action';

export default class TransactionsActions {
  createTransaction: Action<CreateTransactionParams> = new Action();
  deleteTransaction: Action<{ id: string }> = new Action();
}
```

### Connecting Actions to Stores

```typescript
// In store setup
this.actions.transactions.createTransaction.listen(
  this._createTransaction
);

@action _createTransaction = async (params: CreateTransactionParams) => {
  // Implementation
};
```

---

## Cross-Store Communication

### Accessing Other Stores

```typescript
class TransactionsStore extends Store {
  @action async loadTransactions(): Promise<void> {
    // Access wallets store
    const { activeWallet } = this.stores.wallets;
    
    if (!activeWallet) return;
    
    const transactions = await this.api.transactions.getTransactions(
      activeWallet.id
    );
    
    runInAction(() => {
      this.transactions = transactions;
    });
  }
}
```

### Reacting to Other Store Changes

```typescript
import { reaction } from 'mobx';

class TransactionsStore extends Store {
  setup(): void {
    // React to active wallet changes
    reaction(
      () => this.stores.wallets.activeWalletId,
      (walletId) => {
        if (walletId) {
          this.loadTransactions();
        }
      }
    );
  }
}
```

---

## State Persistence

Some state is persisted via Electron Store:

```typescript
import { ELECTRON_STORE_CHANNEL } from '../../../common/ipc/api';

class ProfileStore extends Store {
  @observable locale = 'en-US';
  
  @action async loadPersistedState(): Promise<void> {
    const locale = await electronStore.get('locale');
    runInAction(() => {
      this.locale = locale || 'en-US';
    });
  }
  
  @action async setLocale(locale: string): Promise<void> {
    this.locale = locale;
    await electronStore.set('locale', locale);
  }
}
```

---

## Best Practices

### 1. Keep Stores Focused

Each store should handle one domain:
- `WalletsStore` - wallet operations only
- `TransactionsStore` - transaction operations only

### 2. Use runInAction for Async Updates

```typescript
// Good
@action async loadData(): Promise<void> {
  const data = await fetchData();
  runInAction(() => {
    this.data = data;
  });
}

// Bad - state update outside action
@action async loadData(): Promise<void> {
  this.data = await fetchData(); // May not trigger reactions
}
```

### 3. Prefer Computed Over Derived State

```typescript
// Good - computed value
@computed get filteredWallets(): Wallet[] {
  return this.wallets.filter(w => w.syncState === 'ready');
}

// Bad - storing derived state
@observable filteredWallets: Wallet[] = [];
```

### 4. Avoid Deep Nesting

```typescript
// Good - flat structure
@observable walletIds: string[] = [];
@observable walletsById: Map<string, Wallet> = new Map();

// Avoid - deeply nested
@observable data = {
  wallets: {
    byId: {},
    ids: [],
  },
};
```

---

## Debugging MobX

### Enable Strict Mode

```typescript
import { configure } from 'mobx';

configure({
  enforceActions: 'observed', // Require actions for state changes
  computedRequiresReaction: true, // Warn on unused computeds
  reactionRequiresObservable: true, // Warn on reactions without observables
});
```

### MobX DevTools

Use `mobx-react-devtools` in development:

```typescript
import DevTools from 'mobx-react-devtools';

const App = () => (
  <>
    <MainContent />
    {process.env.NODE_ENV === 'development' && <DevTools />}
  </>
);
```

### Trace Reactions

```typescript
import { trace } from 'mobx';

@computed get activeWallet() {
  trace(); // Logs when this computed is re-evaluated
  return this.wallets.find(w => w.id === this.activeWalletId);
}
```

---

## Common Patterns

### Loading State

```typescript
@observable isLoading = false;
@observable loadingStates: Map<string, boolean> = new Map();

@action setLoading(key: string, loading: boolean): void {
  this.loadingStates.set(key, loading);
}

@computed get isAnyLoading(): boolean {
  return Array.from(this.loadingStates.values()).some(Boolean);
}
```

### Error Handling

```typescript
@observable errors: Map<string, Error> = new Map();

@action setError(key: string, error: Error | null): void {
  if (error) {
    this.errors.set(key, error);
  } else {
    this.errors.delete(key);
  }
}

@action clearErrors(): void {
  this.errors.clear();
}
```

### Pagination

```typescript
@observable page = 0;
@observable pageSize = 20;
@observable totalCount = 0;

@computed get hasMore(): boolean {
  return this.page * this.pageSize < this.totalCount;
}

@action async loadNextPage(): Promise<void> {
  if (!this.hasMore) return;
  this.page += 1;
  await this.loadData();
}
```
