---
description: React/MobX renderer development
---

# Frontend Development Workflow

This workflow guides development on the React renderer process in Daedalus.

## Overview

The frontend is built with:
- **React 16.14.0** — UI library
- **MobX 5.15.7** — State management
- **react-polymorph 1.0.4** — Component library
- **SCSS Modules** — Scoped styling
- **React Router 5.2.0** — Navigation
- **react-intl 2.9.0** — Internationalization (EN, JA)
- **TypeScript** — Type safety

---

## Directory Structure

```
source/renderer/app/
├── index.tsx                # Entry point
├── App.tsx                  # Root component
├── Routes.tsx               # Route definitions
├── routes-config.ts         # Route configuration
├── ThemeManager.tsx         # Theme provider
├── WindowSizeManager.tsx    # Window size handling
├── stores/                  # MobX stores
│   ├── index.ts             # Store initialization
│   ├── WalletsStore.ts      # Wallet state
│   ├── TransactionsStore.ts # Transaction state
│   ├── StakingStore.ts      # Staking state
│   ├── HardwareWalletsStore.ts # Hardware wallet state
│   └── ... (20+ stores)
├── api/                     # cardano-wallet API client
│   ├── api.ts               # Main API class
│   ├── wallets/             # Wallet API
│   ├── transactions/        # Transaction API
│   └── ...
├── components/              # React components (23 categories)
│   ├── wallet/
│   ├── staking/
│   ├── transactions/
│   ├── hardware-wallet/
│   └── ...
├── containers/              # Connected components
├── actions/                 # MobX actions
├── themes/                  # Theme definitions
├── i18n/                    # Internationalization
├── ipc/                     # IPC client channels
├── config/                  # Configuration
├── types/                   # TypeScript types
└── utils/                   # Utilities
```

---

## Quick Commands

### Development Mode

```bash
# Start development (full app)
yarn dev

# Renderer only
yarn dev:renderer
```

### Production Build

```bash
yarn build:renderer
```

### Storybook

```bash
yarn storybook
```

---

## MobX State Management

### Store Architecture

Each domain has its own MobX store in `source/renderer/app/stores/`:

| Store                  | Responsibility              | Size  |
|------------------------|-----------------------------|-------|
| `WalletsStore`         | Wallet CRUD, active wallet  | 55KB  |
| `HardwareWalletsStore` | Ledger/Trezor state         | 120KB |
| `TransactionsStore`    | Transaction history         | 15KB  |
| `StakingStore`         | Delegation, pools           | 36KB  |
| `NetworkStatusStore`   | Sync status, node state     | 29KB  |
| `ProfileStore`         | User preferences            | 24KB  |
| `VotingStore`          | Catalyst voting             | 22KB  |

### Store Pattern

```typescript
import { observable, action, computed, runInAction } from 'mobx';
import Store from './lib/Store';

export default class WalletsStore extends Store {
  @observable wallets: Wallet[] = [];
  @observable activeWalletId: string | null = null;
  @observable isLoading = false;

  @computed get activeWallet(): Wallet | null {
    return this.wallets.find(w => w.id === this.activeWalletId) || null;
  }

  @action async loadWallets(): Promise<void> {
    this.isLoading = true;
    try {
      const wallets = await this.api.wallets.getWallets();
      runInAction(() => {
        this.wallets = wallets;
      });
    } finally {
      runInAction(() => {
        this.isLoading = false;
      });
    }
  }
}
```

### Accessing Stores in Components

```typescript
import { observer, inject } from 'mobx-react';

type Props = {
  stores?: { wallets: WalletsStore };
};

const WalletList = inject('stores')(observer(({ stores }: Props) => {
  const { wallets, isLoading } = stores!.wallets;
  
  if (isLoading) return <Loading />;
  
  return (
    <ul>
      {wallets.map(w => <li key={w.id}>{w.name}</li>)}
    </ul>
  );
}));
```

---

## Component Development

### Component Structure

```
components/wallet/
├── WalletSendForm.tsx       # Component
├── WalletSendForm.scss      # Styles
├── WalletSendForm.scss.d.ts # Generated types
└── WalletSendForm.messages.ts # i18n messages
```

### Component Pattern

```typescript
import React from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './MyComponent.scss';

const messages = defineMessages({
  title: {
    id: 'wallet.myComponent.title',
    defaultMessage: 'My Component',
  },
});

type Props = {
  value: string;
  onChange: (value: string) => void;
};

const MyComponent = observer(({ value, onChange }: Props) => {
  const intl = useContext(IntlContext);
  
  return (
    <div className={styles.container}>
      <h2>{intl.formatMessage(messages.title)}</h2>
      <Input value={value} onChange={onChange} />
    </div>
  );
});

export default MyComponent;
```

### Using react-polymorph

```typescript
import { Input, Button, Select } from 'react-polymorph/lib/components';
import { InputSkin } from 'react-polymorph/lib/skins/simple';

<Input
  skin={InputSkin}
  label="Amount"
  value={amount}
  onChange={setAmount}
/>

<Button
  skin={ButtonSkin}
  label="Send"
  onClick={handleSend}
/>
```

---

## Styling with SCSS Modules

### Component Styles

```scss
// MyComponent.scss
.container {
  padding: 20px;
  background: var(--theme-background-color);
}

.title {
  color: var(--theme-text-color);
  font-size: 18px;
}

.button {
  composes: button from '../common/Button.scss';
  background: var(--theme-primary-color);
}
```

### Using Styles

```typescript
import styles from './MyComponent.scss';

<div className={styles.container}>
  <h2 className={styles.title}>Title</h2>
</div>
```

### Generate SCSS Type Definitions

```bash
yarn typedef:sass
```

This generates `.scss.d.ts` files for type-safe class names.

---

## Theming

### Theme Variables

Themes are defined in `source/renderer/app/themes/`:
- `daedalus/cardano.ts` - Dark theme
- `daedalus/light-blue.ts` - Light theme
- And more...

### Using Theme Variables

```scss
.container {
  background: var(--theme-background-color);
  color: var(--theme-text-color);
  border: 1px solid var(--theme-border-color);
}
```

### Theme Commands

```bash
yarn themes:check:createTheme  # Validate themes
yarn themes:update             # Update theme files
yarn themes:copy               # Copy theme
```

---

## Internationalization (i18n)

### Defining Messages

```typescript
// MyComponent.messages.ts
import { defineMessages } from 'react-intl';

export default defineMessages({
  title: {
    id: 'wallet.myComponent.title',
    defaultMessage: 'My Component',
  },
  description: {
    id: 'wallet.myComponent.description',
    defaultMessage: 'This is a description with {count} items',
  },
});
```

### Using Messages

```typescript
import messages from './MyComponent.messages';

const { intl } = this.context;

// Simple
intl.formatMessage(messages.title)

// With values
intl.formatMessage(messages.description, { count: 5 })
```

### i18n Commands

```bash
yarn i18n:extract   # Extract messages
yarn i18n:check     # Validate translations
yarn i18n:manage    # Extract and validate
```

### Supported Locales

- English (en-US)
- Japanese (ja-JP)

Translation files: `translations/`

---

## IPC Client

### Using IPC Channels

```typescript
import { getLogsChannel } from '../ipc/get-logs';

// Request logs from main process
const logs = await getLogsChannel.request();
```

### IPC Client Pattern

```typescript
// source/renderer/app/ipc/my-channel.ts
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  MY_CHANNEL,
  MyChannelRequest,
  MyChannelResponse,
} from '../../../common/ipc/api';

export const myChannel = new RendererIpcChannel<
  MyChannelRequest,
  MyChannelResponse
>(MY_CHANNEL);

// Usage
const response = await myChannel.send({ param: 'value' });
```

---

## API Client

### Using the API

```typescript
// In a store
const wallets = await this.api.wallets.getWallets();
const tx = await this.api.transactions.createTransaction(params);
```

### API Structure

```
source/renderer/app/api/
├── api.ts           # Main API class (104KB)
├── wallets/         # Wallet endpoints
├── transactions/    # Transaction endpoints
├── addresses/       # Address endpoints
├── staking/         # Staking endpoints
├── assets/          # Native token endpoints
├── network/         # Network status endpoints
└── utils/           # API utilities
```

---

## Testing

### Jest Tests

```bash
yarn test:jest
```

### Component Tests

```typescript
import { render, screen } from '@testing-library/react';
import MyComponent from './MyComponent';

describe('MyComponent', () => {
  it('renders title', () => {
    render(<MyComponent value="test" onChange={jest.fn()} />);
    expect(screen.getByText('My Component')).toBeInTheDocument();
  });
});
```

### Storybook

```bash
yarn storybook
```

Opens at `http://localhost:6006`

---

## Storybook Development

### Story Structure

```
storybook/stories/
├── wallets/
│   ├── WalletSendForm.stories.tsx
│   └── ...
└── ...
```

### Writing Stories

```typescript
import React from 'react';
import { storiesOf } from '@storybook/react';
import MyComponent from '../../source/renderer/app/components/MyComponent';

storiesOf('Wallet|MyComponent', module)
  .add('default', () => (
    <MyComponent value="test" onChange={() => {}} />
  ))
  .add('loading', () => (
    <MyComponent value="" onChange={() => {}} isLoading />
  ));
```

---

## Common Patterns

### Container Components

```typescript
// source/renderer/app/containers/wallet/WalletSendPage.tsx
import { inject, observer } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';

type Props = {
  stores: { wallets: WalletsStore; transactions: TransactionsStore };
  actions: { transactions: TransactionActions };
};

const WalletSendPage = inject('stores', 'actions')(
  observer(({ stores, actions }: Props) => {
    const { activeWallet } = stores.wallets;
    
    const handleSubmit = async (values: SendFormValues) => {
      await actions.transactions.createTransaction(values);
    };
    
    return (
      <WalletSendForm
        wallet={activeWallet}
        onSubmit={handleSubmit}
      />
    );
  })
);
```

### Form Handling

Daedalus uses `mobx-react-form` for forms:

```typescript
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';

const form = new ReactToolboxMobxForm({
  fields: {
    amount: {
      label: 'Amount',
      value: '',
      validators: [({ field }) => [field.value > 0, 'Must be positive']],
    },
  },
});
```

---

## Troubleshooting

### SCSS Module Types Missing

**Problem:** Cannot find module './Component.scss'
**Solution:**
```bash
yarn typedef:sass
```

### Store Not Updating UI

**Problem:** UI doesn't react to store changes
**Solution:**
1. Ensure component is wrapped with `observer`
2. Use `@observable` on properties
3. Use `runInAction` for async updates

### Hot Reload Not Working

**Problem:** Changes don't appear
**Solution:**
1. Check terminal for errors
2. Clear cache: `yarn clear:cache`
3. Restart dev server

### Component Not Rendering

**Problem:** Component shows nothing
**Solution:**
1. Check for errors in console
2. Verify stores are injected
3. Check route configuration

---

## Code Quality

```bash
yarn lint              # ESLint
yarn compile           # TypeScript check
yarn prettier:check    # Formatting check
yarn stylelint         # SCSS linting
```
