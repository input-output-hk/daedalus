---
name: storybook-creation
description: Create or update Storybook stories for Daedalus React components. Use this when asked to write stories, visual tests, storybook entries, or component demos for new or existing features.
---

# Storybook Creation Skill

Create or update Storybook stories for the Daedalus wallet application. This skill covers writing `.stories.tsx` files, domain wrappers, mock data, and registering stories — following existing project conventions.

---

## Storybook Stack

| Component          | Technology                                |
|--------------------|-------------------------------------------|
| Storybook          | 6.4 with webpack5 builder                 |
| Story API          | `storiesOf()` (legacy API — NOT CSF)      |
| Knobs              | `@storybook/addon-knobs`                  |
| Actions            | `@storybook/addon-actions`                |
| State Management   | `@dump247/storybook-state` (`withState`)  |
| Custom Addon       | `DaedalusMenu` (theme/locale/OS switcher) |
| Styling            | SCSS Modules (co-located with components) |
| i18n               | `react-intl` (en-US, ja-JP)              |
| Component Library  | `react-polymorph` (via `StoryDecorator`)  |

> **Important:** Daedalus uses the `storiesOf()` API, NOT Component Story Format (CSF). Do NOT use `export default { title: ... }` or named exports.

---

## Directory Structure

### Centralized Stories (default)

Stories are organized by **domain** under `storybook/stories/`:

```
storybook/stories/
├── index.ts                    # Master entry — imports all story files
├── _support/                   # Shared infrastructure
│   ├── StoryWrapper.tsx        # Top-level wrapper (theme/locale/OS)
│   ├── StoryDecorator.tsx      # react-polymorph ThemeProvider
│   ├── StoryProvider.tsx       # MobX provider + mock data
│   ├── StoryLayout.tsx         # Full app UI shell (sidebar/topbar)
│   ├── config.ts               # Theme/locale/OS constants
│   ├── utils.ts                # Mock data generators
│   └── environment.ts          # Environment stubs
├── common/                     # Shared widgets, dropdowns
├── wallets/                    # Wallet features (send, receive, tokens...)
│   ├── _utils/                 # WalletsWrapper domain decorator
│   ├── index.ts                # Domain barrel import
│   ├── send/
│   │   └── WalletSend.stories.tsx
│   └── ...
├── staking/                    # Delegation, stake pools
├── voting/                     # Voting features
├── settings/                   # Profile, preferences
├── assets/                     # Native tokens, NFTs
├── news/                       # Newsfeed, alerts, overlays
├── navigation/                 # Sidebar, categories
├── notifications/              # System notifications
├── dapps/                      # dApp connector
└── nodes/                      # Node status
```

### Co-located Stories (feature modules)

For self-contained feature modules, stories can live next to the component:

```
source/renderer/app/features/{feature}/ui/
├── MyComponent.tsx
├── MyComponent.scss
└── MyComponent.story.tsx       # Co-located story
```

### When to Use Which Pattern

| Pattern      | When to Use                                                    |
|--------------|----------------------------------------------------------------|
| Centralized  | Default for all domain-level features (wallets, staking, etc.) |
| Co-located   | Self-contained feature modules under `source/renderer/app/features/` |

---

## Step 1: Determine Story Location

### New domain in `storybook/stories/`

Create a new directory and optionally a domain barrel file:

```
storybook/stories/{domain}/
├── index.ts                           # Optional barrel import
├── {FeatureName}.stories.tsx          # Story file
└── _utils/                            # Optional domain wrapper
    └── {Domain}Wrapper.tsx
```

### Existing domain

Add the story file to the appropriate subdirectory:

```
storybook/stories/{domain}/{sub-feature}/
└── {FeatureName}.stories.tsx
```

### Co-located feature story

```
source/renderer/app/features/{feature}/ui/
└── {ComponentName}.story.tsx
```

---

## Step 2: Write the Story File

### File Naming

| Pattern     | Convention                       | Example                        |
|-------------|----------------------------------|--------------------------------|
| Centralized | `{ComponentName}.stories.tsx`    | `WalletSend.stories.tsx`       |
| Co-located  | `{ComponentName}.story.tsx`      | `DiscreetModeToggle.story.tsx` |

### Story Naming Convention

Use `storiesOf('Domain / Sub-feature', module)` with ` / ` as separator:

```typescript
storiesOf('Wallets / Send', module)
storiesOf('Common / Widgets', module)
storiesOf('Staking', module)
storiesOf('Discreet Mode / Discreet Mode Toggle', module)
```

### Minimal Story Template

```typescript
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean, text, number } from '@storybook/addon-knobs';
import MyComponent from '../../../../source/renderer/app/components/{domain}/MyComponent';

storiesOf('{Domain} / {Feature}', module)
  .addDecorator(withKnobs)
  .add('default', () => (
    <MyComponent
      label={text('Label', 'Default')}
      disabled={boolean('Disabled', false)}
      onClick={action('onClick')}
    />
  ));
```

### Full Story Template (with decorators + state)

```typescript
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { observable, action as mobxAction } from 'mobx';
import {
  withKnobs,
  boolean,
  text,
  number,
  select,
} from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import StoryLayout from '../_support/StoryLayout';
import MyComponent from '../../../../source/renderer/app/components/{domain}/MyComponent';

storiesOf('{Domain} / {Feature}', module)
  .addDecorator((story: any, context: any) => {
    const onChangeAction = action('onChange');
    const state = observable({
      value: '',
      onChange: mobxAction((newValue) => {
        state.value = newValue;
        onChangeAction(newValue);
      }),
    });
    return (
      <StoryDecorator propsForChildren={state}>
        <StoryProvider>
          <StoryLayout activeSidebarCategory="/{domain}" {...context}>
            {story()}
          </StoryLayout>
        </StoryProvider>
      </StoryDecorator>
    );
  })
  .addDecorator(withKnobs)
  .add('default', () => (
    <MyComponent
      label={text('Label', 'Default')}
      isActive={boolean('Active', true)}
      count={number('Count', 5)}
      onClick={action('onClick')}
    />
  ))
  .add('loading', () => (
    <MyComponent isLoading />
  ))
  .add('error', () => (
    <MyComponent error="Something went wrong" />
  ));
```

### Story with `withState` (for interactive two-way binding)

```typescript
import { withState } from '@dump247/storybook-state';

storiesOf('{Domain} / {Feature}', module)
  .addDecorator(withKnobs)
  .add(
    'interactive',
    withState({ value: '' }, (store) => (
      <MyInput
        value={store.state.value}
        onChange={(value) => store.set({ value })}
      />
    ))
  );
```

---

## Step 3: Use Support Infrastructure

### Decorator Stack

The standard decorator stack wraps stories with theme, intl, and layout:

| Decorator         | Purpose                                                  | Import Path                          |
|-------------------|----------------------------------------------------------|--------------------------------------|
| `StoryWrapper`    | Top-level: theme, locale, OS selection (auto-applied)    | `../_support/StoryWrapper`           |
| `StoryDecorator`  | react-polymorph `ThemeProvider` + skin                   | `../_support/StoryDecorator`         |
| `StoryProvider`   | MobX store provider + mock data                         | `../_support/StoryProvider`          |
| `StoryLayout`     | Full app shell (sidebar, topbar, content area)           | `../_support/StoryLayout`            |

**`StoryWrapper`** is automatically applied via `storybook/preview.tsx` — do NOT add it manually.

**When to use each:**

- **Simple component** (no app context needed): Just `withKnobs` decorator — no StoryDecorator/Provider/Layout.
- **Component needing theme**: Add `StoryDecorator`.
- **Component needing MobX stores**: Add `StoryDecorator` + `StoryProvider`.
- **Full page/screen component**: Add all three: `StoryDecorator` + `StoryProvider` + `StoryLayout`.

### Domain Wrappers

For domains with shared layout (e.g., wallets with navigation tabs), create a domain wrapper:

```typescript
// storybook/stories/{domain}/_utils/{Domain}Wrapper.tsx
import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';

export default function (story: any, context: any) {
  const storyWithKnobs = withKnobs(story, context);
  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory="/{domain}" {...context}>
          {storyWithKnobs}
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
}
```

Use it in stories:

```typescript
import DomainWrapper from '../_utils/DomainWrapper';

storiesOf('{Domain} / {Feature}', module)
  .addDecorator(DomainWrapper)
  .add('default', () => <MyComponent />);
```

### activeSidebarCategory Values

| Category     | Value           |
|--------------|-----------------|
| Wallets      | `/wallets`      |
| Staking      | `/staking`      |
| Settings     | `/settings`     |
| Voting       | `/voting`       |
| None/Other   | `null`          |

---

## Step 4: Mock Data

### Available Mock Data Generators

Import from `storybook/stories/_support/utils.ts`:

```typescript
import {
  generateHash,
  generatePolicyIdHash,
  generateWallet,
  generateTransaction,
  generateAssetToken,
  generateAssetDomain,
  generateRewardForWallet,
  EXAMPLE_METADATA,
} from '../_support/utils';
```

| Generator              | Returns             | Key Parameters                                                   |
|------------------------|---------------------|------------------------------------------------------------------|
| `generateHash()`       | SHA-512 hex string  | (none)                                                           |
| `generatePolicyIdHash()` | SHA-224 hex string | (none)                                                           |
| `generateWallet()`     | `Wallet` instance   | `name, amount, assets, reward, delegatedStakePool, hasPassword, status, isHardwareWallet, id` |
| `generateTransaction()` | `WalletTransaction` | `type, date, amount, deposit, state, ...`                       |
| `generateAssetToken()` | `AssetToken` object | `policyId, assetName, fingerprint, quantity, metadata`           |
| `generateAssetDomain()` | `Asset` instance   | `policyId, assetName, fingerprint, metadata`                     |
| `generateRewardForWallet()` | `Reward` object | `wallet, unspent`                                               |

### Example mock data setup

```typescript
import BigNumber from 'bignumber.js';
import { generateWallet, generateAssetToken, generateHash } from '../_support/utils';
import Wallet, { WalletSyncStateStatuses } from '../../../../source/renderer/app/domains/Wallet';

const wallet = generateWallet('Test Wallet', '500000000');
const syncingWallet = generateWallet(
  'Syncing Wallet',
  '100000000',
  { available: [], total: [] },
  0,
  null,
  false,
  WalletSyncStateStatuses.SYNCING
);
const asset = generateAssetToken(
  '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
  '',
  'token_fingerprint_here',
  100,
  { name: 'TestToken', ticker: 'TT', description: 'A test token', url: '', logo: '' }
);
```

---

## Step 5: Register the Story

### Centralized stories

Add the import to the appropriate barrel file:

1. **If a domain `index.ts` exists** (e.g., `storybook/stories/wallets/index.ts`):
   ```typescript
   import './{sub-feature}/{FeatureName}.stories';
   ```

2. **If no domain `index.ts` exists**, add the import to `storybook/stories/index.ts`:
   ```typescript
   // {Domain}
   import './{domain}/{FeatureName}.stories';
   ```

### Co-located stories

Add the import to `storybook/stories/index.ts` with the full relative path:

```typescript
import '../../source/renderer/app/features/{feature}/ui/{ComponentName}.story';
```

### Registration Checklist

- [ ] Story file created with correct naming convention
- [ ] Import added to domain `index.ts` OR root `storybook/stories/index.ts`
- [ ] Story renders in Storybook without errors

---

## Step 6: Cover Component States

Every story file should demonstrate the component's key visual states:

### Required States

```typescript
storiesOf('{Domain} / {Feature}', module)
  .addDecorator(/* ... */)
  .add('default', () => <Component /* happy path props */ />)
  .add('loading', () => <Component isLoading />)
  .add('empty', () => <Component items={[]} />)
  .add('error', () => <Component error="Something went wrong" />);
```

### Recommended Additional States

| State           | When to Include                                 |
|-----------------|-------------------------------------------------|
| `disabled`      | Component can be disabled                       |
| `with data`     | Component needs populated mock data             |
| `hardware wallet` | Component behaves differently for HW wallets  |
| `syncing`       | Component shows sync progress                   |
| `restoring`     | Component shows restore progress                |
| `legacy wallet` | Component has legacy wallet variant             |
| `with knobs`    | Interactive prop editing for complex components  |

---

## Knobs Reference

Use `@storybook/addon-knobs` for interactive props:

```typescript
import {
  withKnobs,
  text,       // text('Label', 'default value')
  boolean,    // boolean('Disabled', false)
  number,     // number('Count', 5)
  select,     // select('Size', { Small: 'sm', Large: 'lg' }, 'sm')
  date,       // date('Start Date')
  object,     // object('Config', { key: 'value' })
  array,      // array('Items', ['a', 'b'])
} from '@storybook/addon-knobs';
```

---

## Updating Existing Stories

When updating stories for an existing feature:

1. **Find the existing story file** — search `storybook/stories/` or `source/renderer/app/features/` for `.stories.tsx` or `.story.tsx` files.
2. **Add new story entries** — append `.add()` calls for new states/variants.
3. **Update mock data** — if the component's props changed, update mock data to match.
4. **Update imports** — if new dependencies are needed, add them.
5. **Do NOT change the `storiesOf()` title** unless the feature was renamed.

### Adding a new state to an existing story

```typescript
// Find the existing storiesOf chain and append:
  .add('new state name', () => (
    <ExistingComponent
      newProp={text('New Prop', 'value')}
      /* existing required props */
    />
  ));
```

---

## Internationalization in Stories

For components using `react-intl` messages:

```typescript
import { defineMessages, IntlProvider } from 'react-intl';
import enMessages from '../../../../source/renderer/app/i18n/locales/en-US.json';
import jpMessages from '../../../../source/renderer/app/i18n/locales/ja-JP.json';

const { intl: enIntl } = new IntlProvider({
  locale: 'en-US',
  messages: enMessages,
}).getChildContext();

// Use in story:
.add('with i18n', (_, props) => (
  <Component
    label={enIntl.formatMessage(messages.myLabel)}
  />
))
```

> **Note:** `StoryWrapper` already provides `IntlProvider` globally. You only need manual intl setup when you need to access `intl.formatMessage()` directly in story code (not in the component).

---

## Common Pitfalls

| Pitfall | Solution |
|---------|----------|
| Story not appearing in Storybook | Ensure import is added to `index.ts` barrel file |
| Blank/white story | Missing `StoryDecorator` (theme provider) — add it as a decorator |
| MobX errors | Component needs stores — add `StoryProvider` decorator |
| Missing sidebar/layout | Add `StoryLayout` decorator with correct `activeSidebarCategory` |
| Styles not loading | SCSS modules are auto-loaded; check component imports |
| `storiesOf` title not grouping | Use ` / ` separator (with spaces): `'Domain / Feature'` |
| Using CSF exports | Daedalus uses `storiesOf()` API — do NOT use `export default` |
| Arrow functions in decorators | Use `function` keyword if you need `this` context |

---

## Validation

After creating or updating a story:

1. **Build check**: `yarn storybook:build` — story must compile without errors
2. **Visual check**: `yarn storybook` — story renders correctly at `http://localhost:6006`
3. **Theme check**: Switch themes in the DaedalusMenu addon — verify all themes render properly
4. **Locale check**: Switch to Japanese locale — verify i18n strings render
5. **State coverage**: Verify all visual states are represented (default, loading, error, empty, etc.)

---

## Quick Reference

### New Story Checklist

1. [ ] Determine location (centralized vs co-located)
2. [ ] Create story file with correct naming (`*.stories.tsx` or `*.story.tsx`)
3. [ ] Use `storiesOf()` API with correct domain/feature title
4. [ ] Add appropriate decorators (StoryDecorator, StoryProvider, StoryLayout)
5. [ ] Add `withKnobs` for interactive props
6. [ ] Use `action()` for event handler props
7. [ ] Cover all key visual states (default, loading, error, empty)
8. [ ] Use mock data generators from `_support/utils.ts`
9. [ ] Register story in barrel `index.ts`
10. [ ] Verify story renders in Storybook
