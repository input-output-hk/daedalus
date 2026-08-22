---
description: Component development with Storybook
---

# Storybook Workflow

This workflow guides component development and visual testing using Storybook in Daedalus.

## Overview

Daedalus uses **Storybook 6.4** for isolated component development and visual testing.

---

## Quick Commands

### Start Storybook

```bash
yarn storybook
```

Opens at `http://localhost:6006`

### Build Storybook

```bash
yarn storybook:build
```

Output: `dist/storybook/`

---

## Directory Structure

```
storybook/
├── .storybook/
│   ├── main.js              # Storybook configuration
│   ├── preview.js           # Story decorators
│   └── webpack.config.js    # Webpack overrides
└── stories/
    ├── wallets/
    │   ├── WalletSendForm.stories.tsx
    │   └── ...
    ├── staking/
    ├── transactions/
    └── ...
```

---

## Writing Stories

### Basic Story

```typescript
import React from 'react';
import { storiesOf } from '@storybook/react';
import MyComponent from '../../source/renderer/app/components/MyComponent';

storiesOf('Category|MyComponent', module)
  .add('default', () => (
    <MyComponent value="test" onChange={() => {}} />
  ))
  .add('loading', () => (
    <MyComponent value="" onChange={() => {}} isLoading />
  ))
  .add('error', () => (
    <MyComponent value="" onChange={() => {}} error="Something went wrong" />
  ));
```

### With Knobs (Interactive Props)

```typescript
import { storiesOf } from '@storybook/react';
import { withKnobs, text, boolean, number } from '@storybook/addon-knobs';

storiesOf('Category|MyComponent', module)
  .addDecorator(withKnobs)
  .add('interactive', () => (
    <MyComponent
      label={text('Label', 'Default Label')}
      disabled={boolean('Disabled', false)}
      count={number('Count', 5)}
    />
  ));
```

### With State

```typescript
import { storiesOf } from '@storybook/react';
import { State, Store } from '@dump247/storybook-state';

const store = new Store({
  value: '',
});

storiesOf('Category|Input', module)
  .add('with state', () => (
    <State store={store}>
      {state => (
        <Input
          value={state.value}
          onChange={value => store.set({ value })}
        />
      )}
    </State>
  ));
```

---

## Story Categories

Organize stories by domain:

| Category          | Components                              |
|-------------------|-----------------------------------------|
| `Wallets`         | Wallet creation, send/receive, settings |
| `Staking`         | Pool selection, delegation, rewards     |
| `Transactions`    | Transaction list, details, filters      |
| `Hardware Wallet` | Device connection, signing              |
| `Navigation`      | Sidebar, top navigation                 |
| `Settings`        | Profile, display, security              |
| `Common`          | Buttons, inputs, dialogs                |

---

## Decorators

### Theme Provider

Stories are wrapped with theme provider automatically:

```typescript
// storybook/.storybook/preview.js
import { addDecorator } from '@storybook/react';
import ThemeDecorator from './decorators/ThemeDecorator';

addDecorator(ThemeDecorator);
```

### Intl Provider

For internationalized components:

```typescript
import { IntlProvider } from 'react-intl';

const IntlDecorator = (story) => (
  <IntlProvider locale="en" messages={messages}>
    {story()}
  </IntlProvider>
);
```

---

## Addons

| Addon                       | Purpose                      |
|-----------------------------|------------------------------|
| `@storybook/addon-actions`  | Log component events         |
| `@storybook/addon-knobs`    | Interactive prop editing     |
| `@storybook/addon-links`    | Link between stories         |

### Using Actions

```typescript
import { action } from '@storybook/addon-actions';

storiesOf('Button', module)
  .add('with action', () => (
    <Button onClick={action('button-clicked')}>
      Click Me
    </Button>
  ));
```

---

## Best Practices

### 1. Cover All States

```typescript
storiesOf('Component', module)
  .add('default', () => <Component />)
  .add('loading', () => <Component isLoading />)
  .add('error', () => <Component error="Error message" />)
  .add('empty', () => <Component items={[]} />)
  .add('with data', () => <Component items={mockData} />);
```

### 2. Use Realistic Data

```typescript
import { generateWallet } from '../../utils/mockData';

const mockWallet = generateWallet({
  name: 'Test Wallet',
  balance: 1000000000,
});
```

**Draw populations from a shared generator, never from a hand-written list.**

A story that names its own handful of entries shows a kinder chain than the one
users have. Governance stories used to do this: two DReps where mainnet has a
thousand, everyone active where mainnet is 37% active, every name short ASCII
where 4% carry CJK, katakana, the ada sign or flag emoji. A screen reviewed
against that has only been reviewed against a happy path.

Each domain keeps one generator, seeded so a story renders the same data on
every reload and a screenshot means something, with proportions taken from
measurement rather than guessed. Governance's is
`storybook/stories/governance/_utils/drepPopulation.ts`, whose figures come from
sampling 1,000 mainnet DReps through Koios.

```typescript
import { makeDRepPopulation, drawCohortFrom } from './_utils/drepPopulation';

const POPULATION = makeDRepPopulation(400, { seed: 11 });
const COHORT = drawCohortFrom(POPULATION);
```

**Derive what the app derives.** Where production code selects, sorts or filters
a list, the story calls that same exported function rather than reproducing its
result by hand. `drawCohortFrom` above runs the shipping cohort selection, so a
story's cohort is a cohort the app could actually produce, and changing the
selection changes every story that shows one. A hand-picked list silently stops
matching the code the moment the code moves.

**Include the awkward cases in the generator, not in one story.** Long and
non-Latin names, missing optional fields, values at both ends of a threshold:
if the generator produces them in realistic proportions, every story that uses
it exercises truncation, wrapping and fallback for free.

### 3. Document Props

```typescript
storiesOf('Component', module)
  .add('with all props', () => (
    <Component
      // Required props
      id="test-id"
      name="Test Name"
      // Optional props
      description="Optional description"
      isActive={true}
    />
  ));
```

---

## Troubleshooting

### Storybook Won't Start

**Problem:** Build errors on start
**Solution:**
```bash
yarn clear:cache
yarn storybook
```

### Component Not Rendering

**Problem:** Blank story
**Solution:**
1. Check for missing providers (theme, intl)
2. Verify imports are correct
3. Check browser console for errors

### Style Issues

**Problem:** Styles not applied
**Solution:**
1. Ensure SCSS modules are imported
2. Check Webpack config includes SCSS loader
3. Verify theme CSS variables are available

---

## CI Integration

Storybook is built as part of CI checks:

```bash
yarn check:all  # Includes yarn storybook:build
```

Failed Storybook build will fail CI.
