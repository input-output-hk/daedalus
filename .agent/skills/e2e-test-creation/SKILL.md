---
name: e2e-test-creation
description: Create Cucumber BDD end-to-end tests for new features, enhancements, or updates. Use this when asked to write e2e tests, acceptance tests, feature files, step definitions, or Cucumber scenarios for Daedalus.
---

# E2E Test Creation Skill

Create Cucumber BDD end-to-end tests for the Daedalus wallet application. This skill covers writing `.feature` files, step definitions, and helpers following existing project conventions.

---

## Test Framework

| Component        | Technology                                             |
|------------------|--------------------------------------------------------|
| BDD Framework    | Cucumber.js with Gherkin syntax                        |
| App Driver       | Spectron (Electron testing) + WebDriver.io             |
| Assertions       | Chai (`expect`)                                        |
| Mocking          | Sinon (unit tests only)                                |
| TypeScript       | Compiled via `@swc-node/register`                      |
| Test Runner      | `yarn test:e2e` / `yarn test:unit`                     |

---

## Directory Structure Convention

Tests are organized by **domain** under `tests/`. Each domain may have `e2e/` and/or `unit/` subtrees:

```
tests/{domain}/
├── e2e/
│   ├── features/
│   │   └── {feature-name}.feature    # Gherkin scenarios
│   ├── steps/
│   │   ├── {feature-name}.ts         # Step definitions
│   │   └── helpers.ts                # Domain-specific helpers
│   └── documents/                    # Test fixtures (optional)
└── unit/
    ├── features/
    │   └── {feature-name}.feature
    └── steps/
        └── {feature-name}.ts
```

### Existing Domains

| Domain           | Path                    | Description                       |
|------------------|-------------------------|-----------------------------------|
| `wallets`        | `tests/wallets/`        | Wallet CRUD, restore, ordering    |
| `transactions`   | `tests/transactions/`   | Send, receive, UTxO display       |
| `delegation`     | `tests/delegation/`     | Staking, stake pools, rewards     |
| `navigation`     | `tests/navigation/`     | Sidebar, tabs, routing            |
| `settings`       | `tests/settings/`       | Language, terms of use, preferences |
| `news`           | `tests/news/`           | Newsfeed, alerts, incidents       |
| `paper-wallets`  | `tests/paper-wallets/`  | Paper wallet certificates         |
| `addresses`      | `tests/addresses/`      | Address management                |
| `app`            | `tests/app/`            | App lifecycle, updates, dialogs   |
| `common`         | `tests/common/`         | Shared utilities, mnemonics       |
| `assets`         | `tests/assets/`         | Native tokens, NFTs               |

When adding tests for a **new domain**, create the full subtree. When adding tests for an **existing domain**, add files to the existing structure.

---

## Step 1: Write a Feature File

### File Location

```
tests/{domain}/e2e/features/{feature-name}.feature
```

### Required Tags

Every e2e feature file MUST start with a tag line. Use the appropriate tags:

| Tag          | Meaning                                          |
|--------------|--------------------------------------------------|
| `@e2e`       | **Required** — marks as an end-to-end test       |
| `@unit`      | Marks as a unit test (use for unit/ features)     |
| `@wip`       | Work in progress — skipped in CI                 |
| `@skip`      | Permanently skipped                              |
| `@watch`     | Run in watch mode during development             |
| `@noReload`  | Skip app reload between scenarios                |
| `@restartApp`| Restart Electron app after this scenario         |
| `@reconnectApp` | Reconnect app backend after this scenario     |
| `@rewardsCsv`| Clean up exported rewards CSV after scenario     |

### Feature File Template

```gherkin
@e2e
Feature: {Human-readable Feature Name}

  Background:
    Given I have completed the basic setup

  Scenario: {Descriptive Scenario Name}
    Given {precondition}
    When {user action}
    Then {expected outcome}
```

### Gherkin Conventions

1. **Background section**: Use for shared preconditions across scenarios. Almost all e2e tests include `Given I have completed the basic setup` which handles initial app setup (language, terms of use acceptance).

2. **Scenario Outline with Examples**: Use for testing multiple data variations:
   ```gherkin
   Scenario Outline: Switching Between Wallet Tabs
     Given I am on the "Test wallet" wallet "<FROM>" screen
     When I click the wallet <TO> button
     Then I should be on the "Test wallet" wallet "<TO>" screen

     Examples:
     | FROM    | TO           |
     | summary | send         |
     | summary | receive      |
   ```

3. **Data Tables**: Use for structured test data:
   ```gherkin
   Given I have the following wallets:
     | name        |
     | Test wallet |
   ```

4. **Quoted strings**: Use double quotes for dynamic values referenced in step definitions:
   ```gherkin
   Given I am on the "Test wallet" wallet "send" screen
   ```

### Common Reusable Steps

These steps are already defined and available for use in feature files:

```gherkin
# Setup
Given I have completed the basic setup
Given I dont have a language set
Given I didn't accept "Terms of use"

# Wallet management
Given I have the following wallets:
  | name |
  | My Wallet |
Given I have a "{wallet}" wallet with funds
Given I am on the "{wallet}" wallet "{screen}" screen

# Navigation
When I click the wallet {button} button
When I click on the "{category}" category in the sidebar

# Sidebar
Given the sidebar submenu is hidden|visible
When I click on the sidebar toggle button
Then the sidebar submenu should be hidden|visible

# General assertions
Then I should be on the "{wallet}" wallet "{screen}" screen
```

---

## Step 2: Write Step Definitions

### File Location

```
tests/{domain}/e2e/steps/{feature-name}.ts
```

### Step Definition Template

```typescript
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';

// CSS selectors — group all selectors at the top
const SELECTORS = {
  COMPONENT: '.MyComponent_component',
  BUTTON: '.MyComponent_button',
  DIALOG: '.MyDialog_overlay',
};

Given(/^I am on the my feature screen$/, async function () {
  await this.client.waitForVisible(SELECTORS.COMPONENT);
});

When(/^I click the action button$/, async function () {
  await this.waitAndClick(SELECTORS.BUTTON);
});

Then(/^I should see the dialog$/, async function () {
  await this.client.waitForVisible(SELECTORS.DIALOG);
});

Then(/^I should not see the dialog$/, async function () {
  await this.client.waitForVisible(SELECTORS.DIALOG, null, true);
});
```

### Step Definition Conventions

1. **Import pattern**: Always import `Given`, `When`, `Then` from `'cucumber'` and `expect` from `'chai'`.

2. **Regex matching**: Use regex patterns for step matching. Capture dynamic values with groups:
   ```typescript
   Given(/^I have a "([^"]*)" wallet$/, async function (walletName) {
     // walletName captured from quotes in feature file
   });
   ```

3. **Async functions**: All step definitions MUST use `async function` (not arrow functions — Cucumber needs `this` context).

4. **`this` context**: The test context provides these properties:
   ```typescript
   this.client        // WebDriver.io client — DOM interaction
   this.app           // Spectron Application instance
   this.browserWindow // Electron BrowserWindow
   this.context       // Shared data object between steps
   this.intl          // i18n translation helper
   this.waitAndClick(selector)     // Wait for visible + enabled, then click
   this.waitAndGetText(selector)   // Wait for text, then get it
   this.waitAndSetValue(selector, value) // Wait for element, then set value
   ```

5. **CSS Selectors**: Use CSS class selectors matching React component SCSS module classes. Daedalus uses SCSS modules, so class names follow `{ComponentName}_{className}` pattern:
   ```typescript
   '.WalletSendForm_component'
   '.SidebarCategory_active'
   '.TopBar_leftIcon'
   '.NavButton_component.summary'
   ```

6. **Selectors object**: Group all selectors in a `SELECTORS` const at the top of the file for maintainability.

7. **Executing code in the app context**: Use `this.client.execute()` or `this.client.executeAsync()` to run code inside the Electron renderer process:
   ```typescript
   // Synchronous execution
   const result = await this.client.execute(() => {
     return daedalus.stores.wallets.active?.name;
   });
   // result.value contains the return value

   // Async execution (with callback)
   await this.client.executeAsync((done) => {
     daedalus.stores.wallets.refreshWalletsData()
       .then(done)
       .catch((error) => done(error));
   });
   ```

8. **Accessing Daedalus internals**: Inside `execute`/`executeAsync`, the global `daedalus` object exposes:
   ```typescript
   daedalus.stores     // MobX stores (wallets, networkStatus, sidebar, profile, etc.)
   daedalus.actions    // MobX actions
   daedalus.api        // API client (daedalus.api.ada)
   daedalus.translations // i18n translation objects
   ```

9. **Assertions**: Use Chai's `expect` for assertions:
   ```typescript
   const text = await this.waitAndGetText(SELECTORS.TITLE);
   expect(text).to.equal('Expected Title');
   expect(text).to.include('partial');
   expect(result.value).to.be.true;
   ```

---

## Step 3: Write Domain-Specific Helpers

### File Location

```
tests/{domain}/e2e/steps/helpers.ts
```

### Helper Function Patterns

```typescript
import { DEFAULT_TIMEOUT } from '../../../common/e2e/steps/config';

/**
 * Wait for an element and extract data from it.
 */
export const getElementData = async (client, selector: string) => {
  await client.waitForVisible(selector);
  return client.getText(selector);
};

/**
 * Wait until a condition is true in the app state.
 */
export const waitUntilCondition = async function (conditionFn: () => boolean) {
  await this.client.waitUntil(
    async () => {
      const result = await this.client.execute(conditionFn);
      return result.value === true;
    },
    DEFAULT_TIMEOUT
  );
};

/**
 * Navigate to a specific route in the app.
 */
export const navigateTo = async function (route: string) {
  await this.client.execute((r) => {
    daedalus.actions.router.goToRoute.trigger({ route: r });
  }, route);
};

/**
 * Wait until a specific URL is reached.
 */
export const waitUntilUrlEquals = async function (expectedUrl: string) {
  const maxWait = DEFAULT_TIMEOUT;
  const freq = 500;
  let attempts = maxWait / freq;
  const check = async () => {
    const url = await this.client.url();
    if (url.value.includes(expectedUrl)) return true;
    if (--attempts <= 0) throw new Error(`URL never reached: ${expectedUrl}`);
    await new Promise((r) => setTimeout(r, freq));
    return check();
  };
  await check();
};
```

### Helper Conventions

1. **Export all helpers as named exports** — never use default exports.
2. **Accept `client` as first parameter** for functions that don't need `this` context.
3. **Use `function` (not arrow) for helpers that need `this` binding** — they'll be called with `.call(this, ...)`.
4. **Import `DEFAULT_TIMEOUT`** from `tests/common/e2e/steps/config.ts` for consistent timeouts.
5. **Keep helpers focused** — one responsibility per function.

---

## Step 4: Common Available Helpers

### From `tests/common/e2e/steps/helpers.ts`

| Helper                        | Purpose                                    |
|-------------------------------|--------------------------------------------|
| `waitAndClick(selector)`      | Wait visible + enabled, then click         |
| `waitAndGetText(selector)`    | Wait for text content, then retrieve it    |
| `waitAndSetValue(sel, val)`   | Wait for element, then set its value       |
| `scrollIntoView(client, sel)` | Scroll element into viewport               |
| `clickInputByLabel(label)`    | Click input field by its label text        |
| `clickOptionByValue(value)`   | Click select option by value               |
| `clickOptionByIndex(index)`   | Click select option by index               |
| `saveScreenshot(app, file)`   | Capture screenshot (used on failure)       |
| `skippablePromise(name, fn)`  | Wrap promise with skip support             |

### From `tests/common/e2e/steps/config.ts`

```typescript
export const DEFAULT_TIMEOUT = 20000;
```

### From `tests/wallets/e2e/steps/helpers.ts`

| Helper                            | Purpose                                |
|-----------------------------------|----------------------------------------|
| `createWallets(wallets, options)` | Create wallets via API                 |
| `waitUntilWalletIsLoaded(name)`   | Poll until wallet appears in sidebar   |
| `restoreWalletWithFunds(client, opts)` | Restore Shelley wallet with funds |
| `navigateTo(route)`               | Navigate to a specific app route       |
| `waitUntilUrlEquals(url)`         | Wait until URL matches expected        |
| `getWalletType(type)`             | Determine Shelley vs Byron wallet type |

---

## Step 5: Run & Verify Tests

### Development Workflow

1. **Tag your new test with `@watch`** for iterative development:
   ```gherkin
   @e2e @watch
   Feature: My New Feature
   ```

2. **Run in watch mode**:
   ```bash
   KEEP_APP_AFTER_TESTS=true yarn test:e2e --tags '@e2e and @watch'
   ```

3. **Run a single feature file**:
   ```bash
   yarn cucumber:run tests/{domain}/e2e/features/{feature-name}.feature
   ```

4. **Run all e2e tests**:
   ```bash
   yarn test:e2e
   ```

5. **Run with fail-fast** (stop on first failure):
   ```bash
   yarn test:e2e:fail-fast
   ```

6. **Re-run only failed tests**:
   ```bash
   yarn test:e2e:rerun
   ```

### Before Committing

- Remove `@watch` and `@wip` tags from finished tests
- Ensure the test is tagged `@e2e` (or `@unit` for unit tests)
- Verify tests pass: `yarn test:e2e --tags '@e2e and @{your-feature-tag}'`

---

## Complete Example: New Feature Test

### Scenario: Testing a "Bookmark Wallet" feature

**1. Feature file** — `tests/wallets/e2e/features/bookmark-wallet.feature`:

```gherkin
@e2e
Feature: Bookmark Wallet

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |

  Scenario: User bookmarks a wallet
    Given I am on the "Test wallet" wallet "summary" screen
    When I click the bookmark button
    Then the wallet should be marked as bookmarked
    And I should see the bookmark indicator

  Scenario: User removes a bookmark
    Given I am on the "Test wallet" wallet "summary" screen
    And the wallet is bookmarked
    When I click the bookmark button
    Then the wallet should not be marked as bookmarked
```

**2. Step definitions** — `tests/wallets/e2e/steps/bookmark-wallet.ts`:

```typescript
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';

const SELECTORS = {
  BOOKMARK_BUTTON: '.WalletSummary_bookmarkButton',
  BOOKMARK_ACTIVE: '.WalletSummary_bookmarkActive',
  BOOKMARK_INDICATOR: '.WalletSummary_bookmarkIndicator',
};

Given(/^the wallet is bookmarked$/, async function () {
  const isBookmarked = await this.client.isVisible(SELECTORS.BOOKMARK_ACTIVE);
  if (!isBookmarked) {
    await this.waitAndClick(SELECTORS.BOOKMARK_BUTTON);
    await this.client.waitForVisible(SELECTORS.BOOKMARK_ACTIVE);
  }
});

When(/^I click the bookmark button$/, async function () {
  await this.waitAndClick(SELECTORS.BOOKMARK_BUTTON);
});

Then(/^the wallet should be marked as bookmarked$/, async function () {
  await this.client.waitForVisible(SELECTORS.BOOKMARK_ACTIVE);
});

Then(/^the wallet should not be marked as bookmarked$/, async function () {
  await this.client.waitForVisible(SELECTORS.BOOKMARK_ACTIVE, null, true);
});

Then(/^I should see the bookmark indicator$/, async function () {
  await this.client.waitForVisible(SELECTORS.BOOKMARK_INDICATOR);
});
```

---

## Creating Unit Tests (Non-E2E)

For unit tests that don't require the full Electron app:

### Feature File — `tests/{domain}/unit/features/{name}.feature`

```gherkin
@unit
Feature: {Feature Name}

  Scenario: {Scenario Name}
    Given {setup}
    When {action}
    Then {assertion}
```

### Step Definitions — `tests/{domain}/unit/steps/{name}.ts`

```typescript
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import sinon from 'sinon';

Given('I have input data {string}', function (data) {
  this.context.input = data;
});

When('I process the data', function () {
  this.context.result = processData(this.context.input);
});

Then('the output should be {string}', function (expected) {
  expect(this.context.result).to.equal(expected);
});
```

Unit test conventions:
- Use `@unit` tag (not `@e2e`)
- Use `this.context` to share data between steps (set up in `setup-common.ts`)
- Import source code directly — no Spectron/WebDriver needed
- Use Sinon for mocks/stubs (auto-restored in `After` hook)
- Use plain string matchers instead of regex when parameters are simple

---

## Checklist

When creating e2e tests for a new feature, verify:

- [ ] Feature file is in `tests/{domain}/e2e/features/` with `.feature` extension
- [ ] Feature file starts with `@e2e` tag
- [ ] Background includes `Given I have completed the basic setup`
- [ ] Step definitions are in `tests/{domain}/e2e/steps/` with `.ts` extension
- [ ] Step definitions import `{ Given, When, Then }` from `'cucumber'`
- [ ] Step definitions use `async function` (not arrow functions)
- [ ] CSS selectors follow `{ComponentName}_{className}` SCSS module pattern
- [ ] Selectors are grouped in a `SELECTORS` constant
- [ ] Domain-specific helpers are in a separate `helpers.ts` file
- [ ] Reusable steps from other domains are referenced, not duplicated
- [ ] `@watch` / `@wip` tags are added during development and removed before merge
- [ ] Tests pass with `yarn test:e2e --tags '@e2e and @watch'`
