# Slice-2 Implementation Guide: Software-Wallet Delegate

> **Companion PRD:** [slice-2-PRD.md](./slice-2-PRD.md) | **Task Tracker:** [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
>
> All line anchors below were verified against the live worktree
> `/workspaces/daedalus/.agent/worktrees/drep-slice-2` (branch `feat/drep-discovery-slice-2`,
> base `88f6bc01b`) on 2026-07-22. Re-verify an anchor only if the file was touched by an
> earlier step of this same guide.

---

## Implementation Order

```
task-112 (selector integration + location.state round trip)
→ task-113 (confirmation renders the selected DRep ID)
→ task-114 (in-slice verification: flow Jest, Storybook, walkthrough, gates)
```

Dependencies force this order (113 needs 112's handoff; 114 needs both).

## Cross-Cutting Renderer Note (applies to every task)

- **react-intl is 2.9.0**: use `injectIntl` / `intlShape` / `defineMessages` /
  `FormattedMessage`. Never `useIntl()` or any react-intl hook.
- **React Router is 5.2**: use `withRouter` + class containers (see
  `containers/voting/Governance.tsx:4,65` for the exact import/export pattern).
  `history.push(path, state)` carries `location.state`.
- **Locked invariants stated inline per step.** The global ones for this slice:
  - **#1 local-first** — no hosted governance explorer/API anywhere; the only directory is
    the in-app `/governance/dreps` route.
  - **#2 sanitization floor** — never pass a DRep ID, `abstain`/`no_confidence` literal, or
    any CIP-129/CIP-105 bech32 string to `logger.*`, `analytics.sendEvent`, or an
    electron-store write. New navigation/selection/confirmation code in this slice makes
    **zero** logger/analytics calls. If you ever must log about an ID, log only
    `drepIdLength` (precedent: `DRepIdDisplay.tsx:40-52`).
  - **#4 no second delegation backend** — the DRep-ID handoff travels ONLY through React
    Router `location.state`. No query params, no new store fields, no
    `VotingStore.pendingFormState`, and `VotingStore` never reads `GovernanceStore`.
  - **#10 byte-equality** — the ID rendered in the confirmation dialog === `chosenOption`
    === the `dRepId` sent to `delegateVotes`, byte for byte. No trim, no normalization, no
    re-encoding anywhere in this slice.
  - **#11 preliminary copy** — every NEW or CHANGED en-US and ja-JP string starts with
    `!!!`. Never strip an existing `!!!`.
  - **#13 form-only sentinels** — Abstain / No Confidence never appear as directory rows;
    the Browse-DReps affordance exists only for the `drep` vote type (it lives inside the
    DRep-ID input label, which only renders in `drep` mode).
- **Code comments**: only where logic is not self-evident; 1–3 plain lines stating the
  why/invariant. No task IDs, no ALL-CAPS tags, no change history.
- **Jest assertion style**: never `toHaveBeenCalledWith('str', { literal: 'object' })`
  (prettier 2.1.2 oscillates on it) — always `expect.objectContaining({ … })` for the
  object argument.
- **Verification commands** (run from the worktree root):
  - Typecheck: `node_modules/.bin/tsc --noEmit` — must exit 0 with ZERO errors (verified
    clean pre-change).
  - Lint: `node_modules/.bin/eslint <touched files>`.
  - Focused Jest: `node_modules/.bin/jest --testPathPattern="<p>" --no-coverage --runInBand`.
  - Sanitization floor: `node_modules/.bin/jest --testPathPattern="governance-sanitization"
    --no-coverage --runInBand` → 17/17 must pass.
  - Copy changes: `yarn i18n:manage` (rewrites `translations/messages.json`; that diff rides
    with the task commit; never hand-edit or prettier that file or the locale JSONs).
  - Format: `node_modules/.bin/prettier --write` on the changed `.ts/.tsx/.scss/.md` files
    ONLY (nix is unavailable — this substitutes `nix fmt`; do not run prettier on any JSON).
- **Never commit `.scss.d.ts` files** (gitignored generated artifacts).
- **Commits**: exactly one per task, subject-only Conventional Commits, no body, no
  trailers. Subjects are given at the end of each task.

---

## task-112: Integrate DRep selector into VotingPowerDelegation (list-row selection)

**Files to edit/create:**

| # | File | Action |
|---|---|---|
| 1 | `source/renderer/app/containers/governance/delegationFormState.ts` | CREATE |
| 2 | `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.messages.ts` | EDIT |
| 3 | `source/renderer/app/i18n/locales/en-US.json` | EDIT |
| 4 | `source/renderer/app/i18n/locales/ja-JP.json` | EDIT |
| 5 | `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx` | EDIT |
| 6 | `source/renderer/app/containers/voting/VotingGovernancePage.tsx` | EDIT |
| 7 | `source/renderer/app/components/governance/drep-directory/DRepCard.tsx` | EDIT |
| 8 | `source/renderer/app/components/governance/drep-directory/DRepCard.scss` | EDIT |
| 9 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx` | EDIT |
| 10 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx` | EDIT |
| 11 | `source/renderer/app/containers/governance/DRepDirectoryPage.tsx` | EDIT |
| 12 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | EDIT |
| 13 | `storybook/stories/voting/Governance.stories.tsx` | EDIT (minimal — compile fix only) |
| 14 | `storybook/stories/governance/DRepDirectory.stories.tsx` | EDIT (minimal — compile fix only) |
| 15 | `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` | CREATE |

Do **NOT** touch `routes-config.ts`, `Routes.tsx`, `VotingStore.ts`, or
`GovernanceStore.ts` in this task. No `DREP_DETAIL` route literal is added anywhere in
production code (orchestrator decision D1); the Detail route exists only as a stub inside
the new Jest spec.

### Step-by-Step

#### Step 1: Create the location.state helper module

Create `source/renderer/app/containers/governance/delegationFormState.ts` with exactly:

```ts
import type { VoteType } from '../../components/voting/voting-governance/types';

/**
 * Shape of the delegation-form handoff carried in React Router
 * location.state. This is the only transport for the DRep-ID handoff —
 * never query params, never store-backed pending form state.
 */
export interface DelegationFormNavigationState {
  from?: string;
  selectedWalletId?: string | null;
  voteType?: VoteType;
  selectedDRepId?: string;
}

const VOTE_TYPES: ReadonlyArray<VoteType> = ['abstain', 'no_confidence', 'drep'];

// location.state is untyped at the router boundary; pick only the handoff
// fields so unrelated router state can never reach the form contract.
export function pickDelegationFormNavigationState(
  state: unknown
): DelegationFormNavigationState | undefined {
  if (!state || typeof state !== 'object') return undefined;
  const candidate = state as DelegationFormNavigationState;
  const picked: DelegationFormNavigationState = {};
  if (typeof candidate.from === 'string') {
    picked.from = candidate.from;
  }
  if (
    typeof candidate.selectedWalletId === 'string' ||
    candidate.selectedWalletId === null
  ) {
    picked.selectedWalletId = candidate.selectedWalletId;
  }
  if (VOTE_TYPES.includes(candidate.voteType as VoteType)) {
    picked.voteType = candidate.voteType;
  }
  if (typeof candidate.selectedDRepId === 'string') {
    picked.selectedDRepId = candidate.selectedDRepId;
  }
  return Object.keys(picked).length > 0 ? picked : undefined;
}

// Directory-side forwarding contract: any push toward a detail path forwards
// only { from, selectedWalletId, voteType }. The slice-4 "View details" CTA
// will call this; until then the Jest harness exercises it.
export function pickDelegationFormReturnState(
  state: unknown
): DelegationFormNavigationState | undefined {
  const picked = pickDelegationFormNavigationState(state);
  if (!picked) return undefined;
  const returnState: DelegationFormNavigationState = {};
  if (picked.from !== undefined) returnState.from = picked.from;
  if (picked.selectedWalletId !== undefined) {
    returnState.selectedWalletId = picked.selectedWalletId;
  }
  if (picked.voteType !== undefined) returnState.voteType = picked.voteType;
  return Object.keys(returnState).length > 0 ? returnState : undefined;
}
```

Invariant #4 inline: this module is pure state-picking; it must never gain store imports,
logging, or persistence.

#### Step 2: Rework the DRep-input label messages (decision D2)

In `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.messages.ts`:

The current seam (lines 4–29) is:

```ts
  drepInputLabel: {
    id: 'voting.governance.drepInputLabel',
    defaultMessage:
      '!!!Please type or paste a valid DRep ID here. Look up {drepDirectoryLink}',
    description: 'Label for DRep input on the governance page',
  },
  drepInputLabelPreprod: { … },
  drepInputLabelLinkText: { … },
  drepInputLabelLinkUrl: {
    id: 'voting.governance.drepInputLabelLinkUrl',
    defaultMessage: 'https://gov.tools/drep_directory', … },
  drepInputLabelLinkUrlPreview: { … },
```

1. Change `drepInputLabel.defaultMessage` to:
   `'!!!Please type or paste a valid DRep ID here, or {browseDRepsLink}.'`
2. DELETE the four entries `drepInputLabelPreprod`, `drepInputLabelLinkText`,
   `drepInputLabelLinkUrl`, `drepInputLabelLinkUrlPreview` entirely (dead after D2 — the
   external gov.tools link and the preprod-only label variant are removed; the in-app
   directory exists on every network).
3. ADD one new entry (keep the object keys of `messages` alphabetically ordered, i.e.
   insert before `drepInputError`):

```ts
  browseDRepsLink: {
    id: 'voting.governance.browseDRepsLink',
    defaultMessage: '!!!Browse DReps',
    description:
      'In-app link next to the DRep ID input that opens the DRep directory',
  },
```

Invariant #1 inline: no external governance URL may remain in this file afterward
(`grep -n "gov.tools" source/renderer/app/components/voting/voting-governance/` must
return nothing).

#### Step 3: Update both locale JSONs (keep alphabetical key order)

`source/renderer/app/i18n/locales/en-US.json` (current seam lines 878–892):

1. After line 878 `"voting.governance.abstain": "Abstain",` insert:
   `"voting.governance.browseDRepsLink": "!!!Browse DReps",`
2. Change line 888 to:
   `"voting.governance.drepInputLabel": "!!!Please type or paste a valid DRep ID here, or {browseDRepsLink}.",`
3. Delete lines 889–892 (`drepInputLabelLinkText`, `drepInputLabelLinkUrl`,
   `drepInputLabelLinkUrlPreview`, `drepInputLabelPreprod`).
4. Immediately before `"governance.drepDirectory.copyButton"` (line 284) insert:
   `"governance.drepDirectory.card.select": "!!!Select for delegation",`

`source/renderer/app/i18n/locales/ja-JP.json` (same key lines, 888–892; same neighbors):

1. After `"voting.governance.abstain"` insert:
   `"voting.governance.browseDRepsLink": "!!!DRep一覧",`
2. Change `"voting.governance.drepInputLabel"` to:
   `"voting.governance.drepInputLabel": "!!!有効なDRep IDを入力または貼り付けてください。{browseDRepsLink}から選ぶこともできます。",`
3. Delete the same four `drepInputLabel*` keys.
4. Immediately before `"governance.drepDirectory.copyButton"` insert:
   `"governance.drepDirectory.card.select": "!!!委任先として選択",`

Invariant #11 inline: every value added or changed above starts with `!!!`; no other
string in either file may lose its `!!!`. (Deleting a whole dead key is allowed; stripping
a marker from a kept string is not.)

#### Step 4: Wire the form — `VotingPowerDelegation.tsx`

Current seams (verified): imports :1–21, `Props` :23–42, `initialState` :85–93, component
destructure :95–103, `useState` :104, label/link block :248–275 (uses the `environment`
global at :250 and :264), `chosenOption` :133–136 (do NOT touch).

1. **Props** — inside the `Props` type (after `renderConfirmationDialog`, line 41), add:

```ts
  initialFormState?: {
    selectedWalletId?: string | null;
    voteType?: VoteType;
    selectedDRepId?: string;
  };
  onBrowseDRepsClick: (formState: {
    selectedWalletId: string | null;
    voteType: VoteType;
  }) => void;
```

2. **Destructure** — add `initialFormState` and `onBrowseDRepsClick` to the parameter
   destructuring at :95–103.

3. **Initial state** — replace line 104

```ts
  const [state, setState] = useState<State>(initialState);
```

with:

```ts
  const [state, setState] = useState<State>(() => {
    if (!initialFormState) return initialState;
    const { selectedWalletId, voteType, selectedDRepId } = initialFormState;
    const selectedWallet =
      (selectedWalletId && wallets.find((w) => w.id === selectedWalletId)) ||
      null;
    return {
      ...initialState,
      selectedWallet,
      selectedVoteType: voteType || initialState.selectedVoteType,
      // The directory-selected ID is used verbatim: it must reach chosenOption
      // and the delegateVotes dRepId byte-for-byte (no trim, no re-encoding).
      drepInputState: selectedDRepId
        ? { dirty: true, value: selectedDRepId }
        : initialState.drepInputState,
    };
  });
```

Invariant #10 inline: the AC field names are `selectedWalletId` / `voteType`; the form's
internal names are `selectedWallet` (a `Wallet` object) / `selectedVoteType` — this
initializer is the ONLY place that mapping happens.

4. **Label/link block** — replace the entire `label={…}` attribute of the DRep `Input`
   (lines 248–275, the block containing `environment.isPreprod`,
   `messages.drepInputLabelPreprod`, `onExternalLinkClick`, and
   `messages.drepInputLabelLinkUrl*`) with:

```tsx
              label={
                <FormattedMessage
                  {...messages.drepInputLabel}
                  values={{
                    browseDRepsLink: (
                      <Link
                        className={styles.link}
                        label={intl.formatMessage(messages.browseDRepsLink)}
                        hasIconAfter={false}
                        onClick={() =>
                          onBrowseDRepsClick({
                            selectedWalletId: state.selectedWallet?.id ?? null,
                            voteType: state.selectedVoteType,
                          })
                        }
                      />
                    ),
                  }}
                />
              }
```

After this replacement the identifier `environment` must not appear anywhere in this file
(`grep -n "environment" …/VotingPowerDelegation.tsx` returns nothing). Do NOT remove the
`onExternalLinkClick` prop — it is still used by the `paragraph1` link at :179–189.

Invariant #13 inline: this affordance renders only inside the DRep-ID input label, which
itself renders only when `state.selectedWallet && state.selectedVoteType === 'drep'`
(:233) — sentinels never get a browse affordance.

#### Step 5: Wire the container — `VotingGovernancePage.tsx`

Current seams: imports :1–7, class :13, sync gate :25–33, `<VotingPowerDelegation …>`
:36–75, export :80.

1. Add imports:

```ts
import { withRouter, type RouteComponentProps } from 'react-router-dom';
import type { VoteType } from '../../components/voting/voting-governance/types';
import { pickDelegationFormNavigationState } from '../governance/delegationFormState';
```

2. Change `type Props = InjectedProps;` (line 9) to
   `type Props = InjectedProps & RouteComponentProps;`

3. Add a class method (above `render()`):

```ts
  handleBrowseDRepsClick = (formState: {
    selectedWalletId: string | null;
    voteType: VoteType;
  }) => {
    // The round trip carries wallet + vote type out and back through
    // location.state only (invariant: no second delegation backend).
    this.props.history.push(ROUTES.GOVERNANCE.DREPS, {
      from: ROUTES.VOTING.GOVERNANCE,
      selectedWalletId: formState.selectedWalletId,
      voteType: formState.voteType,
    });
  };
```

4. In `render()`, before the `return`, add:

```ts
    const initialFormState = pickDelegationFormNavigationState(
      this.props.location.state
    );
```

and pass two new props to `<VotingPowerDelegation …>` (alongside the existing ones at
:36–41):

```tsx
        initialFormState={initialFormState}
        onBrowseDRepsClick={this.handleBrowseDRepsClick}
```

5. Change line 80 `export default VotingGovernancePage;` to
   `export default withRouter(VotingGovernancePage);`

`ROUTES` is already imported (:6). Note (recorded, do not "fix"): if the node loses sync
while the user browses, the return hop lands on `VotingUnavailable` (:25–33) and the
pre-fill survives only in `location.state` — expected behavior; sync UX belongs to
`ux-refinement`.

#### Step 6: Add the row CTA — `DRepCard.tsx`

Current seams: messages :10–16, `Props` :18–21, JSX :36–54.

1. Add imports:

```ts
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
```

2. Add to the `defineMessages` block (after `votingPowerLabel`):

```ts
  select: {
    id: 'governance.drepDirectory.card.select',
    defaultMessage: '!!!Select for delegation',
    description: 'Row-level CTA that hands the DRep ID to the delegation form',
  },
```

3. Widen `Props`:

```ts
interface Props {
  entry: AppDRepDirectoryEntry;
  onSelectForDelegation: (drepId: string) => void;
  intl: intlShape.isRequired;
}
```

4. Destructure `onSelectForDelegation` in the component signature (:36) and add a third
   row inside the card `div`, after the existing `bottomRow` closing tag (:51):

```tsx
      <div className={styles.actionsRow}>
        <Button
          label={intl.formatMessage(messages.select)}
          onClick={() => onSelectForDelegation(entry.drepId)}
          skin={ButtonSkin}
        />
      </div>
```

Per shared-design-tokens §10: the CTA is a real `<button>` child (react-polymorph Button
renders one) with native Enter/Space semantics; the card wrapper itself stays
non-focusable. Per D1: this is the ONLY CTA the production card gains this slice — no
"View details".

#### Step 7: `DRepCard.scss`

Append at the end of the file (current file ends at :41 with `.sourceLabel`):

```scss
.actionsRow {
  display: flex;
  justify-content: flex-end;
}
```

(Do not commit any regenerated `.scss.d.ts`.)

#### Step 8: Thread the callback — `DRepDirectoryList.tsx`

1. `Props` (:29–32) becomes:

```ts
interface Props {
  entries: AppDRepDirectoryEntry[];
  onSelectForDelegation: (drepId: string) => void;
  intl: intlShape.isRequired;
}
```

2. Destructure it at :34 and change the card render at :64–66 to:

```tsx
        {pageEntries.map((entry) => (
          <DRepCard
            key={entry.drepId}
            entry={entry}
            onSelectForDelegation={onSelectForDelegation}
          />
        ))}
```

#### Step 9: Thread the callback — `DRepDirectory.tsx`

1. Add to `Props` (:48–55): `onSelectForDelegation: (drepId: string) => void;`
2. Destructure it at :57–64.
3. Change the list render at :136 to:

```tsx
            <DRepDirectoryList
              entries={drepList}
              onSelectForDelegation={onSelectForDelegation}
            />
```

#### Step 10: Directory container handoff — `DRepDirectoryPage.tsx`

Current seams: imports :1–7, `Props` :9–11, render :32–47, export :50.

1. Add imports:

```ts
import { withRouter, type RouteComponentProps } from 'react-router-dom';
import { ROUTES } from '../../routes-config';
import { pickDelegationFormReturnState } from './delegationFormState';
```

2. Change the `Props` interface to:

```ts
interface Props extends RouteComponentProps {
  stores?: StoresMap;
}
```

3. Add a class method (above `render()`):

```ts
  handleSelectForDelegation = (drepId: string) => {
    // Combine the inherited { from, selectedWalletId, voteType } with the
    // row's DRep ID; the handoff travels only through location.state.
    const inherited = pickDelegationFormReturnState(this.props.location.state);
    this.props.history.push(inherited?.from ?? ROUTES.VOTING.GOVERNANCE, {
      ...inherited,
      selectedDRepId: drepId,
    });
  };
```

4. Pass `onSelectForDelegation={this.handleSelectForDelegation}` to `<DRepDirectory …>`
   (:38–45).
5. Change line 50 to `export default withRouter(DRepDirectoryPage);`

Invariant #2 inline: this handler must not call `logger.*` or `analytics.*` — the drepId
and vote fields ride silently in router state.

#### Step 11: Update `DRepDirectory.spec.tsx`

1. Add `fireEvent` to the testing-library import (:4).
2. In `renderComponent` (:39–62): add an `onSelectForDelegation = jest.fn()` option to the
   destructured parameter (with its type `onSelectForDelegation?: jest.Mock` in the options
   type) and pass `onSelectForDelegation={onSelectForDelegation}` to `<DRepDirectory …>`.
3. Add one new test at the end of the `describe` block:

```tsx
  it('invokes onSelectForDelegation with the row DRep ID when the Select CTA is clicked', () => {
    const onSelectForDelegation = jest.fn();
    renderComponent({ onSelectForDelegation });

    fireEvent.click(
      screen.getAllByRole('button', { name: '!!!Select for delegation' })[0]
    );

    expect(onSelectForDelegation).toHaveBeenCalledTimes(1);
    expect(onSelectForDelegation).toHaveBeenCalledWith(baseEntries[0].drepId);
  });
```

The existing `'displays 25 entries per page'` test (:215–221) keeps passing: its
`[class*="card"]` selector matches only the card wrapper class; the new `actionsRow` and
the Button do not contain the substring `card`.

#### Step 12: Minimal story compile fixes (full story work is task-114)

1. `storybook/stories/voting/Governance.stories.tsx` — in `renderGovernancePanel`
   (:198–229), add to the `<VotingPowerDelegation …>` props:
   `onBrowseDRepsClick={action('onBrowseDRepsClick')}`
2. `storybook/stories/governance/DRepDirectory.stories.tsx` — in `renderDirectory`
   (:123–135), add to `<DRepDirectory …>`:
   `onSelectForDelegation={action('onSelectForDelegation')}`

#### Step 13: Create the flow spec — `VotingGovernancePage.spec.tsx`

Create `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`. The harness
follows `Governance.spec.tsx:41–59` (createMemoryHistory + mobx `Provider` + `IntlProvider`
+ pushSpy) plus the react-polymorph `ThemeProvider` wrapper copied from
`storybook/stories/_support/StoryDecorator.tsx:25–29` (VotingPowerDelegation uses skinless
`Input`/`Button`/`Link`, so the theme context is required). Full file:

```tsx
import React from 'react';
import BigNumber from 'bignumber.js';
import { Provider } from 'mobx-react';
import { Route, Router, type RouteComponentProps } from 'react-router-dom';
import { createMemoryHistory } from 'history';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../themes/daedalus';
import { themeOverrides } from '../../themes/overrides';
import { ROUTES } from '../../routes-config';
import { HwDeviceStatuses } from '../../domains/Wallet';
import { GovernanceRefreshState } from '../../stores/GovernanceStore';
import { pickDelegationFormReturnState } from '../governance/delegationFormState';
import VotingGovernancePage from './VotingGovernancePage';
import DRepDirectoryPage from '../governance/DRepDirectoryPage';

// The wallet and vote-type dropdowns are react-polymorph-heavy; the flow tests
// assert the values they RECEIVE, so plain pass-through mocks are enough.
jest.mock('../../components/widgets/forms/WalletsDropdown', () => {
  return function WalletsDropdownMock(props: { value: string | null }) {
    return <div data-testid="wallets-dropdown">{props.value || 'none'}</div>;
  };
});

jest.mock('../../components/widgets/forms/ItemsDropdown', () => {
  return function ItemsDropdownMock(props: { value: string }) {
    return <div data-testid="vote-type-dropdown">{props.value}</div>;
  };
});

const VALID_DREP_ID =
  'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n';
const WALLET_ID = 'wallet-1';

const softwareWallet = {
  id: WALLET_ID,
  name: 'Software Wallet',
  isHardwareWallet: false,
} as any;

const drepEntry = {
  anchor: null,
  drepActivity: 12,
  drepId: VALID_DREP_ID,
  status: 'active' as const,
  votingPower: new BigNumber('23137980123456'),
};

const DETAIL_STUB_PATH = '/governance/dreps/:drepId';

// Test-only stand-in for the slice-4 Detail route: it forwards the inherited
// { from, selectedWalletId, voteType } plus the route's DRep ID back to the
// form, exactly as task-117's acceptance criteria specify. It is registered
// ONLY in this harness — production has no Detail route in this slice.
function DetailRouteStub({
  history,
  location,
  match,
}: RouteComponentProps<{ drepId: string }>) {
  return (
    <button
      type="button"
      onClick={() => {
        const inherited = pickDelegationFormReturnState(location.state);
        history.push(inherited?.from ?? ROUTES.VOTING.GOVERNANCE, {
          ...inherited,
          selectedDRepId: match.params.drepId,
        });
      }}
    >
      Stub select for delegation
    </button>
  );
}

const buildStores = () => ({
  app: {
    currentRoute: ROUTES.VOTING.GOVERNANCE,
    openExternalLink: jest.fn(),
  },
  governance: {
    drepList: [drepEntry],
    error: null,
    lastFetchedAt: Date.now() - 60_000,
    refresh: jest.fn(),
    refreshState: GovernanceRefreshState.Loaded,
  },
  hardwareWallets: {
    checkIsTrezorByWalletId: jest.fn(() => false),
    hwDeviceStatus: HwDeviceStatuses.READY,
  },
  networkStatus: { isSynced: true, syncPercentage: 100 },
  staking: { getStakePoolById: jest.fn(), stakePools: [] },
  voting: {
    delegateVotes: jest.fn(async () => ({ success: true as const })),
    initializeVPDelegationTx: jest.fn(async () => ({
      fees: new BigNumber('0.174257'),
      success: true as const,
    })),
  },
  wallets: { all: [softwareWallet] },
});

type InitialEntry = { pathname: string; state?: Record<string, unknown> };

const renderFlow = (initialEntries: InitialEntry[]) => {
  const history = createMemoryHistory({ initialEntries });
  const pushSpy = jest.spyOn(history, 'push');
  const stores = buildStores();
  const actions = { router: { goToRoute: { trigger: jest.fn() } } };
  render(
    <Provider stores={stores as any} actions={actions as any}>
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <Router history={history}>
            <Route
              path={ROUTES.VOTING.GOVERNANCE}
              component={VotingGovernancePage}
            />
            <Route
              exact
              path={ROUTES.GOVERNANCE.DREPS}
              component={DRepDirectoryPage}
            />
            <Route path={DETAIL_STUB_PATH} component={DetailRouteStub} />
          </Router>
        </IntlProvider>
      </ThemeProvider>
    </Provider>
  );
  return { actions, history, pushSpy, stores };
};

describe('DRep selection handoff via location.state', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('Browse DReps push carries { from, selectedWalletId, voteType } in location.state', () => {
    const { history, pushSpy } = renderFlow([
      {
        pathname: ROUTES.VOTING.GOVERNANCE,
        state: { selectedWalletId: WALLET_ID, voteType: 'drep' },
      },
    ]);

    fireEvent.click(screen.getByText('!!!Browse DReps'));

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.GOVERNANCE.DREPS,
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      })
    );
    expect(history.location.pathname).toBe(ROUTES.GOVERNANCE.DREPS);
  });

  it('list-row Select returns to the form and restores wallet, vote type, and DRep ID', () => {
    renderFlow([
      {
        pathname: ROUTES.GOVERNANCE.DREPS,
        state: {
          from: ROUTES.VOTING.GOVERNANCE,
          selectedWalletId: WALLET_ID,
          voteType: 'drep',
        },
      },
    ]);

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(screen.getByTestId('wallets-dropdown')).toHaveTextContent(WALLET_ID);
    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent('drep');
    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('two-hop Form → Directory → Detail → Form restores wallet + vote type and pre-fills the ID', () => {
    const { history } = renderFlow([
      {
        pathname: ROUTES.VOTING.GOVERNANCE,
        state: { selectedWalletId: WALLET_ID, voteType: 'drep' },
      },
    ]);

    fireEvent.click(screen.getByText('!!!Browse DReps'));

    // Simulate the slice-4 "View details" push: the Directory forwards its
    // inherited state toward the detail path via the production picker.
    act(() => {
      history.push(
        `/governance/dreps/${VALID_DREP_ID}`,
        pickDelegationFormReturnState(history.location.state)
      );
    });

    fireEvent.click(screen.getByText('Stub select for delegation'));

    expect(screen.getByTestId('wallets-dropdown')).toHaveTextContent(WALLET_ID);
    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent('drep');
    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });
});
```

(`waitFor` is imported now because task-114 appends a test that needs it — the import is
harmless if briefly unused; if eslint flags it, add it in task-114 instead.)

#### Step 14: Verify, format, commit

```
node_modules/.bin/tsc --noEmit                       # ZERO errors
node_modules/.bin/eslint \
  source/renderer/app/containers/governance/delegationFormState.ts \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.messages.ts \
  source/renderer/app/containers/voting/VotingGovernancePage.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepCard.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.tsx \
  storybook/stories/voting/Governance.stories.tsx \
  storybook/stories/governance/DRepDirectory.stories.tsx
node_modules/.bin/jest --testPathPattern="VotingGovernancePage|DRepDirectory" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="governance-sanitization" --no-coverage --runInBand   # 17/17
yarn i18n:manage        # let the translations/messages.json diff ride with this commit
node_modules/.bin/prettier --write <the .ts/.tsx/.scss files touched above>   # never JSONs
```

Commit (subject only): `feat(gov): task-112 integrate DRep directory selection into VotingPowerDelegation`

### i18n Keys (task-112)

| id | en-US | ja-JP | status |
|---|---|---|---|
| `voting.governance.drepInputLabel` | `!!!Please type or paste a valid DRep ID here, or {browseDRepsLink}.` | `!!!有効なDRep IDを入力または貼り付けてください。{browseDRepsLink}から選ぶこともできます。` | changed |
| `voting.governance.browseDRepsLink` | `!!!Browse DReps` | `!!!DRep一覧` | new |
| `governance.drepDirectory.card.select` | `!!!Select for delegation` | `!!!委任先として選択` | new |
| `voting.governance.drepInputLabelPreprod` / `…LinkText` / `…LinkUrl` / `…LinkUrlPreview` | — | — | removed (dead, D2) |

### Acceptance (task-112)

- [ ] No second delegation backend: diff contains no `VotingStore`/`GovernanceStore` change.
- [ ] `VotingStore` does not read `GovernanceStore` (nothing new imports either store).
- [ ] Selection works from the directory LIST ROW; no detail view exists in production.
- [ ] Handoff is `location.state` only — `grep -rn "selectedDRepId" source/` hits only the
      form/container/helper/card-path files and specs; no query-param or store usage.
- [ ] Two-hop AC-5 covered by the harness-only `DetailRouteStub` test; `routes-config.ts`
      and `Routes.tsx` untouched.
- [ ] All listed Jest suites green; sanitization floor 17/17; `tsc --noEmit` zero errors.

---

## task-113: Update delegation confirmation with DRep identity (ID only)

**Files to edit/create:**

| # | File | Action |
|---|---|---|
| 1 | `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.messages.ts` | EDIT |
| 2 | `source/renderer/app/i18n/locales/en-US.json` + `ja-JP.json` | EDIT |
| 3 | `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx` | EDIT |
| 4 | `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.scss` | EDIT |
| 5 | `source/renderer/app/containers/voting/VotingGovernancePage.tsx` | EDIT |
| 6 | `storybook/stories/voting/Governance.stories.tsx` | EDIT (compile fix for the widened prop) |
| 7 | `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx` | CREATE |

Scope per orchestrator decision D3: the dialog accepts a `DRepIdentity` and renders
**`drepIdentity.raw` only** — no CIP-105 second line, no signed-payload line, no source
label, no name slot (all deferred: derivation is cv-1, verified names are anchor-2).

### Step-by-Step

#### Step 1: Add the dialog message

`VotingPowerDelegationConfirmationDialog.messages.ts` (current entries :3–40) — insert
after the `vote` entry (:9–13):

```ts
  drepId: {
    id: 'voting.governance.confirmationDialog.drepId',
    defaultMessage: '!!!DRep ID',
    description: 'Label above the DRep ID in the delegation confirmation dialog',
  },
```

Locale JSONs — insert into BOTH files immediately after
`"voting.governance.confirmationDialog.button.confirm"` (en-US.json:880; same neighbor in
ja-JP.json):

- en-US: `"voting.governance.confirmationDialog.drepId": "!!!DRep ID",`
- ja-JP: `"voting.governance.confirmationDialog.drepId": "!!!DRep ID",`

(The term is rendered identically in Japanese; both keep `!!!` — invariant #11.)

#### Step 2: Widen the dialog prop contract and render the raw ID

`VotingPowerDelegationConfirmationDialog.tsx` — current seams: `mapVoteToIntlMessage`
:30–39, props type :53–68, destructure :70–81, vote paragraph render :147–153.

1. Add the type import (5 levels up to `source/`):

```ts
import type { DRepIdentity } from '../../../../../common/types/governance.types';
```

2. Add to `VotingPowerDelegationConfirmationDialogProps` (after `chosenOption`):

```ts
  drepIdentity: DRepIdentity | null;
```

3. Add `drepIdentity` to the destructure at :70–81.

4. Replace the vote paragraph block (:147–153):

```tsx
        <p className={styles.paragraphTitle}>
          {intl.formatMessage(messages.vote)}
        </p>
        <p className={styles.paragraphValue}>
          {intl.formatMessage(mapVoteToIntlMessage(chosenOption))}
        </p>
```

with:

```tsx
        {drepIdentity ? (
          <>
            <p className={styles.paragraphTitle}>
              {intl.formatMessage(messages.drepId)}
            </p>
            <p className={styles.paragraphValue}>
              {/* Rendered untouched: must stay byte-equal to chosenOption and
                  the delegateVotes dRepId. Name slot is reserved for anchor-2;
                  unverified names never render here. */}
              <code className={styles.drepIdValue}>{drepIdentity.raw}</code>
            </p>
          </>
        ) : (
          <>
            <p className={styles.paragraphTitle}>
              {intl.formatMessage(messages.vote)}
            </p>
            <p className={styles.paragraphValue}>
              {intl.formatMessage(mapVoteToIntlMessage(chosenOption))}
            </p>
          </>
        )}
```

Leave `mapVoteToIntlMessage` (:30–39) as-is — it still serves the sentinel branch and the
defensive default. Invariant #13: Abstain / No Confidence keep their label rendering.

#### Step 3: Style the ID

Append to `VotingPowerDelegationConfirmationDialog.scss` (file currently ends at :21):

```scss
.drepIdValue {
  font-family: monospace;
  font-size: 14px;
  word-break: break-all;
}
```

The full ID must be visible (confirmation is a security surface) — do NOT reuse the
truncating `DRepIdDisplay` here.

#### Step 4: Build `drepIdentity` in the container

`VotingGovernancePage.tsx` — first add the type import at the top of the file (4 levels
up to `source/`, matching the containers-tree precedent in
`containers/status/DaedalusDiagnosticsDialog.tsx:4-6`):

```ts
import type { DRepIdentity } from '../../../../common/types/governance.types';
```

then, inside the `renderConfirmationDialog` callback (:42–74), before the `return`:

```tsx
        renderConfirmationDialog={({
          chosenOption,
          fees,
          onClose,
          selectedWallet,
        }) => {
          // Sentinels render as labels; a drep target renders its raw ID.
          // credentialType is a syntactic classification only — the rendered
          // and submitted string is chosenOption itself, untouched.
          const drepIdentity: DRepIdentity | null =
            chosenOption === 'abstain' || chosenOption === 'no_confidence'
              ? null
              : {
                  credentialType: chosenOption.startsWith('drep_script')
                    ? 'script'
                    : 'key',
                  raw: chosenOption,
                };
          return (
            <VotingPowerDelegationConfirmationDialog
              chosenOption={chosenOption}
              drepIdentity={drepIdentity}
              …(all existing props unchanged, :49–72)…
            />
          );
        }}
```

(The callback body changes from an expression to a block with `const` + `return`; every
existing prop stays byte-identical.)

#### Step 5: Story compile fixes for the widened prop

`storybook/stories/voting/Governance.stories.tsx`:

1. Add near `VALID_DREP_ID` (:54–55):

```ts
import type { DRepIdentity } from '../../../source/common/types/governance.types';

const toStoryDRepIdentity = (option: string): DRepIdentity | null =>
  option === 'abstain' || option === 'no_confidence'
    ? null
    : { credentialType: 'key', raw: option };
```

2. In `renderGovernanceConfirmationDialog` (:262–306) add prop:
   `drepIdentity={toStoryDRepIdentity(chosenOption)}`
3. In the `'Confirmation dialog - software wallet'` story (:395–428): the `chosenOption`
   knob value is `select('Vote option', voteOptions, VALID_DREP_ID)` — hoist it to a
   `const voteOption = select('Vote option', voteOptions, VALID_DREP_ID);` at the top of
   the story function, pass `chosenOption={voteOption}` and
   `drepIdentity={toStoryDRepIdentity(voteOption)}`.
4. Same change in `'Confirmation dialog - hardware wallet'` (:429–459).

#### Step 6: Create the dialog spec

Create
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx`:

```tsx
import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import { HwDeviceStatuses } from '../../../domains/Wallet';
import VotingPowerDelegationConfirmationDialog from './VotingPowerDelegationConfirmationDialog';

const VALID_DREP_ID =
  'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n';

const softwareWallet = {
  id: 'wallet-1',
  isHardwareWallet: false,
  name: 'Test Wallet',
} as any;

const renderDialog = (overrides: Record<string, unknown> = {}) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <VotingPowerDelegationConfirmationDialog
          chosenOption={VALID_DREP_ID}
          drepIdentity={{ credentialType: 'key', raw: VALID_DREP_ID }}
          fees={new BigNumber('0.174257')}
          hwDeviceStatus={HwDeviceStatuses.READY}
          isTrezor={false}
          onClose={jest.fn()}
          onExternalLinkClick={jest.fn()}
          onSubmit={jest.fn(async () => ({ success: true as const }))}
          redirectToWallet={jest.fn()}
          selectedWallet={softwareWallet}
          {...overrides}
        />
      </IntlProvider>
    </ThemeProvider>
  );

describe('VotingPowerDelegationConfirmationDialog — DRep identity', () => {
  afterEach(cleanup);

  it('renders the full raw DRep ID (byte-equal) instead of the generic label', () => {
    renderDialog();

    expect(screen.getByText('!!!DRep ID')).toBeInTheDocument();
    const idNode = screen.getByText(VALID_DREP_ID);
    expect(idNode.textContent).toBe(VALID_DREP_ID);
    expect(
      screen.queryByText('Delegate to DRep (default)')
    ).not.toBeInTheDocument();
  });

  it('still renders the Abstain sentinel label', () => {
    renderDialog({ chosenOption: 'abstain', drepIdentity: null });

    expect(screen.getByText('Vote')).toBeInTheDocument();
    expect(screen.getByText('Abstain')).toBeInTheDocument();
    expect(screen.queryByText(VALID_DREP_ID)).not.toBeInTheDocument();
  });

  it('still renders the No Confidence sentinel label', () => {
    renderDialog({ chosenOption: 'no_confidence', drepIdentity: null });

    expect(screen.getByText('No Confidence')).toBeInTheDocument();
    expect(screen.queryByText(VALID_DREP_ID)).not.toBeInTheDocument();
  });

  it('never renders a name field, even if extra fields sneak into the identity', () => {
    renderDialog({
      drepIdentity: {
        credentialType: 'key',
        givenName: 'Sneaky Unverified Name',
        raw: VALID_DREP_ID,
      } as any,
    });

    expect(screen.queryByText('Sneaky Unverified Name')).not.toBeInTheDocument();
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);
  });
});
```

Note the expected strings: the pre-slice dialog keys are FINALIZED in en-US.json (no
`!!!`): `Vote`, `Abstain`, `No Confidence`, `Delegate to DRep (default)`,
`Confirm Transaction`. Only the NEW key renders `!!!DRep ID`.

#### Step 7: Verify, format, commit

```
node_modules/.bin/tsc --noEmit
node_modules/.bin/eslint \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.messages.ts \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.tsx \
  storybook/stories/voting/Governance.stories.tsx
node_modules/.bin/jest --testPathPattern="VotingPowerDelegationConfirmationDialog" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="VotingGovernancePage|DRepDirectory" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="governance-sanitization" --no-coverage --runInBand
yarn i18n:manage
node_modules/.bin/prettier --write <touched .ts/.tsx/.scss files>
```

Commit: `feat(gov): task-113 render selected DRep ID in delegation confirmation`

### i18n Keys (task-113)

| id | en-US | ja-JP | status |
|---|---|---|---|
| `voting.governance.confirmationDialog.drepId` | `!!!DRep ID` | `!!!DRep ID` | new |

### Acceptance (task-113)

- [ ] Prop contract accepts `DRepIdentity` (`drepIdentity: DRepIdentity | null`).
- [ ] drep targets render the full raw ID; the generic "Delegate to DRep" label no longer
      renders for them.
- [ ] Sentinels still render Abstain / No Confidence labels (invariant #13).
- [ ] No name renders (anchor-2 reservation) — regression test in place.
- [ ] Byte-equality: the rendered string is `drepIdentity.raw` where
      `raw === chosenOption`, and `chosenOption` flows unchanged into
      `voting.delegateVotes` (`VotingStore.ts:417-421`); nothing in this task touches
      `VotingStore`.
- [ ] All listed suites green; `tsc --noEmit` zero errors.

---

## task-114: In-slice verification of the software-wallet delegate path

**Files to edit/create:**

| # | File | Action |
|---|---|---|
| 1 | `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` | EDIT (append flow test) |
| 2 | `storybook/stories/voting/Governance.stories.tsx` | EDIT (add prefilled story) |
| 3 | `.vscode/docs/walkthroughs/governance/02-voting-power-delegation.md` | EDIT (see gitignore note) |
| 4 | `.vscode/docs/walkthroughs/governance/04-troubleshooting.md` | EDIT |
| 5 | `.vscode/docs/walkthroughs/governance/05-improvements-vs-light-wallets.md` | EDIT |
| 6 | `.agent/plans/governance/drep-discovery/research/slice-2-findings.md` | CREATE |

> **Walkthrough gitignore note:** `.vscode/` is gitignored (`.gitignore:135`, entry
> `.vscode`), so the walkthrough sources exist only in the MAIN checkout at
> `/workspaces/daedalus/.vscode/docs/walkthroughs/governance/` and are absent from the
> worktree. Never modify the main checkout. Instead run (from the worktree root):
>
> ```
> mkdir -p .vscode/docs/walkthroughs
> cp -r /workspaces/daedalus/.vscode/docs/walkthroughs/governance .vscode/docs/walkthroughs/
> ```
>
> then edit the worktree copies. They cannot be committed (gitignored); the findings note
> and PRD final outcome must record that the updated copies need a manual sync back to the
> main checkout at slice close.

### Step-by-Step

#### Step 1: Append the end-to-end payload test

In `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`, append inside the
existing `describe` block (uses the finalized en-US strings `Submit`,
`Confirm Transaction`, `Confirm` — they carry no `!!!`):

```tsx
  it('propagates the selected DRep ID byte-for-byte: row select → confirmation → delegateVotes payload', async () => {
    const { stores } = renderFlow([
      {
        pathname: ROUTES.GOVERNANCE.DREPS,
        state: {
          from: ROUTES.VOTING.GOVERNANCE,
          selectedWalletId: WALLET_ID,
          voteType: 'drep',
        },
      },
    ]);

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    // The confirmation renders the selected ID itself (task-113), byte-equal.
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);

    const passwordInput = document.querySelector('input[type="password"]');
    expect(passwordInput).not.toBeNull();
    fireEvent.change(passwordInput as Element, {
      target: { value: 'secret123' },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Confirm' }));

    await waitFor(() =>
      expect(stores.voting.delegateVotes).toHaveBeenCalledTimes(1)
    );
    expect(stores.voting.delegateVotes).toHaveBeenCalledWith(
      expect.objectContaining({
        chosenOption: VALID_DREP_ID,
        passphrase: 'secret123',
      })
    );
    expect(stores.voting.initializeVPDelegationTx).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: VALID_DREP_ID })
    );
  });
```

(If `waitFor` was not imported in task-112, add it to the `@testing-library/react` import
now.) This covers task-114 AC-1 and AC-2: browse → select → confirm (ID only) → delegate
through the existing `delegateVotes` with the byte-equal payload
(`VotingStore.ts:361-438` executes `{ dRepId: chosenOption, … }` at :417-421 — the mock
here stands in for `voting.delegateVotes`, whose payload contract the sanitization suite
already pins at the store level).

#### Step 2: Add the prefilled-selector story

In `storybook/stories/voting/Governance.stories.tsx`, after the
`'Voting power delegation'` story (:392–394), add:

```tsx
  .add('Voting power delegation - prefilled from directory', () => (
    <div style={CENTERED_STORY_STYLE}>
      <VotingPowerDelegation
        getStakePoolById={getStakePoolById}
        initiateTransaction={async (params) => {
          action('initiateTransaction')(params);
          return { success: true, fees: new BigNumber('0.174257') };
        }}
        initialFormState={{
          selectedDRepId: VALID_DREP_ID,
          selectedWalletId: 'governance-wallet-1',
          voteType: 'drep',
        }}
        onBrowseDRepsClick={action('onBrowseDRepsClick')}
        onExternalLinkClick={action('onExternalLinkClick')}
        renderConfirmationDialog={renderGovernanceConfirmationDialog}
        stakePools={STAKE_POOLS_LIST}
        wallets={GOVERNANCE_WALLETS}
      />
    </div>
  ))
```

Locale coverage (AC-4) comes from the GLOBAL English/Japanese toggle — do NOT add a local
`IntlProvider` or per-locale story duplicates. The selector-in-form is now covered by
`'Connected flow'` + `'Voting power delegation'` (browse affordance, from task-112) and
this prefilled story; the ID-only confirmation is covered by the two existing confirmation
stories (drepIdentity wired in task-113).

#### Step 3: Walkthrough de-GovTool-ing (scoped per decision D2)

After the copy step from the gitignore note above, make exactly these edits to the
WORKTREE copies (anchors verified against the main-checkout originals on 2026-07-22):

**`02-voting-power-delegation.md`**

- Line 60 (`- **Label:** "Please type or paste a valid DRep ID here. Look up DRep directory"`) →
  `- **Label:** "Please type or paste a valid DRep ID here, or Browse DReps." (preliminary copy)`
- Line 62 (`- The "DRep directory" link opens the external [GovTool DRep Directory](https://gov.tools/drep_directory) in your browser.`) →
  `- The "Browse DReps" link opens the in-app DRep directory (Governance → Directory). Your selected wallet and voting type are preserved while you browse, and choosing "Select for delegation" on a directory row returns you to this form with the DRep ID pre-filled.`
- Line 78 (the `> **Tip:**` paragraph mentioning GovTool) →
  `> **Tip:** To find a DRep, click the "Browse DReps" link next to the DRep ID input. The in-app DRep directory opens; click "Select for delegation" on a row to come back with the DRep ID filled in. Your wallet and voting-type selections are preserved for the round trip.`
- Line 84 (`- Your chosen vote type label — "Abstain", "No Confidence", or "Delegate to DRep" (the specific DRep ID is not displayed in the dialog)`) →
  `- Your chosen vote — "Abstain", "No Confidence", or, when delegating to a DRep, the full DRep ID you selected (displayed in the dialog so you can verify it before signing)`

**`04-troubleshooting.md`**

- Line 17: replace the trailing `(with preprod-specific DRep directory links)` with
  `(the in-app DRep directory is available on all networks)`.
- Line 57 (`1. Copy the DRep ID directly from the [GovTool DRep Directory](https://gov.tools/drep_directory).`) →
  `1. Open the in-app DRep directory (Governance → Directory, or the "Browse DReps" link next to the DRep ID input) and use "Select for delegation" — the ID is filled in for you.`
- Line 140 (`- **Bookmark the DRep directory** to easily find DRep IDs: [gov.tools/drep_directory](https://gov.tools/drep_directory).`) →
  `- **Use the in-app DRep directory** (Governance → Directory) to find DReps and their IDs.`

**`05-improvements-vs-light-wallets.md`** (only rows describing THIS flow's routing)

- Line 23 (`| DRep directory (in-app) | ❌ | ✅ ¹ | Partial | Daedalus links to external GovTool |`) →
  `| DRep directory (in-app) | Partial | ✅ ¹ | Partial | Daedalus ships an in-app directory; list-row selection pre-fills the delegation form |`
- Line 204 (`- **What:** Display DRep names, platforms, values, and voting track records within the app instead of linking to GovTool externally.`) →
  `- **What:** Display DRep names, platforms, values, and voting track records within the app. The in-app directory and list-row selection are shipped; verified metadata display is pending the anchor phases.`
- Line 252 (the GovTool ecosystem-table row ending `Currently linked to; maintenance risk noted in official docs`) — replace that trailing cell with
  `No longer linked from the delegation flow; listed for ecosystem context`.
- Leave line 33 (Lace release-notes fact) untouched.

Verification:
`grep -rn -i "gov\.tools\|govtool" .vscode/docs/walkthroughs/governance/` must hit ONLY
05-improvements line 33 (Lace note) and the GovTool name in the 05 ecosystem table row.

#### Step 4: Copy audit (AC-5)

- `git diff feat/drep-discovery -- source/renderer/app/i18n/locales/` — every added or
  changed value starts with `!!!`; no removed line deletes a `!!!` from a KEPT key (the
  four deleted `drepInputLabel*` keys are whole-key removals sanctioned by D2).
- `grep -c '"!!!' source/renderer/app/i18n/locales/en-US.json` must be ≥ its base-commit
  count + 3 (browseDRepsLink, card.select, confirmationDialog.drepId, drepInputLabel
  changed to `!!!`) — same for ja-JP.
- `yarn i18n:manage` exits clean (necessary but not sufficient — the `!!!` grep above is
  the sufficiency check).

#### Step 5: Anchor-content check (AC-3)

`git diff feat/drep-discovery --stat` must contain no anchor-fetch, anchor-parse, or
name-rendering change: the only rendered identity anywhere new is the raw bech32 ID.
(The task-113 spec's "never renders a name field" test is the automated pin.)

#### Step 6: Full gates (AC-7, AC-8)

```
node_modules/.bin/tsc --noEmit                            # zero errors (yarn compile equivalent)
yarn lint                                                 # if it fails with the devEngines npm error, use:
node_modules/.bin/eslint source storybook utils --ext .ts,.tsx
node_modules/.bin/jest --testPathPattern="VotingGovernancePage|VotingPowerDelegationConfirmationDialog|DRepDirectory" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="governance-sanitization" --no-coverage --runInBand   # 17/17 (AC-8)
yarn i18n:manage
node_modules/.bin/prettier --write <touched .ts/.tsx files>   # NOT the walkthrough copies (untracked), NOT JSONs
```

#### Step 7: Findings note

Create `.agent/plans/governance/drep-discovery/research/slice-2-findings.md` recording (at
minimum): D1 resolution as implemented (stub Detail route in harness; production helpers
`pickDelegationFormReturnState` reused by the stub); D2 removals (which keys died, label
unification across networks including preprod); D3 scope (raw-ID-only render; the
`credentialType` prefix classification); D4 deviation (`prettier --write` substituted for
`nix fmt` — user should run `nix fmt` before merge); the `.vscode` walkthrough gitignore
deviation (updated copies live in the worktree and need manual sync to the main checkout);
and the un-synced-return behavior note (return hop lands on `VotingUnavailable` when
`!networkStatus.isSynced`; pre-fill survives in `location.state`; owned by
`ux-refinement`).

#### Step 8: Commit

Commit (includes spec, story, findings note, tracker/PRD updates riding along; walkthrough
copies are untracked and will not appear in the diff):
`test(gov): task-114 verify software-wallet browse-select-delegate path`

### i18n Keys (task-114)

None — task-114 adds no new copy; it audits the slice's copy.

### Acceptance (task-114)

- [ ] AC-1/AC-2: end-to-end Jest asserts row select → ID-only confirmation → byte-equal
      `chosenOption` payload into `delegateVotes`.
- [ ] AC-3: no anchor-derived content anywhere in the slice diff.
- [ ] AC-4: stories cover selector-in-form + ID-only confirmation; locales via global toggle.
- [ ] AC-5: `!!!` audit passes both directions.
- [ ] AC-6: walkthrough GovTool-routing language removed for this flow (worktree copies;
      sync-back recorded).
- [ ] AC-7: typecheck + lint clean.
- [ ] AC-8: sanitization floor 17/17 green.

---

## Cross-Cutting Acceptance (All Tasks)

- `node_modules/.bin/tsc --noEmit` → zero errors after every task.
- Sanitization floor suite green after every task (17/17).
- No new `logger.*`, `analytics.*`, or electron-store call in any file this slice touches.
- `routes-config.ts` / `Routes.tsx` byte-identical to base (D1).
- `VotingStore.ts` / `GovernanceStore.ts` byte-identical to base (invariant #4).
- Every new/changed locale string starts with `!!!`; no `!!!` stripped (invariant #11).
- One subject-only commit per task; `translations/messages.json` diffs ride with the task
  that caused them; no `.scss.d.ts` committed.

## References

- Grounding brief: `/home/node/.claude/jobs/25eb7a06/tmp/slice-2-grounding-brief.md`
- PRD: [slice-2-PRD.md](./slice-2-PRD.md)
- Design: [drep-discovery-design.md](../designs/drep-discovery-design.md) (:38, :45, :47, :49, :151)
- Shared tokens: [shared-design-tokens.md](../designs/shared-design-tokens.md) (§4, §7, §9, §10)
- Research: [external-research.md](../research/external-research.md) (CTA labels),
  [slice-1-final-pass-findings.md](../research/slice-1-final-pass-findings.md) (§4 runner, §8 push guard)
