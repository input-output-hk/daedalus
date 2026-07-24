# Slice-4 Implementation Guide: DRep Detail View (On-Chain Only)

> **Companion PRD:** [slice-4-PRD.md](./slice-4-PRD.md) | **Task Tracker:** [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
>
> All line anchors below were verified against the live worktree
> `/workspaces/daedalus/.agent/worktrees/slice-4` (branch `job/slice-4`, base
> `d5e3a03f2`) on 2026-07-24. Re-verify an anchor only if the file was touched by an
> earlier step of this same guide.

---

## Implementation Order

```
task-116 (detail components + container + i18n + Jest + Storybook)
→ task-117 (route wiring + card CTA + analytics masking + harness migration + findings)
```

Dependencies force this order (117 needs 116's components and container).

## Cross-Cutting Renderer Note (applies to every task)

- **react-intl is 2.9.0**: use `injectIntl` / `intlShape` / `defineMessages` /
  `FormattedMessage`. Never `useIntl()` or any react-intl hook.
- **React Router is 5.2**: `withRouter` + class containers
  (`DRepDirectoryPage.tsx:1-21` is the exact pattern to copy); `history.push(path, state)`
  carries `location.state`.
- **Locked invariants stated inline per step.** The global ones for this slice:
  - **#1 local-first** — the detail renders from `GovernanceStore` only. No per-DRep
    CLI/IPC call, no hosted explorer/indexer/API. The only data path is the existing bulk
    two-phase refresh.
  - **#2 sanitization floor** — never pass a DRep ID, `abstain`/`no_confidence` literal,
    or any CIP-129/CIP-105 bech32 string to `logger.*`, `analytics.sendEvent`, or an
    electron-store write. New component code in this slice makes **zero** logger/analytics
    calls (the only sanctioned pattern is logging `drepIdLength`, precedent
    `DRepIdDisplay.tsx:40-52`). Task-117 additionally **masks** the detail route's
    `:drepId` URL segment out of every Matomo payload.
  - **#3 anchor floor (negative)** — anchor URL + hash render as non-interactive text
    under the "On-chain anchor reference" label. No `<a>`, no `Link`, no
    `onExternalLinkClick`, no fetch, no copy button on anchor values.
  - **#4 no second delegation backend** — the DRep-ID handoff travels ONLY through React
    Router `location.state` via the existing pickers in
    `containers/governance/delegationFormState.ts`. No query params, no store fields.
  - **#10 byte-equality** — the id in the route param is used untransformed
    (bech32 is lowercase alphanumerics; nothing URL-encodes) and becomes
    `selectedDRepId` verbatim.
  - **#11 preliminary copy** — every NEW en-US and ja-JP string starts with `!!!`.
    Never strip an existing `!!!`.
- **Code comments**: only where logic is not self-evident; 1–3 plain lines stating the
  why/invariant. No task IDs, no review labels, no ALL-CAPS tags, no change history.
- **Jest assertion style**: never `toHaveBeenCalledWith('str', { literal: 'object' })`
  (prettier 2.1.2 oscillates on it) — always `expect.objectContaining({ … })` for object
  arguments.
- **Verification commands** (run from the worktree root
  `/workspaces/daedalus/.agent/worktrees/slice-4`):
  - **`npx` DOES NOT WORK in this devcontainer**: npm 11.13.0 rejects the repo
    `package.json`'s string-form `devEngines` and every `npx <tool>` exits with
    `npm error Invalid property "devEngines.node"` before the tool runs. That error is
    environmental, NOT a code failure — never try to "fix" it. Invoke every tool as
    `node_modules/.bin/<tool>` (verified working: tsc 4.9.5, eslint 8.13.0, jest
    27.5.1, prettier 2.1.2) or `yarn <tool>`.
  - Typecheck: `node_modules/.bin/tsc --noEmit` — must exit 0 with ZERO errors
    (`yarn compile` is unreliable under Node 24 — do not use it).
  - Lint: `node_modules/.bin/eslint <touched paths> --ext .ts,.tsx`. Note: the floor
    suite `tests/jest/security/governance-sanitization.spec.ts` is excluded by a
    pre-existing eslint ignore (slice-3 finding I-5) — hold its conventions by hand.
  - Focused Jest: `node_modules/.bin/jest <spec paths> --no-coverage --runInBand`.
  - Sanitization floor: `node_modules/.bin/jest
    tests/jest/security/governance-sanitization.spec.ts --no-coverage --runInBand` →
    **20/20 at baseline (re-verified via this exact path 2026-07-24); 23/23 after
    task-117**.
  - Copy changes: `yarn i18n:manage` (works under Node 24 — ux-refinement F-7; it
    rewrites `translations/messages.json` — that diff rides with the task commit; never
    hand-edit or prettier that file or the locale JSONs).
  - Format: `node_modules/.bin/prettier --write` on the changed `.ts/.tsx/.scss` files
    ONLY (nix is unavailable in this devcontainer — prettier substitutes `nix fmt`;
    never run it on JSON). Expect prettier 2.1.2 to reformat pre-existing drift hunks
    in files it touches (slice-3 finding I-4) — keep those, they are formatting-only.
- **Never commit `.scss.d.ts` files.** The global `declare module '*.scss'` in
  `source/renderer/declaration.d.ts` types the new `DRepDetail.scss` (ux-refinement F-2).
- **Commits**: exactly one per task, subject-only Conventional Commits, no body, no
  trailers. Subjects are given at the end of each task.
- **Never push, never open a PR** (no credentials in this devcontainer).

---

## task-116: Build DRep detail view component (on-chain only)

**Files to edit/create:**

| # | File | Action |
|---|---|---|
| 1 | `source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx` | EDIT (replace file) |
| 2 | `source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx` | EDIT |
| 3 | `source/renderer/app/components/governance/_shared/DRepIdDisplay.scss` | EDIT (append) |
| 4 | `source/renderer/app/components/governance/drep-detail/DRepDetailOnchainSection.tsx` | CREATE |
| 5 | `source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection.tsx` | CREATE |
| 6 | `source/renderer/app/components/governance/drep-detail/DRepDetailActions.tsx` | CREATE |
| 7 | `source/renderer/app/components/governance/drep-detail/DRepDetail.tsx` | CREATE |
| 8 | `source/renderer/app/components/governance/drep-detail/DRepDetail.scss` | CREATE |
| 9 | `source/renderer/app/containers/governance/DRepDetailPage.tsx` | CREATE |
| 10 | `source/renderer/app/i18n/locales/en-US.json` | EDIT |
| 11 | `source/renderer/app/i18n/locales/ja-JP.json` | EDIT |
| 12 | `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx` | CREATE |
| 13 | `storybook/stories/governance/DRepDetail.stories.tsx` | CREATE |

Do **NOT** touch `routes-config.ts`, `Routes.tsx`, `DRepCard.tsx`, `MatomoClient.ts`,
`VotingGovernancePage.spec.tsx`, or the sanitization suite in this task — all of that is
task-117. `VotingStore.ts` and `GovernanceStore.ts` stay byte-identical for the whole
slice.

### Step-by-Step

#### Step 1: Extend `DRepSourceLabel` with the anchor-reference variant

Replace the entire contents of
`source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx` (currently 29
lines; the existing `'on-chain'` behavior must be preserved exactly) with:

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';

const messages = defineMessages({
  onChain: {
    id: 'governance.drepDirectory.source.onChain',
    defaultMessage: '!!!On-chain',
    description: 'Source label for on-chain DRep data',
  },
  anchorReference: {
    id: 'governance.drepDetail.sourceLabel.anchorReference',
    defaultMessage: '!!!On-chain anchor reference',
    description:
      'Source label for the raw anchor URL and hash pair recorded on-chain',
  },
});

export type DRepSourceLabelVariant = 'on-chain' | 'on-chain-anchor-reference';

type Props = {
  source: DRepSourceLabelVariant;
  className?: string;
  intl: intlShape.isRequired;
};

function DRepSourceLabel({ source, className, intl }: Props) {
  const messageBySource = {
    'on-chain': messages.onChain,
    'on-chain-anchor-reference': messages.anchorReference,
  };
  const message = messageBySource[source];
  if (!message) return null;

  return (
    <span className={className}>{intl.formatMessage(message)}</span>
  );
}

export default injectIntl(DRepSourceLabel);
```

The `'on-chain'` id stays `governance.drepDirectory.source.onChain` (live slice-1 key —
do not rename it to the §9 `sourceLabel.onchain` form; that reconciliation is not owned
here). The new variant id `governance.drepDetail.sourceLabel.anchorReference` is PRD
decision P-1. Existing call sites (`DRepCard.tsx:90`) pass `source="on-chain"` and are
unaffected.

#### Step 2: Add the copied confirmation to `DRepIdDisplay`

`source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx` — current seams:
imports :1-8, messages :10-21, `Props` :23-26, `handleCopy` :39-53, JSX :57-72.

1. Change line 1 from

```ts
import React, { useCallback } from 'react';
```

to

```ts
import React, { useCallback, useState } from 'react';
```

2. Add to the `defineMessages` block (after `copyLabel`, before the closing `});` at
   :21):

```ts
  copiedToast: {
    id: 'governance.drepDetail.copyIdToast',
    defaultMessage: '!!!DRep ID copied',
    description: 'Inline confirmation shown after copying a DRep ID',
  },
```

3. Widen `Props` (:23-26) to:

```ts
interface Props {
  drepId: string;
  showCopiedConfirmation?: boolean;
  intl: intlShape.isRequired;
}
```

4. Change the component signature (:38) to:

```ts
function DRepIdDisplay({ drepId, showCopiedConfirmation = false, intl }: Props) {
  const [copied, setCopied] = useState(false);
```

5. In `handleCopy`, change the copy call (:47-52) from

```ts
    navigator.clipboard.writeText(drepId).catch((error) => {
```

to

```ts
    navigator.clipboard
      .writeText(drepId)
      .then(() => setCopied(true))
      .catch((error) => {
```

(keep the existing `logger.warn('DRepIdDisplay: failed to copy DRep ID', { error,
drepIdLength: drepId.length });` body of the catch exactly as-is — invariant #2: only
`drepIdLength` is ever logged, never the id).

6. Inside the returned `<span className={styles.container}>`, after the `<Button …/>`
   (:64-70), add:

```tsx
      {showCopiedConfirmation && copied && (
        <span
          className={styles.copiedConfirmation}
          role="status"
          aria-live="polite"
        >
          {intl.formatMessage(messages.copiedToast)}
        </span>
      )}
```

The confirmation is persistent (no timer) and announced politely — shared tokens §4.
The prop defaults to `false`, so every existing card render is byte-identical in
behavior; only the detail view opts in (PRD P-2).

#### Step 3: `DRepIdDisplay.scss`

Append at the end of
`source/renderer/app/components/governance/_shared/DRepIdDisplay.scss` (file currently
ends with the `.copyButton` rule):

```scss
.copiedConfirmation {
  font-size: 12px;
  color: var(--theme-text-secondary, #6b7384);
}
```

#### Step 4: Create `DRepDetailOnchainSection.tsx`

Create
`source/renderer/app/components/governance/drep-detail/DRepDetailOnchainSection.tsx`
with exactly:

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import DRepStatusBadge from '../_shared/DRepStatusBadge';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import { VotingPowerEnrichState } from '../../../stores/GovernanceStore';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDetail.onchain.title',
    defaultMessage: '!!!On-chain',
    description: 'Heading of the on-chain section on the DRep detail view',
  },
  statusLabel: {
    id: 'governance.drepDetail.status',
    defaultMessage: '!!!Status',
    description: 'Label for the DRep status field on the detail view',
  },
  expiresInLabel: {
    id: 'governance.drepDetail.expiresIn',
    defaultMessage: '!!!Expires in',
    description: 'Label for the remaining-epochs field on the detail view',
  },
  expiresInValue: {
    id: 'governance.drepDetail.expiresInEpochs',
    defaultMessage: '!!!{count} epochs',
    description: 'Remaining epochs until the DRep expires',
  },
  votingPowerLabel: {
    id: 'governance.drepDetail.votingPower',
    defaultMessage: '!!!Voting power',
    description: 'Label for the voting power field on the detail view',
  },
  votingPowerLovelace: {
    id: 'governance.drepDetail.votingPowerLovelace',
    defaultMessage: '!!!({amount} lovelace)',
    description: 'Secondary raw-lovelace line under the ADA voting power',
  },
  votingPowerLoadingTooltip: {
    id: 'governance.drepDetail.votingPower.loadingTooltip',
    defaultMessage: '!!!Loading voting power…',
    description: 'Tooltip on the voting-power placeholder during enrichment',
  },
  votingPowerUnavailableTooltip: {
    id: 'governance.drepDetail.votingPower.unavailableTooltip',
    defaultMessage: '!!!Stake distribution unavailable this refresh.',
    description: 'Tooltip on the voting-power placeholder when stake failed',
  },
  votePositionsLabel: {
    id: 'governance.drepDetail.votePositions',
    defaultMessage: '!!!Current votes',
    description: 'Label for the current-epoch vote positions field',
  },
  votePositionsUnavailable: {
    id: 'governance.drepDetail.votePositions.unavailable',
    defaultMessage: '!!!Vote positions are not available in this version.',
    description: 'Graceful value when vote positions cannot be shown',
  },
});

interface Props {
  entry: AppDRepDirectoryEntry;
  votingPowerState: VotingPowerEnrichState;
  intl: intlShape.isRequired;
}

// Detail-form rendering: full ADA with thousands separators; the raw
// lovelace renders on a secondary line, never rounded away.
function formatAdaExact(lovelace: BigNumber): string {
  return `₳ ${lovelace.div(1_000_000).toFormat()}`;
}

function DRepDetailOnchainSection({ entry, votingPowerState, intl }: Props) {
  // Loading vs unavailable follows the enrich state, mirroring the card.
  const votingPowerTooltip =
    entry.votingPower === null
      ? intl.formatMessage(
          votingPowerState === VotingPowerEnrichState.Loading
            ? messages.votingPowerLoadingTooltip
            : messages.votingPowerUnavailableTooltip
        )
      : undefined;

  return (
    <section
      className={styles.section}
      aria-label={intl.formatMessage(messages.title)}
    >
      <h2 className={styles.sectionTitle}>
        {intl.formatMessage(messages.title)}
      </h2>
      <dl className={styles.fieldList}>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.statusLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            <DRepStatusBadge status={entry.status} />
          </dd>
        </div>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.expiresInLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            {entry.status === 'active' && entry.drepActivity != null
              ? intl.formatMessage(messages.expiresInValue, {
                  count: entry.drepActivity,
                })
              : '—'}
          </dd>
        </div>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.votingPowerLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            {entry.votingPower ? (
              <>
                <span className={styles.votingPowerAda}>
                  {formatAdaExact(entry.votingPower)}
                </span>
                <span className={styles.votingPowerLovelace}>
                  {intl.formatMessage(messages.votingPowerLovelace, {
                    amount: entry.votingPower.toFormat(0),
                  })}
                </span>
              </>
            ) : (
              <span title={votingPowerTooltip} aria-label={votingPowerTooltip}>
                —
              </span>
            )}
          </dd>
        </div>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.votePositionsLabel)}
          </dt>
          <dd className={styles.mutedValue}>
            {intl.formatMessage(messages.votePositionsUnavailable)}
          </dd>
        </div>
      </dl>
    </section>
  );
}

export default injectIntl(DRepDetailOnchainSection);
```

Notes pinned by the PRD: the expiry row shows `—` unless `status === 'active'` and
`drepActivity != null` (P-9 — `drepActivity` is "0 when inactive, null if unknown",
`governance.types.ts:37-38`); the vote-positions row is the D1 graceful-unavailable
state (no `gov-state` query exists — do NOT add one); the section heading is the sole
"On-chain" provenance marker (P-3 — no `DRepSourceLabel` in this section); the Status
row here is the detail's ONLY `DRepStatusBadge` instance (the header in Step 7 renders
none — wireframe placement, design :84-94). `BigNumber` is imported as a value to
mirror `DRepCard.tsx:3` even though it is only used as a type.

#### Step 5: Create `DRepDetailAnchorSection.tsx`

Create
`source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection.tsx`
with exactly:

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import DRepSourceLabel from '../_shared/DRepSourceLabel';
import type { DRepAnchorPresence } from '../../../../../common/types/governance.types';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDetail.anchor.title',
    defaultMessage: '!!!Anchor',
    description: 'Heading of the anchor section on the DRep detail view',
  },
  urlLabel: {
    id: 'governance.drepDetail.anchor.url',
    defaultMessage: '!!!Anchor URL',
    description: 'Label for the on-chain anchor URL field',
  },
  hashLabel: {
    id: 'governance.drepDetail.anchor.hash',
    defaultMessage: '!!!Anchor hash',
    description: 'Label for the on-chain anchor hash field',
  },
  sourceRowLabel: {
    id: 'governance.drepDetail.anchor.source',
    defaultMessage: '!!!Source',
    description: 'Label for the anchor source-label row',
  },
  none: {
    id: 'governance.drepDetail.anchor.none',
    defaultMessage: '!!!No anchor is recorded on-chain for this DRep.',
    description: 'Shown when the DRep registered without an anchor',
  },
});

interface Props {
  anchor: DRepAnchorPresence | null;
  intl: intlShape.isRequired;
}

function DRepDetailAnchorSection({ anchor, intl }: Props) {
  return (
    <section
      className={styles.section}
      aria-label={intl.formatMessage(messages.title)}
    >
      <h2 className={styles.sectionTitle}>
        {intl.formatMessage(messages.title)}
      </h2>
      {anchor ? (
        <dl className={styles.fieldList}>
          <div className={styles.fieldRow}>
            <dt className={styles.fieldLabel}>
              {intl.formatMessage(messages.urlLabel)}
            </dt>
            {/* Deliberately inert text: no anchor may be fetched, rendered as
                a link, or opened before the hardened anchor pipeline lands. */}
            <dd className={styles.anchorValue}>{anchor.url}</dd>
          </div>
          <div className={styles.fieldRow}>
            <dt className={styles.fieldLabel}>
              {intl.formatMessage(messages.hashLabel)}
            </dt>
            <dd className={styles.anchorValue}>{anchor.hash}</dd>
          </div>
          <div className={styles.fieldRow}>
            <dt className={styles.fieldLabel}>
              {intl.formatMessage(messages.sourceRowLabel)}
            </dt>
            <dd className={styles.fieldValue}>
              <DRepSourceLabel
                source="on-chain-anchor-reference"
                className={styles.sourceLabel}
              />
            </dd>
          </div>
        </dl>
      ) : (
        <p className={styles.mutedValue}>
          {intl.formatMessage(messages.none)}
        </p>
      )}
    </section>
  );
}

export default injectIntl(DRepDetailAnchorSection);
```

Anchor floor inline (D7): URL and hash are plain `<dd>` text — no `<a>`, no react-polymorph
`Link`, no copy button, no `onExternalLinkClick` prop anywhere in this file. The
"On-chain anchor reference" label (tokens §2) is mandatory whenever an anchor exists.

#### Step 6: Create `DRepDetailActions.tsx`

Create `source/renderer/app/components/governance/drep-detail/DRepDetailActions.tsx`
with exactly:

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  select: {
    id: 'governance.drepDirectory.card.select',
    defaultMessage: '!!!Select for delegation',
    description: 'CTA that hands the DRep ID to the delegation form',
  },
});

interface Props {
  drepId: string;
  onSelectForDelegation: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepDetailActions({ drepId, onSelectForDelegation, intl }: Props) {
  return (
    <div className={styles.actions}>
      <Button
        label={intl.formatMessage(messages.select)}
        onClick={() => onSelectForDelegation(drepId)}
        skin={ButtonSkin}
      />
    </div>
  );
}

export default injectIntl(DRepDetailActions);
```

The `governance.drepDirectory.card.select` id is deliberately re-declared here with an
identical `defaultMessage` (PRD P-5; duplicate-id precedent:
`governance.drepDirectory.title` in both `DRepDirectory.tsx:20-24` and
`DRepDirectoryBanner.tsx:9-13`). NO favorite toggle ships in this component (D3 —
slice-7 owns favorites; no stub UI).

#### Step 7: Create `DRepDetail.tsx`

Create `source/renderer/app/components/governance/drep-detail/DRepDetail.tsx` with
exactly:

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import DRepIdDisplay from '../_shared/DRepIdDisplay';
import DRepDetailOnchainSection from './DRepDetailOnchainSection';
import DRepDetailAnchorSection from './DRepDetailAnchorSection';
import DRepDetailActions from './DRepDetailActions';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../../stores/GovernanceStore';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDetail.title',
    defaultMessage: '!!!DRep detail',
    description: 'Title of the DRep detail page',
  },
  backToDirectory: {
    id: 'governance.drepDirectory.backToDirectory',
    defaultMessage: '!!!Back to directory',
    description: 'Link from the DRep detail back to the directory',
  },
  loading: {
    id: 'governance.drepDetail.loading',
    defaultMessage: '!!!Loading DRep data…',
    description: 'Loading state on the DRep detail page',
  },
  notFound: {
    id: 'governance.drepDetail.notFound',
    defaultMessage: '!!!This DRep was not found in the latest on-chain data.',
    description: 'Inline error when the requested DRep is not in the index',
  },
});

interface Props {
  entry: AppDRepDirectoryEntry | null;
  refreshState: GovernanceRefreshState;
  votingPowerState: VotingPowerEnrichState;
  onSelectForDelegation: (drepId: string) => void;
  onBackToDirectory: () => void;
  intl: intlShape.isRequired;
}

function DRepDetail({
  entry,
  refreshState,
  votingPowerState,
  onSelectForDelegation,
  onBackToDirectory,
  intl,
}: Props) {
  const backLink = (
    <Link
      className={styles.backLink}
      label={intl.formatMessage(messages.backToDirectory)}
      hasIconAfter={false}
      onClick={onBackToDirectory}
      skin={LinkSkin}
    />
  );

  if (!entry) {
    // Deep links land here before the list query answers; only a settled
    // store (loaded or failed) may declare the DRep missing.
    if (
      refreshState === GovernanceRefreshState.Idle ||
      refreshState === GovernanceRefreshState.Loading ||
      refreshState === GovernanceRefreshState.Refreshing
    ) {
      return (
        <div className={styles.container}>
          {backLink}
          <div className={styles.stateContainer}>
            <LoadingSpinner />
            <p>{intl.formatMessage(messages.loading)}</p>
          </div>
        </div>
      );
    }
    return (
      <div className={styles.container}>
        {backLink}
        <div className={styles.stateContainer}>
          <p className={styles.errorMessage}>
            {intl.formatMessage(messages.notFound)}
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className={styles.container}>
      {backLink}
      <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
      <div className={styles.header}>
        <DRepIdDisplay drepId={entry.drepId} showCopiedConfirmation />
      </div>
      <DRepDetailOnchainSection
        entry={entry}
        votingPowerState={votingPowerState}
      />
      <DRepDetailAnchorSection anchor={entry.anchor} />
      <DRepDetailActions
        drepId={entry.drepId}
        onSelectForDelegation={onSelectForDelegation}
      />
    </div>
  );
}

export default injectIntl(DRepDetail);
```

State mapping is PRD P-4 (entry → content, incl. stale-while-refresh during
`Refreshing`; no entry + unsettled store → loading; no entry + settled store →
not-found + back link — design :205). The back link's explicit `skin={LinkSkin}` is PRD
P-6. The ID display is CIP-129-only via the existing shared component (D5 — no dual-ID,
no bech32 dependency). The header renders NO `DRepStatusBadge` — the badge appears
exactly once, in the on-chain section's Status row (the wireframe's placement, design
:84-94); a second instance would make `screen.getByText('!!!Active')` in the Step 11
spec match twice and fail. `AppDRepDirectoryEntry` uses a separate `import type`
(repo precedent `DRepCard.tsx:9`).

#### Step 8: Create `DRepDetail.scss`

Create `source/renderer/app/components/governance/drep-detail/DRepDetail.scss` with
exactly:

```scss
.container {
  display: flex;
  flex-direction: column;
  gap: 16px;
  padding: 20px;
}

.backLink {
  align-self: flex-start;
  font-size: 14px;
}

.title {
  margin: 0;
  font-size: 20px;
  font-weight: 600;
  color: var(--theme-text-primary);
}

.header {
  display: flex;
  align-items: center;
  gap: 12px;
}

.section {
  padding: 16px;
  border: 1px solid var(--theme-separator, #e0e0e0);
  border-radius: 8px;
}

.sectionTitle {
  margin: 0 0 12px;
  font-size: 14px;
  font-weight: 600;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  color: var(--theme-text-secondary, #6b7384);
}

.fieldList {
  display: flex;
  flex-direction: column;
  gap: 10px;
  margin: 0;
}

.fieldRow {
  display: flex;
  align-items: baseline;
  gap: 8px;
}

.fieldLabel {
  min-width: 120px;
  font-size: 14px;
  color: var(--theme-text-secondary, #6b7384);
}

.fieldValue {
  margin: 0;
  font-size: 14px;
  color: var(--theme-text-primary);
}

.mutedValue {
  margin: 0;
  font-size: 14px;
  color: var(--theme-text-tertiary, #8e939e);
}

.votingPowerAda {
  display: block;
  font-weight: 600;
}

.votingPowerLovelace {
  display: block;
  font-size: 12px;
  color: var(--theme-text-secondary, #6b7384);
}

.anchorValue {
  margin: 0;
  font-family: var(--font-mono, 'SF Mono', 'Fira Code', monospace);
  font-size: 13px;
  word-break: break-all;
  color: var(--theme-text-primary);
}

.sourceLabel {
  font-size: 12px;
  color: var(--theme-text-tertiary, #8e939e);
}

.actions {
  display: flex;
  justify-content: flex-start;
}

.stateContainer {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 12px;
  padding: 40px 0;
}

.errorMessage {
  font-weight: 600;
  color: var(--theme-color-error, #ea4c5b);
}
```

(The explicit `margin: 0` on `dd`-bearing classes cancels the browser's default
`dd` indent. Token fallbacks mirror `DRepCard.scss`.)

#### Step 9: Create the container — `DRepDetailPage.tsx`

Create `source/renderer/app/containers/governance/DRepDetailPage.tsx` with exactly
(the mount/unmount lifecycle is copied from `DRepDirectoryPage.tsx:24-56` — D9):

```tsx
import React from 'react';
import { observer, inject } from 'mobx-react';
import { reaction } from 'mobx';
import type { IReactionDisposer } from 'mobx';
import { withRouter } from 'react-router-dom';
import type { RouteComponentProps } from 'react-router-dom';
import DRepDetail from '../../components/governance/drep-detail/DRepDetail';
import GovernanceStore, {
  GovernanceRefreshState,
} from '../../stores/GovernanceStore';
import type { StoresMap } from '../../stores';
import { ROUTES } from '../../routes-config';
import { pickDelegationFormReturnState } from './delegationFormState';

interface Props extends RouteComponentProps<{ drepId: string }> {
  stores?: StoresMap;
}

@inject('stores')
@observer
class DRepDetailPage extends React.Component<Props> {
  syncReactionDisposer: IReactionDisposer | null = null;

  componentDidMount() {
    const { stores } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;

    if (!governanceStore) {
      return;
    }

    // Deep links and restarts land here with an empty store; the same
    // refresh contract as the directory keeps the detail self-sufficient.
    if (
      governanceStore.refreshState === GovernanceRefreshState.Idle ||
      governanceStore.refreshState === GovernanceRefreshState.Failed
    ) {
      governanceStore.refresh();
    }

    // Replace the possibly-incomplete syncing snapshot exactly once when the
    // node reaches the tip; reaction fires only on the false -> true edge.
    this.syncReactionDisposer = reaction(
      () => stores?.networkStatus.isNodeInSync,
      (isNodeInSync) => {
        if (isNodeInSync) {
          governanceStore.refresh();
        }
      }
    );
  }

  componentWillUnmount() {
    if (this.syncReactionDisposer) {
      this.syncReactionDisposer();
      this.syncReactionDisposer = null;
    }
  }

  handleSelectForDelegation = (drepId: string) => {
    // The inherited { from, selectedWalletId, voteType } plus the id return
    // to the form through location.state only — never query params or stores.
    const inherited = pickDelegationFormReturnState(this.props.location.state);
    this.props.history.push(inherited?.from ?? ROUTES.VOTING.GOVERNANCE, {
      ...inherited,
      selectedDRepId: drepId,
    });
  };

  handleBackToDirectory = () => {
    // Keep the round-trip state alive so a row-select after backing out
    // still restores the user's wallet and vote type.
    this.props.history.push(
      ROUTES.GOVERNANCE.DREPS,
      pickDelegationFormReturnState(this.props.location.state)
    );
  };

  render() {
    const { stores, match } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;

    if (!governanceStore) return null;

    // The route param is used untransformed: bech32 ids contain no characters
    // that URL-encode, so byte-equality with the directory entry holds.
    const { drepId } = match.params;

    return (
      <DRepDetail
        entry={governanceStore.drepIndex.get(drepId) ?? null}
        refreshState={governanceStore.refreshState}
        votingPowerState={governanceStore.votingPowerState}
        onSelectForDelegation={this.handleSelectForDelegation}
        onBackToDirectory={this.handleBackToDirectory}
      />
    );
  }
}

export default withRouter(DRepDetailPage);
```

Invariants inline: reads `drepIndex` only (#1 local-first — NO new IPC call, no
per-DRep query); `handleSelectForDelegation` is the production implementation of the
slice-2 `DetailRouteStub` contract (D10 — same picker, same fallback target
`inherited?.from ?? ROUTES.VOTING.GOVERNANCE`, `selectedDRepId` from the route param
byte-equal, invariant #10); the back push forwards the picked state (P-7). Neither
handler may call `logger.*` or `analytics.*` (#2).

#### Step 10: Update both locale JSONs (keep alphabetical key order)

**`source/renderer/app/i18n/locales/en-US.json`** — insert this block immediately
BEFORE line 284 (`"governance.drepDirectory.card.select": "!!!Select for delegation",`);
`governance.drepDetail.*` and `governance.drepDirectory.backToDirectory` both sort
before `governance.drepDirectory.card.select`:

```json
  "governance.drepDetail.anchor.hash": "!!!Anchor hash",
  "governance.drepDetail.anchor.none": "!!!No anchor is recorded on-chain for this DRep.",
  "governance.drepDetail.anchor.source": "!!!Source",
  "governance.drepDetail.anchor.title": "!!!Anchor",
  "governance.drepDetail.anchor.url": "!!!Anchor URL",
  "governance.drepDetail.copyIdToast": "!!!DRep ID copied",
  "governance.drepDetail.expiresIn": "!!!Expires in",
  "governance.drepDetail.expiresInEpochs": "!!!{count} epochs",
  "governance.drepDetail.loading": "!!!Loading DRep data…",
  "governance.drepDetail.notFound": "!!!This DRep was not found in the latest on-chain data.",
  "governance.drepDetail.onchain.title": "!!!On-chain",
  "governance.drepDetail.sourceLabel.anchorReference": "!!!On-chain anchor reference",
  "governance.drepDetail.status": "!!!Status",
  "governance.drepDetail.title": "!!!DRep detail",
  "governance.drepDetail.votePositions": "!!!Current votes",
  "governance.drepDetail.votePositions.unavailable": "!!!Vote positions are not available in this version.",
  "governance.drepDetail.votingPower": "!!!Voting power",
  "governance.drepDetail.votingPower.loadingTooltip": "!!!Loading voting power…",
  "governance.drepDetail.votingPower.unavailableTooltip": "!!!Stake distribution unavailable this refresh.",
  "governance.drepDetail.votingPowerLovelace": "!!!({amount} lovelace)",
  "governance.drepDirectory.backToDirectory": "!!!Back to directory",
```

**`source/renderer/app/i18n/locales/ja-JP.json`** — insert immediately BEFORE the same
neighbor (`"governance.drepDirectory.card.select"`, line 284):

```json
  "governance.drepDetail.anchor.hash": "!!!アンカーハッシュ",
  "governance.drepDetail.anchor.none": "!!!このDRepのアンカーはオンチェーンに記録されていません。",
  "governance.drepDetail.anchor.source": "!!!ソース",
  "governance.drepDetail.anchor.title": "!!!アンカー",
  "governance.drepDetail.anchor.url": "!!!アンカーURL",
  "governance.drepDetail.copyIdToast": "!!!DRep IDをコピーしました",
  "governance.drepDetail.expiresIn": "!!!失効まで",
  "governance.drepDetail.expiresInEpochs": "!!!{count}エポック",
  "governance.drepDetail.loading": "!!!DRepデータを読み込み中…",
  "governance.drepDetail.notFound": "!!!このDRepは最新のオンチェーンデータに見つかりませんでした。",
  "governance.drepDetail.onchain.title": "!!!オンチェーン",
  "governance.drepDetail.sourceLabel.anchorReference": "!!!オンチェーンアンカー参照",
  "governance.drepDetail.status": "!!!ステータス",
  "governance.drepDetail.title": "!!!DRep詳細",
  "governance.drepDetail.votePositions": "!!!現在の投票",
  "governance.drepDetail.votePositions.unavailable": "!!!投票状況はこのバージョンでは利用できません。",
  "governance.drepDetail.votingPower": "!!!投票権",
  "governance.drepDetail.votingPower.loadingTooltip": "!!!投票権を読み込み中…",
  "governance.drepDetail.votingPower.unavailableTooltip": "!!!今回の更新ではステーク分布を利用できません。",
  "governance.drepDetail.votingPowerLovelace": "!!!({amount} lovelace)",
  "governance.drepDirectory.backToDirectory": "!!!ディレクトリに戻る",
```

Invariant #11 inline: every value above starts with `!!!` in BOTH files; no existing
string loses its marker; no existing key changes.

#### Step 11: Create the container spec — `DRepDetailPage.spec.tsx`

Create `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx`. The harness
follows `DRepDirectoryPage.spec.tsx` (observable networkStatus + mobx `Provider` +
`IntlProvider` + memory Router). Full file:

```tsx
import React from 'react';
import BigNumber from 'bignumber.js';
import { observable, runInAction } from 'mobx';
import { Provider } from 'mobx-react';
import { Route, Router } from 'react-router-dom';
import { createMemoryHistory } from 'history';
import { IntlProvider } from 'react-intl';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import jaTranslations from '../../i18n/locales/ja-JP.json';
import { ROUTES } from '../../routes-config';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../stores/GovernanceStore';
import type { AppDRepDirectoryEntry } from '../../stores/GovernanceStore';
import DRepDetailPage from './DRepDetailPage';

const DREP_ID = 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b';
// The route literal lands with the route-wiring task; deriving the path from
// the directory literal keeps this harness aligned with it.
const DETAIL_PATH = `${ROUTES.GOVERNANCE.DREPS}/:drepId`;

const baseEntry: AppDRepDirectoryEntry = {
  anchor: {
    hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
    url: 'https://governance-preview.example.org/dreps/1.json',
  },
  drepActivity: 34,
  drepId: DREP_ID,
  status: 'active',
  votingPower: new BigNumber('23137980123456'),
};

const buildGovernanceStore = (overrides: Record<string, unknown> = {}) => ({
  drepIndex: new Map([[DREP_ID, baseEntry]]),
  drepList: [baseEntry],
  error: null,
  lastFetchedAt: Date.now() - 60_000,
  refresh: jest.fn(),
  refreshState: GovernanceRefreshState.Loaded,
  votingPowerState: VotingPowerEnrichState.Loaded,
  ...overrides,
});

const renderPage = ({
  governanceOverrides = {},
  isNodeInSync = true,
  locale = 'en-US',
  locationState,
  syncProgress = 100,
}: {
  governanceOverrides?: Record<string, unknown>;
  isNodeInSync?: boolean;
  locale?: string;
  locationState?: Record<string, unknown>;
  syncProgress?: number | null;
} = {}) => {
  // Observable so the container's reaction sees the flip like the real store.
  const networkStatus = observable({ isNodeInSync, syncProgress });
  const governance = buildGovernanceStore(governanceOverrides);
  const history = createMemoryHistory({
    initialEntries: [
      {
        pathname: `${ROUTES.GOVERNANCE.DREPS}/${DREP_ID}`,
        state: locationState,
      },
    ],
  });
  const pushSpy = jest.spyOn(history, 'push');
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  const view = render(
    <Provider stores={{ governance, networkStatus } as any}>
      <IntlProvider locale={locale} messages={messages}>
        <Router history={history}>
          <Route path={DETAIL_PATH} component={DRepDetailPage} />
        </Router>
      </IntlProvider>
    </Provider>
  );
  return { governance, history, networkStatus, pushSpy, ...view };
};

describe('DRepDetailPage', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('renders the on-chain fields for a loaded entry', () => {
    renderPage();

    expect(screen.getByText('!!!DRep detail')).toBeInTheDocument();
    expect(screen.getByText('!!!Active')).toBeInTheDocument();
    expect(screen.getByText('!!!34 epochs')).toBeInTheDocument();
    expect(screen.getByText('₳ 23,137,980.123456')).toBeInTheDocument();
    expect(
      screen.getByText('!!!(23,137,980,123,456 lovelace)')
    ).toBeInTheDocument();
    expect(
      screen.getByText('!!!Vote positions are not available in this version.')
    ).toBeInTheDocument();
  });

  it('renders the anchor presence with the on-chain anchor reference label', () => {
    renderPage();

    expect(
      screen.getByText('https://governance-preview.example.org/dreps/1.json')
    ).toBeInTheDocument();
    expect(screen.getByText(baseEntry.anchor!.hash)).toBeInTheDocument();
    expect(
      screen.getByText('!!!On-chain anchor reference')
    ).toBeInTheDocument();
    // The anchor URL renders as inert text, never inside an anchor element.
    expect(
      screen
        .getByText('https://governance-preview.example.org/dreps/1.json')
        .closest('a')
    ).toBeNull();
  });

  it('renders the anchor-absent message when no anchor is recorded', () => {
    renderPage({
      governanceOverrides: {
        drepIndex: new Map([[DREP_ID, { ...baseEntry, anchor: null }]]),
      },
    });

    expect(
      screen.getByText('!!!No anchor is recorded on-chain for this DRep.')
    ).toBeInTheDocument();
    expect(
      screen.queryByText('!!!On-chain anchor reference')
    ).not.toBeInTheDocument();
  });

  it('shows — with the unavailable tooltip when stake enrichment failed', () => {
    renderPage({
      governanceOverrides: {
        drepIndex: new Map([[DREP_ID, { ...baseEntry, votingPower: null }]]),
        votingPowerState: VotingPowerEnrichState.Failed,
      },
    });

    expect(screen.getByText('—')).toHaveAttribute(
      'title',
      '!!!Stake distribution unavailable this refresh.'
    );
  });

  it('refreshes on mount from an empty Idle store and shows the loading state', () => {
    const { governance } = renderPage({
      governanceOverrides: {
        drepIndex: new Map(),
        drepList: [],
        refreshState: GovernanceRefreshState.Idle,
      },
    });

    expect(governance.refresh).toHaveBeenCalledTimes(1);
    expect(screen.getByText('!!!Loading DRep data…')).toBeInTheDocument();
    expect(
      screen.queryByText(/was not found in the latest on-chain data/)
    ).not.toBeInTheDocument();
  });

  it('shows the inline not-found error with a working Back to directory link', () => {
    const { pushSpy } = renderPage({
      governanceOverrides: { drepIndex: new Map() },
      locationState: {
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      },
    });

    expect(
      screen.getByText('!!!This DRep was not found in the latest on-chain data.')
    ).toBeInTheDocument();

    fireEvent.click(screen.getByText('!!!Back to directory'));

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.GOVERNANCE.DREPS,
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      })
    );
  });

  it('forwards inherited state plus the byte-equal id on Select for delegation', () => {
    const { pushSpy } = renderPage({
      locationState: {
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      },
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.VOTING.GOVERNANCE,
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedDRepId: DREP_ID,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      })
    );
  });

  it('falls back to the governance form route when no state was inherited', () => {
    const { pushSpy } = renderPage();

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.VOTING.GOVERNANCE,
      expect.objectContaining({ selectedDRepId: DREP_ID })
    );
  });

  it('refetches exactly once when the node reaches the tip', () => {
    const { governance, networkStatus } = renderPage({
      isNodeInSync: false,
      syncProgress: 99,
    });
    expect(governance.refresh).not.toHaveBeenCalled();

    act(() => {
      runInAction(() => {
        networkStatus.isNodeInSync = true;
        networkStatus.syncProgress = 100;
      });
    });

    expect(governance.refresh).toHaveBeenCalledTimes(1);
  });

  it('shows the copied confirmation after the copy button is clicked', async () => {
    const writeText = jest.fn(async () => undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });
    try {
      renderPage();

      fireEvent.click(
        screen.getByRole('button', { name: '!!!Copy DRep ID' })
      );

      expect(await screen.findByText('!!!DRep ID copied')).toBeInTheDocument();
      expect(writeText).toHaveBeenCalledWith(DREP_ID);
    } finally {
      delete (navigator as any).clipboard;
    }
  });

  it('renders the detail field labels in ja-JP', () => {
    renderPage({ locale: 'ja-JP' });

    expect(screen.getByText('!!!DRep詳細')).toBeInTheDocument();
    expect(screen.getByText('!!!ステータス')).toBeInTheDocument();
    expect(screen.getByText('!!!アンカー')).toBeInTheDocument();
    expect(screen.getByText('!!!34エポック')).toBeInTheDocument();
  });
});
```

Query notes (why these strings are safe): the on-chain section heading is
`'!!!On-chain'` and no source pill duplicates it inside the detail (P-3);
`'!!!Active'` matches once because the status badge renders only in the on-chain
Status row (never in the header — Step 7); the copy button's accessible name is its
`aria-label` `'!!!Copy DRep ID'` (`DRepIdDisplay.tsx:69`).

#### Step 12: Create the stories — `DRepDetail.stories.tsx`

Create `storybook/stories/governance/DRepDetail.stories.tsx` with exactly:

```tsx
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import DRepDetail from '../../../source/renderer/app/components/governance/drep-detail/DRepDetail';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../../source/renderer/app/stores/GovernanceStore';
import type { AppDRepDirectoryEntry } from '../../../source/renderer/app/stores/GovernanceStore';

const CENTERED_STYLE = {
  margin: '0 auto',
  maxWidth: 720,
  padding: 24,
};

const STATUS_OPTIONS = {
  Active: 'active',
  Inactive: 'inactive',
};

const withAnchorEntry: AppDRepDirectoryEntry = {
  anchor: {
    hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
    url: 'https://governance-preview.example.org/dreps/1.json',
  },
  drepActivity: 34,
  drepId: 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k4',
  status: 'active',
  votingPower: new BigNumber('23137980123456'),
};

const withoutAnchorEntry: AppDRepDirectoryEntry = {
  ...withAnchorEntry,
  anchor: null,
  drepId: 'drep1xj23tk3y_qyv7c9m2z89w3t8mvk9e2uwc3q8u6j7r2x5y9w0p1',
};

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
const renderDetail = (
  entry: AppDRepDirectoryEntry | null,
  refreshState: GovernanceRefreshState = GovernanceRefreshState.Loaded,
  votingPowerState: VotingPowerEnrichState = VotingPowerEnrichState.Loaded
) => (
  <div style={CENTERED_STYLE}>
    <DRepDetail
      entry={entry}
      onBackToDirectory={action('onBackToDirectory')}
      onSelectForDelegation={action('onSelectForDelegation')}
      refreshState={refreshState}
      votingPowerState={votingPowerState}
    />
  </div>
);

const drepStoryDecorator = (story: () => React.ReactNode) => (
  <StoryProvider>
    <StoryDecorator>{story()}</StoryDecorator>
  </StoryProvider>
);

storiesOf('Governance / DRep Detail', module)
  .addDecorator(drepStoryDecorator)
  .addDecorator(withKnobs)
  .add('Loaded — with anchor', () =>
    renderDetail({
      ...withAnchorEntry,
      drepActivity: number('Remaining epochs (drepActivity)', 34, {
        max: 60,
        min: 0,
        range: true,
        step: 1,
      }),
      status: select(
        'Status',
        STATUS_OPTIONS,
        'active'
      ) as AppDRepDirectoryEntry['status'],
    })
  )
  .add('Loaded — no anchor', () => renderDetail(withoutAnchorEntry))
  .add('Ranking unavailable', () =>
    renderDetail(
      { ...withAnchorEntry, votingPower: null },
      GovernanceRefreshState.Loaded,
      VotingPowerEnrichState.Failed
    )
  )
  .add('Loading', () => renderDetail(null, GovernanceRefreshState.Loading))
  .add('Not found', () => renderDetail(null, GovernanceRefreshState.Loaded));
```

Storybook convention inline: locale coverage comes ONLY from the global
English/Japanese toggle — do NOT add a local `IntlProvider` or per-locale story
duplicates. This covers task-116 AC-3 (local-only state in en-US and ja-JP).

#### Step 13: Verify, format, commit

```
node_modules/.bin/tsc --noEmit                       # ZERO errors
node_modules/.bin/eslint \
  source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx \
  source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx \
  source/renderer/app/components/governance/drep-detail \
  source/renderer/app/containers/governance/DRepDetailPage.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.spec.tsx \
  storybook/stories/governance/DRepDetail.stories.tsx \
  --ext .ts,.tsx
node_modules/.bin/jest source/renderer/app/containers/governance/DRepDetailPage.spec.tsx --no-coverage --runInBand
# Regression on the surfaces that render the two edited shared components:
node_modules/.bin/jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx --no-coverage --runInBand
node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts --no-coverage --runInBand   # 20/20
yarn i18n:manage        # let the translations/messages.json diff ride with this commit
node_modules/.bin/prettier --write <the .tsx/.scss files created or edited above>   # never JSONs
```

Commit (subject only):
`feat(gov): task-116 build DRep detail view with on-chain fields and anchor presence`

### i18n Keys (task-116)

| id | en-US | ja-JP | status |
|---|---|---|---|
| `governance.drepDetail.anchor.hash` | `!!!Anchor hash` | `!!!アンカーハッシュ` | new |
| `governance.drepDetail.anchor.none` | `!!!No anchor is recorded on-chain for this DRep.` | `!!!このDRepのアンカーはオンチェーンに記録されていません。` | new |
| `governance.drepDetail.anchor.source` | `!!!Source` | `!!!ソース` | new |
| `governance.drepDetail.anchor.title` | `!!!Anchor` | `!!!アンカー` | new |
| `governance.drepDetail.anchor.url` | `!!!Anchor URL` | `!!!アンカーURL` | new |
| `governance.drepDetail.copyIdToast` | `!!!DRep ID copied` | `!!!DRep IDをコピーしました` | new |
| `governance.drepDetail.expiresIn` | `!!!Expires in` | `!!!失効まで` | new |
| `governance.drepDetail.expiresInEpochs` | `!!!{count} epochs` | `!!!{count}エポック` | new |
| `governance.drepDetail.loading` | `!!!Loading DRep data…` | `!!!DRepデータを読み込み中…` | new |
| `governance.drepDetail.notFound` | `!!!This DRep was not found in the latest on-chain data.` | `!!!このDRepは最新のオンチェーンデータに見つかりませんでした。` | new |
| `governance.drepDetail.onchain.title` | `!!!On-chain` | `!!!オンチェーン` | new |
| `governance.drepDetail.sourceLabel.anchorReference` | `!!!On-chain anchor reference` | `!!!オンチェーンアンカー参照` | new |
| `governance.drepDetail.status` | `!!!Status` | `!!!ステータス` | new |
| `governance.drepDetail.title` | `!!!DRep detail` | `!!!DRep詳細` | new |
| `governance.drepDetail.votePositions` | `!!!Current votes` | `!!!現在の投票` | new |
| `governance.drepDetail.votePositions.unavailable` | `!!!Vote positions are not available in this version.` | `!!!投票状況はこのバージョンでは利用できません。` | new |
| `governance.drepDetail.votingPower` | `!!!Voting power` | `!!!投票権` | new |
| `governance.drepDetail.votingPower.loadingTooltip` | `!!!Loading voting power…` | `!!!投票権を読み込み中…` | new |
| `governance.drepDetail.votingPower.unavailableTooltip` | `!!!Stake distribution unavailable this refresh.` | `!!!今回の更新ではステーク分布を利用できません。` | new |
| `governance.drepDetail.votingPowerLovelace` | `!!!({amount} lovelace)` | `!!!({amount} lovelace)` | new |
| `governance.drepDirectory.backToDirectory` | `!!!Back to directory` | `!!!ディレクトリに戻る` | new |

### Acceptance (task-116)

- [ ] AC-1: no anchor metadata rendered — anchor URL + hash appear as inert text only;
      `grep -rn "onExternalLinkClick\|<a \|href=" source/renderer/app/components/governance/drep-detail/`
      returns nothing.
- [ ] AC-2: the vote-positions field renders the graceful unavailable value (D1); the
      rest of the view is fully functional without it.
- [ ] AC-3: Storybook covers the local-only state; locales via the global toggle only.
- [ ] AC-4: `DRepDetailPage.spec.tsx` green (11 tests: on-chain render, anchor
      presence/absence, `—` tooltip, deep-link refresh, not-found + back, select
      forward + fallback, sync reaction, copy confirmation, ja-JP).
- [ ] AC-5: all 21 keys present in BOTH locales with leading `!!!`.
- [ ] AC-6 (inherited floor): sanitization suite 20/20; no new `logger.*`/`analytics.*`
      call in the diff (the only logger lines are the pre-existing `drepIdLength` ones
      in `DRepIdDisplay`).
- [ ] `routes-config.ts` / `Routes.tsx` / `VotingStore.ts` / `GovernanceStore.ts`
      byte-identical to base.
- [ ] `node_modules/.bin/tsc --noEmit` zero errors; scoped eslint clean.

---

## task-117: Wire the DRep detail route

**Files to edit/create:**

| # | File | Action |
|---|---|---|
| 1 | `source/renderer/app/routes-config.ts` | EDIT |
| 2 | `source/renderer/app/Routes.tsx` | EDIT |
| 3 | `source/renderer/app/components/governance/drep-directory/DRepCard.tsx` | EDIT |
| 4 | `source/renderer/app/components/governance/drep-directory/DRepCard.scss` | EDIT |
| 5 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx` | EDIT |
| 6 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx` | EDIT |
| 7 | `source/renderer/app/containers/governance/DRepDirectoryPage.tsx` | EDIT |
| 8 | `source/renderer/app/i18n/locales/en-US.json` + `ja-JP.json` | EDIT |
| 9 | `source/renderer/app/analytics/maskAnalyticsRoute.ts` | CREATE |
| 10 | `source/renderer/app/analytics/MatomoClient.ts` | EDIT |
| 11 | `tests/jest/security/governance-sanitization.spec.ts` | EDIT (append only) |
| 12 | `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` | EDIT (stub migration) |
| 13 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | EDIT |
| 14 | `storybook/stories/governance/DRepDirectory.stories.tsx` | EDIT (compile fix) |
| 15 | `.agent/plans/governance/drep-discovery/research/slice-4-findings.md` | CREATE |

### Step-by-Step

#### Step 1: Add the route literal

`source/renderer/app/routes-config.ts` — the current seam (:39-42) is:

```ts
  GOVERNANCE: {
    ROOT: '/governance',
    DREPS: '/governance/dreps',
  },
```

Change it to:

```ts
  GOVERNANCE: {
    ROOT: '/governance',
    DREPS: '/governance/dreps',
    DREP_DETAIL: '/governance/dreps/:drepId',
  },
```

#### Step 2: Wire `Routes.tsx` (decision D8 — `exact` is mandatory)

Current seams: imports end at :42
(`import DRepDirectoryPage from './containers/governance/DRepDirectoryPage';`);
the Governance block is :226-239 and its children are NOT inside a `Switch`:

```tsx
        <Route path={ROUTES.GOVERNANCE.ROOT}>
          <Governance>
            <Route
              exact
              path={ROUTES.GOVERNANCE.ROOT}
              component={() => <Redirect to={ROUTES.GOVERNANCE.DREPS} />}
            />
            <TrackedRoute
              pageTitle="DRep Directory"
              path={ROUTES.GOVERNANCE.DREPS}
              component={DRepDirectoryPage}
            />
          </Governance>
        </Route>
```

1. Add after line 42:

```tsx
import DRepDetailPage from './containers/governance/DRepDetailPage';
```

2. Replace the directory `TrackedRoute` (:233-237) with (adds `exact`, then the detail
   route):

```tsx
            <TrackedRoute
              exact
              pageTitle="DRep Directory"
              path={ROUTES.GOVERNANCE.DREPS}
              component={DRepDirectoryPage}
            />
            <TrackedRoute
              pageTitle="DRep Detail"
              path={ROUTES.GOVERNANCE.DREP_DETAIL}
              component={DRepDetailPage}
            />
```

Without `exact`, both pages render simultaneously on the detail path (the siblings are
not in a `Switch`) — this is the drift D8 exists to close; the Jest harness already
models the fixed shape (`VotingGovernancePage.spec.tsx:169-175`). The `pageTitle`
strings are static — no id ever enters `sendPageNavigationEvent`. The Governance
container's Directory tab stays active on the detail route via the existing
`startsWith` check (`Governance.tsx:48-51`) — no nav change needed.

#### Step 3: Add the "View details" CTA — `DRepCard.tsx`

Current seams: messages :13-34 (`select` entry at :19-23), `Props` :36-41, actions row
JSX :92-98.

1. Add to `defineMessages` (immediately AFTER the `select` entry at :19-23):

```ts
  viewDetails: {
    id: 'governance.drepDirectory.card.viewDetails',
    defaultMessage: '!!!View details',
    description: 'Card CTA that opens the DRep detail view',
  },
```

2. Widen `Props` (:36-41) — add after `onSelectForDelegation`:

```ts
  onViewDetails: (drepId: string) => void;
```

3. Destructure `onViewDetails` in the component signature (:56-61).

4. In the actions row (:92-98), add the View-details button BEFORE the existing Select
   button (shared tokens §10 Tab order: View details, then Select):

```tsx
      <div className={styles.actionsRow}>
        <Button
          label={intl.formatMessage(messages.viewDetails)}
          onClick={() => onViewDetails(entry.drepId)}
          skin={ButtonSkin}
        />
        <Button
          label={intl.formatMessage(messages.select)}
          onClick={() => onSelectForDelegation(entry.drepId)}
          skin={ButtonSkin}
        />
      </div>
```

#### Step 4: `DRepCard.scss`

The `.actionsRow` rule (last rule in the file) becomes:

```scss
.actionsRow {
  display: flex;
  gap: 8px;
  justify-content: flex-end;
}
```

#### Step 5: Thread the callback — `DRepDirectoryList.tsx`

1. `Props` (:30-35): add `onViewDetails: (drepId: string) => void;` after
   `onSelectForDelegation`.
2. Destructure it (:37-42).
3. The card render (:72-79) becomes:

```tsx
        {pageEntries.map((entry) => (
          <DRepCard
            key={entry.drepId}
            entry={entry}
            onSelectForDelegation={onSelectForDelegation}
            onViewDetails={onViewDetails}
            votingPowerState={votingPowerState}
          />
        ))}
```

#### Step 6: Thread the callback — `DRepDirectory.tsx`

1. `Props` (:58-69): add `onViewDetails: (drepId: string) => void;` after
   `onSelectForDelegation`.
2. Destructure it (:71-82).
3. The list render (:169-173) becomes:

```tsx
            <DRepDirectoryList
              entries={drepList}
              onSelectForDelegation={onSelectForDelegation}
              onViewDetails={onViewDetails}
              votingPowerState={votingPowerState}
            />
```

(`onViewDetails` is a REQUIRED prop so tsc forces every call site — container, spec,
stories — to be updated within this task; slice-2 P-4 precedent.)

#### Step 7: Directory container push — `DRepDirectoryPage.tsx`

Current seams: `handleSelectForDelegation` :58-66, render props :75-87.

1. Add a class method after `handleSelectForDelegation`:

```ts
  handleViewDetails = (drepId: string) => {
    // Forward only { from, selectedWalletId, voteType } toward the detail
    // hop; the detail view returns it plus selectedDRepId to the form.
    const inherited = pickDelegationFormReturnState(this.props.location.state);
    this.props.history.push(`${ROUTES.GOVERNANCE.DREPS}/${drepId}`, inherited);
  };
```

2. Pass `onViewDetails={this.handleViewDetails}` to `<DRepDirectory …>` (after
   `onSelectForDelegation` at :85).

This is exactly the push the slice-2 two-hop test simulated
(`VotingGovernancePage.spec.tsx:243-248`): the path is built from
`ROUTES.GOVERNANCE.DREPS` + the raw id (bech32 never URL-encodes — invariant #10), the
state goes through the production picker (D10). `pickDelegationFormReturnState` and
`ROUTES` are already imported (:12-13).

#### Step 8: Locale JSONs

Insert into BOTH files immediately AFTER
`"governance.drepDirectory.card.select"` (its position after task-116's insertions —
locate by key, not line number):

- en-US: `"governance.drepDirectory.card.viewDetails": "!!!View details",`
- ja-JP: `"governance.drepDirectory.card.viewDetails": "!!!詳細を表示",`

#### Step 9: Create the analytics route mask

Create `source/renderer/app/analytics/maskAnalyticsRoute.ts` with exactly:

```ts
// The detail route embeds a DRep id in the URL; analytics payloads carry the
// current route, so the id segment is replaced with a literal placeholder
// before any URL leaves the renderer.
const DREP_DETAIL_SEGMENT = /^(governance\/dreps\/)[^/?#]+/;

export function maskAnalyticsRoute(route: string): string {
  return route.replace(DREP_DETAIL_SEGMENT, '$1:drepId');
}
```

The regex rewrites only the first segment after `governance/dreps/`; the bare list
route `governance/dreps` (no third segment) and every non-governance route pass through
unchanged.

#### Step 10: Mask the URL at the Matomo boundary — `MatomoClient.ts`

Current seam (:61-63):

```ts
  private getAnalyticsURL() {
    return `http://daedalus/${window.location.hash.replace('#/', '')}`;
  }
```

1. Add to the imports (after the `analyticsConfig` import at :4-8):

```ts
import { maskAnalyticsRoute } from './maskAnalyticsRoute';
```

2. Replace the method with:

```ts
  private getAnalyticsURL() {
    // Sanitization floor: the DRep id in the detail route must never reach
    // an analytics payload.
    return `http://daedalus/${maskAnalyticsRoute(
      window.location.hash.replace('#/', '')
    )}`;
  }
```

`getAnalyticsURL` feeds BOTH `sendPageNavigationEvent` (:39) and `sendEvent` (:57), so
one mask covers every tracked payload. Do not touch anything else in this file.

#### Step 11: Extend the sanitization floor suite (20 → 23; append only)

`tests/jest/security/governance-sanitization.spec.ts` — current seams: the
`delegateVotes` module mock :10-15, imports :17-21, test vectors :26-33, file ends
:399. The 20 existing tests must remain byte-identical.

1. Add a second module mock immediately after the existing `jest.mock(...)` block
   (:10-15):

```ts
jest.mock('matomo-tracker', () =>
  jest.fn().mockImplementation(() => ({ track: jest.fn() }))
);
```

2. Add to the import block (:17-21):

```ts
import MatomoTracker from 'matomo-tracker';
import { maskAnalyticsRoute } from '../../../source/renderer/app/analytics/maskAnalyticsRoute';
import { MatomoClient } from '../../../source/renderer/app/analytics/MatomoClient';
```

3. Append a new describe block at the end of the file (after the closing `});` at
   :399):

```ts
describe('Governance sanitization — analytics URL masking', () => {
  it('masks the DRep id out of the detail route for both CIP forms', () => {
    expect(maskAnalyticsRoute(`governance/dreps/${CIP129_DREP}`)).toBe(
      'governance/dreps/:drepId'
    );
    expect(maskAnalyticsRoute(`governance/dreps/${CIP105_SCRIPT}`)).toBe(
      'governance/dreps/:drepId'
    );
  });

  it('leaves non-detail routes untouched', () => {
    expect(maskAnalyticsRoute('governance/dreps')).toBe('governance/dreps');
    expect(maskAnalyticsRoute('voting/governance')).toBe('voting/governance');
    expect(maskAnalyticsRoute('wallets/add')).toBe('wallets/add');
  });

  it('never embeds the current detail-route DRep id in a tracked event URL', async () => {
    window.location.hash = `#/governance/dreps/${CIP129_DREP}`;
    try {
      const client = new MatomoClient(
        { isDev: true } as any,
        {} as any,
        'user-1'
      );
      await client.sendEvent('Governance', 'Test event');

      const tracker = (MatomoTracker as unknown as jest.Mock).mock.results[0]
        .value;
      expect(tracker.track).toHaveBeenCalledTimes(1);
      const { url } = tracker.track.mock.calls[0][0];
      expect(url).toBe('http://daedalus/governance/dreps/:drepId');
      expect(url).not.toContain(CIP129_DREP);
    } finally {
      window.location.hash = '';
    }
  });
});
```

Why this works with no further setup: `MatomoClient`'s only runtime dependencies are
the mocked `matomo-tracker`, `analyticsConfig` (plain constants), and
`getCustomDimensions` (imported but not called by `sendEvent`); `{ isDev: true }`
short-circuits `getMatomoSiteId` before the network map is read; jsdom supports hash
assignment; jest's `globals.environment` (jest.config.js:63-67) keeps the transitive
renderer-logging import alive. This suite sits outside the eslint gate (slice-3 I-5) —
match the file's existing formatting by hand.

#### Step 12: Migrate the harness stub to the production detail —
`VotingGovernancePage.spec.tsx`

The slice-2/3 pins in this file are **verified contract**: the ONLY permitted edits are
the nine below. The browse-push test (:189-208), single-hop restore test (:210-229),
byte-equal payload test (:257-297), and all three HW tests (:300-387) stay
byte-identical.

1. DELETE the type import at :5 (`import type { RouteComponentProps } …` — only the
   stub used it).
2. In the testing-library import (:11-18), DELETE `act,` (only the old two-hop
   simulation used it).
3. DELETE the picker import at :30
   (`import { pickDelegationFormReturnState } …` — the production pages own it now).
4. ADD after the `DRepDirectoryPage` import (:32):

```ts
import DRepDetailPage from '../governance/DRepDetailPage';
```

5. DELETE lines 74-99 (the `DETAIL_STUB_PATH` constant and the whole `DetailRouteStub`
   component with its comment).
6. In `buildStores` (:107-143), add `drepIndex` to the `governance` store object
   (:116-123), keeping alphabetical order:

```ts
  governance: {
    drepIndex: new Map([[VALID_DREP_ID, drepEntry]]),
    drepList: [drepEntry],
    error: null,
    lastFetchedAt: Date.now() - 60_000,
    refresh: jest.fn(),
    refreshState: GovernanceRefreshState.Loaded,
    votingPowerState: VotingPowerEnrichState.Loaded,
  },
```

7. Replace the stub route registration (:174,
   `<Route path={DETAIL_STUB_PATH} component={DetailRouteStub} />`) with:

```tsx
            <Route
              path={ROUTES.GOVERNANCE.DREP_DETAIL}
              component={DRepDetailPage}
            />
```

   (the `exact` on the DREPS route at :169-173 already mirrors production after D8 —
   leave it untouched).

8. Replace the two-hop test (:231-255) with the production-CTA version — the three
   final assertions are IDENTICAL to the old ones (that is the "no weakening" proof):

```tsx
  it('two-hop Form → Directory → Detail → Form restores wallet + vote type and pre-fills the ID', () => {
    renderFlow([
      {
        pathname: ROUTES.VOTING.GOVERNANCE,
        state: { selectedWalletId: WALLET_ID, voteType: 'drep' },
      },
    ]);

    fireEvent.click(screen.getByText('!!!Browse DReps'));
    fireEvent.click(screen.getByRole('button', { name: '!!!View details' }));
    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(screen.getByTestId('wallets-dropdown')).toHaveTextContent(WALLET_ID);
    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent('drep');
    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });
```

   (After "View details" the directory unmounts — the DREPS route is `exact` — so the
   detail's Select button is the only `'!!!Select for delegation'` match.)

9. ADD one new test immediately after the two-hop test, pinning the task-117 AC
   forwarding contract:

```tsx
  it('View details forwards { from, selectedWalletId, voteType } without selectedDRepId', () => {
    const { history, pushSpy } = renderFlow([
      {
        pathname: ROUTES.GOVERNANCE.DREPS,
        state: {
          from: ROUTES.VOTING.GOVERNANCE,
          selectedWalletId: WALLET_ID,
          voteType: 'drep',
        },
      },
    ]);

    fireEvent.click(screen.getByRole('button', { name: '!!!View details' }));

    expect(history.location.pathname).toBe(
      `${ROUTES.GOVERNANCE.DREPS}/${VALID_DREP_ID}`
    );
    expect(pushSpy).toHaveBeenCalledWith(
      `${ROUTES.GOVERNANCE.DREPS}/${VALID_DREP_ID}`,
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      })
    );
    const forwardedState = pushSpy.mock.calls[0][1] as Record<string, unknown>;
    expect(forwardedState.selectedDRepId).toBeUndefined();
  });
```

Byte-equality chain note (do not add more tests for it): this file pins route-param →
pre-fill; the untouched payload test (:257-297) pins pre-fill → `delegateVotes`
payload; together the chain is closed.

#### Step 13: Directory component spec + stories compile fixes

**`DRepDirectory.spec.tsx`** — current seams: `renderComponent` :40-75
(`onSelectForDelegation` param :46, its type :55, component props :62-72), Select-CTA
test :236-246.

1. Add to the destructured params (:46, after `onSelectForDelegation = jest.fn(),`):
   `onViewDetails = jest.fn(),`
2. Add to the options type (:55): `onViewDetails?: jest.Mock;`
3. Pass `onViewDetails={onViewDetails}` to `<DRepDirectory …>` (:62-72, after
   `onSelectForDelegation`).
4. Add one test after the Select-CTA test (:246):

```tsx
  it('invokes onViewDetails with the row DRep ID when the View details CTA is clicked', () => {
    const onViewDetails = jest.fn();
    renderComponent({ onViewDetails });

    fireEvent.click(
      screen.getAllByRole('button', { name: '!!!View details' })[0]
    );

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(baseEntries[0].drepId);
  });
```

   (The existing `'displays 25 entries per page'` test keeps passing: its
   `[class*="card"]` selector matches only the card wrapper class — the new Button
   contains no `card` substring.)

**`storybook/stories/governance/DRepDirectory.stories.tsx`** — two `<DRepDirectory …>`
call sites need the new required prop:

1. `renderDirectory` (:136-153): add `onViewDetails={action('onViewDetails')}` after
   `onSelectForDelegation`.
2. The inline `'Ranking unavailable'` story instance (:352-366): add the same prop.

#### Step 14: Write the findings note

Create `.agent/plans/governance/drep-discovery/research/slice-4-findings.md` recording
(at minimum):

- **Vote-positions deferral (D1):** current-epoch vote positions did not ship — they
  need a `gov-state` query no main-process task owns; the plan's "DRep query shape"
  Key-Decisions row (~:138) both defers `gov-state` "to the slices that need them" and
  says "proposal vote positions need gov-state in slice-4" — that conflict is resolved
  in favor of the graceful `governance.drepDetail.votePositions.unavailable` state
  (task-116 AC-2 only requires the view to stay useful without positions). A future
  slice adding `gov-state` should replace the unavailable value in
  `DRepDetailOnchainSection` only.
- **Wireframe drift (D4):** the design's "Registered: epoch N" row
  (`drep-discovery-design.md:92`) has no data source (`drep-state` has no registration
  epoch; `DRepDirectoryEntry` carries none) and was dropped. The "Current votes" row
  (:93) renders the unavailable state per D1.
- **Analytics masking boundary (D2):** `MatomoClient.getAnalyticsURL` embeds
  `window.location.hash` in every `track()` URL; the detail route made this a DRep-id
  leak, closed by `maskAnalyticsRoute` at that single boundary and pinned by the three
  new floor tests (suite 20 → 23). Any future id-bearing route must extend
  `maskAnalyticsRoute` and its tests.
- **Stub migration (D10):** the slice-2 `DetailRouteStub` was replaced by the
  production `DRepDetailPage` with the two-hop test's final assertions unchanged; the
  browse-push, single-hop, payload, and HW pins were untouched.
- **Stale `npx` verification convention (for every later slice):** `npx <tool>` fails
  in this devcontainer — npm 11.13.0 rejects the repo `package.json`'s string-form
  `devEngines` with `npm error Invalid property "devEngines.node"` before the tool
  runs. It is environmental, not a code failure. Use `node_modules/.bin/<tool>`
  (tsc 4.9.5, eslint 8.13.0, jest 27.5.1, prettier 2.1.2) or `yarn <tool>` instead;
  earlier-slice docs that say `npx` are stale on this point.
- Anchor-count drift found during planning (for future planners): the harness routes
  live at `VotingGovernancePage.spec.tsx:165-175`; the floor suite was already 20
  tests before this slice (slice-2 docs said 17 — slice-3 added 3); §9 pre-assigns no
  id for the "On-chain anchor reference" label (new id
  `governance.drepDetail.sourceLabel.anchorReference`).
- Any additional durable findings from the build.

#### Step 15: Verify, format, commit

```
node_modules/.bin/tsc --noEmit                       # ZERO errors
node_modules/.bin/eslint \
  source/renderer/app/routes-config.ts \
  source/renderer/app/Routes.tsx \
  source/renderer/app/analytics/MatomoClient.ts \
  source/renderer/app/analytics/maskAnalyticsRoute.ts \
  source/renderer/app/components/governance/drep-directory/DRepCard.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  storybook/stories/governance/DRepDirectory.stories.tsx \
  --ext .ts,.tsx
node_modules/.bin/jest \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.spec.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx \
  --no-coverage --runInBand
node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts --no-coverage --runInBand   # 23/23
yarn i18n:manage
node_modules/.bin/prettier --write <the .ts/.tsx/.scss files touched above>   # never JSONs
```

Expected focused-test counts: `VotingGovernancePage` 8 (7 + 1 new),
`DRepDirectory` 20 (19 + 1), `DRepDetailPage` 11, `DRepDirectoryPage` 3, floor 23.

Commit (subject only; the findings note and tracker/PRD updates ride with it):
`feat(gov): task-117 wire DRep detail route and mask drep id from analytics urls`

### i18n Keys (task-117)

| id | en-US | ja-JP | status |
|---|---|---|---|
| `governance.drepDirectory.card.viewDetails` | `!!!View details` | `!!!詳細を表示` | new |

### Acceptance (task-117)

- [ ] AC (tracker): from the Directory with `location.state.from === '/voting/governance'`,
      the Detail route passes the full state (`from`, `selectedWalletId`, `voteType`)
      through to "Select for delegation" — documented by the migrated two-hop test and
      the new forwarding-state test.
- [ ] `exact` on the directory route; no double render (the two-hop test fails loudly if
      both pages mount).
- [ ] `ROUTES.GOVERNANCE.DREP_DETAIL === '/governance/dreps/:drepId'`; detail reachable
      via the card CTA; Directory tab active on detail (existing `startsWith` logic —
      no nav change in the diff).
- [ ] Analytics: floor suite 23/23; the tracked URL for the detail route is
      `http://daedalus/governance/dreps/:drepId`; the 20 inherited tests unmodified.
- [ ] Slice-2/3 pins byte-identical: `git diff` of `VotingGovernancePage.spec.tsx`
      touches ONLY the nine permitted edits of Step 12.
- [ ] `VotingStore.ts` / `GovernanceStore.ts` / `delegationFormState.ts` byte-identical
      to base.
- [ ] All listed suites green; `node_modules/.bin/tsc --noEmit` zero errors; findings
      note written.

---

## Cross-Cutting Acceptance (All Tasks)

- `node_modules/.bin/tsc --noEmit` → zero errors after every task (`npx` fails in this
  devcontainer — see the cross-cutting verification note).
- Sanitization floor green after every task (20/20 after task-116; 23/23 after
  task-117); the inherited 20 tests never modified.
- No new `logger.*`, `analytics.*`, or electron-store call in any file this slice
  touches (the D2 mask REMOVES data from analytics; it adds no event).
- No anchor fetch/link/copy control anywhere; anchor values are inert text under the
  "On-chain anchor reference" label.
- `VotingStore.ts` / `GovernanceStore.ts` byte-identical to base; `delegationFormState.ts`
  unchanged (both tasks only consume it).
- Every new locale string starts with `!!!` in BOTH locales; no `!!!` stripped.
- One subject-only commit per task; `translations/messages.json` diffs ride with the
  task that caused them; no `.scss.d.ts` committed; nothing pushed.

## References

- PRD: [slice-4-PRD.md](./slice-4-PRD.md) (decisions D1–D12, P-1…P-11)
- Design: [drep-discovery-design.md](../designs/drep-discovery-design.md) (:27-49 IA,
  :78-105 wireframe, :145-188 hierarchy, :190-205 states, :207-215 anchor treatment)
- Shared tokens: [shared-design-tokens.md](../designs/shared-design-tokens.md)
  (§2 source labels, §3 voting-power forms, §4 ID display, §9 message IDs, §10 a11y)
- Research: [slice-2-findings.md](../research/slice-2-findings.md) (D1 stub contract),
  [slice-3-findings.md](../research/slice-3-findings.md) (I-4 prettier drift, I-5 floor
  suite lint gap), [ux-refinement-findings.md](../research/ux-refinement-findings.md)
  (F-2 scss typing, F-7 i18n:manage, F-9 prettier scoping)
