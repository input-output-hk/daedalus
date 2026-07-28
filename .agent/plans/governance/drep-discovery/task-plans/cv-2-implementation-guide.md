# CV-2 Implementation Guide: Current-Vote Enrichment

> **Phase:** `cv-2` — "Current-vote 2 - Enrichment" (riskLevel medium) |
> **Date:** 2026-07-28 |
> **PRD:** [cv-2-PRD.md](cv-2-PRD.md) |
> **Tracker:** [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json) |
> **Review log:** [cv-2-code-review.md](cv-2-code-review.md) |
> **Findings:** [research/cv-2-findings.md](../research/cv-2-findings.md)
>
> The findings note carries the doc-vs-repo conflicts cv-2 resolved at planning;
> the PRD's "Corpus-vs-Repo Corrections cv-2 Inherits" section is its companion.
>
> Every `file:line` anchor below was verified against branch
> `feat/drep-discovery` at `504b44c1a` (pre-implementation). Line numbers shift
> as tasks land — **re-anchor by the quoted content, never by the number.** Run
> every command from the worktree root.
>
> This guide was assembled from six authored sections. Task blocks are ordered
> by the PRD's binding build order; the `##` group headings record the authoring
> shards, not an execution unit.

## Table of Contents

Task blocks appear below in build order. Build position first, then the task id
and its primary target.

| # | task | subject | primary target |
|---|---|---|---|
| 1 | task-143 | `currentVoteOptions` + pure wallet factory | `storybook/stories/governance/_utils/fixtures.ts` |
| 2 | task-136 | live DRep status badge from `drepIndex` | `CurrentVoteSummary.tsx` |
| 3 | task-137 | `selectedWalletId` replaces the selected `Wallet` object | `VotingPowerDelegation.tsx` |
| 4 | task-138 | pre-fill from the current on-chain delegation | `VotingPowerDelegation.tsx` |
| 5 | task-139 | mount `CurrentVoteSummary` in the delegation panel | `VotingPowerDelegation.tsx` |
| 6 | task-140 | disable submit on an identical delegation | `utils/governance/isSameVoteTarget.ts` |
| 7 | task-173 | build the dialog identity with `normalizeDRepIdentity` | `VotingGovernancePage.tsx` |
| 8 | task-141 | keep the confirmation dialog current-target only | `VotingGovernancePage.tsx` |
| 9 | task-142 | verify the dialog is unchanged for current-vote display | `VotingPowerDelegationConfirmationDialog.spec.tsx` |
| 10 | task-175 | render the pre-anchor §7 confirmation identity block | `VotingPowerDelegationConfirmationDialog.tsx` |
| 11 | task-144 | key-based remount in `GovernanceWrapper` | `storybook/stories/governance/_utils/GovernanceWrapper.tsx` |
| 12 | task-145 | `currentVote` knob on the governance stories | `storybook/stories/voting/Governance.stories.tsx` |
| 13 | task-146 | remaining enrichment i18n keys | `source/renderer/app/i18n/locales/` |
| 14 | task-147 | Jest current-vote regressions + HW path | `tests/jest/`, colocated specs |
| 15 | task-148 | same-vote path regression | `tests/jest/`, `VotingStore` specs |

Group headings, in document order:

1. Storybook current-vote fixtures (task-143)
2. Group 1 — task-136: live DRep status badge on `CurrentVoteSummary`
3. task-137 and task-138: `selectedWalletId` state + pre-fill
4. task-139 and task-140: mount the panel, then block the identical vote
5. Section 4 — Confirmation-dialog identity: task-173, task-141, task-142, task-175
6. Storybook wrapper and knob (task-144, task-145)
7. task-146, task-147, task-148: catalogs, Jest regressions, same-vote path

## Implementation Order

The canonical cv-2 build order is binding (cv-2-PRD.md, "Canonical Build
Order"). Execute strictly in this sequence:

```
143 → 136 → 137 → 138 → 139 → 140 → 173 → 141 → 142 → 175 → 144 → 145 → 146 → 147 → 148
```

This is the tasks-JSON listing order with **one amendment, D-13: task-143 is
hoisted to position 1.** Its only dependency is task-131 (`complete`), so the
hoist breaks no edge. The reason: task-136 AC-4 requires a `drepVerified`
Storybook knob that does not exist at HEAD, and building task-136 first would
force a throwaway local knob edit that task-143 / task-145 would then have to
delete. With task-143 first, task-136 consumes the shared fixtures directly.

Ordering constraints that are **not** free and must not be re-derived:

- **137 before 138** — 138's AC-3 ("does not cache the selected wallet object")
  is only expressible once selection is an id.
- **139 after 136 and 138** — 139 mounts the finished panel and needs both the
  `drepEntry` prop (136) and the derived selected wallet (137/138).
- **173 before 141** — stated in task-173's own description: it must land before
  task-141, which edits the same container.
- **175 after 142** — 142's assertions are scoped and authored to survive 175,
  and 175 re-runs them as its own gate (D-3).
- **145 after 139 and 144** — the knob exercises the mounted panel through the
  remounting wrapper.
- **136 + 140 + 175 before 146** — 146 populates both catalogs from the message
  descriptors those three rows mint (D-9).
- **140 / 142 / 145 / 146 before 147** — 147 is the slice's regression harness
  over all of them.

## Locked Invariants (all fourteen, in force for every task)

These are the project-wide locked invariants. They bind every task in this
guide, whether or not a task section repeats them. Where a section restates one
inline, the two must agree; if they ever disagree, this list wins.

1. **Local-first.** Discovery data comes only from the local node via the
   main-process `GovernanceQueryService`. No hosted explorers, indexers,
   GovTool, Koios, Blockfrost, or public governance APIs.
2. **Sanitization floor (inherited by every slice).** No DRep id, no `abstain` /
   `no_confidence` literal, no CIP-129/CIP-105 bech32 string in any logger,
   analytics, or electron-store payload — re-asserted via the task-111 spy suite
   in every slice. The task-168 DRep-state snapshot is the one documented
   exception: public on-chain directory data that deliberately bypasses
   `filterLogData` and must never include the user's own vote.
3. **Anchor transport-security floor.** The full anchor-1 guard set (TLS on,
   redirects off, ≤10s timeouts, ~1 MB cap, JSON content-type allow-list, SSRF +
   DNS-rebinding mitigation, Blake2b-256 hash-verify before parse/cache/render,
   immutable hash-keyed cache) lands complete in `anchor-1` and is never thinned.
   No anchor-derived content renders without verification plus a verified
   off-chain source label. Anchor URLs open only through the
   HTTPS-only-hardened `open-external-url` path (task-152).
4. **No second delegation backend.** Selection supplies a DRep ID to the
   existing `delegateVotes` / `VotingStore` signing paths via React Router
   `location.state` only. `VotingStore` never reads `GovernanceStore` directly.
5. **Lovelace losslessness.** `json-bigint` lossless parse → decimal-string IPC
   → renderer `BigNumber` rehydration. Never route raw `JSONbig` objects across
   IPC or into observables.
6. **CLI discipline.** Bulk `--all-dreps` once per refresh — per-DRep CLI
   invocations are forbidden. The network flag (`--mainnet` /
   `--testnet-magic <N>`) derives from node config only, never from
   renderer/IPC input. The socket goes through `CARDANO_NODE_SOCKET_PATH` in
   `spawn.env`, not argv. Era token `latest` with a `conway` fallback.
7. **Default cohort is binding.** Exclude the top 35 by voting power; take up to
   the next 200 eligible (active, remaining `drepActivity` > 6 epochs, completed
   metadata when available), randomized. The 6-epoch floor is binding in
   production — fixtures that violate it must not ship. The default cohort IS
   the "Recommended" sort: no Recommended tab, no per-card Recommended badge.
8. **Badges are informational only.** The category badge (slice-5: Primary /
   Threshold / Non-metadata; High value only after anchor-1) never reorders,
   filters, or overrides the cohort.
9. **No auto-delegation.** Daedalus never picks a delegation. The `noDelegation`
   state shows the CIP-1694 reward-withdrawal warning plus a CTA.
10. **Byte-equality.** CIP-129, CIP-105, and the signed payload `vote.id` remain
    byte-equal through every identity-display change; the on-device DRep ID
    equals `vote.chosenOption`.
11. **Preliminary copy.** Every new en-US and ja-JP string keeps the leading
    `!!!` marker. Removing `!!!` is a release-end manual review, never a
    per-slice task.
12. **Favorites are per-device** via the Electron local store — not per-wallet,
    not synced.
13. **`Abstain` / `No Confidence` are form-only sentinels**, never DRep
    directory entries.
14. **DRep status grounding.** Canonical on-chain status is `active | inactive`
    (`currentEpoch >= expiry`); `expiring` is renderer-derived display state;
    `retired` is deferred until a distinct unregistration signal exists.

## Environment and Verification Commands

Measured in this devcontainer at `504b44c1a`. Do not re-litigate any of it.

- `nix` is **absent** — `nix fmt` cannot run here. `prettier` on explicit paths
  is the recorded substitute, and the deviation must be reported so the user
  runs `nix fmt` before merge.
- `gh` and git push credentials are **absent** — all work stays local.
- `node` is v24.16.0; `jest` 27.5.1; `prettier` 2.1.2 (pinned).

```bash
# focused tests (the known-good form)
node_modules/.bin/jest --testPathPattern=<pattern> --no-coverage --runInBand

# typecheck
node_modules/.bin/tsc --noEmit          # exit 0 at HEAD; does not regenerate *.scss.d.ts
yarn compile                            # same check plus the `typedef:sass` precompile hook,
                                        # which regenerates the gitignored *.scss.d.ts files

# lint
yarn lint                               # baseline exit 0 with ~5591 warnings

# formatting — explicit paths only, and only for files this slice creates
node_modules/.bin/prettier --write <path> [<path> …]
node_modules/.bin/prettier --check <path>
```

Traps, all measured:

- **Never `yarn prettier`.** Its `package.json` script embeds a repo-wide
  `"**/*.*"` glob and reformats ~240 unrelated files.
- **`prettier --check` is already RED at HEAD** on `VotingPowerDelegation.tsx`,
  `VotingPowerDelegationConfirmationDialog.tsx`, `VotingGovernancePage.tsx` and
  `storybook/stories/voting/Governance.stories.tsx`. Format only newly created
  files; never blanket-format a pre-existing one, or the diff carries unrelated
  reversions.
- **`yarn check:all` is RED at HEAD** for unrelated reasons (the
  `storybook:build` manager-webpack JSX loader gap, plus the prettier drift
  above). Never read either as a cv-2 regression. `yarn storybook` (dev server)
  is the real automated Storybook floor.
- **`yarn i18n:manage` writes** to both locale catalogs and
  `translations/messages.json`. Anything that runs it must `git restore` every
  file that was clean at HEAD.
- **The per-task i18n gate is deliberately deferred, on every copy-minting row.**
  `prompt.md:196-199` requires `yarn i18n:manage` before code review "whenever
  copy changed". Three cv-2 rows mint descriptors — task-136, task-140,
  task-175 — and **none of them runs it**, by D-9: the catalogs have a single
  owner, task-146, so three separate runs would produce three competing catalog
  diffs on files no other row may touch. The interim state between those rows and
  task-146 is **descriptor present / catalog key absent**, which `react-intl`
  resolves by falling back to the `defaultMessage` — the `!!!`-carrying string —
  while logging `[React Intl] Missing message` in Jest and Storybook. That is
  expected; do not silence it and do not add the key early. The gate is
  discharged **once**, in task-146 Step 4, which is also where the catalog diff
  is kept rather than restored. A verifier seeing no i18n run on 136 / 140 / 175
  is looking at this deviation, not a skipped gate.
- **`git stash` is banned** (the stash stack is shared across worktrees and
  concurrent sessions). Discard with `git restore` or `git checkout -- <paths>`.
- **`tests/jest` is only ~8% of the suite** — specs are overwhelmingly colocated
  under `source/`. Never report `jest tests/jest` as "the suite".
- **No browser in this container.** The ja-JP overflow / visual passes for
  task-145 and task-146 cannot be executed here and must be recorded as OWED,
  never faked green.

## Formatting, Commit and Comment Conventions

- **Code comments:** the default is no comment. Add one only when the logic or
  constraint is not self-evident and better naming cannot fix it. When
  warranted: 1–3 lines, plain sentence case, stating the invariant, constraint
  or reason. Never state the *what*, never change history ("was removed", "now
  does X"), never a defense of correctness ("this fixes…"). Never cite process
  artifacts (task ids, `CAT-*`, `CP-*`, plan names, PR numbers) in comments **or
  in test names**. No ALL-CAPS emphasis.
- **Commits:** exactly one Conventional Commits subject line per task —
  `<type>(gov): task-NNN <short imperative summary>`. No body, no
  `Co-Authored-By` trailer. Task ids do belong in commit subjects; the ban above
  covers only comments and test names.
- **Formatting:** `node_modules/.bin/prettier --write <explicit paths>` only,
  and only on files this slice creates. Never on tool-managed JSON (the tasks
  tracker, the locale catalogs, `translations/messages.json`).
- **Storybook:** never wrap a story in its own `IntlProvider` and never create
  per-locale story variants — `storybook/preview.tsx` applies a global
  `StoryWrapper` with an English/Japanese toggle, and a local provider shadows
  it. Prefer an integrated "Connected flow" story modeled on
  `Voting / Governance > Connected flow` in
  `storybook/stories/voting/Governance.stories.tsx`.
- **Doc and tracker conventions:** discover the real convention from existing
  sibling entries (cv-1, slice-7, ux-refinement) — date formats, evidence path
  style, heading structure and `auditSummary` shape — not from prose
  descriptions of it. `updatedAt` is `YYYY-MM-DD`; `evidence` is an array of
  repo-relative paths, source files first, then plan docs. Only `slice-1`
  carries an `auditSummary`; cv-1 deliberately added none, so cv-2 must not
  invent one.

---

## Storybook current-vote fixtures (task-143)

### task-143: `currentVoteOptions` + pure wallet factory in `_utils/fixtures`

**Files created:**

- `storybook/stories/governance/_utils/` (new directory)
- `storybook/stories/governance/_utils/fixtures.ts` (new file)

**Files touched:** none. Do not edit `CurrentVoteSummary.stories.tsx`,
`Governance.stories.tsx`, or `storybook/stories/index.ts` in this task.

#### Context (verified anchors, current code quoted)

`storybook/stories/governance/` currently holds only five story files —
`CurrentVoteSummary.stories.tsx`, `DRepCategoryBadge.stories.tsx`,
`DRepDetail.stories.tsx`, `DRepDirectoryBanner.stories.tsx`,
`DRepDirectory.stories.tsx`. There is **no `_utils` directory** and
`GovernanceWrapper` does not exist anywhere in the repo. The sibling convention
that establishes the directory name is `storybook/stories/news/_utils`,
`storybook/stories/nodes/_utils`, `storybook/stories/wallets/_utils`.

`storybook/stories/_support/utils.ts:104-142` — the existing wallet helper. It
takes nine positional parameters and its `new Wallet({…})` literal has **no
`votingTarget` key**, so it cannot express a current vote:

```ts
export const generateWallet = (
  name: string,
  amount: string,
  assets: WalletTokens = { available: [], total: [] },
  reward: string | number = 0,
  delegatedStakePool: StakePool = null,
  hasPassword = false,
  status: SyncStateStatus = WalletSyncStateStatuses.READY,
  isHardwareWallet = false,
  id: string = generateHash()
) =>
  new Wallet({
    id,
    addressPoolGap: 20,
    amount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    …
```

Do **not** widen it (it is shared by every wallet story). Build `Wallet`
directly instead.

`source/renderer/app/domains/Wallet.ts:113-134` — `WalletProps`. Required keys
are `id`, `addressPoolGap`, `name`, `amount`, `availableAmount`, `reward`,
`assets`, `passwordUpdateDate`, `syncState`, `isLegacy`, `discovery`,
`hasPassword`; `votingTarget?: WalletVotingTarget | null` is at `:130`.
`createdAt` is **not** a `WalletProps` key (`generateWallet` passes it anyway;
nothing reads it) — omit it.

`source/renderer/app/domains/Wallet.ts:22-32` — `WalletSyncStateStatuses.READY`
is `'ready'`, `SYNCING` and `RESTORING` are both `'syncing'`.
`storybook/stories/_support/utils.ts:94-102` adds
`progress: { quantity: 50, unit: 'percentage' }` whenever the status is
`'syncing'`, which is why the syncing fixture below carries that progress block.

`source/renderer/app/api/wallets/types.ts:86-93`:

```ts
export type WalletVotingTarget =
  | { kind: 'drep'; drep: DRepIdentity; source: 'verified' | 'unverified' | 'onchain' }
  | { kind: 'abstain' }
  | { kind: 'no_confidence' };
```

`source/common/types/governance.types.ts:20-31` — `DRepIdentity` has required
`raw` and `credentialType: 'key' | 'script'`; `cip129`, `cip105` and
`credentialHex` are optional. This module also exports a runtime enum, so it
must be imported with `import type`.

`source/renderer/app/stores/GovernanceStore.ts:20-31`:

```ts
export interface AppDRepDirectoryEntry {
  drepId: string;
  votingPower: BigNumber | null;
  status: DRepDirectoryEntry['status'];
  drepActivity: DRepDirectoryEntry['drepActivity'];
  anchor: DRepAnchorPresence | null;
}
```

`source/renderer/app/components/governance/drep-directory/helpers.ts:139-153` —
the lookup the panel will perform against this fixture index:

```ts
export function resolveExactDRepMatch<T>(
  rawQuery: string,
  drepIndex: ReadonlyMap<string, T>
): T | null {
  const { full } = normalizeDRepQuery(rawQuery);
  if (!Cardano.DRepID.isValid(full)) return null;
  try {
    const canonical = String(Cardano.DRepID.toCip129DRepID(Cardano.DRepID(full)));
    return drepIndex.get(canonical) ?? null;
```

Measured in this repo: `Cardano.DRepID.isValid()` returns `false` for a
`drep_vkh1…` string and `true` for the two `drep1…` vectors this module declares,
and `toCip129DRepID` returns each `drep1…` vector unchanged. **The Map key must
therefore be the CIP-129 (`drep1…`) string, verbatim.**

`storybook/stories/governance/CurrentVoteSummary.stories.tsx:15-52` holds the
inline block this module supersedes (`KEY_CIP129` / `KEY_CIP105` /
`KEY_CREDENTIAL_HEX`, `CURRENT_VOTE_OPTIONS`, a local `resolveCurrentVote`). The
unverified vectors below are copied from it byte-for-byte. **task-136 deletes
that block and re-imports from this module — task-143 leaves the file alone.**

#### Locked invariants this change must not break (stated in full)

- **Fixtures are per-render, never shared mutable state.** No module-level
  wallet array, no `let`, no `.push`, no reassignment of a `Wallet` instance.
  Every call to `makeGovernanceWallets` allocates new `Wallet` objects.
- **Byte-equality.** The bech32 strings are used verbatim everywhere: as
  `raw`, as `cip129`, as the `drepId` field, and as the Map key. Bech32 is
  case-insensitive, so never re-case, re-derive or re-encode them.
- **Default cohort is binding, and these fixtures are outside it.** The
  `drepActivity: 4` entry is the user's own delegation shown in the current-vote
  panel; it never enters a cohort or directory-list computation, so the
  production 6-epoch cohort floor is untouched. Do not add these entries to any
  directory or cohort fixture.
- **`Abstain` / `No Confidence` are form-only sentinels**, never DRep directory
  entries: their options return `{ kind: 'abstain' }` / `{ kind: 'no_confidence' }`
  and an **empty** index Map.
- **No auto-delegation.** `noDelegation` must return `null` — never a
  fallback DRep.
- **DRep status grounding.** `status` may only be `'active'` or `'inactive'`.
  `'expiring'` is renderer-derived from `drepActivity` and must never appear as
  a stored status value in a fixture.
- **Sanitization floor.** These ids exist in Storybook fixtures only; nothing in
  this module logs, calls analytics, or writes to electron-store.

#### Resolved judgment calls (do not revisit)

- `makeDRepIndex` ships in this task even though task-143's acceptance criteria
  do not name it: task-136 and task-145 both import it, and it belongs to the
  fixtures charter.
- Knob **label** is exactly `Current vote (mock)`; the default is
  `'noDelegation'`.
- `drepVerified` and `drepUnverified` differ by the lifecycle state cv-2 can
  actually render (`drepActivity: 30` vs `4`), not by anchor verification —
  there is no anchor fetch or hash verification in this slice.
- Both DRep options use `source: 'onchain'`, the only value the production
  mapper emits.
- `votingPower` values are display-only filler; use the exact literals below.
- **No Jest spec is added.** `jest.config.js:129` sets
  `roots: ['<rootDir>/tests', '<rootDir>/source']`, so a spec placed under
  `storybook/` would never run. Acceptance is proved structurally (Step 3 and
  Step 5) and visually in Storybook.

#### Step 1: Create the directory

```bash
mkdir -p storybook/stories/governance/_utils
```

#### Step 2: Create `storybook/stories/governance/_utils/fixtures.ts`

Exact file content (this text is eslint-clean and `prettier@2.1.2`-clean as
written — do not reflow it):

```ts
import BigNumber from 'bignumber.js';
import { select } from '@storybook/addon-knobs';
import Wallet, {
  WalletSyncStateStatuses,
} from '../../../../source/renderer/app/domains/Wallet';
import { LOVELACES_PER_ADA } from '../../../../source/renderer/app/config/numbersConfig';
import type {
  WalletSyncState,
  WalletVotingTarget,
} from '../../../../source/renderer/app/api/wallets/types';
import type { DRepIdentity } from '../../../../source/common/types/governance.types';
import type { AppDRepDirectoryEntry } from '../../../../source/renderer/app/stores/GovernanceStore';

export type CurrentVoteOption =
  | 'noDelegation'
  | 'drepVerified'
  | 'drepUnverified'
  | 'abstain'
  | 'noConfidence';

export const currentVoteOptions: Record<string, CurrentVoteOption> = {
  'Not delegated (warning)': 'noDelegation',
  'DRep — verified anchor': 'drepVerified',
  'DRep — unverified anchor': 'drepUnverified',
  Abstain: 'abstain',
  'No Confidence': 'noConfidence',
};

export function useCurrentVoteKnob(): CurrentVoteOption {
  return select('Current vote (mock)', currentVoteOptions, 'noDelegation');
}

// The unverified pair is copied from the committed CurrentVoteSummary story;
// the verified pair encodes the Cardano Academy preprod DRep key hash. Both
// stay lower-case: the drepIndex lookup canonicalizes to lower-case CIP-129.
const VERIFIED_CIP129 =
  'drep1ytnglv2y7s8dxpmylw35egsum63yqzcm0upvkf7qffg4hhqnhj0yh';
const VERIFIED_CIP105 =
  'drep_vkh1u68mz385pmfswe8m5dx2y8x75fqqkxmlqt9j0sz229dac0zl65v';
const VERIFIED_CREDENTIAL_HEX =
  'e68fb144f40ed30764fba34ca21cdea2400b1b7f02cb27c04a515bdc';

const UNVERIFIED_CIP129 =
  'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const UNVERIFIED_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
const UNVERIFIED_CREDENTIAL_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';

const VERIFIED_DREP: DRepIdentity = {
  raw: VERIFIED_CIP129,
  cip129: VERIFIED_CIP129,
  cip105: VERIFIED_CIP105,
  credentialHex: VERIFIED_CREDENTIAL_HEX,
  credentialType: 'key',
};

const UNVERIFIED_DREP: DRepIdentity = {
  raw: UNVERIFIED_CIP129,
  cip129: UNVERIFIED_CIP129,
  cip105: UNVERIFIED_CIP105,
  credentialHex: UNVERIFIED_CREDENTIAL_HEX,
  credentialType: 'key',
};

export function resolveCurrentVote(
  option: CurrentVoteOption
): WalletVotingTarget | null {
  switch (option) {
    case 'drepVerified':
      return { kind: 'drep', drep: VERIFIED_DREP, source: 'onchain' };
    case 'drepUnverified':
      return { kind: 'drep', drep: UNVERIFIED_DREP, source: 'onchain' };
    case 'abstain':
      return { kind: 'abstain' };
    case 'noConfidence':
      return { kind: 'no_confidence' };
    case 'noDelegation':
    default:
      return null;
  }
}

type WalletSeed = {
  id: string;
  name: string;
  lovelace: string;
  hasPassword: boolean;
  isHardwareWallet: boolean;
  syncState: WalletSyncState;
  votingTarget: WalletVotingTarget | null;
};

const buildWallet = ({ lovelace, ...rest }: WalletSeed): Wallet =>
  new Wallet({
    ...rest,
    addressPoolGap: 20,
    amount: new BigNumber(lovelace).dividedBy(LOVELACES_PER_ADA),
    availableAmount: new BigNumber(lovelace).dividedBy(LOVELACES_PER_ADA),
    reward: new BigNumber(0),
    assets: { available: [], total: [] },
    passwordUpdateDate: new Date(),
    isLegacy: false,
    discovery: 'random',
    delegatedStakePoolId: null,
  });

export function makeGovernanceWallets(option: CurrentVoteOption): Wallet[] {
  return [
    buildWallet({
      id: 'governance-wallet-1',
      name: 'Governance wallet',
      lovelace: '125000000000',
      hasPassword: true,
      isHardwareWallet: false,
      syncState: { status: WalletSyncStateStatuses.READY },
      votingTarget: resolveCurrentVote(option),
    }),
    buildWallet({
      id: 'governance-wallet-2',
      name: 'Ledger governance wallet',
      lovelace: '58000000000',
      hasPassword: false,
      isHardwareWallet: true,
      syncState: { status: WalletSyncStateStatuses.READY },
      votingTarget: null,
    }),
    buildWallet({
      id: 'governance-wallet-3',
      name: 'Syncing wallet',
      lovelace: '42000000000',
      hasPassword: true,
      isHardwareWallet: false,
      syncState: {
        status: WalletSyncStateStatuses.SYNCING,
        progress: { quantity: 50, unit: 'percentage' },
      },
      votingTarget: null,
    }),
  ];
}

export function makeDRepIndex(
  option: CurrentVoteOption
): Map<string, AppDRepDirectoryEntry> {
  const index = new Map<string, AppDRepDirectoryEntry>();

  if (option === 'drepVerified') {
    index.set(VERIFIED_CIP129, {
      drepId: VERIFIED_CIP129,
      votingPower: new BigNumber('4500000000000'),
      status: 'active',
      drepActivity: 30,
      anchor: null,
    });
  }

  if (option === 'drepUnverified') {
    index.set(UNVERIFIED_CIP129, {
      drepId: UNVERIFIED_CIP129,
      votingPower: new BigNumber('120000000000'),
      status: 'active',
      drepActivity: 4,
      anchor: null,
    });
  }

  return index;
}
```

Only wallet 1 ever carries a `votingTarget`; wallets 2 and 3 are always `null`.

#### Step 3: Re-verify the bech32 vectors (executable provenance check)

Where the four strings come from — state this, not a CIP-119 claim, wherever the
provenance is recorded:

- `UNVERIFIED_CIP129` / `UNVERIFIED_CIP105` / `UNVERIFIED_CREDENTIAL_HEX` are
  copied byte-for-byte from the committed story fixture
  `storybook/stories/governance/CurrentVoteSummary.stories.tsx:17-21`.
- `VERIFIED_*` is derived here with the repo's `bech32` dependency from the
  Cardano Academy preprod DRep key hash
  `e68fb144f40ed30764fba34ca21cdea2400b1b7f02cb27c04a515bdc`, committed at
  `.agent/plans/governance/drep-discovery/research/drep-state-preprod-epoch295-sample.json:2849`
  with the `Cardano Academy.jsonld` anchor at `:2852-2853` — one of the three
  fixture provenances the plan names (`governance-drep-discovery-plan.md:103`).
- Neither pair is a CIP-119 *test vector*, and no anchor hash is verified: cv-2
  has no anchor fetch and no Blake2b-256 path (D-7). Do not write that they are.

```bash
node -e "
const {bech32} = require('bech32');
const v = {
 verified129:'drep1ytnglv2y7s8dxpmylw35egsum63yqzcm0upvkf7qffg4hhqnhj0yh',
 verified105:'drep_vkh1u68mz385pmfswe8m5dx2y8x75fqqkxmlqt9j0sz229dac0zl65v',
 unverified129:'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy',
 unverified105:'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l',
};
for (const [k,s] of Object.entries(v)) {
  const d = bech32.decode(s, 200);
  const b = Buffer.from(bech32.fromWords(d.words));
  console.log(k, d.prefix, b.length, b.toString('hex'));
}"
```

Expected output, exactly (measured in this worktree):

```
verified129 drep 29 22e68fb144f40ed30764fba34ca21cdea2400b1b7f02cb27c04a515bdc
verified105 drep_vkh 28 e68fb144f40ed30764fba34ca21cdea2400b1b7f02cb27c04a515bdc
unverified129 drep 29 22a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c
unverified105 drep_vkh 28 a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c
```

Both CIP-129 forms carry header byte `0x22` (key-hash) and both CIP-105 forms
decode to the same 28 credential bytes as their CIP-129 partner, which is the
`credentialHex` constant. If any line differs, stop — a vector was mistyped.

Optional second check that the Map key form is the one the lookup will use:

```bash
node -e "
const { Cardano } = require('@cardano-sdk/core');
for (const id of ['drep1ytnglv2y7s8dxpmylw35egsum63yqzcm0upvkf7qffg4hhqnhj0yh','drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy']) {
  console.log(id === String(Cardano.DRepID.toCip129DRepID(Cardano.DRepID(id))));
}"
```

Both lines must print `true`.

#### Step 4: Format, typecheck, lint

```bash
node_modules/.bin/prettier --write storybook/stories/governance/_utils/fixtures.ts
node_modules/.bin/tsc --noEmit
yarn lint
```

`prettier --write` is allowed here because the file is newly created. Never run
`yarn prettier` (its script carries a repo-wide `**/*.*` glob) and never format
a pre-existing file in this slice. `tsc --noEmit` must exit 0; `yarn lint` must
exit 0 (its warning count is a long-standing baseline, not a gate).

#### Step 5: Structural proof of purity

```bash
grep -nE "new Wallet\(|\blet |\.push\(|GOVERNANCE_WALLETS|^export " storybook/stories/governance/_utils/fixtures.ts
```

Expected: exactly one `new Wallet(` line (inside `buildWallet`), **no** `let`,
**no** `.push(`, **no** `GOVERNANCE_WALLETS`, and exactly six `export` lines —
`CurrentVoteOption`, `currentVoteOptions`, `useCurrentVoteKnob`,
`resolveCurrentVote`, `makeGovernanceWallets`, `makeDRepIndex` — with no
`export default`. (Use the `-E` form above: a plain `let ` pattern also matches
`buildWallet = (`.)

Suggested commit subject: `feat(gov): task-143 add governance storybook current-vote fixtures`

#### Acceptance

- [ ] AC-1 "makeGovernanceWallets always returns a freshly-constructed array" —
      the function body is a bare array literal of three `buildWallet(...)`
      calls, each of which runs `new Wallet({…})`; there is no cached array to
      return (Step 5 proof).
- [ ] AC-2 "No knob handler mutates a pre-existing Wallet instance" — no module
      state exists to mutate: the only module-level bindings are `const`
      strings, two `const` identity literals and the option record (Step 5).
- [ ] AC-3 "currentVoteOptions enumerates exactly five values" —
      `noDelegation | drepVerified | drepUnverified | abstain | noConfidence`,
      typed by `CurrentVoteOption`.
- [ ] AC-4 "CIP-119 test vectors … with verified hash" — **satisfied in part on
      both halves.**
      *Named provenance:* only the `drepVerified` pair carries one of the three
      provenances the plan names (`governance-drep-discovery-plan.md:103`,
      `designs/current-vote-display-design.md:227`) — it encodes the Cardano
      Academy preprod key hash committed at
      `research/drep-state-preprod-epoch295-sample.json:2849`. The
      `drepUnverified` pair is the repo's own committed story vector
      (`CurrentVoteSummary.stories.tsx:17-21`), not the SIPO mainnet or
      canonical CIP-119 example credential; no such credential is committed
      anywhere in the repo, and cv-2 mints none.
      *Verified hash:* has **no mechanism in cv-2** — anchor fetching and
      Blake2b-256 verification arrive in the anchor-1 slice, and both fixture
      entries ship `anchor: null`.
      Both vectors are decoded and checksum-checked in Step 3, and the module's
      comment records exactly that and nothing stronger. Record **both**
      shortfalls in the task's tracker `statusReason`; do not claim the
      criterion whole and do not scope the shortfall to the hash half alone.

---

## Group 1 — task-136: live DRep status badge on `CurrentVoteSummary`

### task-136: Add live DRep status badge to CurrentVoteSummary from drepIndex

**Build position:** 2nd in the canonical cv-2 order (`143 → **136** → 137 → …`).
task-143 must already be committed — this task imports from
`storybook/stories/governance/_utils/fixtures.ts`. Confirm before starting:

```bash
grep -n "useCurrentVoteKnob\|resolveCurrentVote" storybook/stories/governance/_utils/fixtures.ts
```

**Files touched (all pre-existing; no file is created):**

- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.messages.ts`
- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.scss`
- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx`
- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx`
- `source/renderer/app/components/voting/voting-governance/__snapshots__/CurrentVoteSummary.spec.tsx.snap` (regenerated, not hand-edited)
- `storybook/stories/governance/CurrentVoteSummary.stories.tsx`

**Files this task must NOT touch** (other owners, or frozen for the slice):
`DRepStatusBadge.tsx` / `.scss`, `source/common/types/governance.types.ts`,
`VotingPowerDelegation.tsx` (task-139 mounts the panel),
`VotingGovernancePage.tsx`, both i18n catalogs and `translations/messages.json`
(task-146), `storybook/stories/governance/_utils/fixtures.ts` (task-143),
`storybook/stories/voting/Governance.stories.tsx` (task-145),
`storybook/stories/index.ts` (nobody in cv-2).

#### Context — the exact seams, quoted from the live worktree

`CurrentVoteSummary.tsx:11-14` — props today:

```ts
type Props = {
  currentVote: WalletVotingTarget | null;
  intl: intlShape.isRequired;
};
```

`CurrentVoteSummary.tsx:49-72` — the whole `drep` branch today (this is the block
you rewrite; the `statusRow` div is `:58-66`, the id row is `:67-69`):

```tsx
  if (currentVote.kind === 'drep') {
    return (
      <section
        className={styles.component}
        aria-label={intl.formatMessage(messages.headerCurrent)}
      >
        <h3 className={styles.header}>
          {intl.formatMessage(messages.headerCurrent)}
        </h3>
        <div className={styles.statusRow}>
          <span className={styles.statusBadge}>
            <span className={styles.glyph} aria-hidden="true">
              ●
            </span>
            {intl.formatMessage(messages.statusDelegatedToDRep)}
          </span>
          <DRepSourceLabel source="on-chain" className={styles.sourceLabel} />
        </div>
        <div className={styles.idRow}>
          <DRepIdDisplay drepId={currentVote.drep.raw} />
        </div>
      </section>
    );
  }
```

`source/renderer/app/stores/GovernanceStore.ts:19-31` — the entry type you consume
(there is **no** `givenName`, **no** `expiry`, **no** `epoch` on it):

```ts
export interface AppDRepDirectoryEntry {
  drepId: string;
  votingPower: BigNumber | null;
  status: DRepDirectoryEntry['status'];
  drepActivity: DRepDirectoryEntry['drepActivity'];
  anchor: DRepAnchorPresence | null;
}
```

`source/common/types/governance.types.ts:35` — `export type DRepStatus = 'active' | 'inactive';`
(closed union — do not widen).
`source/common/types/governance.types.ts:37-38` — `export type DrepActivity = number | null;`
("Remaining epochs until expiry; 0 when inactive, null if unknown").

`source/renderer/app/components/governance/_shared/DRepStatusBadge.tsx:20-23` — the
shared badge's props; it renders `<span class="badge <status>" aria-label="…"><span class="dot"/><span class="label">!!!Active</span></span>`
and its two labels already ship in **both** catalogs
(`governance.drepDirectory.status.active` / `.inactive`, `en-US.json:355-356`,
`ja-JP.json:355-356`), so this task mints **no** key for them:

```ts
interface Props {
  status: DRepStatus;
  intl: intlShape.isRequired;
}
```

`CurrentVoteSummary.spec.tsx:61-63` — the committed assertion that this task must
rewrite (the shared badge renders `!!!Active` / `!!!Inactive`, which match this
regex, so it fails the moment the badge lands):

```tsx
    expect(
      screen.queryByText(/Active|Inactive|Expiring/)
    ).not.toBeInTheDocument();
```

`storybook/stories/governance/CurrentVoteSummary.stories.tsx:15-52` — the inline
fixture block that task-143 superseded and this task deletes (comment `:15-16`,
`KEY_CIP129` / `KEY_CIP105` / `KEY_CREDENTIAL_HEX` `:17-21`,
`CURRENT_VOTE_OPTIONS` `:23-28` — only four options, no `drepVerified` —, local
`resolveCurrentVote` `:30-52`).

#### Locked invariants this change must not break (written out)

- **DRep status grounding.** Canonical on-chain status is `active | inactive`
  only. `expiring` is a **renderer-derived display state**: it is never stored,
  never written back, never a `DRepStatus` value, and never added to
  `DRepStatus` or to `DRepStatusBadge`. `retired` does not exist. No file
  outside the six listed above may change.
- **Badges are informational only.** The status badge never reorders, filters,
  gates, or overrides anything — it is display-only inside this panel.
- **Local-first / CLI discipline.** The status comes in through the `drepEntry`
  prop and nothing else. `CurrentVoteSummary` performs no store read, no
  `@inject`, no `observer`, no IPC, and issues no `cardano-cli` invocation. The
  only `GovernanceStore` reference allowed in the file is an `import type`.
- **Never default to Active.** When no entry is supplied, the badge is omitted
  and the neutral "status is loading" caption renders. A missing entry must
  never fall back to `Active` and must never trigger a lookup to compensate.
- **Sentinels are form-only.** `abstain` / `no_confidence` never carry a DRep
  status badge or a status caption — the badge lives strictly inside the
  `currentVote.kind === 'drep'` branch.
- **No auto-delegation.** The `currentVote == null` branch (`:20-47`: warning,
  subline, CTA) is untouched.
- **Byte-equality.** The id row keeps rendering `currentVote.drep.raw`
  untouched. Nothing in this task trims, re-cases, re-encodes, or writes back
  any DRep string.
- **Sanitization floor.** DRep ids may appear in the DOM; they may never reach a
  logger, analytics call, or electron-store payload. Add no `console.*`, no
  `logger.*`, and never log `drepEntry` or `drepEntry.drepId`.
- **Preliminary copy.** Every new `defaultMessage` keeps its leading `!!!`.
  Stripping `!!!` is a release-end, user-owned review — not this task.
- **No `givenName`, no anchor content.** The field does not exist on
  `AppDRepDirectoryEntry`; it arrives in `anchor-2`. `DRepIndexEntry` is a
  design-doc-only name and must never appear in code.

#### Resolved judgment calls (do not revisit)

1. **`active` / `inactive` reuse the shared `DRepStatusBadge`; `expiring` is a
   component-local badge.** Widening the shared badge would change two shipped
   consumers (`DRepCard.tsx`, `DRepDetailOnchainSection.tsx`) outside this
   slice's fence. The two renderers are mutually exclusive and never adjacent.
2. **Threshold is `<= 12` remaining epochs, with no lower bound**, declared
   locally as `EXPIRING_MAX_REMAINING_EPOCHS = 12`. `DRepCategoryBadge`'s
   `7`/`12` constants
   (`source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx:50-51`)
   are **not exported** and
   must not be imported; this panel shows the user's own delegation, which is
   not cohort-scoped, so the `7–12` cohort window does not apply here.
3. **`inactive` is tested before the epoch window.** The main process derives
   `drepActivity = max(0, expiry - currentEpoch)`, so `inactive` implies
   `drepActivity === 0`; checking status first keeps an inactive DRep off the
   expiring path.
4. **The derivation is a named module-scope helper**,
   `deriveCurrentVoteBadgeState`, **not exported** — no other file consumes it,
   and the spec exercises it through rendering.
5. **`drepEntry` is optional (`?`) and may be `null`.** The four committed specs
   render `<CurrentVoteSummary currentVote={…} />` with no second prop and must
   keep compiling.
6. **Exactly one caption ever renders**, as the last child of the `drep`
   `<section>`, below the id row.
7. **The ICU argument is named `n`** in both epoch strings (not `count`).
8. **The catalogs are not edited here.** task-146 seeds `en-US.json` /
   `ja-JP.json` from these descriptors. Until it lands, react-intl 2.9 logs
   `[React Intl] Missing message: "voting.governance.currentVote.status.…"` and
   falls back to `defaultMessage` (`node_modules/react-intl/lib/index.js:837-848`).
   Those console errors are expected, do not fail Jest, and must **not** be
   silenced or worked around by adding keys to a catalog.

---

#### Step 1: Add the four message descriptors

File: `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.messages.ts`.
Append these four entries **after** `noConfidenceCaption` (which ends at `:72`),
inside the closing `});` at `:73`. Change nothing else in the file.

```ts
  statusExpiringBadge: {
    id: 'voting.governance.currentVote.status.expiringBadge',
    defaultMessage: '!!!Expiring in {n} epochs',
    description:
      'Badge label for a delegated DRep whose registration lapses within the remaining-epoch window',
  },
  statusExpiring: {
    id: 'voting.governance.currentVote.status.expiring',
    defaultMessage:
      "!!!This DRep's voting power will lapse in {n} epochs — consider re-delegating.",
    description:
      'Caption shown when the delegated DRep registration lapses soon',
  },
  statusInactive: {
    id: 'voting.governance.currentVote.status.inactive',
    defaultMessage:
      '!!!This DRep is currently inactive. Your voting power will not be counted until they vote again — consider re-delegating.',
    description: 'Caption shown when the delegated DRep is inactive',
  },
  statusUnavailable: {
    id: 'voting.governance.currentVote.status.unavailable',
    defaultMessage: '!!!DRep status is loading.',
    description:
      'Neutral caption shown when the DRep directory has no record for the delegated DRep yet',
  },
```

Notes that are binding: the ids are exact; `statusExpiringBadge` and
`statusExpiring` take one ICU argument named `n`; `statusInactive` and
`statusUnavailable` take none; every `defaultMessage` starts with `!!!`. Use the
double-quoted string for `statusExpiring` exactly as written — it contains an
apostrophe in `DRep's` (a lone `'` not followed by `{`, `}` or `'` is a literal
apostrophe in ICU, the same as the committed `noDelegationWarning` at `:34`).

#### Step 2: Add the local expiring-badge style

File: `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.scss`.
Insert this rule immediately after the existing `.statusBadge` rule (which ends
at `:31`), so the two badge rules sit together. Token values copy the committed
warning-badge precedent — the `.threshold` rule at
`source/renderer/app/components/governance/_shared/DRepCategoryBadge.scss:34-41`.
In this file `.glyph` (`:33-35`) and `.caption` (`:47-52`) already exist and are
reused as-is.

```scss
.expiringBadge {
  align-items: center;
  background: var(--badge-warning-bg, rgba(230, 162, 60, 0.12));
  border-radius: 4px;
  color: var(--badge-warning-fg, #b26a00);
  display: inline-flex;
  font-size: 14px;
  font-weight: 500;
  gap: 6px;
  line-height: 1;
  padding: 4px 8px;
}
```

The declarations are **alphabetical**, unlike the rules around them.
`.stylelintrc` enables `order/properties-alphabetical-order` and this file is
already red — exactly **12** pre-existing order errors at HEAD (measured;
task-140 Step 11 cites the same number). An alphabetical block keeps the count at
12 instead of raising it. Do **not** reorder the pre-existing 12: they belong to
no cv-2 task, and fixing them would bury this change in unrelated churn.

`styles.expiringBadge` type-checks through the global `declare module '*.scss';`
(`source/renderer/declaration.d.ts:1`); there is no committed
`CurrentVoteSummary.scss.d.ts` (`*.scss.d.ts` is gitignored, `.gitignore:141`).
If you run `yarn compile`, its `precompile` hook regenerates those files — do not
commit them.

#### Step 3: Rewrite `CurrentVoteSummary.tsx`

**3a — imports.** After `import DRepSourceLabel …` (`:6`) add the badge import;
after `import type { WalletVotingTarget } …` (`:7`) add the entry type import:

```ts
import DRepStatusBadge from '../../governance/_shared/DRepStatusBadge';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
```

Both specifiers are exact. The second **must** be `import type` — that module
also exports a store class and a runtime enum, and a value import would pull the
store into the component bundle. Precedent for the identical specifier:
`source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx:4`.

**3b — props.** Replace `:11-14` with:

```ts
type Props = {
  currentVote: WalletVotingTarget | null;
  drepEntry?: AppDRepDirectoryEntry | null;
  intl: intlShape.isRequired;
};
```

**3c — threshold, derived state, helper.** Insert between the `Props` type and
the existing comment at `:16-18` (leave that comment exactly where it is):

```ts
const EXPIRING_MAX_REMAINING_EPOCHS = 12;

type CurrentVoteBadgeState = 'unavailable' | 'inactive' | 'expiring' | 'active';

// This panel shows the wallet's own delegation, which is not cohort-scoped, so
// the expiring window is the full remaining-epoch threshold rather than the
// narrower window the directory badge applies to cohort members.
function deriveCurrentVoteBadgeState(
  drepEntry: AppDRepDirectoryEntry | null | undefined
): CurrentVoteBadgeState {
  if (drepEntry == null) return 'unavailable';
  if (drepEntry.status === 'inactive') return 'inactive';
  if (
    drepEntry.drepActivity != null &&
    drepEntry.drepActivity <= EXPIRING_MAX_REMAINING_EPOCHS
  ) {
    return 'expiring';
  }
  return 'active';
}
```

That three-line comment is the only comment you add. Do not write a task id, a
change history, or a defence of correctness anywhere in this file.

**3d — component signature.** `:19` becomes:

```ts
function CurrentVoteSummary({ currentVote, drepEntry, intl }: Props) {
```

**3e — the `drep` branch.** Replace `:49-72` in full with:

```tsx
  if (currentVote.kind === 'drep') {
    const badgeState = deriveCurrentVoteBadgeState(drepEntry);
    return (
      <section
        className={styles.component}
        aria-label={intl.formatMessage(messages.headerCurrent)}
      >
        <h3 className={styles.header}>
          {intl.formatMessage(messages.headerCurrent)}
        </h3>
        <div className={styles.statusRow}>
          <span className={styles.statusBadge}>
            <span className={styles.glyph} aria-hidden="true">
              ●
            </span>
            {intl.formatMessage(messages.statusDelegatedToDRep)}
          </span>
          <DRepSourceLabel source="on-chain" className={styles.sourceLabel} />
          {(badgeState === 'active' || badgeState === 'inactive') && (
            <DRepStatusBadge status={drepEntry.status} />
          )}
          {badgeState === 'expiring' && (
            <span className={styles.expiringBadge}>
              <span className={styles.glyph} aria-hidden="true">
                ▲
              </span>
              {intl.formatMessage(messages.statusExpiringBadge, {
                n: drepEntry.drepActivity,
              })}
            </span>
          )}
        </div>
        <div className={styles.idRow}>
          <DRepIdDisplay drepId={currentVote.drep.raw} />
        </div>
        {badgeState !== 'active' && (
          <p className={styles.caption}>
            {badgeState === 'expiring'
              ? intl.formatMessage(messages.statusExpiring, {
                  n: drepEntry.drepActivity,
                })
              : intl.formatMessage(
                  badgeState === 'inactive'
                    ? messages.statusInactive
                    : messages.statusUnavailable
                )}
          </p>
        )}
      </section>
    );
  }
```

Structural rules encoded above, each of which is checked at review: the existing
"Delegated to DRep" chip (`styles.statusBadge`) is unchanged; the lifecycle badge
is appended to `styles.statusRow` **after** `<DRepSourceLabel …/>`; the caption is
the **last** child of the section, below the id row; the local badge pairs an
`aria-hidden` glyph with visible localized text, so colour is never the sole
indicator. Do not touch the `currentVote == null` branch (`:20-47`), the sentinel
branch (`:74-99`), or the export (`:102`).

#### Step 4: Update `CurrentVoteSummary.spec.tsx`

File: `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx`.

**4a — add the type import** after `import type { WalletVotingTarget } …` (`:12`):

```tsx
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
```

**4b — add a base entry fixture** after `DREP_VOTE` (ends `:28`):

```tsx
const ACTIVE_ENTRY: AppDRepDirectoryEntry = {
  drepId: KEY_CIP129,
  votingPower: null,
  status: 'active',
  drepActivity: 30,
  anchor: null,
};
```

**4c — widen the render helper** (`:30-42`): add a second optional parameter and
pass it through. Everything else in the helper stays byte-identical:

```tsx
const renderSummary = (
  currentVote: WalletVotingTarget | null,
  drepEntry?: AppDRepDirectoryEntry | null
) =>
```

and, at `:39`:

```tsx
        <CurrentVoteSummary currentVote={currentVote} drepEntry={drepEntry} />
```

**4d — rewrite the second `it` (`:55-65`) in full.** With no entry supplied the
component now renders the neutral caption, so both the test name and the
assertions change:

```tsx
  it('renders the DRep id row with the on-chain label and the neutral status caption when no directory entry is supplied (snapshot)', () => {
    const { container } = renderSummary(DREP_VOTE);
    expect(screen.getByText('!!!Delegated to DRep')).toBeInTheDocument();
    // DRepIdDisplay truncates the visible text but exposes the full raw id.
    expect(screen.getByLabelText(KEY_CIP129)).toBeInTheDocument();
    expect(screen.getByText('!!!On-chain')).toBeInTheDocument();
    expect(screen.getByText('!!!DRep status is loading.')).toBeInTheDocument();
    expect(screen.queryByText('!!!Active')).not.toBeInTheDocument();
    expect(screen.queryByText('!!!Inactive')).not.toBeInTheDocument();
    expect(screen.queryByText(/!!!Expiring in/)).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });
```

The first, third and fourth `it` blocks are unchanged.

**4e — append a second `describe` after the existing one** (after `:94`):

```tsx
describe('CurrentVoteSummary DRep status badge', () => {
  afterEach(cleanup);

  it('renders the shared active badge with no status caption', () => {
    const { container } = renderSummary(DREP_VOTE, ACTIVE_ENTRY);
    expect(screen.getByText('!!!Active')).toBeInTheDocument();
    expect(
      screen.queryByText(/lapse in|currently inactive|status is loading/)
    ).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('renders the local expiring badge and caption inside the remaining-epoch window', () => {
    const { container } = renderSummary(DREP_VOTE, {
      ...ACTIVE_ENTRY,
      drepActivity: 4,
    });
    expect(screen.getByText('!!!Expiring in 4 epochs')).toBeInTheDocument();
    expect(
      screen.getByText(
        "!!!This DRep's voting power will lapse in 4 epochs — consider re-delegating."
      )
    ).toBeInTheDocument();
    expect(screen.queryByText('!!!Active')).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('renders the shared inactive badge and caption', () => {
    const { container } = renderSummary(DREP_VOTE, {
      ...ACTIVE_ENTRY,
      status: 'inactive',
      drepActivity: 0,
    });
    expect(screen.getByText('!!!Inactive')).toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!This DRep is currently inactive. Your voting power will not be counted until they vote again — consider re-delegating.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/!!!Expiring in/)).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('treats the window boundary as expiring', () => {
    renderSummary(DREP_VOTE, { ...ACTIVE_ENTRY, drepActivity: 12 });
    expect(screen.getByText('!!!Expiring in 12 epochs')).toBeInTheDocument();
  });

  it('treats one epoch beyond the window as active', () => {
    renderSummary(DREP_VOTE, { ...ACTIVE_ENTRY, drepActivity: 13 });
    expect(screen.getByText('!!!Active')).toBeInTheDocument();
    expect(screen.queryByText(/!!!Expiring in/)).not.toBeInTheDocument();
  });

  it('keeps the active badge when the remaining epochs are unknown', () => {
    renderSummary(DREP_VOTE, { ...ACTIVE_ENTRY, drepActivity: null });
    expect(screen.getByText('!!!Active')).toBeInTheDocument();
    expect(screen.queryByText(/!!!Expiring in/)).not.toBeInTheDocument();
  });

  it('renders no status badge or caption for the abstain sentinel', () => {
    renderSummary({ kind: 'abstain' }, ACTIVE_ENTRY);
    expect(screen.queryByText('!!!Active')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/lapse in|currently inactive|status is loading/)
    ).not.toBeInTheDocument();
  });
});
```

Each `it` renders once, and `afterEach(cleanup)` unmounts between tests — never
call `renderSummary` twice inside one `it`, or `getByText` will find two matches.
No test name may cite a task id, a `CAT-`/`CP-` label, or a plan name.

#### Step 5: Rewrite `CurrentVoteSummary.stories.tsx`

File: `storybook/stories/governance/CurrentVoteSummary.stories.tsx`.

**5a — delete `:15-52`** — the comment block plus `KEY_CIP129`, `KEY_CIP105`,
`KEY_CREDENTIAL_HEX`, `CURRENT_VOTE_OPTIONS` and the local `resolveCurrentVote`.
Do not append a fifth option to `CURRENT_VOTE_OPTIONS`; the shared five-option
knob comes from task-143's module.

**5b — imports.** Keep `:1-13` (`React`, `storiesOf`, `withKnobs, select`,
`StoryDecorator`, `StoryProvider`, `CurrentVoteSummary`, the `WalletVotingTarget`
type import, `PANEL_STYLE`). Add after the existing type import at `:7`:

```tsx
import type { AppDRepDirectoryEntry } from '../../../source/renderer/app/stores/GovernanceStore';
import { resolveCurrentVote, useCurrentVoteKnob } from './_utils/fixtures';
```

**5c — insert the status knob options and resolver** where the deleted block was
(above the `storiesOf` call at `:57`):

```tsx
type DRepStatusOption = 'none' | 'active' | 'expiring' | 'inactive';

const DREP_STATUS_OPTIONS: Record<string, DRepStatusOption> = {
  'No record yet': 'none',
  Active: 'active',
  'Expiring soon': 'expiring',
  Inactive: 'inactive',
};

const resolveDRepEntry = (
  statusOption: DRepStatusOption,
  currentVote: WalletVotingTarget | null
): AppDRepDirectoryEntry | null => {
  if (
    statusOption === 'none' ||
    currentVote == null ||
    currentVote.kind !== 'drep'
  ) {
    return null;
  }
  return {
    drepId: currentVote.drep.cip129 ?? currentVote.drep.raw,
    votingPower: null,
    status: statusOption === 'inactive' ? 'inactive' : 'active',
    drepActivity: DREP_ACTIVITY_BY_STATUS[statusOption],
    anchor: null,
  };
};
```

with, directly above it:

```tsx
const DREP_ACTIVITY_BY_STATUS: Record<DRepStatusOption, number | null> = {
  none: null,
  active: 30,
  expiring: 4,
  inactive: 0,
};
```

**5d — the story body.** Keep the locale comment at `:54-56` verbatim, the
`storiesOf('Governance / Current Vote Summary', module)` id, both decorators, and
the single `.add('Core states', …)` id. Replace the body (`:64-78`) with:

```tsx
  .add('Core states', () => {
    const option = useCurrentVoteKnob();
    const statusOption = select(
      'DRep status (mock)',
      DREP_STATUS_OPTIONS,
      'none'
    );
    const currentVote = resolveCurrentVote(option);
    return (
      <div style={PANEL_STYLE}>
        <CurrentVoteSummary
          key={option}
          currentVote={currentVote}
          drepEntry={resolveDRepEntry(statusOption, currentVote)}
        />
      </div>
    );
  });
```

Binding story rules: the two knobs are independent — the wallet knob
(`Current vote (mock)`, five options including `drepVerified`, default
`noDelegation`) comes from `useCurrentVoteKnob()`; the status knob
(`DRep status (mock)`, default `'none'`) is the only way to reach the "no record
yet" and `inactive` states. `key={option}` stays the option id verbatim — no
composite key, no index. Add **no** `IntlProvider` and **no** per-locale story
variants: `storybook/preview.tsx:8` applies the global `StoryWrapper`, whose
English/Japanese toggle drives every label, and a local provider would shadow it.
Do not rename or add a story id, and do not touch `storybook/stories/index.ts`.

#### Step 6: Verify

Run from the repo root, in this order:

```bash
node_modules/.bin/tsc --noEmit
yarn lint
# regenerate the snapshots (one rewritten name is pruned, three are added):
node_modules/.bin/jest --testPathPattern=CurrentVoteSummary --no-coverage --runInBand -u
# re-run clean, with no -u: expect 1 suite, 11 tests, 7 snapshots, all green
node_modules/.bin/jest --testPathPattern=CurrentVoteSummary --no-coverage --runInBand
# neighbouring suites must be untouched (3 suites / 25 tests green at HEAD):
node_modules/.bin/jest --testPathPattern="voting-governance|VotingGovernancePage" --no-coverage --runInBand
node_modules/.bin/prettier --check \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.messages.ts \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.scss \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx \
  storybook/stories/governance/CurrentVoteSummary.stories.tsx
```

All five files are prettier-2.1.2-clean at HEAD (measured), so if `--check` is
red it is your new code: re-run the identical command with `--write` on **exactly
these five paths**. Never run `yarn prettier` — its package.json script carries a
repo-wide `"**/*.*"` glob that reformats ~240 unrelated files. Never format the
snapshot file or any catalog JSON.

Style order — the count must not move:

```bash
node_modules/.bin/stylelint source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.scss
# expected: exit 2 with exactly 12 order/properties-alphabetical-order errors,
# the same 12 as at HEAD. If it reports 13+, the .expiringBadge block was not
# written alphabetically — fix that block only.
```

Boundary checks — all must hold:

```bash
# the ONLY GovernanceStore reference in the component is the type import:
grep -n "GovernanceStore" source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx
# no store/IPC/observer coupling and no anchor content:
grep -nE "@inject|observer|drepIndex|ipcRenderer|cardano-cli|givenName|DRepIndexEntry" \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx \
  || echo "OK: no store/IPC/anchor coupling"
# the shared badge and the status union are untouched:
git diff --stat source/renderer/app/components/governance/_shared source/common/types/governance.types.ts
# the catalogs are untouched:
git status --short source/renderer/app/i18n/locales translations/messages.json
# the mount point is still absent (task-139 owns it):
grep -n "CurrentVoteSummary" source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx \
  || echo "OK: panel not mounted yet"
# exactly six paths changed:
git status --short
```

Expected `[React Intl] Missing message: "voting.governance.currentVote.status.…"`
console errors during the Jest run — the four ids are not in `en-US.json` until
task-146. They do not fail the suite. Do not silence them and do not add the keys.

If you need to discard work, use `git restore <paths>` or `git checkout -- <paths>`.
Never `git stash` — the stash stack is shared across worktrees and sessions.
`nix` is absent in this container, so `nix fmt` cannot run; the prettier command
above is the recorded substitute and the deviation is reported for a pre-merge
`nix fmt` by the user.

Commit (single Conventional Commits subject line, no body, no trailer):

```
feat(gov): task-136 render the live DRep status badge in the current-vote summary
```

#### Acceptance

- [ ] **AC-1** — "DRep state renders the delegated DRep's live active / inactive /
      expiring-in-{n}-epochs status badge … sourced from
      `GovernanceStore.drepIndex[drepId]` (no new IPC / cardano-cli invocation
      issued by this component)." Satisfied by Steps 1–3: `active` / `inactive`
      render `<DRepStatusBadge status={drepEntry.status} />`, `expiring` renders
      the local `styles.expiringBadge` with `statusExpiringBadge` and `{n}`. The
      "sourced from `drepIndex`" half is satisfied **through the prop chain** —
      the component takes `drepEntry` and reads no store; task-139 resolves the
      entry out of `drepIndex` and passes it down. Step 6's boundary greps prove
      no IPC / cli / store coupling.
- [ ] **AC-2** — "When the DRep status is Inactive or Expiring, a single-line
      caption (`…status.inactive` / `…status.expiring`) appears below the id
      row." Satisfied by Step 3e's caption paragraph (last child of the section,
      below `styles.idRow`) and pinned by the two caption tests in Step 4e.
- [ ] **AC-3** — "When `GovernanceStore` has no record for the delegated DRep
      yet, the badge is omitted and a neutral `…status.unavailable` caption is
      shown — never a default-to-Active fallback and never a fallback IPC
      lookup." Satisfied by `deriveCurrentVoteBadgeState` returning
      `'unavailable'` for a null/absent entry; pinned by the rewritten Step 4d
      test (caption present, all three badge labels absent) and reachable in
      Storybook via the `No record yet` status knob value.
- [ ] **AC-4** — "`drepVerified` Storybook knob renders without console errors in
      en-US and ja-JP." **Satisfied in part.** The knob itself is delivered: Step
      5 wires `useCurrentVoteKnob()`, whose five options include `drepVerified`,
      and the global `StoryWrapper` supplies both locales. Two clauses are not
      dischargeable at this commit and must be recorded in the task's
      `statusReason`: (a) there is **no browser in this container**, so "renders
      without console errors" cannot be observed here — it is OWED and must never
      be asserted green; (b) until task-146 seeds both catalogs, the four new ids
      legitimately log `[React Intl] Missing message` in Storybook, so the
      console-clean observation can only be made **after** task-146 lands.
- [ ] Spec debt paid: `CurrentVoteSummary.spec.tsx:61-63`'s
      `queryByText(/Active|Inactive|Expiring/)` assertion is rewritten and the
      snapshot file is regenerated by `jest -u`, not hand-edited.
- [ ] `DRepStatus` and `DRepStatusBadge` are unwidened in every file;
      `expiring` exists only as a renderer-local display state.
- [ ] `tsc --noEmit` clean, `yarn lint` clean, prettier clean on the five source
      files, `CurrentVoteSummary` suite green, neighbouring voting-governance
      suites unchanged.

---

## task-137 and task-138: `selectedWalletId` state + pre-fill from the current on-chain delegation

Both tasks edit the same file and must land in this order: **137 first, then 138**.
task-138's seed chain is only expressible once the wallet is derived from an id
rather than cached in state. Neither task may touch `submitButtonDisabled`
(`VotingPowerDelegation.tsx:139-143`), the `chosenOption` derivation (`:160-163`),
the JSX layout, or import `CurrentVoteSummary` — those belong to task-139 and
task-140.

---

### task-137: Replace the selected `Wallet` object state with `selectedWalletId`

**Files touched:**

- `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`
  (edit — the whole task)

No spec file changes. No new file. No i18n key. No prettier run (the file is
pre-existing and `prettier --check` is already red on it at HEAD for unrelated
reasons — reformatting it would drag ~unrelated reversions into the diff).

**Context.** The component holds the selected `Wallet` **object** in local React
state. `stores.wallets.all` is replaced with fresh `Wallet` instances on every
poll, so the cached object goes stale and its `currentVote` stops matching the
chain. The fix is to hold the id and re-derive the object from the latest
`wallets` prop on every render.

The eight live seams, verified in the worktree at HEAD (`504b44c1a`):

`VotingPowerDelegation.tsx:53-66` — the state types:

```ts
type FormData = {
  selectedWallet: Wallet;
  selectedVoteType: VoteType;
  drepInputState: {
    dirty: boolean;
    value: string;
  };
  fees?: BigNumber;
};

type Form = Omit<FormData, 'selectedWallet'> & {
  selectedWallet: Wallet | null;
  status: 'form';
};
```

`:94-102` — the initial state:

```ts
const initialState: State = {
  status: 'form',
  selectedWallet: null,
  selectedVoteType: 'drep',
  drepInputState: {
    dirty: false,
    value: '',
  },
};
```

`:115-131` — the lazy `useState` initializer:

```tsx
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

`:135-137` — `formIsValid`; `:172-175` — the `initiateTransaction` call;
`:231-237` — the `WalletsDropdown.onChange` reset; `:239` — the dropdown
`value`; `:244` and `:260` — the two render gates; `:286` — the browse-DReps
handoff payload; `:333` — the confirmation-dialog argument. Seven of those eight
sites *read* `state.selectedWallet` (`:136`, `:174`, `:239`, `:244`, `:260`,
`:286`, `:333`); `:231-237` is the odd one out — it *writes* the field from a
shadowing local `const selectedWallet = wallets.find(…)` at `:232`, which is why
its migration is a deletion rather than a substitution.

**Locked invariants (inline).**

- **No second delegation backend.** The form's selection stays in *local React
  state* and the DRep-ID handoff stays in React Router `location.state`. Do not
  introduce a MobX store read, an `@inject`, or a `GovernanceStore` /
  `VotingStore` import into this component. The `wallets` prop
  (`VotingPowerDelegation.tsx:35`) — fed by the container from `stores.wallets.all`
  at `VotingGovernancePage.tsx:63` — is the only wallet source.
- **Byte-equality.** The DRep id string that reaches `chosenOption` and
  `voting.delegateVotes({ dRepId })` must stay byte-for-byte what the user typed
  or what the directory handed over. This task must not trim, lower-case,
  re-encode or re-normalize `drepInputState.value` anywhere.
- **No auto-delegation.** A wallet with no current vote still starts with a blank
  DRep input. Nothing in this task may pick a DRep.

**Resolved judgment calls (do not revisit):**

- `tsconfig.json:79` sets `"strict": false` and `strictNullChecks` is commented
  out, so `Wallet | null` flows into a `Wallet` parameter without a compile
  error. **Do not add null guards to the `initiateTransaction` call.** An early
  `return` there would strand `status: 'form-submitted'` and permanently disable
  the submit button. `formIsValid` (`:135-137`) already reads the derived wallet,
  so the button re-disables by itself if the wallet leaves the snapshot.
- **One guard is added**, at the confirmation-render site (Step 5c): the derived
  wallet can legitimately be `null` there, and
  `VotingPowerDelegationConfirmationDialog` dereferences
  `selectedWallet.isHardwareWallet` (`VotingPowerDelegationConfirmationDialog.tsx:179`)
  unconditionally.
- The initializer keeps storing `initialWallet?.id ?? null` rather than the raw
  `initialFormState.selectedWalletId`, so an id absent from the current snapshot
  is not adopted — byte-identical behaviour to today, where an unknown id yielded
  `selectedWallet: null`.
- Locals are renamed to avoid shadowing the new derived `selectedWallet`
  (`no-shadow` is `warn` at `.eslintrc:83`): the initializer's local becomes
  `initialWallet`, the `onChange` local becomes `nextWallet`.

#### Step-by-Step

##### Step 1: Swap the field in `FormData` and `Form`

Replace `VotingPowerDelegation.tsx:53-66` with:

```ts
type FormData = {
  selectedWalletId: string | null;
  selectedVoteType: VoteType;
  drepInputState: {
    dirty: boolean;
    value: string;
  };
  fees?: BigNumber;
};

type Form = Omit<FormData, 'selectedWalletId'> & {
  selectedWalletId: string | null;
  status: 'form';
};
```

`FormWithError` (`:68-71`), `StateFormComplete` (`:73-75`), `StateConfirmation`
(`:77-80`) and `State` (`:82`) are unchanged — they all derive from `FormData`.

##### Step 2: `initialState`

At `:94-102`, replace the `selectedWallet: null,` line with:

```ts
  selectedWalletId: null,
```

Nothing else in that object changes.

##### Step 3: Rename the initializer's local and store the id

Replace the body of the `useState` initializer (`:115-131`) with:

```tsx
  const [state, setState] = useState<State>(() => {
    if (!initialFormState) return initialState;
    const { selectedWalletId, voteType, selectedDRepId } = initialFormState;
    const initialWallet =
      (selectedWalletId && wallets.find((w) => w.id === selectedWalletId)) ||
      null;
    return {
      ...initialState,
      selectedWalletId: initialWallet?.id ?? null,
      selectedVoteType: voteType || initialState.selectedVoteType,
      // The directory-selected ID is used verbatim: it must reach chosenOption
      // and the delegateVotes dRepId byte-for-byte (no trim, no re-encoding).
      drepInputState: selectedDRepId
        ? { dirty: true, value: selectedDRepId }
        : initialState.drepInputState,
    };
  });
```

The comment is carried across unchanged — it states the byte-equality invariant,
not a change.

##### Step 4: Declare the derived wallet

Immediately after the closing `});` of the `useState` call (currently `:131`) and
before `const drepInputIsValid` (`:133`), insert:

```tsx
  const selectedWallet =
    wallets.find((w) => w.id === state.selectedWalletId) ?? null;
```

No `useMemo`, no store read, no comment. This is the "derive from the latest
snapshot" seam: `wallets` is a fresh array on every poll, so the local always
carries the current `currentVote`.

##### Step 5: Migrate the eight sites (seven reads plus the `onChange` write)

Each is a literal `state.selectedWallet` → `selectedWallet` substitution except
where noted.

**(a)** `:135-137`:

```tsx
  const formIsValid =
    !!selectedWallet &&
    (state.selectedVoteType === 'drep' ? drepInputIsValid : true);
```

**(b)** `:172-175`:

```tsx
      const result = await initiateTransaction({
        chosenOption,
        wallet: selectedWallet,
      });
```

**(c)** `:231-239` — rename the local, store the id:

```tsx
            onChange={(walletId: string) => {
              const nextWallet = wallets.find((w) => w.id === walletId) ?? null;
              setState({
                ...initialState,
                selectedWalletId: nextWallet?.id ?? null,
              });
            }}
            placeholder={intl.formatMessage(messages.selectWalletPlaceholder)}
            value={selectedWallet?.id || null}
```

**(d)** `:244` → `{selectedWallet && (`

**(e)** `:260` → `{selectedWallet && state.selectedVoteType === 'drep' && (`

**(f)** `:286` → `selectedWalletId: selectedWallet?.id ?? null,`

**(g)** `:323-334` — add the null guard described in the judgment calls:

```tsx
      {state.status === 'confirmation' &&
        selectedWallet &&
        renderConfirmationDialog({
          chosenOption,
          fees: state.fees,
          onClose: () => {
            setState({
              ...state,
              status: 'form',
            });
          },
          selectedWallet,
        })}
```

`renderConfirmationDialog`'s parameter is still named `selectedWallet`
(`Props`, `:36-41`) — that prop contract does not change; only the shorthand
value does.

##### Step 6: Verify

```bash
# from the worktree root
grep -nE "state\.selectedWallet([^I]|$)" source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx
# expected: no output — every state read now goes through selectedWalletId

grep -n "selectedWalletId" source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx
# expected: the two Props entries, the FormData + Form fields, initialState,
# the initializer destructure + assignment, the derived find, the onChange
# assignment, and the browse-DReps payload

node_modules/.bin/tsc --noEmit
node_modules/.bin/jest --testPathPattern="voting-governance|VotingGovernancePage" --no-coverage --runInBand
# expected: 3 suites / 25 tests / 4 snapshots, all green (the HEAD baseline)

node_modules/.bin/jest --runInBand      # closing gate: all 86 suites green
yarn lint                                # exit 0 (warning count only)
```

Do **not** run prettier on this file.

#### Acceptance

- [ ] `VotingPowerDelegation` stores `selectedWalletId` (string) and not a
      `Wallet` object in local React state (AC-1 — Steps 1-3; the Step 6 grep
      returns no `state.selectedWallet` read).
- [ ] The selected wallet is derived against the latest wallets snapshot (AC-2 —
      Step 4). **Deviation, deliberate:** the `find()` runs over the `wallets`
      prop, not a direct `stores.wallets.all` read. The container feeds that prop
      from `stores.wallets.all` (`VotingGovernancePage.tsx:63`); adding a store
      read inside a presentational component would break the container split and
      the no-second-backend invariant. Same snapshot, same reactivity.
- [ ] The `Wallet`-object-in-state pattern is gone, so a poll refresh cannot
      strand a stale `currentVote` (AC-3 — Steps 1-5).
- [ ] No behavioural change is visible to the existing suites: the focused run
      and the unfiltered run are green with unchanged counts.

**Commit:** `refactor(gov): task-137 hold selectedWalletId instead of the wallet object in VotingPowerDelegation`

---

### task-138: Pre-fill `VotingPowerDelegation` from the current on-chain delegation

**Files touched:**

- `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`
  (edit — Steps 1-4)
- `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`
  (edit — Steps 5-6: the `WalletsDropdown` mock, the `renderFlow` harness, four
  new cases)

No new file, no i18n key, no prettier run (both files are pre-existing).

**Context.** After task-137 the component derives `selectedWallet` from
`state.selectedWalletId` on every render, so the wallet always carries the latest
`currentVote` (`Wallet.ts:255-256`, `@computed get currentVote(): WalletVotingTarget | null`).
Two seed sites still ignore it:

1. the lazy `useState` initializer (task-137 Step 3), which seeds only from
   `initialFormState`;
2. `WalletsDropdown.onChange`, which performs an unconditional reset — after
   task-137 it reads:

```tsx
            onChange={(walletId: string) => {
              const nextWallet = wallets.find((w) => w.id === walletId) ?? null;
              setState({
                ...initialState,
                selectedWalletId: nextWallet?.id ?? null,
              });
            }}
```

`WalletVotingTarget` (`source/renderer/app/api/wallets/types.ts:86`) is:

```ts
export type WalletVotingTarget =
  | { kind: 'drep'; drep: DRepIdentity; source: 'verified' | 'unverified' | 'onchain' }
  | { kind: 'abstain' }
  | { kind: 'no_confidence' };
```

`DRepIdentity.raw` is required and is the input string byte-for-byte
(`source/common/types/governance.types.ts:20-31`).

**Locked invariants (inline).**

- **Byte-equality.** Both seeded ids — `wallet.currentVote.drep.raw` and the
  inherited `initialFormState.selectedDRepId` — are used **verbatim**. No trim,
  no `toLowerCase`, no re-encode, no `normalizeDRepIdentity` call. The string
  seeded into `drepInputState.value` is the string that reaches `chosenOption`
  and `voting.delegateVotes({ dRepId })`.
- **No auto-delegation.** When the wallet has no `currentVote` and no id was
  inherited, the form stays blank. Never substitute a directory entry, a first
  list item, or any default DRep.
- **No second delegation backend.** The handoff still travels only through React
  Router `location.state` plus local React state. No `GovernanceStore` read is
  added anywhere, and none is added to `VotingStore`
  (`grep -n "GovernanceStore" source/renderer/app/stores/VotingStore.ts` must
  still return nothing).
- **Sanitization floor.** No DRep id, no `abstain` / `no_confidence` literal and
  no bech32 string may reach a logger, an analytics payload or electron-store.
  This task adds **no** logging at all — not in the helper, not in the effect,
  not in the spec's fixtures path. Test fixtures may contain DRep ids; runtime
  log/analytics paths may not.

**Resolved judgment calls (do not revisit):**

- **The "data changed" indicator alternative in AC-3 is not taken.** AC-3 offers
  "re-seeds (or surfaces a 'data changed' indicator)". Build the re-seed. No new
  UI, no new i18n key.
- **The re-seed only fires while `drepInputState.dirty === false`.** A dirty
  input is never overwritten — that would destroy typing mid-edit. Note the
  consequence and accept it: seeding from a `drep` current vote sets
  `dirty: true`, so a *later* change of the same wallet's on-chain vote does not
  rewrite an already-populated input. The live wallet state keeps surfacing
  through the summary panel (task-139); the form input is user-owned once it has
  a value. The re-seed's real job is the blank-form case: a wallet selected
  before its vote was known, or a delegation that lands while the form is open.
- **The effect is gated on there being a current vote** (`currentVoteKind === null`
  → return). Without that gate the mount pass would overwrite an inherited
  `initialFormState.voteType` of `abstain` / `no_confidence` with the chain's
  default `drep`, silently regressing the directory round trip.
- **`voteType` precedence in the initializer:** the wallet's `currentVote` wins;
  otherwise the inherited `initialFormState.voteType` wins; otherwise the seed's
  default. This preserves today's round-trip behaviour exactly.
- **`onChange` does not inherit `voteType`.** Picking a different wallet is a
  reset (today's semantics); only the fallback chain seeds it.
- **The spec's `stores` object is never replaced across a re-render.** Step 6's
  re-render helper mutates `stores.wallets.all` and re-renders with the same
  object. `mobx-react@6.3.1`'s `Provider` compares the provided store set on
  every render and throws
  `MobX Provider: The set of provided stores has changed` when an identity
  differs (`node_modules/mobx-react/dist/mobxreact.cjs.development.js:481-497`),
  so handing `<Provider stores={…}>` a second `buildStores()` result fails the
  case before any assertion runs.
- `react-hooks/exhaustive-deps` is **not enabled** (`.eslintrc` extends `airbnb`,
  `plugin:@typescript-eslint/recommended`, `plugin:react/recommended`,
  `prettier`, `plugin:jest/recommended` — no `react-hooks` plugin). Do not add an
  `eslint-disable` directive for it; the dependency array below is deliberately
  the two primitives that determine the outcome.

#### Step-by-Step

##### Step 1: Add `deriveFormSeed` at module scope

Insert immediately **after** the `initialState` object (task-137 Step 2, ends at
the `};` around `:102`) and **before** `function VotingPowerDelegation(`. It is
not exported.

```ts
// Both the on-chain and the directory-supplied id are seeded verbatim: the
// value must reach chosenOption and the delegateVotes dRepId byte-for-byte.
function deriveFormSeed(
  wallet: Wallet | null,
  inheritedDRepId?: string
): Pick<FormData, 'selectedVoteType' | 'drepInputState'> {
  const currentVote = wallet?.currentVote ?? null;

  if (currentVote?.kind === 'drep') {
    return {
      selectedVoteType: 'drep',
      drepInputState: { dirty: true, value: currentVote.drep.raw },
    };
  }

  if (currentVote) {
    return {
      selectedVoteType: currentVote.kind,
      drepInputState: initialState.drepInputState,
    };
  }

  if (inheritedDRepId) {
    return {
      selectedVoteType: 'drep',
      drepInputState: { dirty: true, value: inheritedDRepId },
    };
  }

  return {
    selectedVoteType: initialState.selectedVoteType,
    drepInputState: initialState.drepInputState,
  };
}
```

The four branches are the fallback chain in order: current `drep` vote → current
sentinel vote → inherited directory id → blank. Nothing else may be added to it,
and it must stay pure (no logging, no `setState`, no store access).

##### Step 2: Seed the lazy `useState` initializer

Replace the initializer body written by task-137 Step 3 with:

```tsx
  const [state, setState] = useState<State>(() => {
    if (!initialFormState) return initialState;
    const { selectedWalletId, voteType, selectedDRepId } = initialFormState;
    const initialWallet =
      (selectedWalletId && wallets.find((w) => w.id === selectedWalletId)) ||
      null;
    const seed = deriveFormSeed(initialWallet, selectedDRepId);
    return {
      ...initialState,
      selectedWalletId: initialWallet?.id ?? null,
      selectedVoteType: initialWallet?.currentVote
        ? seed.selectedVoteType
        : voteType || seed.selectedVoteType,
      drepInputState: seed.drepInputState,
    };
  });
```

The byte-equality comment moves onto `deriveFormSeed` (Step 1) — do not leave a
duplicate here.

##### Step 3: Seed the `WalletsDropdown.onChange` reset

Replace the `onChange` body (task-137 Step 5c) with:

```tsx
            onChange={(walletId: string) => {
              const nextWallet = wallets.find((w) => w.id === walletId) ?? null;
              setState({
                ...initialState,
                selectedWalletId: nextWallet?.id ?? null,
                ...deriveFormSeed(nextWallet, initialFormState?.selectedDRepId),
              });
            }}
```

The spread order matters: `deriveFormSeed`'s result must come **after**
`...initialState` so it overrides the blank defaults.

##### Step 4: Add the reactive re-seed

Insert directly after the derived `selectedWallet` local (task-137 Step 4) and
before `const drepInputIsValid`:

```tsx
  const currentVote = selectedWallet?.currentVote ?? null;
  const currentVoteKind = currentVote?.kind ?? null;
  const currentVoteDRepId =
    currentVote?.kind === 'drep' ? currentVote.drep.raw : null;

  // A wallet poll can deliver a new on-chain vote after mount; re-seed only
  // while the DRep input is untouched so user input is never overwritten.
  useEffect(() => {
    if (currentVoteKind === null) return;
    setState((previous) => {
      if (previous.status !== 'form' || previous.drepInputState.dirty) {
        return previous;
      }
      const seed = deriveFormSeed(
        selectedWallet,
        initialFormState?.selectedDRepId
      );
      if (
        previous.selectedVoteType === seed.selectedVoteType &&
        previous.drepInputState.dirty === seed.drepInputState.dirty &&
        previous.drepInputState.value === seed.drepInputState.value
      ) {
        return previous;
      }
      return { ...previous, ...seed };
    });
  }, [currentVoteKind, currentVoteDRepId]);
```

Why it is shaped this way — keep all four properties:

- the dependency array is **two primitives**, so the effect runs only when the
  derived wallet's on-chain vote actually changes, not on every poll that hands
  back a new `Wallet` instance;
- `currentVoteKind === null` short-circuits the no-vote case, protecting an
  inherited `voteType`;
- the functional `setState` updater returns `previous` **by reference** when
  nothing changes, so React bails out and the submit effect at `:165-191` (deps
  `[initiateTransaction, intl, state]`) does not re-run;
- `previous.status !== 'form'` keeps the re-seed out of the submitting,
  errored and confirmation states.

`useEffect` is already imported (`:1`). Do not add a second import.

##### Step 5: Extend the `WalletsDropdown` mock in the container spec

`source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx:32-38`
currently reads:

```tsx
// The wallet and vote-type dropdowns are react-polymorph-heavy; the flow tests
// assert the values they RECEIVE, so plain pass-through mocks are enough.
jest.mock('../../components/widgets/forms/WalletsDropdown', () => {
  return function WalletsDropdownMock(props: { value: string | null }) {
    return <div data-testid="wallets-dropdown">{props.value || 'none'}</div>;
  };
});
```

Replace the factory with (the comment above it is unchanged):

```tsx
jest.mock('../../components/widgets/forms/WalletsDropdown', () => {
  return function WalletsDropdownMock(props: {
    onChange: (walletId: string) => void;
    value: string | null;
    wallets: Array<{ id: string }>;
  }) {
    return (
      <div data-testid="wallets-dropdown">
        {props.value || 'none'}
        {props.wallets.map((wallet) => (
          <button
            data-testid={`wallets-dropdown-option-${wallet.id}`}
            key={wallet.id}
            onClick={() => props.onChange(wallet.id)}
            type="button"
          />
        ))}
      </div>
    );
  };
});
```

The option buttons carry no text and no accessible name, so
`toHaveTextContent(WALLET_ID)` at `:208` / `:227` and every
`getByRole('button', { name: … })` query in the file keep matching exactly what
they match today. This is the **only** permitted change to this mock; the
`ItemsDropdown` mock (`:40-44`) is untouched.

##### Step 6: Add the fixtures, the harness hook, and four cases

**(a)** After the `hardwareWallet` fixture (`:58-62`) add two wallets. Reuse the
existing `VALID_DREP_ID` constant (`:46-47`) — do not mint a new id.

```tsx
const VOTING_WALLET_ID = 'voting-wallet-1';

const votingWallet = {
  id: VOTING_WALLET_ID,
  name: 'Voting Wallet',
  isHardwareWallet: false,
  currentVote: {
    kind: 'drep',
    drep: { raw: VALID_DREP_ID, credentialType: 'key' },
    source: 'onchain',
  },
} as any;

const abstainWallet = {
  id: 'abstain-wallet-1',
  name: 'Abstain Wallet',
  isHardwareWallet: false,
  currentVote: { kind: 'abstain' },
} as any;
```

**(b)** Make `renderFlow` (`:126-163`) re-renderable with a refreshed wallet
snapshot. Extract the existing JSX into a local `tree` function and return a
bound `rerenderWithWallets`; everything else in the helper stays as it is. The
**same** `stores` object is handed to `tree` on every render — mutate
`stores.wallets.all`, never build a second store object (`mobx-react@6.3.1`'s
`Provider` throws
`MobX Provider: The set of provided stores has changed` when the identity of a
provided store changes:
`node_modules/mobx-react/dist/mobxreact.cjs.development.js:481-497`).
`stores.wallets.all` is a plain array on a plain object
(`VotingGovernancePage.spec.tsx:121`, `wallets: { all: wallets },`) and the
container re-reads it on every render (`VotingGovernancePage.tsx:63`,
`wallets={wallets.all}`), so the mutation is what the re-render observes:

```tsx
const renderFlow = (
  initialEntries: InitialEntry[],
  storeOverrides: StoreOverrides = {}
) => {
  const history = createMemoryHistory({ initialEntries });
  const pushSpy = jest.spyOn(history, 'push');
  const stores = buildStores(storeOverrides);
  const actions = { router: { goToRoute: { trigger: jest.fn() } } };
  const tree = (currentStores: ReturnType<typeof buildStores>) => (
    <Provider stores={currentStores as any} actions={actions as any}>
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
            <Route
              path={ROUTES.GOVERNANCE.DREP_DETAIL}
              component={DRepDetailPage}
            />
          </Router>
        </IntlProvider>
      </ThemeProvider>
    </Provider>
  );
  const { rerender } = render(tree(stores));
  return {
    actions,
    history,
    pushSpy,
    rerenderWithWallets: (wallets: any[]) => {
      stores.wallets.all = wallets;
      rerender(tree(stores));
    },
    stores,
  };
};
```

This is additive: every existing destructure (`{ history, pushSpy }`,
`{ stores }`) is unaffected, and later cv-2 tasks that call `renderFlow` do not
need to change.

**(c)** Append a new `describe` at the **end** of the file, after the
hardware-wallet describe (`:304-391`), so the sibling tasks that append into the
two existing describes never collide with it:

```tsx
describe('Delegation form pre-fill from the selected wallet', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('seeds the DRep input from the wallet current on-chain vote', () => {
    renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [votingWallet],
    });

    fireEvent.click(
      screen.getByTestId(`wallets-dropdown-option-${VOTING_WALLET_ID}`)
    );

    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent('drep');
    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('seeds the vote type and no DRep id from a sentinel on-chain vote', () => {
    renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [abstainWallet],
    });

    fireEvent.click(
      screen.getByTestId('wallets-dropdown-option-abstain-wallet-1')
    );

    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent(
      'abstain'
    );
    expect(screen.queryByDisplayValue(VALID_DREP_ID)).toBeNull();
  });

  it('keeps the inherited directory id byte-identical when the wallet is picked afterwards', () => {
    renderFlow([
      {
        pathname: ROUTES.VOTING.GOVERNANCE,
        state: { selectedDRepId: VALID_DREP_ID },
      },
    ]);

    fireEvent.click(
      screen.getByTestId(`wallets-dropdown-option-${WALLET_ID}`)
    );

    const drepInput = screen.getByDisplayValue(VALID_DREP_ID);
    expect(drepInput).toBeInTheDocument();
    expect((drepInput as HTMLInputElement).value).toBe(VALID_DREP_ID);
  });

  it('re-seeds an untouched form when a refreshed snapshot carries a new vote', () => {
    const { rerenderWithWallets } = renderFlow([
      { pathname: ROUTES.VOTING.GOVERNANCE },
    ]);

    fireEvent.click(screen.getByTestId(`wallets-dropdown-option-${WALLET_ID}`));
    expect(screen.queryByDisplayValue(VALID_DREP_ID)).toBeNull();

    rerenderWithWallets([
      {
        ...softwareWallet,
        currentVote: {
          kind: 'drep',
          drep: { raw: VALID_DREP_ID, credentialType: 'key' },
          source: 'onchain',
        },
      },
    ]);

    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });
});
```

Case 3 is the AC-5 regression: `location.state` carries `selectedDRepId` and **no**
`selectedWalletId` (`pickDelegationFormNavigationState`,
`source/renderer/app/containers/governance/delegationFormState.ts:23-45`, keeps
`selectedDRepId` on its own), the DRep input is gated off until a wallet exists,
and the mock's `onChange` is what makes it appear. Case 4 is the AC-3 pin: the
refreshed snapshot carries a *new* wallet object with the same `id`, which is the
poll shape task-137 made observable.

##### Step 7: Verify

```bash
node_modules/.bin/tsc --noEmit

node_modules/.bin/jest --testPathPattern="voting-governance|VotingGovernancePage" --no-coverage --runInBand
# expected: 3 suites green; VotingGovernancePage.spec.tsx grows from 8 to 12 tests

node_modules/.bin/jest --testPathPattern=governance-sanitization --no-coverage --runInBand
# expected: 24 tests green — the inherited sanitization floor, unchanged

grep -n "GovernanceStore" source/renderer/app/stores/VotingStore.ts || echo "OK: no store crossover"
grep -nE "toLowerCase|trim\(|normalizeDRepIdentity" source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx || echo "OK: ids untouched"
grep -nE "logger|analytics|Analytics" source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx || echo "OK: no logging added"

node_modules/.bin/jest --runInBand      # closing gate: all 86 suites green
yarn lint                                # exit 0
```

Do **not** run prettier on either file, and do **not** run `yarn i18n:manage`
(this task mints no copy).

#### Acceptance

- [ ] Selecting a wallet with `currentVote` pre-fills the form (AC-1 — Steps 1,
      3; pinned by Step 6 cases 1 and 2).
- [ ] The wallet-change reset runs the fallback chain `currentVote` → inherited
      `initialFormState.selectedDRepId` → blank, replacing the unconditional
      reset (AC-2 — Steps 1, 3).
- [ ] Pre-fill is reactive from `selectedWalletId` against the latest snapshot
      and the wallet object is not cached (AC-3 — task-137 Step 4 plus Step 4
      here; pinned by Step 6 case 4). **Satisfied in part, deliberately:** the
      re-seed applies only while `drepInputState.dirty === false`, so a vote
      change arriving *after* the input was populated does not rewrite it. The
      "data changed" indicator alternative offered by the same criterion is not
      built. Record both in the task's `statusReason`.
- [ ] A directory-reached wallet with no `currentVote` keeps the supplied id
      byte-identical, and only a wallet with neither a vote nor an inherited id
      starts blank (AC-4 — Step 1 branches 3 and 4; pinned by Step 6 case 3 and
      by the greps in Step 7).
- [ ] The directory-select-then-pick-wallet Jest regression exists and the
      `WalletsDropdown` mock exposes `onChange` (AC-5 — Steps 5 and 6, case 3).
- [ ] The handoff still travels only through `location.state` and local React
      state; no `GovernanceStore` read from `VotingStore`, no store-backed
      pending form state (AC-6 — Step 7 grep returns nothing).
- [ ] Inherited sanitization floor holds: no DRep id, sentinel or bech32 string
      reaches a logger or analytics payload, including from the new fixtures
      (AC-7 — no logging added; the `governance-sanitization` suite re-run in
      Step 7 is the proof).

**Commit:** `feat(gov): task-138 pre-fill the delegation form from the wallet current on-chain vote`

---

## task-139 and task-140: mount the panel, then block the identical vote

Both tasks edit
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`,
in this order and in **disjoint regions**: task-139 adds the `drepIndex` prop, the
index lookup and the `<CurrentVoteSummary/>` element; task-140 adds the comparator,
extends `submitButtonDisabled` and adds the hint paragraph. task-139 must not touch
`submitButtonDisabled`; task-140 must not touch the mount.

Both land **after task-137 and task-138**, which rewrite the same file's state shape.
Every line number below is the pre-slice (`504b44c1a`) number — after 137/138 they
have shifted. **Re-anchor by the quoted content, never by the number.** The two
post-137/138 facts you must assume:

- `FormData`/`Form` hold `selectedWalletId: string | null`, not a `Wallet`; the
  wallet is a derived local declared immediately after the `useState` call:
  ```ts
  const selectedWallet =
    wallets.find((w) => w.id === state.selectedWalletId) ?? null;
  ```
- Every former `state.selectedWallet` read is now that local — including the render
  gates that read `{state.selectedWallet && (` at `:244` and `:260`, which read
  `{selectedWallet && (` after task-137.

---

### task-139: Render CurrentVoteSummary in VotingPowerDelegation

**Files touched (no new files):**

- `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`
  (Steps 1-4)
- `source/renderer/app/containers/voting/VotingGovernancePage.tsx` (Step 5 — the
  store destructure and the `<VotingPowerDelegation …>` prop list only; the
  `renderConfirmationDialog` body belongs to task-173/task-141 and is not touched)
- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
  line 1241 (Step 6 — task-139's own AC-3 string)

**Context — the four seams, quoted from HEAD.**

1. `VotingPowerDelegation.tsx:23-51`, the `Props` type. Its last member today:
   ```ts
     onBrowseDRepsClick: (formState: {
       selectedWalletId: string | null;
       voteType: VoteType;
     }) => void;
   };
   ```
2. `VotingPowerDelegation.tsx:94-102`, `initialState` — the module-scope block the
   new `EMPTY_DREP_INDEX` goes after.
3. `VotingPowerDelegation.tsx:238-244`, the mount point. Current code:
   ```tsx
               placeholder={intl.formatMessage(messages.selectWalletPlaceholder)}
               value={state.selectedWallet?.id || null}
               getStakePoolById={getStakePoolById}
               disableSyncingWallets
             />

             {state.selectedWallet && (
               <ItemsDropdown
   ```
   The new element goes in the blank line between the `/>` and the
   `{state.selectedWallet && (` gate — a **sibling** of both, inside neither.
4. `VotingGovernancePage.tsx:37-39` and `:57-65`:
   ```ts
     render() {
       const { wallets, staking, app, voting, hardwareWallets, networkStatus } =
         this.props.stores;
   ```
   ```tsx
         <VotingPowerDelegation
           onExternalLinkClick={openExternalLink}
           initiateTransaction={voting.initializeVPDelegationTx}
           initialFormState={initialFormState}
           onBrowseDRepsClick={this.handleBrowseDRepsClick}
           wallets={wallets.all}
           stakePools={staking.stakePools}
           getStakePoolById={staking.getStakePoolById}
   ```

**Locked invariants (inline, written out).**

- **No auto-delegation.** Daedalus never picks a delegation for the user. The panel
  must render in the `noDelegation` state too — that state is the CIP-1694
  reward-withdrawal warning plus the "choose a delegation" nudge. Rendering it is
  the point of the unconditional mount; never gate the element on a wallet being
  selected or on a vote existing.
- **Local-first, no second backend.** Mounting the panel triggers no wallet
  re-poll, no IPC call and no cardano-cli invocation. The index is read from an
  already-populated observable Map; `resolveExactDRepMatch` performs no IPC by
  construction. `VotingStore` gains no `GovernanceStore` reference.
- **Badges are informational.** Nothing read from `drepIndex` may reorder, filter
  or gate anything in the form.
- **Byte-equality.** The lookup is a read-only transform. `currentVote.drep.cip129`
  / `.raw` are passed to the lookup and nothing is written back; the string that
  reaches `chosenOption` and `delegateVotes({ dRepId })` is unchanged.

**Resolved judgment calls (do not revisit).**

- `CurrentVoteSummary` stays a plain presentational component: no `@inject`, no
  `observer`, no store-class import. It receives `drepEntry` as a prop. Its four
  committed specs render it with no MobX provider and would break otherwise.
- `VotingPowerDelegation` reads **no** store either. The container reads
  `stores.governance` and prop-drills `drepIndex`.
- The lookup query is `currentVote.drep.cip129 ?? currentVote.drep.raw`, **not**
  `raw`. Measured in this worktree:
  `Cardano.DRepID.isValid('drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l')`
  returns `false`, and `resolveExactDRepMatch` returns `null` for anything
  `isValid` rejects (`helpers.ts:143-144`). A CIP-105 `raw` would therefore report
  "no record yet" for a DRep that *is* indexed. `normalizeDRepIdentity` always
  populates `cip129` on the production path, and `?? raw` covers hand-built
  fixtures.
- Use `resolveExactDRepMatch`, never `drepIndex.get(raw)` and never
  `drepIndex[drepId]`. `tsconfig.json` has `strict`/`noImplicitAny` off, so bracket
  access compiles and silently yields `undefined`.
- `drepIndex` is optional with a module-scope empty-Map default so this commit and
  every later one type-checks while the stories still omit the prop (task-145 wires
  them).
- task-139 adds **no spec file**. `VotingGovernancePage.spec.tsx` is owned in cv-2
  by tasks 138 → 173 → 141 → 147 and `CurrentVoteSummary.spec.tsx` by task-136;
  task-139's gate is "every existing suite stays green" plus the greps in Step 7.

#### Step-by-Step

##### Step 1: Imports

In `VotingPowerDelegation.tsx`, after the existing
`import { messages } from './VotingPowerDelegation.messages';` (`:11`) group, add:

```ts
import CurrentVoteSummary from './CurrentVoteSummary';
import { resolveExactDRepMatch } from '../../governance/drep-directory/helpers';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
```

`AppDRepDirectoryEntry` is `import type` — `GovernanceStore.ts` exports a runtime
enum as well, and a value import would pull the store class into the component
bundle. Do not re-declare the type anywhere.

##### Step 2: The `drepIndex` prop and its empty default

In `Props` (`:23-51`), add as the last member, after `onBrowseDRepsClick`:

```ts
  drepIndex?: ReadonlyMap<string, AppDRepDirectoryEntry>;
```

Immediately **after** the `initialState` block (`:94-102`), add the single shared
empty map:

```ts
const EMPTY_DREP_INDEX: ReadonlyMap<string, AppDRepDirectoryEntry> = new Map();
```

Add `drepIndex = EMPTY_DREP_INDEX` to the component's destructured parameter list
(`:104-114`), keeping the list's existing alphabetical-ish order — put it directly
after `getStakePoolById`:

```ts
function VotingPowerDelegation({
  getStakePoolById,
  drepIndex = EMPTY_DREP_INDEX,
  initiateTransaction,
```

Never write `drepIndex = new Map()` in the parameter list: that allocates a new map
on every render and defeats referential-equality checks downstream.

##### Step 3: Resolve the entry for the selected wallet's current vote

Immediately after the derived `selectedWallet` local that task-137 added (and
before `const drepInputIsValid = …`), add:

```ts
  const currentDRepEntry =
    selectedWallet?.currentVote?.kind === 'drep'
      ? resolveExactDRepMatch<AppDRepDirectoryEntry>(
          selectedWallet.currentVote.drep.cip129 ??
            selectedWallet.currentVote.drep.raw,
          drepIndex
        )
      : null;
```

No `useMemo`. No store read. `resolveExactDRepMatch` trims and lower-cases the
query, then canonicalizes it to CIP-129 before `.get()`
(`source/renderer/app/components/governance/drep-directory/helpers.ts:139-153`).
Canonicalization is gated on `Cardano.DRepID.isValid(full)` (`helpers.ts:144`),
which accepts only the `drep1…` and `drep_script1…` forms — a `drep_vkh1…`
string returns `null`. That is why the query is `cip129 ?? raw`.

##### Step 4: Mount the panel

Between the `WalletsDropdown` closing `/>` (`:242`) and the
`{state.selectedWallet && (` vote-type gate (`:244`, `{selectedWallet && (` after
task-137), insert exactly:

```tsx
          <CurrentVoteSummary
            currentVote={selectedWallet?.currentVote ?? null}
            drepEntry={currentDRepEntry}
          />
```

No wrapping element, no condition, no `className`. When no wallet is selected this
passes `currentVote={null}`, which renders the `noDelegation` warning + nudge.

##### Step 5: Container wiring

In `VotingGovernancePage.tsx`, replace the destructure at `:38-39`:

```ts
    const { wallets, staking, app, voting, hardwareWallets, networkStatus } =
      this.props.stores;
```

with:

```ts
    const {
      wallets,
      staking,
      app,
      voting,
      hardwareWallets,
      networkStatus,
      governance,
    } = this.props.stores;
```

Then add one prop to the `<VotingPowerDelegation …>` element, directly after
`getStakePoolById={staking.getStakePoolById}` (`:65`):

```tsx
          drepIndex={governance.drepIndex}
```

Do **not** run prettier on this file — `prettier@2.1.2 --check` is already red on
it at HEAD, and a `--write` would revert unrelated formatting. The exploded
destructure above is the form to hand-write.

Reactivity comes for free: the container is `@observer` and `GovernanceStore`
**reassigns** `drepIndex` (`GovernanceStore.ts:254`, `:297`) instead of mutating
it, so a directory refresh re-renders the container and the whole chain. Add no
`reaction`, no `autorun`, no polling.

##### Step 6: Reconcile task-139's AC-3 in the tracker

`.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
line 1241 currently reads (one JSON string, inside task-139's
`acceptanceCriteria` array):

```
"The `drep` state reads `givenName` from `GovernanceStore.drepIndex[drepId]?.givenName`. The panel updates reactively when `drepIndex` is populated or updated; no wallet re-poll is triggered. A Storybook story covers the transition from unverified to verified name."
```

Replace that string — and nothing else in the file — with:

```
"The panel updates reactively when `drepIndex` is populated or updated; no wallet re-poll is triggered."
```

Reason to record in the commit's `statusReason` (Scribe step): the struck clauses
are not buildable in cv-2. `AppDRepDirectoryEntry` (`GovernanceStore.ts:20-31`)
carries exactly `drepId`, `votingPower`, `status`, `drepActivity`, `anchor` — no
name field; its IPC counterpart `DRepDirectoryEntry`
(`source/common/types/governance.types.ts:51-62`) is the same shape; `givenName`
appears exactly once in `source/`, `storybook/` and `tests/`, as a *negative*
fixture asserting the field never renders
(`VotingPowerDelegationConfirmationDialog.spec.tsx:85-98`). The clauses are
deferred to `anchor-2`; also record them in
`.agent/plans/governance/drep-discovery/research/cv-2-findings.md` (the findings
file, not this guide).

This JSON is tool-managed: hand-edit the one string, preserve the surrounding
formatting, and **never** run prettier on it.

##### Step 7: Verify

```bash
node_modules/.bin/tsc --noEmit
node_modules/.bin/jest --testPathPattern="voting-governance|VotingGovernancePage" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=governance --no-coverage --runInBand
yarn lint

# the component reads no store and the panel is not gated
grep -n "CurrentVoteSummary\|drepIndex\|currentDRepEntry" source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx
grep -n "inject\|GovernanceStore" source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx || echo "OK: no store read in the panel"
# invariant 4 still holds
grep -n "GovernanceStore" source/renderer/app/stores/VotingStore.ts || echo "OK: VotingStore does not know the governance store"
# no process artifacts in code
grep -rn "task-1[0-9][0-9]" source storybook tests || echo "OK: no task ids in code"
```

Expected: `tsc` exit 0; every governance-adjacent suite green (at HEAD the first
pattern selects 3 suites / 25 tests — the count changes once 136/137/138 land, so
gate on green, not on the number).

**The one regression to expect and check by eye.** `VotingGovernancePage.spec.tsx`
builds its wallets as plain objects (`softwareWallet`, `:50-54`) with no
`currentVote`, so the panel now renders its `noDelegation` branch into that spec's
DOM: an extra heading `!!!No governance delegation`, a `role="alert"` paragraph,
and a react-polymorph button labelled `!!!Choose a delegation`. None of the
existing queries collide with it (they use `Submit`, `Confirm`,
`!!!Select for delegation`, `!!!View details`, `!!!Browse DReps`, `getByTestId`,
`getByDisplayValue`). If a query does become ambiguous, fix the query in the spec
— never weaken an assertion and never gate the mount to make a test pass.

Do not run prettier on either edited file (both are pre-existing) and do not run
`yarn check:all` (red at HEAD for unrelated reasons: `storybook:build` and
`prettier:check`).

Commit (subject only, no body, no trailers):
`feat(gov): task-139 mount CurrentVoteSummary in the delegation form`

#### Acceptance

- [ ] **AC-1** — "CurrentVoteSummary always renders above the form, including
      noDelegation warning + nudge state." Step 4: the element is unconditional and
      sits above the vote-type controls; with no wallet selected it receives
      `currentVote={null}` and renders the warning + nudge.
- [ ] **AC-2** — "Layout matches design (verified via Storybook in cv-1/cv-2)."
      Satisfied in part: the mount point matches the IA (panel between wallet
      picker and vote-type controls). The Storybook confirmation of the *mounted*
      panel with wallet fixtures is task-145's, and no browser exists in this
      container, so the visual check is recorded **OWED** — never asserted green.
- [ ] **AC-3 (as rewritten in Step 6)** — "The panel updates reactively when
      `drepIndex` is populated or updated; no wallet re-poll is triggered."
      Steps 3 + 5: container `@observer` + Map reassignment gives the re-render;
      the Step 7 greps show no store read in either component, no new IPC and no
      poll trigger.
- [ ] The struck AC-3 clauses (`givenName` read; unverified→verified story) are
      recorded as deferred to `anchor-2` in `statusReason` and in
      `research/cv-2-findings.md`, naming `GovernanceStore.ts:20-31` as evidence.
- [ ] `submitButtonDisabled`, `chosenOption`, the form state shape and the seed
      chain are untouched by this task.

---

### task-140: Disable submit on identical-to-current delegation

**Files created:**

- `source/renderer/app/utils/governance/isSameVoteTarget.ts` (Step 1)
- `tests/jest/governance/isSameVoteTarget.spec.ts` (Step 6)

**Files touched:**

- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.messages.ts`
  (Step 2 — one new descriptor)
- `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`
  (Steps 3-5)
- `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.scss`
  (Step 5 — one new rule)
- `.agent/plans/governance/drep-discovery/designs/current-vote-display-design.md`
  line 97 (Step 7 — append one sentence)
- `.agent/plans/governance/drep-discovery/task-plans/cv-1-code-review.md`
  (Step 8 — append an entry at end of file; this file is append-only)
- `.agent/plans/governance/drep-discovery/research/cv-1-findings.md` lines 220-227
  (Step 9 — correct in place)
- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
  line 1263 (Step 10 — re-anchor `:95` → `:97` in task-140's own AC-7)

**Context — the two seams, quoted from HEAD.**

`VotingPowerDelegation.tsx:135-143` (`formIsValid` reads `selectedWallet` after
task-137):

```ts
  const formIsValid =
    !!state.selectedWallet &&
    (state.selectedVoteType === 'drep' ? drepInputIsValid : true);

  const submitButtonDisabled =
    !formIsValid ||
    state.status === 'form-submitted' ||
    state.status === 'form-with-error' ||
    state.status === 'form-initiating-tx';
```

`VotingPowerDelegation.tsx:160-163`, the chosen option (**do not modify this
derivation**):

```ts
  const chosenOption =
    state.selectedVoteType === 'drep'
      ? state.drepInputState.value
      : state.selectedVoteType;
```

and `:304-320`, the error paragraph the hint is modelled on plus the submit button:

```tsx
          {state.status === 'form-with-error' && (
            <p className={styles.generalError}>
              {intl.formatMessage(mapOfTxErrorCodeToIntl[state.txInitError])}
            </p>
          )}

          <Button
            label={intl.formatMessage(messages.submitLabel)}
            className={styles.voteSubmit}
            disabled={submitButtonDisabled}
```

**Locked invariants (inline, written out).**

- **Byte-equality.** The comparator mutates nothing. It takes two values and
  returns a boolean; the id reaching `chosenOption` and
  `voting.delegateVotes({ chosenOption })` stays the form input string
  byte-for-byte — no trim, no lower-casing, no re-encoding anywhere on the
  submission path. Lower-casing happens only on a *copy* of `credentialHex` inside
  the comparison expression.
- **Case-stable key.** Compare on the (`credentialHex`, `credentialType`) pair.
  Never compare `raw`, `cip129` or `cip105` as strings: BIP-173 permits an
  all-uppercase encoding, `normalizeDRepIdentity` returns those fields
  byte-untouched (`normalizeDRepIdentity.ts:39-40`, `:56`), and the form gate
  `Cardano.DRepID.isValid` (`VotingPowerDelegation.tsx:133`) accepts uppercase.
- **`credentialHex` is optional, `credentialType` is required**
  (`source/common/types/governance.types.ts:26-30`). Two identities that both lack
  the hex are **never** equal, and `credentialType` alone never establishes
  equality.
- **Sentinels are form-only.** `abstain` / `no_confidence` are never DRep directory
  entries and are never bech32-decoded. They compare as sentinels: an `abstain`
  choice equals an `abstain` current vote and nothing else.
- **The server `same_vote` net stays reachable.** The client-side disable is an
  affordance, not the enforcement. Every path where equality cannot be established
  (undecodable input, missing hex, unpopulated `currentVote`) returns `false` and
  submits, letting `VotingStore`'s `same_vote` error path (`VotingStore.ts:61-64`)
  do its job. Do not add a second guard anywhere else.
- **Sanitization floor.** No DRep id, no `abstain`/`no_confidence` literal and no
  bech32 string may enter a logger, analytics or electron-store payload. The
  comparator logs nothing; add no logging while debugging it.

**Resolved judgment calls (do not revisit).**

- **`submitButtonDisabled` moves.** `isSameAsCurrent` needs `chosenOption`, which
  is declared *below* `submitButtonDisabled`. Referencing it from `:139` would be a
  temporal-dead-zone `ReferenceError` at render time. Resolution: leave
  `formIsValid` where it is, **delete** the `submitButtonDisabled` block from
  `:139-143` and re-declare it below `chosenOption`. `submitButtonDisabled` is used
  only in the JSX (`:313`), so moving it is safe; `chosenOption` itself is not
  moved and not modified.
- **The hint is a visible paragraph, not a hover tooltip.** Visible text is more
  accessible and satisfies the "tooltip / hint" requirement.
- **`disabled` stays a real `disabled`.** Measured: react-polymorph's `Button`
  spreads `...rest` into `ButtonSkin`, which renders `<button {...pickDOMProps(props)}>`,
  and `filter-invalid-dom-props` preserves `aria-*` — so `aria-describedby` does
  reach the DOM node. But swapping `disabled` for `aria-disabled` would leave
  `onClick` live and re-enable submission, breaking AC-1. Keep `disabled`, add
  `aria-describedby`, and record the UX doc's "button stays focusable with
  `aria-disabled='true'`" half as a **deviation** in `statusReason`.
- **The descriptor lives in `CurrentVoteSummary.messages.ts`**, with the other four
  `voting.governance.currentVote.*` enrichment keys, even though it renders in
  `VotingPowerDelegation`. It is imported under an alias because that file already
  imports a different `messages`. Do **not** add a `sameVoteHint` entry to
  `VotingPowerDelegation.messages.ts`.
- **Do not run `yarn i18n:manage`.** Catalog seeding for all seven cv-2 keys is
  task-146's single responsibility, and the command writes both locale catalogs,
  `translations/messages.json` and `defaultMessages.json`. Until then react-intl
  falls back to the `defaultMessage`, so the hint renders as
  `!!!This wallet already votes for this DRep.` If you run it by accident,
  `git restore` every file that was clean before (never `git stash`).
- **The letter-case regression vector belongs to task-147**, by AC-5's own text.
  Do not add an uppercase vector to the spec created here.
- **The end-to-end "submit is disabled" flow test belongs to task-147**
  (`VotingGovernancePage.spec.tsx`); task-140 owns the comparator's unit vectors
  only.

#### Step-by-Step

##### Step 1: Create the comparator

Create `source/renderer/app/utils/governance/isSameVoteTarget.ts` (sibling of
`normalizeDRepIdentity.ts`) with exactly:

```ts
import type { WalletVotingTarget } from '../../api/wallets/types';
import { normalizeDRepIdentity } from './normalizeDRepIdentity';

/**
 * Same-vote comparison keyed on the case-stable (credentialHex, credentialType)
 * pair: bech32 letter case is not meaningful, and an absent credentialHex never
 * establishes equality because credentialType alone cannot identify a credential.
 */
export function isSameVoteTarget(
  chosenOption: string,
  currentVote: WalletVotingTarget | null
): boolean {
  if (currentVote == null) return false;
  if (chosenOption === 'abstain') return currentVote.kind === 'abstain';
  if (chosenOption === 'no_confidence') {
    return currentVote.kind === 'no_confidence';
  }
  if (currentVote.kind !== 'drep') return false;

  const selected = normalizeDRepIdentity(chosenOption);
  if (selected == null) return false;
  if (
    selected.credentialHex == null ||
    currentVote.drep.credentialHex == null
  ) {
    return false;
  }
  return (
    selected.credentialHex.toLowerCase() ===
      currentVote.drep.credentialHex.toLowerCase() &&
    selected.credentialType === currentVote.drep.credentialType
  );
}
```

Named export only, no default. The clause order is the contract — do not
reorder, do not collapse the two sentinel branches into a single equality on
`kind`, and never touch `raw` / `cip129` / `cip105`.

##### Step 2: Mint the `sameVoteHint` descriptor

In `CurrentVoteSummary.messages.ts`, append one entry as the **last** member of the
`defineMessages({ … })` object (after `noConfidenceCaption`, `:67-72`):

```ts
  sameVoteHint: {
    id: 'voting.governance.currentVote.sameVoteHint',
    defaultMessage:
      '!!!This wallet already votes {target, select, drep {for this DRep} abstain {Abstain} no_confidence {No Confidence} other {the same way}}.',
    description:
      'Hint shown when the chosen delegation is identical to the wallet current on-chain delegation',
  },
```

The leading `!!!` is mandatory (preliminary-copy rule) and stays until a
release-end, user-owned review. The ICU argument is named `target`. Verified in
this worktree that `intl-messageformat@2.2.0` (react-intl 2.9.0's formatter)
resolves this pattern for `drep`, `abstain` and `no_confidence`.

##### Step 3: Imports and the hint id in `VotingPowerDelegation.tsx`

Add next to the existing `import { messages } from './VotingPowerDelegation.messages';`
(`:11`):

```ts
import { messages as currentVoteMessages } from './CurrentVoteSummary.messages';
import { isSameVoteTarget } from '../../../utils/governance/isSameVoteTarget';
```

And, at module scope next to `initialState` (`:94-102`):

```ts
const SAME_VOTE_HINT_ID = 'votingPowerDelegationSameVoteHint';
```

##### Step 4: Comparator + the moved `submitButtonDisabled`

Delete this block at `:139-143`:

```ts
  const submitButtonDisabled =
    !formIsValid ||
    state.status === 'form-submitted' ||
    state.status === 'form-with-error' ||
    state.status === 'form-initiating-tx';
```

Leave `formIsValid` (`:135-137`) and `voteTypes` (`:145-158`) exactly as they are.
Then, directly **after** the `chosenOption` block (`:160-163`), insert the two
consts below. They must sit above the submit `useEffect` (`:165`); if task-138's
re-seed `useEffect` now sits between `chosenOption` and that effect, put them
immediately after `chosenOption` and before both effects.

```ts
  const isSameAsCurrent = isSameVoteTarget(
    chosenOption,
    selectedWallet?.currentVote ?? null
  );

  const submitButtonDisabled =
    !formIsValid ||
    isSameAsCurrent ||
    state.status === 'form-submitted' ||
    state.status === 'form-with-error' ||
    state.status === 'form-initiating-tx';
```

`selectedWallet` is the derived local from task-137. Nothing else in the file
changes shape.

##### Step 5: The hint paragraph, the aria wiring and its style

In the JSX, insert the hint between the `form-with-error` paragraph (`:304-308`)
and the `<Button …>` (`:310`):

```tsx
          {isSameAsCurrent && (
            <p className={styles.sameVoteHint} id={SAME_VOTE_HINT_ID}>
              {intl.formatMessage(currentVoteMessages.sameVoteHint, {
                target: state.selectedVoteType,
              })}
            </p>
          )}
```

Add one attribute to the `Button`, directly after `disabled={submitButtonDisabled}`
(`:313`):

```tsx
            aria-describedby={isSameAsCurrent ? SAME_VOTE_HINT_ID : undefined}
```

Leave `disabled={submitButtonDisabled}` in place.

Append this rule to the **end** of
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.scss`
(after the `.voteSubmit` block):

```scss
.sameVoteHint {
  color: var(--theme-text-secondary, #6b7384);
  font-size: 13px;
  line-height: 1.4;
  margin-top: 28px;
}
```

Properties **must** be alphabetical: `.stylelintrc` enables
`order/properties-alphabetical-order`, and `VotingPowerDelegation.scss` is
stylelint-clean at HEAD (measured) — keep it clean. No `.scss.d.ts` regeneration is
needed: `source/renderer/declaration.d.ts:1` declares `module '*.scss'` untyped.

##### Step 6: Create the comparator spec

Create `tests/jest/governance/isSameVoteTarget.spec.ts`. All vectors below were
decoded in this worktree; the key/script pair deliberately shares its 28
credential bytes, and the script vector is the one already committed at
`tests/jest/governance/normalizeDRepIdentity.spec.ts:12-13`.

```ts
import { isSameVoteTarget } from '../../../source/renderer/app/utils/governance/isSameVoteTarget';
import { normalizeDRepIdentity } from '../../../source/renderer/app/utils/governance/normalizeDRepIdentity';
import type { WalletVotingTarget } from '../../../source/renderer/app/api/wallets/types';

const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
// These two carry the same 28 credential bytes under a 0x22 key header and a
// 0x23 script header, so only credentialType separates them.
const OTHER_KEY_CIP129 =
  'drep1yg83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq5ah2yv';
const SCRIPT_CIP129 =
  'drep1yv83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq508myt';
// HRP `drep` over a bare 28-byte credential: the form gate accepts it, the
// decoder rejects it.
const UNDECODABLE_DREP =
  'drep15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94cymwqu9';

const drepVote = (id: string): WalletVotingTarget => ({
  kind: 'drep',
  drep: normalizeDRepIdentity(id),
  source: 'onchain',
});

describe('isSameVoteTarget', () => {
  it('is false for every choice when the wallet has no current vote', () => {
    expect(isSameVoteTarget(KEY_CIP129, null)).toBe(false);
    expect(isSameVoteTarget('abstain', null)).toBe(false);
    expect(isSameVoteTarget('no_confidence', null)).toBe(false);
  });

  it('matches a sentinel only against the same sentinel', () => {
    expect(isSameVoteTarget('abstain', { kind: 'abstain' })).toBe(true);
    expect(isSameVoteTarget('abstain', { kind: 'no_confidence' })).toBe(false);
    expect(isSameVoteTarget('abstain', drepVote(KEY_CIP129))).toBe(false);
    expect(isSameVoteTarget('no_confidence', { kind: 'no_confidence' })).toBe(
      true
    );
    expect(isSameVoteTarget('no_confidence', { kind: 'abstain' })).toBe(false);
  });

  it('is false when a DRep id is compared against a sentinel vote', () => {
    expect(isSameVoteTarget(KEY_CIP129, { kind: 'abstain' })).toBe(false);
    expect(isSameVoteTarget(KEY_CIP129, { kind: 'no_confidence' })).toBe(false);
  });

  it('matches the same DRep across CIP-129 and CIP-105 encodings', () => {
    expect(isSameVoteTarget(KEY_CIP129, drepVote(KEY_CIP129))).toBe(true);
    expect(isSameVoteTarget(KEY_CIP105, drepVote(KEY_CIP129))).toBe(true);
    expect(isSameVoteTarget(KEY_CIP129, drepVote(KEY_CIP105))).toBe(true);
  });

  it('is false for a different DRep', () => {
    expect(isSameVoteTarget(OTHER_KEY_CIP129, drepVote(KEY_CIP129))).toBe(false);
  });

  it('never equates a key DRep and a script DRep sharing credential bytes', () => {
    expect(isSameVoteTarget(OTHER_KEY_CIP129, drepVote(SCRIPT_CIP129))).toBe(
      false
    );
    expect(isSameVoteTarget(SCRIPT_CIP129, drepVote(OTHER_KEY_CIP129))).toBe(
      false
    );
    expect(isSameVoteTarget(SCRIPT_CIP129, drepVote(SCRIPT_CIP129))).toBe(true);
  });

  it('is false when the stored identity carries no credential hex', () => {
    const withoutHex: WalletVotingTarget = {
      kind: 'drep',
      drep: { raw: KEY_CIP129, credentialType: 'key' },
      source: 'onchain',
    };
    expect(isSameVoteTarget(KEY_CIP129, withoutHex)).toBe(false);
    expect(isSameVoteTarget(KEY_CIP105, withoutHex)).toBe(false);
  });

  it('is false, and does not throw, when the choice cannot be decoded', () => {
    expect(isSameVoteTarget('', drepVote(KEY_CIP129))).toBe(false);
    expect(isSameVoteTarget(UNDECODABLE_DREP, drepVote(KEY_CIP129))).toBe(false);
    expect(isSameVoteTarget('not-a-bech32-string', drepVote(KEY_CIP129))).toBe(
      false
    );
  });

  it('leaves the compared identity untouched', () => {
    const currentVote = drepVote(KEY_CIP129);
    const before = JSON.stringify(currentVote);
    expect(isSameVoteTarget(KEY_CIP105, currentVote)).toBe(true);
    expect(JSON.stringify(currentVote)).toBe(before);
  });
});
```

Note on the absent-hex case: `normalizeDRepIdentity` always populates
`credentialHex` on success, so the only side that can lack it is the wallet's
stored identity — which is why the fixture is hand-built rather than decoded.

##### Step 7: Append one sentence to the design doc

`.agent/plans/governance/drep-discovery/designs/current-vote-display-design.md`
line 97 is a single long paragraph beginning "The same-vote comparator
(`task-140`) must key on a case-stable form…" and ending "…`credentialType` alone
never establishes equality." Do **not** rewrite it and do **not** touch line 95
(its header-byte classification sentence is already correct and belongs to
task-173). Append this sentence to the end of that same paragraph:

```
cv-2 ships the first of those two forms and retires the second: `isSameVoteTarget` (`source/renderer/app/utils/governance/isSameVoteTarget.ts`) compares `credentialHex` case-insensitively, requires `credentialType` equality, and returns `false` whenever either side has no `credentialHex`; the `case-insensitive cip129` alternative offered earlier in this paragraph is not available from cv-2 onward, because task-140 AC-4 forbids keying on `raw`, `cip129` or `cip105`.
```

The appended clause is what keeps the design from sanctioning a key AC-4 bans.
It is a **supersession, not a deletion** — the original offer stays in the
sentence because this row's edit rule is append-only (D-4). Record the
consequence honestly in `statusReason`: on a literal reading of AC-7 ("the
comparator sentence … *no longer offers* a canonical CIP-129 string as an
acceptable comparison key") the first conjunct is **satisfied in part**, since
the clause is still present, merely superseded.

##### Step 8: Append the correction entry to the cv-1 code-review log

`.agent/plans/governance/drep-discovery/task-plans/cv-1-code-review.md` is
**append-only**. Do not edit `:736-738`. Append at the end of the file, matching
the file's existing entry style (`---` separator, `## Role: date — subject`
heading, bold lead-ins, closing `Decision:` line):

```md
---

## Planner: 2026-07-28 — comparator-note correction discharged (cv-2 task-140)

**Discharges** the "Correction owed on the comparator note" promise at
`:1224-1234`.

**The note.** `:736-738` offers `cip129` or the (`credentialHex`,
`credentialType`) pair as the same-vote key. Read as a plain string comparison the
`cip129` half is case-unstable: BIP-173 permits an all-uppercase encoding,
`normalizeDRepIdentity` returns `raw` / `cip129` byte-untouched
(`normalizeDRepIdentity.ts:39-40`, `:56`), and the form gate
`Cardano.DRepID.isValid` (`VotingPowerDelegation.tsx:133`) accepts it. The note's
acceptable keys were the pair or an explicitly case-insensitive `cip129`
comparison; cv-2 ships the pair and retires the alternative (task-140 AC-4). The
promise block's own self-reference reads `:737-739`; the sentence actually
occupies `:736-738` (`:739` opens the next refutation item).

**What shipped.**
`source/renderer/app/utils/governance/isSameVoteTarget.ts` compares
`credentialHex` case-insensitively, requires `credentialType` equality, and
returns `false` when either side lacks a `credentialHex`.
`designs/current-vote-display-design.md:97` records the same choice, and F-9's
"Tasked:" paragraph in `research/cv-1-findings.md` was corrected in place in the
same commit.

**Scope of this entry.** Documentation only, appended rather than edited in place.

Decision: correction discharged.
```

##### Step 9: Correct F-9's "Tasked:" paragraph in place

`.agent/plans/governance/drep-discovery/research/cv-1-findings.md` lines 220-227
end F-9's "Tasked:" paragraph. Findings files are not append-only. The claim about
the design doc is false at HEAD: commit `2ee5f74cf` corrected that sentence before
cv-2 opened, and it now lives at `:97`. Replace this text:

```
Worth recording with the marker: this
finding prescribed the correct key at review time and two governing docs then
drifted from it — `designs/current-vote-display-design.md:95` still offers
"canonical CIP-129 string including the type-byte header" as an acceptable
comparison key, and `task-plans/cv-1-code-review.md:736-738` still offers
`cip129` alone. Both are corrected under the same amendment; the code-review
file is append-only (`README.md:14`), so its correction is appended rather than
edited in place.
```

with:

```
Worth recording with the marker: this
finding prescribed the correct key at review time and two governing docs then
drifted from it. `designs/current-vote-display-design.md` was corrected by
`2ee5f74cf` before cv-2 opened — the comparator sentence now sits at `:97` and
rejects a case-sensitive canonical-CIP-129 comparison — while
`task-plans/cv-1-code-review.md:736-738` still offered `cip129` alone and is
discharged by an appended correction entry, since that file is append-only
(`README.md:14`) and is never edited in place.
```

Keep the surrounding paragraph, the line-wrap width and the trailing structure of
the file unchanged.

##### Step 10: Re-anchor AC-7 in the tracker

In `governance-drep-discovery-plan-tasks.json`, **line 1263 only** (task-140's own
AC-7 string), change `designs/current-vote-display-design.md:95` to
`designs/current-vote-display-design.md:97`. Leave line 1283 (task-173's AC, which
legitimately cites `:95`) alone. Hand-edit; never run prettier on this file.

##### Step 11: Verify

```bash
# newly created files only
node_modules/.bin/prettier --write source/renderer/app/utils/governance/isSameVoteTarget.ts tests/jest/governance/isSameVoteTarget.spec.ts

node_modules/.bin/jest --testPathPattern=isSameVoteTarget --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="voting-governance|VotingGovernancePage" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=governance --no-coverage --runInBand
node_modules/.bin/tsc --noEmit
node_modules/.bin/stylelint source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.scss
yarn lint

# the comparator is the only client-side gate, and the server net is intact
grep -n "isSameAsCurrent\|isSameVoteTarget" source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx
grep -n "same_vote" source/renderer/app/stores/VotingStore.ts
# nothing logs the comparison
grep -rn "console\.\|logger\." source/renderer/app/utils/governance/isSameVoteTarget.ts || echo "OK: comparator logs nothing"
grep -rn "task-1[0-9][0-9]" source tests storybook || echo "OK: no task ids in code"
```

Expected: the new suite green; every governance-adjacent suite still green; `tsc`
exit 0; `stylelint` exit 0 on that one file. `stylelint` over the whole tree is
already red on `CurrentVoteSummary.scss` (12 pre-existing alphabetical-order
errors, measured at HEAD) — that file belongs to task-136 and is not fixed here.
Never run `yarn prettier`, never `yarn i18n:manage`, never `yarn check:all` as a
gate. Discard mistakes with `git restore`, never `git stash`.

Commit (subject only, no body, no trailers):
`feat(gov): task-140 disable submit when the chosen delegation matches the current one`

#### Acceptance

- [ ] **AC-1** — "Submit is disabled when the selected vote equals currentVote
      after normalization." Steps 1 + 4; unit-pinned by
      `tests/jest/governance/isSameVoteTarget.spec.ts`. The rendered end-to-end
      disabled-submit flow is **not** proven here — that assertion is task-147's
      row in `VotingGovernancePage.spec.tsx`, by the slice's file-ownership map.
- [ ] **AC-2** — "Disabled state shows the appropriate tooltip hint." Step 5: a
      visible hint paragraph carrying `SAME_VOTE_HINT_ID`, referenced by the
      button's `aria-describedby`. **Partial:** the UX doc also asks that the
      button stay focusable with `aria-disabled="true"`; the button keeps a real
      `disabled` because dropping it would re-enable submission and break AC-1.
      Record as a deviation in `statusReason`.
- [ ] **AC-3** — "Existing same_vote server error remains reachable." Steps 1 + 4:
      every branch that cannot establish equality returns `false` and submits; no
      code in `VotingStore` changes; `mapOfTxErrorCodeToIntl.same_vote`
      (`VotingPowerDelegation.tsx:89`) still renders. Formally pinned by task-148.
- [ ] **AC-4** — keys on the case-stable (`credentialHex`, `credentialType`) pair,
      never on `raw` / `cip129` / `cip105`. Step 1; the cross-encoding spec case
      proves it. The uppercase-input vector is assigned to task-147 by this AC's
      own text and is deliberately absent here.
- [ ] **AC-5** — absent-hex behaviour is explicit and `credentialType` alone never
      establishes equality. Step 1's guard; the "either side carries no credential
      hex" and "key vs script sharing credential bytes" spec cases.
- [ ] **AC-6** — byte-equality preserved: the comparator returns a boolean and
      writes nothing; `chosenOption` (`:160-163`) and the `delegateVotes` payload
      are untouched. The "leaves the compared identity untouched" spec case pins
      it.
- [ ] **AC-7** — Steps 7-10: the design doc's comparator paragraph (at `:97`, not
      `:95`) records the shipped key; `cv-1-code-review.md` carries an **appended**
      discharge entry and its `:736-738` line is unedited; `cv-1-findings.md` F-9
      is corrected in place; the tracker's AC-7 anchor is re-pointed to `:97`.
      **First conjunct satisfied in part.** At HEAD `:97` still reads "must key on
      a case-stable form: the (`credentialHex`, `credentialType`) pair, **or a
      case-insensitive `cip129` comparison**" — a key AC-4 bans outright. Step 7's
      appended clause supersedes that alternative rather than deleting it, because
      this row's edit rule for the file is append-only (D-4, seam contract R-10).
      So the offer is retired in force but still present in the text. Record that
      in `statusReason`; do not report AC-7 whole.

---

## Section 4 — Confirmation-dialog identity: task-173, task-141, task-142, task-175

These four rows are built strictly in this order: **173 → 141 → 142 → 175**. 173 and
141 edit `VotingGovernancePage.tsx` / its spec; 142 and 175 edit the confirmation
dialog and its spec. Every anchor below was opened at `504b44c1a`; re-anchor by the
quoted content, not the number, because task-137/138/139/140 land first and shift
line numbers in `VotingPowerDelegation.tsx` (they do **not** touch either file this
section edits).

One commit per task, subject line only:

- task-173 — `fix(gov): task-173 derive the confirmation dialog identity via normalizeDRepIdentity`
- task-141 — `test(gov): task-141 pin the confirmation dialog to current-target props`
- task-142 — `test(gov): task-142 pin the confirmation dialog fee, hardware and passphrase sections`
- task-175 — `feat(gov): task-175 render the pre-anchor confirmation identity block`

Never run `prettier` on any file in this section: all four are pre-existing and
`prettier --check` is already red at HEAD on `VotingGovernancePage.tsx`,
`VotingPowerDelegationConfirmationDialog.tsx` and `Governance.stories.tsx`. Match the
surrounding style by hand — the snippets below are already formatted to prettier
2.1.2's 80-column output.

---

### task-173: Build the confirmation-dialog DRep identity with `normalizeDRepIdentity`

**Files touched:**

- `source/renderer/app/containers/voting/VotingGovernancePage.tsx` (edit)
- `storybook/stories/voting/Governance.stories.tsx` (edit)
- `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` (edit — adds
  a props recorder plus two cases)

**Context.**

The container currently classifies the credential by string prefix.
`source/renderer/app/containers/voting/VotingGovernancePage.tsx:72-83`, verbatim:

```tsx
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
```

This is wrong for every CIP-129 script DRep: a `drep1…` string carries its type in
the leading header byte (`0x22` key, `0x23` script), and the main-process query
service emits CIP-129 for both key-hash and script-hash credentials, so every script
DRep is labelled `'key'`.

The replacement helper already exists and is pure —
`source/renderer/app/utils/governance/normalizeDRepIdentity.ts:17`:

```ts
export function normalizeDRepIdentity(raw: string): DRepIdentity | null
```

On success it returns `{ raw, cip129, cip105, credentialHex, credentialType }` with
`raw` byte-identical to the input (`:39`, `:54`) and `credentialHex` always
lower-case (`toHex`, `:8-9`). It returns `null` — never throws, never logs — for the
two sentinels, a wrong payload length, a bad header byte, a bad checksum, or an
unknown HRP. At HEAD it has **no production call site**; this task creates the first.

The Storybook twin is at `storybook/stories/voting/Governance.stories.tsx:58-61`,
verbatim:

```ts
const toStoryDRepIdentity = (option: string): DRepIdentity | null =>
  option === 'abstain' || option === 'no_confidence'
    ? null
    : { credentialType: 'key', raw: option };
```

`VALID_DREP_ID` (`:55-56`) and both `voteOptions` sentinels (`:99-102`) are the only
values ever fed to it, and `VALID_DREP_ID` decodes cleanly, so no story changes
behaviour except that `credentialType` becomes correct.

**Locked invariants (inline).**

- **Byte-equality.** `chosenOption` is handed to `normalizeDRepIdentity` untouched —
  no `trim()`, no `toLowerCase()`, no re-encode. The string rendered in the dialog and
  the string reaching `voting.delegateVotes({ chosenOption, … })`
  (`VotingGovernancePage.tsx:95-101`) stay character-for-character identical to what
  the form produced.
- **`Abstain` / `No Confidence` are form-only sentinels**, never DRep identities. The
  explicit `chosenOption === 'abstain' || chosenOption === 'no_confidence'` guard
  stays in the source, ahead of the decode call; do not rely on the decoder returning
  `null` for them.
- **Sanitization floor.** No DRep id, no `abstain`/`no_confidence` literal and no
  bech32 string may enter any logger, analytics or electron-store payload. Add **no**
  logging on either branch, including the `null` branch — a "could not decode" warning
  is exactly the leak this floor forbids.

**Resolved judgment calls (do not revisit).**

1. **AC-5 is already satisfied at HEAD — verify, do not edit.**
   `.agent/plans/governance/drep-discovery/designs/current-vote-display-design.md:95`
   already reads "…a `drep1...` id carries its type in the leading header byte — `0x22`
   -> `'key'`, `0x23` -> `'script'`…". Confirm with the grep in the Verify step and
   move on. The comparator sentence at `:97` belongs to task-140 — do not touch it.
2. **AC-2's two halves land in two commits.** The dialog today branches on
   `drepIdentity ?` (`VotingPowerDelegationConfirmationDialog.tsx:151`), so once the
   identity can be `null` for a non-sentinel id, that id would fall into the sentinel
   label branch. The predicate fix belongs to task-175 (which owns that file, step 3
   below). At **this** commit assert only what is true here: the identity prop is
   `null` and the submitted string is still byte-equal. The rendering half is asserted
   in task-175's spec. Do not edit the dialog in this task.
3. **The props recorder is created here**, in `VotingGovernancePage.spec.tsx`, because
   `credentialType` is not rendered anywhere at this commit. task-141 reuses it.
4. **Test vectors** (checksum-verified; do not synthesize others):
   - script CIP-129 (`0x23`): `drep1ydwykw3frpmsda0y60ptrgyl3e7kck628y5pwph4unfu9vg6sn5zd`
   - legacy 28-byte `drep1…` (accepted by `Cardano.DRepID.isValid`, rejected by
     `normalizeDRepIdentity`): `drep1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg4v077e`
   Both were measured: `Cardano.DRepID.isValid` returns `true` for each, so both reach
   the form gate at `VotingPowerDelegation.tsx:133`.

#### Step 1: Import the helper in the container

In `source/renderer/app/containers/voting/VotingGovernancePage.tsx`, immediately after
the existing import at `:11`:

```ts
import { pickDelegationFormNavigationState } from '../governance/delegationFormState';
```

add:

```ts
import { normalizeDRepIdentity } from '../../utils/governance/normalizeDRepIdentity';
```

Leave the `import type { DRepIdentity } …` line at `:12` in place — the annotation
below still uses it.

#### Step 2: Replace the derivation

Replace `VotingGovernancePage.tsx:72-83` (the block quoted in Context) with exactly:

```tsx
          // Sentinels carry no identity; a drep target is decoded for display
          // only — the rendered and submitted string stays chosenOption itself,
          // untouched.
          const drepIdentity: DRepIdentity | null =
            chosenOption === 'abstain' || chosenOption === 'no_confidence'
              ? null
              : normalizeDRepIdentity(chosenOption);
```

Nothing else in `renderConfirmationDialog` changes. The prop list at `:85-111` stays
exactly as it is.

#### Step 3: Replace the Storybook helper

In `storybook/stories/voting/Governance.stories.tsx`, after the existing import at
`:52` (`import { generateWallet } from '../_support/utils';`) add:

```ts
import { normalizeDRepIdentity } from '../../../source/renderer/app/utils/governance/normalizeDRepIdentity';
```

Then replace `:58-61` with:

```ts
const toStoryDRepIdentity = (option: string): DRepIdentity | null =>
  option === 'abstain' || option === 'no_confidence'
    ? null
    : normalizeDRepIdentity(option);
```

Do not rename the helper and do not change its call sites (`:282`, `:430`, `:467`).

#### Step 4: Add the dialog-props recorder to the container spec

In `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`, insert the
following immediately **after** the `ItemsDropdown` mock that ends at `:44`, before
`const VALID_DREP_ID` at `:46`. Leave the `WalletsDropdown` mock at `:34-38`
untouched.

```tsx
const mockDialogProps: Array<Record<string, unknown>> = [];

// The recorder wraps the real dialog, so the rendered DOM the other flow tests
// assert on is unchanged; only the prop object is captured.
jest.mock(
  '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog',
  () => {
    const actual = jest.requireActual(
      '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog'
    );
    const { createElement } = jest.requireActual('react');
    return {
      __esModule: true,
      default: function DialogPropsRecorder(props: Record<string, unknown>) {
        mockDialogProps.push(props);
        return createElement(actual.default, props);
      },
    };
  }
);
```

The `mock` name prefix is required — babel's jest hoister rejects any other
out-of-scope identifier inside a `jest.mock` factory. Use `jest.requireActual`, never
a bare `require`, or `@typescript-eslint/no-var-requires` fails `yarn lint`.

Then add a shared helper at module scope, immediately after `renderFlow` ends at
`:163`:

```tsx
const openConfirmation = async (drepId: string) => {
  const flow = renderFlow([
    {
      pathname: ROUTES.VOTING.GOVERNANCE,
      state: {
        selectedDRepId: drepId,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      },
    },
  ]);
  fireEvent.click(screen.getByRole('button', { name: 'Submit' }));
  await screen.findByText('Confirm Transaction');
  return flow;
};
```

#### Step 5: Add the two cases

Append at the end of the file (after the hardware-wallet `describe` that closes at
`:391`):

```tsx
describe('Confirmation dialog identity derivation', () => {
  const SCRIPT_DREP_ID =
    'drep1ydwykw3frpmsda0y60ptrgyl3e7kck628y5pwph4unfu9vg6sn5zd';
  const LEGACY_DREP_ID =
    'drep1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg4v077e';

  beforeEach(() => {
    mockDialogProps.length = 0;
  });

  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('classifies a CIP-129 script DRep by its header byte', async () => {
    await openConfirmation(SCRIPT_DREP_ID);

    const props = mockDialogProps[mockDialogProps.length - 1];
    expect(props.drepIdentity).toEqual(
      expect.objectContaining({
        credentialType: 'script',
        raw: SCRIPT_DREP_ID,
      })
    );
    expect(props.chosenOption).toBe(SCRIPT_DREP_ID);
  });

  it('passes a null identity for an id the decoder rejects and still submits it byte-for-byte', async () => {
    const { stores } = await openConfirmation(LEGACY_DREP_ID);

    expect(mockDialogProps[mockDialogProps.length - 1].drepIdentity).toBeNull();
    expect(stores.voting.initializeVPDelegationTx).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: LEGACY_DREP_ID })
    );
  });
});
```

Do not modify the existing byte-equality case at `:261-301` or the hardware-wallet
`describe` at `:304-391`; both must pass unchanged.

#### Step 6: Verify

Run from the worktree root:

```bash
node_modules/.bin/tsc --noEmit
node_modules/.bin/jest --testPathPattern=VotingGovernancePage --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=governance-sanitization --no-coverage --runInBand
yarn lint
```

Expected: `tsc` exit 0; the container spec green including the two new cases;
`governance-sanitization` green at 24 tests; lint exit 0 (warnings only).

Then these greps — **no output is the pass condition** (grep exits 1 when it matches
nothing, which is expected here):

```bash
grep -n "startsWith('drep_script')" source/renderer/app/containers/voting/VotingGovernancePage.tsx storybook/stories/voting/Governance.stories.tsx
grep -nE "logger|console\.|Logger" source/renderer/app/containers/voting/VotingGovernancePage.tsx
```

And this one **must** print a line (AC-5, already discharged — do not edit the file):

```bash
grep -n "leading header byte" .agent/plans/governance/drep-discovery/designs/current-vote-display-design.md
```

Do not run prettier on any file in this task.

#### Acceptance

- [ ] AC-1 — identity comes from `normalizeDRepIdentity(chosenOption)`; the
      `startsWith('drep_script')` heuristic is gone (Step 2 + the grep in Step 6); the
      sentinel branch still yields `null` (explicit guard retained in Step 2).
- [ ] AC-2 — **satisfied in part at this commit.** "Submission is unaffected; nothing
      is re-encoded, trimmed, or dropped" is proved by the legacy-id case in Step 5.
      "The dialog still renders the raw string verbatim" is discharged by task-175
      Step 3, which owns the dialog's branch predicate; record that in the tracker
      `statusReason` for this row.
- [ ] AC-3 — `toStoryDRepIdentity` calls the same helper (Step 3).
- [ ] AC-4 — the script-header case asserts `credentialType: 'script'`; the existing
      row-select → confirmation → `delegateVotes` case at `:261-301` still passes
      untouched (Step 6 jest run).
- [ ] AC-5 — already satisfied at `current-vote-display-design.md:95`; verified by
      grep, no edit made.
- [ ] AC-6 — no logging added on either branch (Step 6 grep); the task-111 spy suite
      re-run green.

---

### task-141: Keep the confirmation dialog current-target only

**Files touched:**

- `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` (edit —
  appends one `describe`)

**No production file changes.** This is a verification row: the container already
passes no historical prop, and the deliverable is an executable pin that stops one
from being added.

**Context.**

`VotingGovernancePage.tsx:85-111` hands the dialog exactly ten props, in this order:
`chosenOption`, `drepIdentity`, `fees`, `hwDeviceStatus`, `isTrezor`, `onClose`,
`onExternalLinkClick`, `onSubmit`, `redirectToWallet`, `selectedWallet`. That matches
`VotingPowerDelegationConfirmationDialogProps` at
`VotingPowerDelegationConfirmationDialog.tsx:54-70` (which additionally declares
`intl`, supplied by `injectIntl`, not by the container).

The deferred Previous → New comparison reserves two message ids —
`voting.governance.confirmationDialog.previousVote` and `.newVote` — which exist in
**no** source file and in **neither** catalog at HEAD
(`VotingPowerDelegationConfirmationDialog.messages.ts:3-46` defines eight keys;
`en-US.json:946-953` lists eight). They stay unwired in v1.

**Locked invariants (inline).**

- **No second delegation backend.** The dialog's contract is the selected target only:
  no historical vote-target prop, no store-backed comparison state, no
  `GovernanceStore` read from the dialog or from `VotingStore`.

**Resolved judgment calls (do not revisit).**

1. The pin reuses `mockDialogProps` and `openConfirmation` created by task-173 —
   do not add a second mock of the same module and do not rewrite task-173's
   `describe`.
2. Assert the prop **key set**, not a deep-equality of prop values (`onSubmit` and
   friends are freshly-created closures on every render).

#### Step 1: Confirm there is nothing to change in production

```bash
grep -nE "previousVote|newVote|previousDRepId|historicalVote" source/renderer/app storybook -r
```

Expected: no output. If it prints anything, stop and report — the row's premise has
changed.

#### Step 2: Append the pin

Append at the end of `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`:

```tsx
describe('Confirmation dialog prop contract', () => {
  const EXPECTED_DIALOG_PROPS = [
    'chosenOption',
    'drepIdentity',
    'fees',
    'hwDeviceStatus',
    'isTrezor',
    'onClose',
    'onExternalLinkClick',
    'onSubmit',
    'redirectToWallet',
    'selectedWallet',
  ];

  beforeEach(() => {
    mockDialogProps.length = 0;
  });

  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('hands the dialog exactly the current-target prop set', async () => {
    await openConfirmation(VALID_DREP_ID);

    const props = mockDialogProps[mockDialogProps.length - 1];
    expect(Object.keys(props).sort()).toEqual(
      [...EXPECTED_DIALOG_PROPS].sort()
    );
  });

  it('passes no historical vote-target prop', async () => {
    await openConfirmation(VALID_DREP_ID);

    const props = mockDialogProps[mockDialogProps.length - 1];
    ['previousVote', 'newVote', 'previousDRepId', 'currentVote'].forEach(
      (key) => {
        expect(props).not.toHaveProperty(key);
      }
    );
  });
});
```

#### Step 3: Verify

```bash
node_modules/.bin/tsc --noEmit
node_modules/.bin/jest --testPathPattern=VotingGovernancePage --no-coverage --runInBand
yarn lint
git diff --stat -- source/renderer/app/containers/voting/VotingGovernancePage.tsx
```

The `git diff --stat` must print nothing: this task changes no production file.

#### Acceptance

- [ ] AC-1 — the ten-key pin plus the four negative-key assertions prove no historical
      vote-target prop is required or passed (Step 2).
- [ ] AC-2 — backward compatibility is proved by the pre-existing flow cases
      (`:261-301` software, `:304-391` hardware) still passing unchanged in the Step 3
      run; the dialog receives only the selected current target.

---

### task-142: Verify the confirmation dialog is unchanged for current-vote display

**Files touched:**

- `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx`
  (edit — appends one `describe`)

**No production file changes.**

**Context.**

The sections this row pins, all in
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx`:

- fee rows `:174-177`:

```tsx
        <p className={styles.paragraphTitle}>
          {intl.formatMessage(messages.fee)}
        </p>
        <p className={styles.paragraphValue}>{formattedWalletAmount(fees)}</p>
```

- the hardware/passphrase branch `:179-202`:

```tsx
        {selectedWallet.isHardwareWallet ? (
          <HardwareWalletStatus
            hwDeviceStatus={hwDeviceStatus}
            walletName={selectedWallet.name}
            isTrezor={isTrezor}
            onExternalLinkClick={onExternalLinkClick}
          />
        ) : (
          <Input
            …
            label={intl.formatMessage(messages.password)}
            skin={InputSkin}
          />
        )}
```

Rendered strings under the en-US catalog the spec already loads:
`messages.fee` → `Transaction fee` (`en-US.json:950`), `messages.password` →
`Spending password` (`:951`), `messages.title` → `Confirm Transaction` (`:952`).
`formattedWalletAmount(new BigNumber('0.174257'))` → `0.174257 ADA` (six decimals plus
the currency suffix, `utils/formatters.ts:18-43`, `DECIMAL_PLACES_IN_ADA = 6`). The
react-polymorph `InputSkin` renders the label as a real `<label>` element, so
`getByText` finds it.

`renderDialog` (spec `:30-54`) already supplies every prop with a software-wallet
default and merges an `overrides` object — reuse it, do not write a second harness.

**Locked invariants (inline).**

- These pins must **survive task-175**, which is chartered to grow the identity block
  at `:151-172` of the same file. Therefore: **no whole-dialog snapshot, and no
  element-count assertion** (no `getAllBy…().length`, no "exactly N paragraphs").
  Query by role, by label text, and by exact value strings only.
- AC-3's "lines ~L118-L127" citation is wrong at every commit in this repo's history;
  the HW status section is identified semantically as the
  `selectedWallet.isHardwareWallet ?` branch (`:179-185`).
- The "unchanged" claim is scoped to the fee rows, the HW-status branch and the
  passphrase input. The identity block is explicitly **out of scope** here.

**Resolved judgment calls (do not revisit).**

1. Do not assert on `getByLabelText` for the passphrase field — the `Input` is
   rendered without an `id`, so the `<label>` has no `htmlFor` binding. Use
   `screen.getByText('Spending password')` for the label plus
   `document.querySelector('input[type="password"]')` for the control (the file's
   existing precedent at `:153`).
2. For the HW branch reuse `HwDeviceStatuses.VERIFYING_TRANSACTION`, whose copy
   (`Confirm the transaction using the "HW Test Wallet" device`) is already proven to
   render by the case at `:122-124`.

#### Step 1: Append the pin

Append at the end of
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx`:

```tsx
describe('VotingPowerDelegationConfirmationDialog — fee, hardware and passphrase sections', () => {
  afterEach(cleanup);

  it('renders the fee row with the formatted amount', () => {
    renderDialog();

    expect(screen.getByText('Transaction fee')).toBeInTheDocument();
    expect(screen.getByText('0.174257 ADA')).toBeInTheDocument();
  });

  it('renders the labelled passphrase input for a software wallet', () => {
    renderDialog();

    expect(screen.getByText('Spending password')).toBeInTheDocument();
    expect(document.querySelector('input[type="password"]')).not.toBeNull();
    expect(
      screen.queryByText('Confirm the transaction using the "HW Test Wallet" device')
    ).not.toBeInTheDocument();
  });

  it('renders the device status instead of the passphrase input for a hardware wallet', () => {
    renderDialog({
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      selectedWallet: hardwareWallet,
    });

    expect(
      screen.getByText('Confirm the transaction using the "HW Test Wallet" device')
    ).toBeInTheDocument();
    expect(document.querySelector('input[type="password"]')).toBeNull();
    expect(screen.queryByText('Spending password')).not.toBeInTheDocument();
  });

  it('keeps the dialog chrome and introduces no comparison rows', () => {
    renderDialog();

    expect(screen.getByText('Confirm Transaction')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Cancel' })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeInTheDocument();
    expect(screen.queryByText(/previous vote/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/new vote/i)).not.toBeInTheDocument();
    expect(messages).not.toHaveProperty('previousVote');
    expect(messages).not.toHaveProperty('newVote');
  });
});
```

This needs one added import at the top of the spec file, next to the existing
component import at `:13`:

```tsx
import { messages } from './VotingPowerDelegationConfirmationDialog.messages';
```

`hardwareWallet` (`:24-28`) and `HwDeviceStatuses` (`:12`) are already in scope.

#### Step 2: Verify

```bash
node_modules/.bin/tsc --noEmit
node_modules/.bin/jest --testPathPattern=VotingPowerDelegationConfirmationDialog --no-coverage --runInBand
yarn lint
git diff --stat -- source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx
```

The `git diff --stat` must print nothing. The jest run must show the four new cases
plus every pre-existing case in the file green.

#### Acceptance

- [ ] AC-1 — today's layout is pinned by the fee, passphrase, HW and chrome cases;
      scoped to exclude the identity block, which task-175 owns.
- [ ] AC-2 — no historical comparison rows: DOM absence plus the descriptor-object
      assertions for the reserved `previousVote` / `newVote` keys.
- [ ] AC-3 — the HW status section is pinned semantically (the
      `selectedWallet.isHardwareWallet ?` branch at `:179-185`), and the empty
      `git diff --stat` proves the file is untouched. The AC's "~L118-L127" line
      citation does not correspond to this file at any commit; record the re-anchoring
      in the tracker `statusReason`.

---

### task-175: Render the pre-anchor §7 confirmation identity block

**Files touched:**

- `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.messages.ts`
  (edit — two new descriptors)
- `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx`
  (edit — the identity block only)
- `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx`
  (edit — appends one `describe`)

**Context.**

The block to grow is `VotingPowerDelegationConfirmationDialog.tsx:150-172`, verbatim:

```tsx
      <div className={styles.content}>
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

Target template (`designs/shared-design-tokens.md:115-120`), four parts in this order:
CIP-129 primary, CIP-105 when derivable, the signed-payload line carrying the
credential hex, and the on-chain source label.

`DRepSourceLabel` already exists at
`source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx`; its variant
union is `'on-chain' | 'on-chain-anchor-reference'` (`:18`) and it returns `null` for
anything else (`:32`), so the string must be exactly `'on-chain'`. Its `'on-chain'`
copy is `!!!On-chain` in both catalogs (`en-US.json:354`, `ja-JP.json:354`). The
import specifier from this directory is `../../governance/_shared/DRepSourceLabel`
(same as `CurrentVoteSummary.tsx:6`).

Existing style classes are enough: `.paragraphTitle`, `.paragraphValue` and
`.drepIdValue` (monospace, `word-break: break-all`) are all defined in
`VotingPowerDelegationConfirmationDialog.scss:8-27`.

**Locked invariants (inline).**

- **Byte-equality.** The primary line's text must equal `chosenOption` and the
  `dRepId` handed to `voting.delegateVotes` character-for-character, in every branch.
  `cip105` and `credentialHex` are display-only derivations; when
  `normalizeDRepIdentity` returned `null`, only the primary line renders, verbatim —
  **no representation is ever fabricated**. All three rendered representations must
  decode to the same 28 credential bytes, and the hex must equal what the hardware
  path sends (`shelleyLedger.ts:71-83` / `shelleyTrezor.ts:71-83` assign
  `Cardano.DRepID.toCredential(...).hash` to `keyHashHex` / `scriptHashHex`).
- **Sentinels are form-only.** `abstain` / `no_confidence` keep the existing `Vote`
  label branch with **no identity block at all**.
- **Pre-anchor only.** No `givenName`, no "Name: Verified off-chain content", no
  anchor-derived content of any kind. Those belong to anchor-2 and extend this block
  later; never render an unverified name on this surface.
- **Sanitization floor.** The CIP-105 string and the credential hex exist in the DOM
  only. Add no logging, no analytics call and no store write.
- **Preliminary copy.** Both new `defaultMessage` values keep the leading `!!!`.

**Resolved judgment calls (do not revisit).**

1. **The branch predicate becomes sentinel-based.** Keeping `drepIdentity ?` would
   send a decodable-by-the-form-but-not-by-the-decoder id (the legacy 28-byte
   `drep1…` form) into the sentinel label branch, which contradicts the byte-equality
   rule above and task-173 AC-2. Introduce `isSentinelVote` and render
   `{drepIdentity?.raw ?? chosenOption}` as the primary value. The `:` branch body
   (today `:163-172`) is copied through **unchanged**.
2. **No SCSS change.** The source label is wrapped in the existing
   `styles.paragraphValue` paragraph, so no new class is minted, no
   `*.scss.d.ts` regeneration is needed, and plain `node_modules/.bin/tsc --noEmit`
   is a sufficient typecheck.
3. **The signed-payload string is the compact JSON form**
   `{"vote":{"type":"drep","id":"<hex>"}}` — no spaces. The design template renders it
   with spaces for legibility; the compact form is what the spec parses with
   `JSON.parse`.
4. **This task does not touch the catalogs.** `en-US.json` / `ja-JP.json` /
   `translations/messages.json` are seeded by task-146. Until then react-intl falls
   back to `defaultMessage`, so the rendered text — and therefore every assertion
   below — is identical before and after task-146.
5. **Vectors** (checksum-verified, all decoded during planning; do not re-case, do not
   re-derive):

   | | CIP-129 | CIP-105 | credential hex |
   |---|---|---|---|
   | key | `drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy` | `drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l` | `a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c` |
   | script | `drep1ydwykw3frpmsda0y60ptrgyl3e7kck628y5pwph4unfu9vg6sn5zd` | `drep_script1t39n52gcwur0texnc2c6p8uw04k9kj3e9qtsda0y60ptzae75nh` | `5c4b3a29187706f5e4d3c2b1a09f8e7d6c5b4a39281706f5e4d3c2b1` |

   Legacy (decoder rejects, form accepts):
   `drep1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg4v077e`.

#### Step 1: Add the two message descriptors

In `VotingPowerDelegationConfirmationDialog.messages.ts`, insert immediately after the
`drepId` entry that ends at `:19`:

```ts
  drepIdCip105: {
    id: 'voting.governance.confirmationDialog.drepIdCip105',
    defaultMessage: '!!!CIP-105 DRep ID',
    description:
      'Label above the CIP-105 DRep ID in the delegation confirmation dialog',
  },
  signedPayload: {
    id: 'voting.governance.confirmationDialog.signedPayload',
    defaultMessage: '!!!Signed payload',
    description:
      'Label above the signed payload vote id in the delegation confirmation dialog',
  },
```

Do not re-word the existing `drepId` value (`!!!DRep ID`); it becomes the CIP-129
line's label as-is.

#### Step 2: Import the source label

In `VotingPowerDelegationConfirmationDialog.tsx`, after the `HardwareWalletStatus`
import at `:12`:

```tsx
import DRepSourceLabel from '../../governance/_shared/DRepSourceLabel';
```

#### Step 3: Add the sentinel predicate

Insert directly above `const confirmButtonLabel =` (`:116`):

```tsx
  // Keyed on the vote kind, not on a successful decode: an id the decoder
  // rejects still renders verbatim rather than as a vote label.
  const isSentinelVote =
    chosenOption === 'abstain' || chosenOption === 'no_confidence';
```

#### Step 4: Replace the identity block

Replace `:151-172` (from `{drepIdentity ? (` through the closing `)}` of the ternary)
with exactly:

```tsx
        {!isSentinelVote ? (
          <>
            <p className={styles.paragraphTitle}>
              {intl.formatMessage(messages.drepId)}
            </p>
            <p className={styles.paragraphValue}>
              {/* Rendered untouched: must stay byte-equal to chosenOption and
                  the delegateVotes dRepId. Name slot is reserved for anchor-2;
                  unverified names never render here. */}
              <code className={styles.drepIdValue}>
                {drepIdentity?.raw ?? chosenOption}
              </code>
            </p>
            {drepIdentity?.cip105 && (
              <>
                <p className={styles.paragraphTitle}>
                  {intl.formatMessage(messages.drepIdCip105)}
                </p>
                <p className={styles.paragraphValue}>
                  <code className={styles.drepIdValue}>
                    {drepIdentity.cip105}
                  </code>
                </p>
              </>
            )}
            {drepIdentity?.credentialHex && (
              <>
                <p className={styles.paragraphTitle}>
                  {intl.formatMessage(messages.signedPayload)}
                </p>
                <p className={styles.paragraphValue}>
                  <code className={styles.drepIdValue}>
                    {`{"vote":{"type":"drep","id":"${drepIdentity.credentialHex}"}}`}
                  </code>
                </p>
              </>
            )}
            {drepIdentity && (
              <p className={styles.paragraphValue}>
                <DRepSourceLabel source="on-chain" />
              </p>
            )}
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

Everything below — the fee rows, the `selectedWallet.isHardwareWallet ?` branch, the
passphrase `Input`, the error paragraph, the `actions` array — is untouched.

#### Step 5: Append the spec

Add these imports to
`VotingPowerDelegationConfirmationDialog.spec.tsx`, after the existing component
import at `:13`:

```tsx
import { bech32 } from 'bech32';
import { Cardano } from '@cardano-sdk/core';
import { normalizeDRepIdentity } from '../../../utils/governance/normalizeDRepIdentity';
```

Then append at the end of the file:

```tsx
describe('VotingPowerDelegationConfirmationDialog — identity block', () => {
  const KEY_CIP129 =
    'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
  const KEY_CIP105 =
    'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
  const KEY_CREDENTIAL_HEX =
    'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
  const SCRIPT_CIP129 =
    'drep1ydwykw3frpmsda0y60ptrgyl3e7kck628y5pwph4unfu9vg6sn5zd';
  const SCRIPT_CIP105 =
    'drep_script1t39n52gcwur0texnc2c6p8uw04k9kj3e9qtsda0y60ptzae75nh';
  const SCRIPT_CREDENTIAL_HEX =
    '5c4b3a29187706f5e4d3c2b1a09f8e7d6c5b4a39281706f5e4d3c2b1';
  const LEGACY_DREP_ID =
    'drep1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg4v077e';

  // CIP-129 carries a one-byte credential-type header ahead of the credential;
  // CIP-105 carries the bare credential.
  const credentialHexOf = (id: string): string => {
    const decoded = bech32.decode(id);
    const bytes = bech32.fromWords(decoded.words);
    const credential = decoded.prefix === 'drep' ? bytes.slice(1) : bytes;
    return credential.map((b) => b.toString(16).padStart(2, '0')).join('');
  };

  const renderIdentity = (drepId: string) =>
    renderDialog({
      chosenOption: drepId,
      drepIdentity: normalizeDRepIdentity(drepId),
    });

  afterEach(cleanup);

  it('renders all four parts for a key DRep', () => {
    renderIdentity(KEY_CIP129);

    expect(screen.getByText('!!!DRep ID')).toBeInTheDocument();
    expect(screen.getByText(KEY_CIP129).textContent).toBe(KEY_CIP129);
    expect(screen.getByText('!!!CIP-105 DRep ID')).toBeInTheDocument();
    expect(screen.getByText(KEY_CIP105).textContent).toBe(KEY_CIP105);
    expect(screen.getByText('!!!Signed payload')).toBeInTheDocument();
    expect(
      screen.getByText(`{"vote":{"type":"drep","id":"${KEY_CREDENTIAL_HEX}"}}`)
    ).toBeInTheDocument();
    expect(screen.getByText('!!!On-chain')).toBeInTheDocument();
  });

  it('renders the script CIP-105 form for a script DRep', () => {
    renderIdentity(SCRIPT_CIP129);

    expect(screen.getByText(SCRIPT_CIP129).textContent).toBe(SCRIPT_CIP129);
    expect(screen.getByText(SCRIPT_CIP105).textContent).toBe(SCRIPT_CIP105);
    expect(
      screen.getByText(
        `{"vote":{"type":"drep","id":"${SCRIPT_CREDENTIAL_HEX}"}}`
      )
    ).toBeInTheDocument();
  });

  it.each([
    [KEY_CIP129, KEY_CIP105],
    [SCRIPT_CIP129, SCRIPT_CIP105],
  ])(
    'renders three representations of one credential for %s',
    (cip129, cip105) => {
      renderIdentity(cip129);

      const payload = JSON.parse(screen.getByText(/"vote"/).textContent);
      expect(payload.vote.type).toBe('drep');
      expect(payload.vote.id).toHaveLength(56);
      expect(credentialHexOf(screen.getByText(cip129).textContent)).toBe(
        payload.vote.id
      );
      expect(credentialHexOf(screen.getByText(cip105).textContent)).toBe(
        payload.vote.id
      );
    }
  );

  it.each([
    [KEY_CIP129, KEY_CREDENTIAL_HEX, Cardano.CredentialType.KeyHash],
    [SCRIPT_CIP129, SCRIPT_CREDENTIAL_HEX, Cardano.CredentialType.ScriptHash],
  ])(
    'renders the same credential hex the hardware path sends for %s',
    (cip129, expectedHex, expectedType) => {
      // Both hardware mappers hand the device this hash as keyHashHex /
      // scriptHashHex, while the dialog shows bech32 — the two are only
      // comparable through the credential.
      const { hash, type } = Cardano.DRepID.toCredential(Cardano.DRepID(cip129));
      expect(hash).toBe(expectedHex);
      expect(type).toBe(expectedType);

      renderIdentity(cip129);
      expect(
        screen.getByText(`{"vote":{"type":"drep","id":"${expectedHex}"}}`)
      ).toBeInTheDocument();
    }
  );

  it('renders only the verbatim primary line when the decoder rejects the id', () => {
    expect(normalizeDRepIdentity(LEGACY_DREP_ID)).toBeNull();
    renderIdentity(LEGACY_DREP_ID);

    expect(screen.getByText('!!!DRep ID')).toBeInTheDocument();
    expect(screen.getByText(LEGACY_DREP_ID).textContent).toBe(LEGACY_DREP_ID);
    expect(screen.queryByText('!!!CIP-105 DRep ID')).not.toBeInTheDocument();
    expect(screen.queryByText('!!!Signed payload')).not.toBeInTheDocument();
    expect(screen.queryByText('!!!On-chain')).not.toBeInTheDocument();
    expect(screen.queryByText('Vote')).not.toBeInTheDocument();
  });

  it.each(['abstain', 'no_confidence'])(
    'renders no identity block for the %s sentinel',
    (option) => {
      renderDialog({ chosenOption: option, drepIdentity: null });

      expect(screen.getByText('Vote')).toBeInTheDocument();
      expect(screen.queryByText('!!!DRep ID')).not.toBeInTheDocument();
      expect(screen.queryByText('!!!CIP-105 DRep ID')).not.toBeInTheDocument();
      expect(screen.queryByText('!!!Signed payload')).not.toBeInTheDocument();
      expect(screen.queryByText('!!!On-chain')).not.toBeInTheDocument();
    }
  );
});
```

Two notes for whoever runs this: `getByText` matches an element on its **direct** text
children only, so each `<code>` is matched uniquely and no ancestor collides;
`screen.getByText(/"vote"/)` therefore resolves to the payload `<code>` alone.

#### Step 6: Verify

```bash
node_modules/.bin/tsc --noEmit
node_modules/.bin/jest --testPathPattern=VotingPowerDelegationConfirmationDialog --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=VotingGovernancePage --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=governance-sanitization --no-coverage --runInBand
yarn lint
git diff --stat -- source/renderer/app/i18n/locales translations
```

Gates:

- task-142's four cases **and** every pre-existing dialog case must pass unchanged.
  If one of them fails, the production edit above is wrong — fix the edit, never
  weaken the pin.
- The container spec's byte-equality cases (`:261-301`, `:319-356`) must still pass:
  after this change they resolve the CIP-129 id in the primary `<code>` and the
  all-zero credential hex in the payload line, which are different strings, so no
  ambiguous match appears.
- `governance-sanitization` green at 24 tests.
- The last `git diff --stat` must print nothing — this task edits no catalog.

Then confirm nothing was logged:

```bash
grep -nE "logger|console\.|analytics" source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx
```

No output is the pass condition. Do not run prettier on any of these files.

#### Acceptance

- [ ] AC-1 — four parts render in template order, both bech32 forms full and
      monospaced (`.drepIdValue`), byte-untouched, with
      `<DRepSourceLabel source="on-chain" />` (Step 4 + the first spec case).
- [ ] AC-2 — secondary forms come only from the identity task-173 supplies; the
      legacy-id case proves only the primary line renders and nothing is fabricated.
      This also discharges the rendering half of task-173 AC-2.
- [ ] AC-3 — decode-equality across CIP-129 / CIP-105 / hex, plus the
      `Cardano.DRepID.toCredential` case pinning the hardware hex, plus the container
      spec's `delegateVotes` byte-equality flow (re-run in Step 6).
- [ ] AC-4 — sentinel case asserts no identity block; the two new descriptors carry
      `!!!` in their `defaultMessage`; the existing `confirmationDialog.drepId` label
      carries the CIP-129 line and is not re-worded. **The catalog half (both locales
      populated, `yarn i18n:manage` idempotent) is carried by task-146**, which runs
      the mint procedure over these descriptors — record that in this row's tracker
      `statusReason`.
- [ ] AC-5 — new renderings are DOM-only; Step 6 grep is empty and the task-111 spy
      suite is green.

---

## Storybook wrapper and knob (task-144, task-145)

Both tasks are Storybook-only. task-144 creates the second file under
`storybook/stories/governance/_utils/` — `GovernanceWrapper.tsx`; task-143
created `fixtures.ts` — and task-145 rewires
`storybook/stories/voting/Governance.stories.tsx`. Neither touches
`source/`, `storybook/stories/index.ts`, or
`storybook/stories/governance/CurrentVoteSummary.stories.tsx` (that file belongs
to task-136).

Line anchors below were read at branch `feat/drep-discovery`, commit
`504b44c1a`. task-143 runs first in the slice, so its anchors are exact;
task-144 and task-145 run late, after other tasks have already edited
`VotingPowerDelegation.tsx` and `Governance.stories.tsx` — **re-anchor by the
quoted content, not by the number.**

---

### task-144: Key-based remount in `GovernanceWrapper`

**Files created:**

- `storybook/stories/governance/_utils/GovernanceWrapper.tsx` (new file)

**Files touched:** none. `fixtures.ts` is already complete from task-143 — do
not edit it here.

#### Context (verified anchors, current code quoted)

The task title says "extend"; there is nothing to extend. `GovernanceWrapper`
does not exist anywhere in the repo — this task creates it.

The default-export convention for a `_utils` wrapper comes from
`storybook/stories/wallets/_utils/WalletsWrapper.tsx:8`:

```tsx
export default function (story: any, context: any) {
```

That file is a **Storybook decorator** and its `(story, context)` shape cannot
hand fixtures to a story body, so `GovernanceWrapper` keeps the default export
but is a **render-prop component** instead of a decorator.

The state that must be cleared on a knob change lives in
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx:115`
— a lazy `useState` initializer that runs only on mount:

```tsx
  const [state, setState] = useState<State>(() => {
    if (!initialFormState) return initialState;
```

Because that initializer does not re-run on prop changes, changing the knob
without remounting would leave the previous option's form state (selected
wallet, vote type, typed DRep id) on screen. Changing the React key is what
forces the remount.

#### Locked invariants this change must not break (stated in full)

- **The key is the option id verbatim** — no composite key, no array index, no
  hash. `key={option}`.
- **The wrapper does not read the knob.** The story reads it once via
  `useCurrentVoteKnob()` and passes it down, so one knob read drives both the
  remount key and any other per-story wiring.
- **No local `IntlProvider` and no per-locale story variants.**
  `storybook/preview.tsx:8` applies a global `StoryWrapper` that owns the single
  `IntlProvider` and the English/Japanese toggle; a local provider shadows it.
  This wrapper adds no provider of any kind.
- **Fixtures stay per-render.** The wrapper calls the factories inside its own
  render; it caches nothing in a module variable, a ref, or state.

#### Resolved judgment calls (do not revisit)

- It is a component, not a decorator, and it is **default-exported**.
- The keyed node is a `React.Fragment`. React compares the key of a single
  rendered child, so a changed key on the returned fragment unmounts and
  remounts the whole subtree — that is the entire mechanism.
- `drepIndex` is exposed as `ReadonlyMap<…>` because that is the prop type
  `VotingPowerDelegation` declares; `makeDRepIndex`'s `Map` satisfies it.

#### Step 1: Create `storybook/stories/governance/_utils/GovernanceWrapper.tsx`

Exact file content (eslint-clean and `prettier@2.1.2`-clean as written):

```tsx
import React from 'react';
import type Wallet from '../../../../source/renderer/app/domains/Wallet';
import type { AppDRepDirectoryEntry } from '../../../../source/renderer/app/stores/GovernanceStore';
import { makeDRepIndex, makeGovernanceWallets } from './fixtures';
import type { CurrentVoteOption } from './fixtures';

export type GovernanceStoryFixtures = {
  wallets: Wallet[];
  drepIndex: ReadonlyMap<string, AppDRepDirectoryEntry>;
};

type Props = {
  option: CurrentVoteOption;
  children: (fixtures: GovernanceStoryFixtures) => React.ReactNode;
};

// The React key is the option id verbatim: changing the knob must remount the
// subtree so VotingPowerDelegation drops the form state it holds locally.
export default function GovernanceWrapper({ option, children }: Props) {
  return (
    <React.Fragment key={option}>
      {children({
        wallets: makeGovernanceWallets(option),
        drepIndex: makeDRepIndex(option),
      })}
    </React.Fragment>
  );
}
```

#### Step 2: Format, typecheck, lint

```bash
node_modules/.bin/prettier --write storybook/stories/governance/_utils/GovernanceWrapper.tsx
node_modules/.bin/tsc --noEmit
yarn lint
```

`tsc --noEmit` must exit 0. `yarn lint` must exit 0; this file adds one
`no-unused-vars` **warning** for the `fixtures` parameter name inside the
`children` function type — that warning class already exists throughout the
repo. Do not rename the parameter to work around it and do not add an
`eslint-disable` comment.

Suggested commit subject: `feat(gov): task-144 add key-based GovernanceWrapper remount for storybook`

#### Acceptance

- [ ] AC-1 "Wrapper exposes a key derived from the selected current-vote option
      id" — `key={option}` on the returned `React.Fragment`, with `option`
      typed as the five-value `CurrentVoteOption`.
- [ ] AC-2 "Children consume the key so `VotingPowerDelegation` remounts on
      knob change" — **split, like the other browser-dependent criteria.**
      *Structural half, satisfied here:* children are rendered inside the keyed
      fragment, so a knob change replaces the subtree and re-runs the `useState`
      initializer at `VotingPowerDelegation.tsx:115`; proved by the file's shape
      plus `tsc --noEmit`. *Observed half:* the remount itself (type a DRep id,
      switch the knob, the field is blank again) is task-145's Step 8 and needs a
      browser, which this container does not have — record it **OWED** in the
      task's `statusReason` and discharge it with task-145's visual pass. Never
      assert it green at this commit.

---

### task-145: `currentVote` knob on the governance stories

**Files touched:**

- `storybook/stories/voting/Governance.stories.tsx` (edit — the only file)

**Files created:** none.

**Do not touch:** `storybook/stories/governance/CurrentVoteSummary.stories.tsx`
(task-136 owns it, including its second `DRep status (mock)` knob),
`storybook/stories/index.ts` (no story is added or removed, and the three
unregistered governance stories stay unregistered — that is a deliberate
deferral, not this task's job), and any file under `source/`.

**Precondition.** task-139 must already have added the `drepIndex` prop to
`VotingPowerDelegation`. If `tsc` reports that `drepIndex` is not a known prop,
stop and report it — do **not** add the prop yourself.

#### Context (verified anchors, current code quoted)

Anchors are from commit `504b44c1a`. task-173 lands before this task and
replaces `toStoryDRepIdentity` at `:58-61`, which shifts everything below it by
a line or two — **match the quoted text, not the numbers.**

`:37-41` and `:53` — the imports to change:

```tsx
import Wallet, {
  HwDeviceStatus,
  HwDeviceStatuses,
  WalletSyncStateStatuses,
} from '../../../source/renderer/app/domains/Wallet';
…
import { generateWallet } from '../_support/utils';
```

`:63-97` — the module-level fixture array to delete (three `generateWallet`
calls: software `governance-wallet-1`, Ledger `governance-wallet-2`, syncing
`governance-wallet-3`), followed by a blank line at `:98`:

```tsx
const GOVERNANCE_WALLETS = [
  generateWallet(
    'Governance wallet',
    '125000000000',
    …
];
```

`:204-236` — the shared panel renderer, currently taking no arguments and
reading `GOVERNANCE_WALLETS` at `:233`:

```tsx
const renderGovernancePanel = () => {
  const transactionFee = new BigNumber(
    number('Initialized transaction fee', 0.174257, { min: 0, step: 0.000001 })
  );
  text('Valid DRep ID fixture', VALID_DREP_ID);

  return (
    <VotingPowerDelegation
      getStakePoolById={getStakePoolById}
      …
      wallets={GOVERNANCE_WALLETS}
    />
  );
};
```

`:327-328` and `:386-388` — inside `Connected flow`:

```tsx
      (store) => {
        const isVotingSection =
…
                    {store.state.activeVotingRoute === ROUTES.VOTING.GOVERNANCE
                      ? renderGovernancePanel()
                      : renderCatalystPanel()}
```

`:400-402` — `Voting power delegation`:

```tsx
  .add('Voting power delegation', () => (
    <div style={CENTERED_STORY_STYLE}>{renderGovernancePanel()}</div>
  ))
```

`:403-423` — `Voting power delegation - prefilled from directory` (its own
inline `<VotingPowerDelegation …>` with `initialFormState`, `wallets` at `:420`).

`:457` and `:492` — the two confirmation-dialog stories:
`selectedWallet={GOVERNANCE_WALLETS[0]}` and `selectedWallet={GOVERNANCE_WALLETS[1]}`.

`:497-507` — `Unavailable while syncing` renders only `<VotingUnavailable …>`
and **does not reference `GOVERNANCE_WALLETS` at all**; it needs no edit.

There are exactly **four** reuse sites (`:233`, `:420`, `:457`, `:492`), not the
three the tracker text implies.

#### Locked invariants this change must not break (stated in full)

- **No local `IntlProvider`, no per-locale story variants.**
  `storybook/preview.tsx:8` applies the global `StoryWrapper` whose
  English/Japanese toggle drives every label; a local provider shadows it.
- **No module-level mutable wallet state.** After this task, wallets exist only
  as per-render return values of `makeGovernanceWallets`.
- **Story ids are not renamed**, and none are added or removed. Renaming changes
  the story URL for no user benefit.
- **Keep one panel story whose DRep input can differ from the wallet's current
  vote**, so the `Initialization error` knob's `same_vote` value stays
  reachable behind task-140's client-side disable. The `noDelegation` knob
  default satisfies this: with no current vote, nothing is same-vote.
- **Byte-equality.** The knob only chooses which fixture is handed in; it never
  rewrites, trims or re-encodes a DRep id.
- **No auto-delegation.** The default knob value stays `noDelegation`, which
  renders the warning + nudge, never a pre-picked DRep.
- **Sanitization floor.** No `console.log`, no analytics call, no logger call is
  added to these stories; `action(...)` handlers stay exactly as they are.

#### Resolved judgment calls (do not revisit)

- `renderGovernancePanel` takes `option: CurrentVoteOption` as a parameter and
  contains the `<GovernanceWrapper>` itself, so `Connected flow` and
  `Voting power delegation` share one implementation. Each story reads the knob
  once and passes the value in.
- In `Connected flow` the knob read goes at the **top** of the `withState`
  callback body, before `const isVotingSection`, so the knob stays visible even
  when the Catalyst branch is showing.
- The two confirmation-dialog stories get **no** current-vote knob: they render
  no current-vote surface. They only migrate off `GOVERNANCE_WALLETS`.
  `Unavailable while syncing` gets neither — it renders only
  `<VotingUnavailable syncPercentage={…} />` (`:497-507`) and references
  `GOVERNANCE_WALLETS` nowhere, so there is nothing to migrate. Do not go looking
  for a fifth reuse site; there are four (`:233`, `:420`, `:457`, `:492`).
- `drepIndex` is placed first in the prop list to keep the existing alphabetical
  prop ordering.
- **This file is never run through `prettier --write`.** `prettier@2.1.2 --check`
  is already red at HEAD on it for two pre-existing hunks (the
  `initializeTxErrorOptions` type annotation around `:105` and the
  `STAKE_POOLS_LIST` double assertion at `:125`). Writing would revert unrelated
  committed formatting. Hand-write the blocks below exactly as given; they were
  checked against `prettier@2.1.2` and produce no new drift.

#### Step 1: Fix the imports

Remove `WalletSyncStateStatuses` from the `Wallet` import (it is used only by
the block being deleted; `Wallet` itself is still used as a type at `:278`):

```tsx
import Wallet, {
  HwDeviceStatus,
  HwDeviceStatuses,
} from '../../../source/renderer/app/domains/Wallet';
```

Replace the `generateWallet` import line entirely:

```tsx
import GovernanceWrapper from '../governance/_utils/GovernanceWrapper';
import {
  makeGovernanceWallets,
  useCurrentVoteKnob,
} from '../governance/_utils/fixtures';
import type { CurrentVoteOption } from '../governance/_utils/fixtures';
```

#### Step 2: Delete the module-level fixture array

Delete `const GOVERNANCE_WALLETS = [` through its closing `];` (HEAD `:63-97`)
**plus one of the two blank lines that then sit together**, leaving exactly one
blank line between the end of `toStoryDRepIdentity` and `const voteOptions = {`.

#### Step 3: Rewrite `renderGovernancePanel`

Replace the whole function with:

```tsx
const renderGovernancePanel = (option: CurrentVoteOption) => {
  const transactionFee = new BigNumber(
    number('Initialized transaction fee', 0.174257, {
      min: 0,
      step: 0.000001,
    })
  );
  text('Valid DRep ID fixture', VALID_DREP_ID);

  return (
    <GovernanceWrapper option={option}>
      {({ wallets, drepIndex }) => (
        <VotingPowerDelegation
          drepIndex={drepIndex}
          getStakePoolById={getStakePoolById}
          initiateTransaction={async (params) => {
            action('initiateTransaction')(params);
            return boolean('Initialization succeeds', true)
              ? { success: true, fees: transactionFee }
              : {
                  success: false,
                  errorCode: select(
                    'Initialization error',
                    initializeTxErrorOptions,
                    'same_vote'
                  ),
                };
          }}
          onBrowseDRepsClick={action('onBrowseDRepsClick')}
          onExternalLinkClick={action('onExternalLinkClick')}
          renderConfirmationDialog={renderGovernanceConfirmationDialog}
          stakePools={STAKE_POOLS_LIST}
          wallets={wallets}
        />
      )}
    </GovernanceWrapper>
  );
};
```

Nothing else in the function changes — the three existing knobs
(`Initialized transaction fee`, `Valid DRep ID fixture`,
`Initialization succeeds` / `Initialization error`) stay exactly as they were.

#### Step 4: Wire `Connected flow`

Add the knob read as the first statement of the `withState` callback:

```tsx
      (store) => {
        const option = useCurrentVoteKnob();
        const isVotingSection =
```

and pass it through at the call site:

```tsx
                      ? renderGovernancePanel(option)
```

#### Step 5: Wire `Voting power delegation`

```tsx
  .add('Voting power delegation', () => {
    const option = useCurrentVoteKnob();
    return (
      <div style={CENTERED_STORY_STYLE}>{renderGovernancePanel(option)}</div>
    );
  })
```

#### Step 6: Wire `Voting power delegation - prefilled from directory`

Replace the whole `.add(...)` block with:

```tsx
  .add('Voting power delegation - prefilled from directory', () => {
    const option = useCurrentVoteKnob();
    return (
      <div style={CENTERED_STORY_STYLE}>
        <GovernanceWrapper option={option}>
          {({ wallets, drepIndex }) => (
            <VotingPowerDelegation
              drepIndex={drepIndex}
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
              wallets={wallets}
            />
          )}
        </GovernanceWrapper>
      </div>
    );
  })
```

`initialFormState` is unchanged, including `selectedWalletId:
'governance-wallet-1'` — that id is exactly the id `makeGovernanceWallets`
builds, so the wallet still resolves.

**What this story actually demonstrates, per knob value.** `deriveFormSeed`
(task-138 Step 1) puts the wallet's own `currentVote` **ahead** of the inherited
`initialFormState.selectedDRepId`, and `governance-wallet-1` is the one wallet
`makeGovernanceWallets` gives a knob-derived `votingTarget` (`fixtures.ts`,
`votingTarget: resolveCurrentVote(option)`). So only the **`noDelegation`
default** shows the directory hand-off this story is named for; at
`drepVerified` / `drepUnverified` / `abstain` / `noConfidence` the input shows
the wallet's current vote instead, and the story demonstrates current-vote
precedence. That is correct behaviour, not a defect — say so in the review notes
rather than "fixing" the seed order. Do **not** re-point `selectedWalletId` at
`governance-wallet-2` to dodge it: that wallet is `isHardwareWallet: true`, which
would swap the story's confirmation path.

#### Step 7: Migrate the two confirmation-dialog stories

In `Confirmation dialog - software wallet`:

```tsx
          selectedWallet={makeGovernanceWallets('noDelegation')[0]}
```

In `Confirmation dialog - hardware wallet`:

```tsx
          selectedWallet={makeGovernanceWallets('noDelegation')[1]}
```

No knob, no wrapper, nothing else in either story changes. `Unavailable while
syncing` is untouched.

#### Step 8: Verify

```bash
grep -n "GOVERNANCE_WALLETS\|generateWallet\|WalletSyncStateStatuses" storybook/stories/voting/Governance.stories.tsx
node_modules/.bin/tsc --noEmit
yarn lint
node_modules/.bin/prettier --check storybook/stories/voting/Governance.stories.tsx
```

Expected:

- the `grep` returns **nothing**;
- `tsc --noEmit` exits 0;
- `yarn lint` exits 0;
- `prettier --check` still **fails** on this one file — that is the pre-existing
  HEAD state. Confirm the drift is only the two known hunks and that none of it
  is in code you wrote:

```bash
node_modules/.bin/prettier storybook/stories/voting/Governance.stories.tsx > /tmp/g.pretty.tsx
diff -u storybook/stories/voting/Governance.stories.tsx /tmp/g.pretty.tsx
```

The diff must contain exactly two hunks — `initializeTxErrorOptions` and
`STAKE_POOLS_LIST` — and nothing else. If a third hunk appears, hand-fix that
block to match prettier's output. **Do not** run `prettier --write` on this
file, and never run `yarn prettier`.

Storybook check (manual; the dev server is the only usable Storybook gate here —
`yarn storybook:build` is red at HEAD for an unrelated manager-webpack loader
gap and must not be treated as a regression):

```bash
yarn storybook   # start-storybook -p 6006
```

- `Voting / Governance > Connected flow`,
  `> Voting power delegation` and
  `> Voting power delegation - prefilled from directory` each show a
  `Current vote (mock)` knob with the five labels
  `Not delegated (warning)`, `DRep — verified anchor`,
  `DRep — unverified anchor`, `Abstain`, `No Confidence`, defaulting to
  `Not delegated (warning)`.
- Selecting the wallet `Governance wallet` and cycling the knob shows: the
  warning + nudge for `noDelegation`; an `Active` badge for `drepVerified`; the
  expiring badge and caption for `drepUnverified`; the Abstain / No Confidence
  chip and caption for the two sentinels.
- Remount proof: pick a wallet, type into the DRep field, change the knob — the
  wallet selection and typed text are cleared.
- `Confirmation dialog - software wallet` and `- hardware wallet` render as
  before and show **no** current-vote knob.
- Toggle English → Japanese with the global DaedalusMenu control and re-check
  each knob value.

If no browser is available in this environment, record the console-error and
ja-JP overflow pass as **OWED** in the task's tracker `statusReason` — never
assert it green.

Suggested commit subject: `feat(gov): task-145 wire the current-vote knob into the governance stories`

#### Acceptance

- [ ] AC-1 "Every governance story exposes the five-value Current vote knob" —
      satisfied for the three wallet-bearing current-vote stories
      (`Connected flow`, `Voting power delegation`,
      `Voting power delegation - prefilled from directory`). The two
      confirmation-dialog stories, `Unavailable while syncing`, and the
      directory/detail/badge stories render no current-vote surface and are out
      of scope; `Governance / Current Vote Summary > Core states` gets its knob
      from task-136. Note this scoping in the tracker `statusReason`.
- [ ] AC-2 "No story mutates a module-level `GOVERNANCE_WALLETS`" — the binding
      no longer exists (Step 8 grep).
- [ ] AC-3 "migrated … to per-render `makeGovernanceWallets(option)` calls with
      no shared mutable wallet instances remaining" — all four reuse sites
      (`:233`, `:420`, `:457`, `:492` at HEAD) migrated; grep returns nothing.
- [ ] AC-4 "Every knob value renders without console errors or layout overflow
      in both en-US and ja-JP" — **not satisfiable in this container**: there is
      no browser, so the console and overflow pass cannot be executed. Record it
      as OWED with that reason; the compile-level floor (`tsc`, `lint`, the
      Storybook dev-server bundle) is what this task can prove.

---

## task-146, task-147, task-148: catalogs, current-vote Jest regressions, same-vote path

These three rows close cv-2. They land last, in this order: **146 → 147 → 148**. None of
them edits a production source file; every change is a catalog entry or a test.

---

### task-146: Remaining `CurrentVoteSummary` enrichment i18n keys

**Files touched:**

- `source/renderer/app/i18n/locales/en-US.json` (7 new entries — seeded by the runner, never hand-written)
- `source/renderer/app/i18n/locales/ja-JP.json` (7 new entries — seeded, then hand-translated)
- `source/renderer/app/i18n/locales/defaultMessages.json` (regenerated by the runner)
- `translations/messages.json` (regenerated by `yarn i18n:extract`)
- `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` (three new `it` cases)

**Files deliberately NOT touched:** every `*.messages.ts` (the descriptors are already minted
by task-136 / task-140 / task-175), `source/renderer/app/i18n/locales/whitelist_en-US.json`,
`whitelist_ja-JP.json`, and any pre-existing catalog entry.

#### Context (verified anchors)

The seven descriptors already exist when this task starts. Do not create, rename or re-word
them — this task only carries them into the catalogs:

| descriptor key | message id | defining file | minted by |
|---|---|---|---|
| `statusExpiringBadge` | `voting.governance.currentVote.status.expiringBadge` | `CurrentVoteSummary.messages.ts` | task-136 |
| `statusExpiring` | `voting.governance.currentVote.status.expiring` | `CurrentVoteSummary.messages.ts` | task-136 |
| `statusInactive` | `voting.governance.currentVote.status.inactive` | `CurrentVoteSummary.messages.ts` | task-136 |
| `statusUnavailable` | `voting.governance.currentVote.status.unavailable` | `CurrentVoteSummary.messages.ts` | task-136 |
| `sameVoteHint` | `voting.governance.currentVote.sameVoteHint` | `CurrentVoteSummary.messages.ts` | task-140 |
| `drepIdCip105` | `voting.governance.confirmationDialog.drepIdCip105` | `VotingPowerDelegationConfirmationDialog.messages.ts` | task-175 |
| `signedPayload` | `voting.governance.confirmationDialog.signedPayload` | `VotingPowerDelegationConfirmationDialog.messages.ts` | task-175 |

Pipeline, from `package.json:52-54`:

```
i18n:extract = formatjs extract 'source/**/*.{ts,tsx}' --out-file='translations/messages.json'
i18n:check   = yarn node-swc ./translations/translation-runner.ts
i18n:manage  = yarn i18n:extract && yarn i18n:check
```

`translations/translation-runner.ts:1-13` calls `react-intl-translations-manager` with
`translationsDirectory: 'source/renderer/app/i18n/locales'`, `languages: ['en-US','ja-JP']`,
`singleMessagesFile: true`. It **adds** missing keys to both locale files using the
`defaultMessage` text as the seeded value, preserves existing translations, keeps the files
key-sorted, and regenerates `defaultMessages.json`.

Measured at HEAD in this worktree:

- `en-US.json` and `ja-JP.json` hold **1611 keys each**, with **zero** keys present in only one of them.
- `voting.governance.currentVote.` has **12** keys (`en-US.json:954-965`), all `!!!`-marked in both locales.
- `voting.governance.confirmationDialog.` has **8** keys (`en-US.json:946-953`); only
  `:948` `"voting.governance.confirmationDialog.drepId": "!!!DRep ID"` is marked — the other seven
  legitimately predate the feature.

Current guard, `tests/jest/i18n/preliminaryCopyMarkers.spec.ts:8-26` (quoted verbatim):

```ts
const REVIEWED_JA_JP_EXCEPTIONS = [
  'wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement',
];

const en: Record<string, string> = enUS;
const ja: Record<string, string> = jaJP;

describe('preliminary copy markers', () => {
  it('keeps the ja-JP !!! marker on every key whose en-US copy is still preliminary', () => {
    const unmarked = Object.keys(en).filter(
      (key) =>
        key in ja &&
        en[key].startsWith('!!!') &&
        !ja[key].startsWith('!!!') &&
        !REVIEWED_JA_JP_EXCEPTIONS.includes(key)
    );
    expect(unmarked).toEqual([]);
  });
});
```

Its two blind spots: a key missing from ja-JP entirely is invisible (`key in ja` short-circuits),
and an en-US key minted **without** `!!!` is never flagged. cv-2's seven keys land exactly there.

#### Locked invariants (inline — do not break)

- **Every new en-US and ja-JP string keeps its leading `!!!`.** Removing a marker is a
  release-end, user-owned review; no task in this slice strips one, in either locale.
- **Never hand-edit an en-US value away from its `defaultMessage`.** Component snapshots bake
  the `defaultMessage` fallback in; a divergence silently breaks them. The runner's seeded
  en-US values are final for this slice.
- **`voting.governance.confirmationDialog.drepId` keeps its existing value** (`!!!DRep ID`).
  task-175 only re-purposes it as the CIP-129 line's label; re-wording is out of scope.
- Catalog key order is alphabetical and manager-owned. Do not hand-sort, and **never run
  prettier on a locale catalog, on `defaultMessages.json`, or on `translations/messages.json`.**
- The `sameVoteHint` ICU argument is `target` with branches `drep | abstain | no_confidence | other`;
  the two epoch strings take the argument `n`. Argument names and branch keys are identical in
  both locales — translating a branch key breaks the format at runtime.

#### Resolved judgment calls (do not revisit)

- **The ja-JP values below are the ones to write.** They reuse the reviewed catalog vocabulary:
  失効 / エポック (`ja-JP.json:332`), 非アクティブ (`:356`), 委任 (`:964`), 棄権 (`:944`), 不信任 (`:974`).
- **The namespace marker assertion covers only `voting.governance.currentVote.`.** It is
  deliberately *not* extended to `voting.governance.confirmationDialog.`, because seven of that
  namespace's eight keys are legitimately unmarked. task-175's two new keys are pinned
  individually instead.
- **No allow-list entries are added.** Both new assertions are green on arrival (measured above),
  so an allow-list would only hide future regressions. `REVIEWED_JA_JP_EXCEPTIONS` is untouched.
- **The ja-JP overflow / visual pass (AC-3, second half) cannot run here** — no browser in this
  container. Record it as OWED in the tracker `statusReason` and the code-review log; never
  assert it green.

#### Step-by-Step

##### Step 1: Pre-flight — confirm the seven descriptors exist

```bash
grep -n "status.expiringBadge\|status.expiring'\|status.inactive\|status.unavailable\|currentVote.sameVoteHint" \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.messages.ts
grep -n "drepIdCip105\|signedPayload" \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.messages.ts
```

Expected: five hits in the first file, two in the second. If any is missing, its minting task
(136 / 140 / 175) has not landed — stop and finish that row first; do **not** add the descriptor here.

##### Step 2: Seed both catalogs

```bash
yarn i18n:manage
```

Expected: seven keys added per locale, zero deleted. Both catalogs now carry the en-US
`defaultMessage` text. Confirm the en-US entries read **exactly**:

```json
  "voting.governance.confirmationDialog.drepIdCip105": "!!!CIP-105 DRep ID",
  "voting.governance.confirmationDialog.signedPayload": "!!!Signed payload",
  "voting.governance.currentVote.sameVoteHint": "!!!This wallet already votes {target, select, drep {for this DRep} abstain {Abstain} no_confidence {No Confidence} other {the same way}}.",
  "voting.governance.currentVote.status.expiring": "!!!This DRep's voting power will lapse in {n} epochs — consider re-delegating.",
  "voting.governance.currentVote.status.expiringBadge": "!!!Expiring in {n} epochs",
  "voting.governance.currentVote.status.inactive": "!!!This DRep is currently inactive. Your voting power will not be counted until they vote again — consider re-delegating.",
  "voting.governance.currentVote.status.unavailable": "!!!DRep status is loading."
```

If a seeded en-US value differs from this, the descriptor's `defaultMessage` is wrong — fix it in
the descriptor file and re-run Step 2. Never patch the catalog by hand.

##### Step 3: Hand-write the seven ja-JP values

In `source/renderer/app/i18n/locales/ja-JP.json`, replace **only the values** of the seven seeded
keys (keys and ordering unchanged) with exactly:

```json
  "voting.governance.confirmationDialog.drepIdCip105": "!!!CIP-105 DRep ID",
  "voting.governance.confirmationDialog.signedPayload": "!!!署名対象のペイロード",
  "voting.governance.currentVote.sameVoteHint": "!!!このウォレットは{target, select, drep {すでにこのDRepに委任しています} abstain {すでに棄権に設定されています} no_confidence {すでに不信任に設定されています} other {すでに同じ内容で投票しています}}。",
  "voting.governance.currentVote.status.expiring": "!!!このDRepの投票権はあと{n}エポックで失効します。委任先の変更をご検討ください。",
  "voting.governance.currentVote.status.expiringBadge": "!!!あと{n}エポックで失効",
  "voting.governance.currentVote.status.inactive": "!!!このDRepは現在非アクティブです。再び投票するまで、あなたの投票権は集計されません。委任先の変更をご検討ください。",
  "voting.governance.currentVote.status.unavailable": "!!!DRepのステータスを読み込み中です。"
```

All seven keep the leading `!!!`. `drepIdCip105` stays in Latin script because the catalog already
renders `DRep ID` untranslated at `ja-JP.json:948`.

##### Step 4: Re-run the manager — it must be clean

```bash
yarn i18n:manage
git diff --stat
```

Expected: zero added, zero deleted keys; the hand-written ja-JP values survive (the runner only
seeds *missing* keys); `git diff --stat` lists **only** `en-US.json`, `ja-JP.json`,
`defaultMessages.json` and `translations/messages.json`. If any other file appears, restore it:

```bash
git restore <path>
```

(Never `git stash` — the stash stack is shared across worktrees.)

##### Step 5: Widen the preliminary-copy guard

Edit `tests/jest/i18n/preliminaryCopyMarkers.spec.ts`. Add one constant beside the existing
`REVIEWED_JA_JP_EXCEPTIONS` block:

```ts
const CURRENT_VOTE_NAMESPACE = 'voting.governance.currentVote.';

const PRELIMINARY_CONFIRMATION_KEYS = [
  'voting.governance.confirmationDialog.drepIdCip105',
  'voting.governance.confirmationDialog.signedPayload',
];
```

Then add three `it` cases inside the existing `describe('preliminary copy markers', …)`, after
the committed case (do not modify that case):

```ts
  it('defines every catalog key in both locales', () => {
    const missingInJa = Object.keys(en).filter((key) => !(key in ja));
    const missingInEn = Object.keys(ja).filter((key) => !(key in en));
    expect({ missingInEn, missingInJa }).toEqual({
      missingInEn: [],
      missingInJa: [],
    });
  });

  it('keeps the preliminary marker on every current-vote key in both locales', () => {
    const unmarked = Object.keys(en)
      .filter((key) => key.startsWith(CURRENT_VOTE_NAMESPACE))
      .filter(
        (key) => !en[key].startsWith('!!!') || !ja[key].startsWith('!!!')
      );
    expect(unmarked).toEqual([]);
  });

  it('keeps the preliminary marker on the new confirmation-dialog keys in both locales', () => {
    const unmarked = PRELIMINARY_CONFIRMATION_KEYS.filter(
      (key) => !en[key]?.startsWith('!!!') || !ja[key]?.startsWith('!!!')
    );
    expect(unmarked).toEqual([]);
  });
```

##### Step 6: Verify

```bash
grep -c "voting.governance.currentVote" source/renderer/app/i18n/locales/en-US.json   # 17
grep -c "voting.governance.currentVote" source/renderer/app/i18n/locales/ja-JP.json   # 17
grep -c "voting.governance.confirmationDialog" source/renderer/app/i18n/locales/en-US.json   # 10
grep -c "voting.governance.confirmationDialog" source/renderer/app/i18n/locales/ja-JP.json   # 10

# every currentVote value keeps its marker in both locales
grep "voting.governance.currentVote" source/renderer/app/i18n/locales/en-US.json | grep -v ': "!!!' || echo "OK: en-US markers intact"
grep "voting.governance.currentVote" source/renderer/app/i18n/locales/ja-JP.json | grep -v ': "!!!' || echo "OK: ja-JP markers intact"

# the drepId label was not re-worded
grep -n '"voting.governance.confirmationDialog.drepId"' source/renderer/app/i18n/locales/en-US.json   # "!!!DRep ID"

node_modules/.bin/jest --testPathPattern=preliminaryCopyMarkers --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="voting-governance|VotingGovernancePage" --no-coverage --runInBand
node_modules/.bin/tsc --noEmit
```

The second Jest run is the snapshot-stability check: catalog values are byte-identical to the
`defaultMessage` fallbacks the components already render, so no snapshot may change.

Do **not** run `yarn prettier` (its script embeds a repo-wide `**/*.*` glob), do not run
`nix fmt` (nix is absent here — record it as a pre-merge obligation), and do not run
`node_modules/.bin/prettier` on any file this task touches: all five are tool-managed JSON, and
the spec file is pre-existing.

##### Step 7: Commit

```
feat(gov): task-146 carry the current-vote enrichment copy into both catalogs
```

#### Acceptance

- **AC-1** (`sameVoteHint`, `status.expiring`, `status.inactive`, `status.unavailable` present in
  both catalogs) — Steps 2–4, proven by the Step 6 counts (17 `currentVote` keys per locale) and
  the marker greps. `status.expiringBadge` ships alongside them.
- **AC-2** (confirmation-dialog copy stays compatible with the selected current target) — no
  descriptor is re-worded; `confirmationDialog.drepId` keeps `!!!DRep ID` (Step 6 grep), and the
  two new confirmation keys land in both locales (count 10 per locale).
- **AC-3, first half** (ja-JP copy retains the `!!!` marker) — Step 3 + Step 5's namespace and
  per-key assertions.
- **AC-3, second half** (ja-JP length / layout overflow reviewed) — **NOT satisfied.** It needs a
  running Storybook with the global Japanese toggle; there is no browser in this container.
  Record it as OWED in the task's `statusReason` and in the code-review log.
- **AC-4** (`yarn i18n:manage` runs clean) — Step 4's second run: zero added, zero deleted, no churn.

---

### task-147: Jest governance current-vote regressions + HW path

**Files touched (all existing — append only, never rewrite another task's cases):**

- `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`
- `tests/jest/governance/isSameVoteTarget.spec.ts` (created by task-140)
- `tests/jest/security/governance-sanitization.spec.ts`

**Files deliberately NOT touched:** any production source file; the `WalletsDropdown` /
`ItemsDropdown` mocks at `VotingGovernancePage.spec.tsx:34-44` (task-138 owns them — every case
below selects a wallet through `location.state`, never through the dropdown);
`VotingPowerDelegationConfirmationDialog.spec.tsx` (task-142 / task-175);
`CurrentVoteSummary.spec.tsx` (task-136); `VotingStore.spec.ts` (task-148).

**No Cucumber, no e2e** — v1 ships none; this row is Jest-only.

#### Context (verified anchors)

`VotingGovernancePage.spec.tsx` (391 lines at HEAD) already provides everything needed:

- `VALID_DREP_ID = 'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n'` (`:46-47`),
  `WALLET_ID = 'wallet-1'` (`:48`), `HW_WALLET_ID = 'hw-wallet-1'` (`:56`).
- `softwareWallet` (`:50-54`) and `hardwareWallet` (`:58-62`) are plain objects cast `as any`,
  **not** `Wallet` domain instances — so a `currentVote` field is set directly on them
  (the component reads `selectedWallet?.currentVote`; the domain getter is not involved).
- `drepEntry` (`:64-70`) is `{ anchor: null, drepActivity: 12, drepId: VALID_DREP_ID, status: 'active', votingPower }`.
  **Do not change it** — it also feeds `displayedDRepList` / `drepList` / `showAllList`. Under the
  cv-2 badge rules `drepActivity: 12` renders the *expiring* badge, not `Active`.
- `buildStores` (`:78-122`) already supplies `governance.drepIndex = new Map([[VALID_DREP_ID, drepEntry]])` (`:89`)
  and accepts `{ hwDeviceStatus, isTrezor, wallets }` overrides (`:72-82`).
- `renderFlow(initialEntries, storeOverrides)` (`:126-163`) returns `{ actions, history, pushSpy, stores }`.
- The HW describe is `:304-391`; its shared `hwEntry` (directory route + handoff state) is `:310-317`.

Verified vectors (decoded in this worktree — use verbatim, do not re-derive):

| purpose | value |
|---|---|
| current target, CIP-129 | `drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n` |
| same target, all upper-case | `DREP1YGQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ7VLC9N` |
| its CIP-105 form | `drep_vkh1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq667pyd` |
| its credential hex (28 bytes) | `00000000000000000000000000000000000000000000000000000000` |
| a *different* valid target | `drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy` |

Measured facts these tests rely on: `bech32@2.0.0` decodes an all-upper-case id to the same bytes
and rejects a mixed-case one (`Mixed-case string …`), and `Cardano.DRepID.isValid` returns `true`
for both the lower-case and the all-upper-case form — so the upper-case id passes the form's
validity gate and the *only* reason submit stays disabled is the comparator.

Device-state copy, from `en-US.json:1137-1144`, as rendered by `HardwareWalletStatus` when the
dialog passes `walletName={selectedWallet.name}` (`VotingPowerDelegationConfirmationDialog.tsx:179-185`):

| `hwDeviceStatus` | rendered copy | reads as |
|---|---|---|
| `CONNECTING_FAILED` | `Disconnect and reconnect your hardware wallet to restart the process.` | disconnected |
| `CONNECTING` | `Connect the "<walletName>" device and enter your PIN to unlock it` | locked |
| `LAUNCHING_CARDANO_APP` | `Launch Cardano application on your device` | Cardano app not open |

`CONNECTING` renders through `FormattedMessage` with a `{walletName}` slot, so assert it with a
**regex** (`/enter your PIN to unlock it/`), never a full-string match.

#### Locked invariants (inline — do not break)

- **Sanitization floor.** No DRep id, no `abstain` / `no_confidence` literal, no CIP-129/CIP-105
  bech32 string may reach any logger or analytics payload. This row is the slice's re-assertion of
  that floor: it spies the loggers over the flows task-137/138/140 (`chosenOption`) and
  task-173/175 (`drepIdentity`) create, and re-runs the committed floor suite green.
- **Byte-equality.** The comparator must not mutate anything: `chosenOption` and the
  `delegateVotes` `dRepId` stay byte-identical, including in the upper-case case. Assert what the
  store *received*, never a re-encoded value.
- **The server `same_vote` net stays reachable.** These tests exercise the client-side disable;
  they must not assert that the server error path is gone (task-148 pins it).
- Test names carry no task id, no `CAT-x` / `CP-x`, no plan name, no PR number, and no ALL-CAPS.
  Filenames are `.spec.ts` / `.spec.tsx` — never `.test.ts`.

#### Resolved judgment calls (do not revisit)

- **The analytics vote-kind field is a deliberate exception to "no `abstain` literal".**
  `VotingStore._getVoteKind` (`VotingStore.ts:196-202`) returns the literal `'abstain'` /
  `'no_confidence'` and it is sent as the third analytics argument (`:399-403`, `:430-434`). That is
  the reviewed, shipped shape: a vote *kind*, not a delegation target. **Never write an assertion
  that `'abstain'` is absent from an analytics payload — it will fail.** Assert instead that the
  payload is exactly the three-argument vote-kind shape and carries no bech32 identifier.
- **Do not extend `filterLogData`'s key list, and do not add a test that pins its current
  behaviour for renderer-domain names.** The guarded set (`source/common/utils/logging.ts:24-49`)
  is wire-keyed (`drepId`, `dRepId`, `vote`, `voting`) and matched by exact string equality, so
  `votingTarget` / `currentVote` / `drepIdentity` are unguarded by design. cv-2's discharge is the
  stricter invariant — *no domain `Wallet` and no `DRepIdentity` ever enters a logger or analytics
  payload from a cv-2 code path* — asserted at the call boundary, where it stays true whether or
  not the key list is ever widened.
- **The flow-level logger assertions live in `VotingGovernancePage.spec.tsx`**, because the
  harness (`buildStores`, `renderFlow`, the HW describe) already exists there, colocated under
  `source/`. Do **not** re-implement ~150 lines of that scaffolding under `tests/jest/` just because
  this task's `targetPath` says `tests/jest/`; record the deviation in the task's `statusReason`.
- **The store/analytics assertions live in `tests/jest/security/governance-sanitization.spec.ts`**,
  where a real `VotingStore` with an analytics mock is already the established pattern. The
  container spec's `voting` store is a `jest.fn()` mock, so an analytics assertion there would be vacuous.

#### Step-by-Step

##### Step 1: Add the current-vote fixtures to `VotingGovernancePage.spec.tsx`

Insert after the `drepEntry` block (`:64-70`), before `type StoreOverrides` (`:72`):

```ts
const VALID_DREP_ID_UPPERCASE =
  'DREP1YGQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ7VLC9N';
const OTHER_DREP_ID =
  'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';

const currentVoteForValidDRep = {
  kind: 'drep' as const,
  drep: {
    raw: VALID_DREP_ID,
    cip129: VALID_DREP_ID,
    cip105: 'drep_vkh1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq667pyd',
    credentialHex: '00000000000000000000000000000000000000000000000000000000',
    credentialType: 'key' as const,
  },
  source: 'onchain' as const,
};

const votingSoftwareWallet = {
  ...softwareWallet,
  currentVote: currentVoteForValidDRep,
};

const votingHardwareWallet = {
  ...hardwareWallet,
  currentVote: currentVoteForValidDRep,
};
```

Add one import beside the existing ones (`:18-30`):

```ts
import { logger } from '../../utils/logging';
```

##### Step 2: Append the software-wallet describe

Add at the **end** of `VotingGovernancePage.spec.tsx`, after the HW describe's closing `});`:

```ts
describe('Current-vote enrichment in the delegation form', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  const formEntry = {
    pathname: ROUTES.VOTING.GOVERNANCE,
    state: { selectedWalletId: WALLET_ID, voteType: 'drep' },
  };

  it('shows the current delegation and disables submit while the form matches it', () => {
    const { stores } = renderFlow([formEntry], {
      wallets: [votingSoftwareWallet],
    });

    expect(screen.getByText('!!!Delegated to DRep')).toBeInTheDocument();
    expect(screen.getByText('!!!Expiring in 12 epochs')).toBeInTheDocument();
    expect(
      screen.getByText(
        "!!!This DRep's voting power will lapse in 12 epochs — consider re-delegating."
      )
    ).toBeInTheDocument();
    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
    expect(
      screen.getByText('!!!This wallet already votes for this DRep.')
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Submit' })).toBeDisabled();
    expect(stores.voting.initializeVPDelegationTx).not.toHaveBeenCalled();
  });

  it('resolves the directory entry for a CIP-105 delegation through its CIP-129 form', () => {
    const CIP105_DREP_ID =
      'drep_vkh1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq667pyd';

    renderFlow([formEntry], {
      wallets: [
        {
          ...softwareWallet,
          currentVote: {
            ...currentVoteForValidDRep,
            drep: { ...currentVoteForValidDRep.drep, raw: CIP105_DREP_ID },
          },
        },
      ],
    });

    expect(screen.getByText('!!!Expiring in 12 epochs')).toBeInTheDocument();
    expect(screen.queryByText('!!!DRep status is loading.')).toBeNull();
    expect(screen.getByLabelText(CIP105_DREP_ID)).toBeInTheDocument();
  });

  it('treats a target differing only in bech32 letter case as the current vote', () => {
    renderFlow([formEntry], { wallets: [votingSoftwareWallet] });

    fireEvent.change(screen.getByDisplayValue(VALID_DREP_ID), {
      target: { value: VALID_DREP_ID_UPPERCASE },
    });

    expect(
      screen.getByDisplayValue(VALID_DREP_ID_UPPERCASE)
    ).toBeInTheDocument();
    expect(
      screen.getByText('!!!This wallet already votes for this DRep.')
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Submit' })).toBeDisabled();
  });

  it('re-enables submit and opens the confirmation dialog when the target changes', async () => {
    const { stores } = renderFlow([formEntry], {
      wallets: [votingSoftwareWallet],
    });

    fireEvent.change(screen.getByDisplayValue(VALID_DREP_ID), {
      target: { value: OTHER_DREP_ID },
    });

    const submit = screen.getByRole('button', { name: 'Submit' });
    expect(submit).not.toBeDisabled();
    fireEvent.click(submit);

    await screen.findByText('Confirm Transaction');
    expect(stores.voting.initializeVPDelegationTx).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: OTHER_DREP_ID })
    );
    expect(screen.getByText(OTHER_DREP_ID).textContent).toBe(OTHER_DREP_ID);
  });

  it('keeps the vote target out of renderer logger payloads across the flow', async () => {
    const spies = [
      jest.spyOn(logger, 'debug').mockImplementation(() => undefined),
      jest.spyOn(logger, 'info').mockImplementation(() => undefined),
      jest.spyOn(logger, 'warn').mockImplementation(() => undefined),
      jest.spyOn(logger, 'error').mockImplementation(() => undefined),
    ];

    renderFlow([formEntry], { wallets: [votingSoftwareWallet] });

    fireEvent.change(screen.getByDisplayValue(VALID_DREP_ID), {
      target: { value: OTHER_DREP_ID },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));
    await screen.findByText('Confirm Transaction');

    const logged = JSON.stringify(spies.map((spy) => spy.mock.calls));
    expect(logged).not.toContain(VALID_DREP_ID);
    expect(logged).not.toContain(VALID_DREP_ID_UPPERCASE);
    expect(logged).not.toContain(OTHER_DREP_ID);
    expect(logged).not.toContain('drep_vkh');
    expect(logged).not.toContain('drep_script');
    expect(logged).not.toContain('abstain');
    expect(logged).not.toContain('no_confidence');
  });
});
```

The two badge assertions in the first case and the whole CIP-105 case are the
slice's **only** executable pin on the `drepIndex` → `drepEntry` → badge chain:
task-136's unit cases pass `drepEntry` in directly and exercise no lookup,
task-139 adds no spec file, and nothing else asserts a badge from a store-backed
index. They are cheap here because `buildStores` already supplies
`governance.drepIndex = new Map([[VALID_DREP_ID, drepEntry]])` (`:89`) with
`drepActivity: 12`, which is `<= EXPIRING_MAX_REMAINING_EPOCHS` and so renders
the expiring badge and its caption (task-136 Step 3c).

The CIP-105 case is the executable form of PRD D-6's query rule: the wallet's
`drep.raw` is the CIP-105 encoding of the *same* credential while `drep.cip129`
stays `VALID_DREP_ID`, so it passes only if the lookup queries
`cip129 ?? raw`. A `raw`-keyed lookup returns `null`
(`Cardano.DRepID.isValid` rejects `drep_vkh1…`, `helpers.ts:144`) and the case
fails on the `!!!DRep status is loading.` caption. `DRepIdDisplay` truncates the
id it renders but sets the full string as `aria-label`
(`DRepIdDisplay.tsx:72-75`), so assert the byte-identical `raw` with
`getByLabelText`, never `getByText`.

##### Step 3: Append the hardware-wallet cases

Add **inside** the existing `describe('Hardware-wallet delegate flow via location.state handoff', …)`
block (`:304-391`), immediately before its closing `});`:

```ts
  const deviceStates: Array<[HwDeviceStatus, RegExp]> = [
    [
      HwDeviceStatuses.CONNECTING_FAILED,
      /Disconnect and reconnect your hardware wallet/,
    ],
    [HwDeviceStatuses.CONNECTING, /enter your PIN to unlock it/],
    [
      HwDeviceStatuses.LAUNCHING_CARDANO_APP,
      /Launch Cardano application on your device/,
    ],
  ];

  it('renders the current delegation with no device connected and blocks the same-vote submit', () => {
    const { stores } = renderFlow(
      [
        {
          pathname: ROUTES.VOTING.GOVERNANCE,
          state: { selectedWalletId: HW_WALLET_ID, voteType: 'drep' },
        },
      ],
      {
        hwDeviceStatus: HwDeviceStatuses.CONNECTING_FAILED,
        wallets: [votingHardwareWallet],
      }
    );

    expect(screen.getByText('!!!Delegated to DRep')).toBeInTheDocument();
    expect(
      screen.getByText('!!!This wallet already votes for this DRep.')
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Submit' })).toBeDisabled();
    expect(stores.voting.initializeVPDelegationTx).not.toHaveBeenCalled();
  });

  it.each(deviceStates)(
    'surfaces the %s device state in the confirmation dialog and keeps Confirm disabled',
    async (hwDeviceStatus, expectedCopy) => {
      renderFlow([hwEntry], { hwDeviceStatus, wallets: [hardwareWallet] });

      fireEvent.click(
        screen.getByRole('button', { name: '!!!Select for delegation' })
      );
      fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

      await screen.findByText('Confirm Transaction');
      expect(screen.getByText(expectedCopy)).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
    }
  );
```

`HwDeviceStatus` and `HwDeviceStatuses` are already imported (`:22-23`); `hwEntry` and
`hardwareWallet` (no `currentVote`, so its submit stays enabled) are already in scope.

##### Step 4: Extend the comparator spec with the letter-case vectors

`tests/jest/governance/isSameVoteTarget.spec.ts` is created by task-140. **Reuse the CIP-129 key
vector constants it already declares**; only if the file declares none, add these three (they are
the checksum-verified cv-1 vectors):

```ts
const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
const KEY_CREDENTIAL_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
```

Append a new top-level `describe` at the end of the file (do not edit task-140's cases):

```ts
describe('isSameVoteTarget letter-case stability', () => {
  const currentVote: WalletVotingTarget = {
    kind: 'drep',
    drep: {
      raw: KEY_CIP129,
      cip129: KEY_CIP129,
      cip105: KEY_CIP105,
      credentialHex: KEY_CREDENTIAL_HEX,
      credentialType: 'key',
    },
    source: 'onchain',
  };

  it('matches an all-upper-case bech32 form of the current target', () => {
    expect(isSameVoteTarget(KEY_CIP129.toUpperCase(), currentVote)).toBe(true);
  });

  it('matches when the stored credential hex is upper-case', () => {
    expect(
      isSameVoteTarget(KEY_CIP129, {
        ...currentVote,
        drep: {
          ...currentVote.drep,
          credentialHex: KEY_CREDENTIAL_HEX.toUpperCase(),
        },
      })
    ).toBe(true);
  });

  it('rejects a mixed-case form, which is not a decodable identifier', () => {
    expect(isSameVoteTarget(`D${KEY_CIP129.slice(1)}`, currentVote)).toBe(false);
  });
});
```

If the file does not already import them, add:

```ts
import type { WalletVotingTarget } from '../../../source/renderer/app/api/wallets/types';
```

(`isSameVoteTarget` is already imported by task-140's cases.)

##### Step 5: Extend the sanitization floor suite

`tests/jest/security/governance-sanitization.spec.ts`. Add one import to the block at `:36-43`:

```ts
import walletVotingDRepFixture from '../../mocks/wallets/wallet-voting-drep.json';
```

Add one case at the end of `describe('Governance sanitization — filterLogData', …)`
(before its closing `});` at `:216`):

```ts
  it('redacts the vote target from a full wallet-list wire payload', () => {
    const result = jsonStr(filterLogData(walletVotingDRepFixture));
    expect(result).not.toContain(
      'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy'
    );
    expect(result).toContain('cv1 fixture voting drep');
  });
```

Add one case at the end of `describe('Governance sanitization — call boundaries', …)`
(before its closing `});` at `:456`):

```ts
  it('reduces a sentinel delegation to the vote kind in the analytics payload', async () => {
    const analytics = {
      disableTracking: jest.fn(),
      enableTracking: jest.fn(),
      sendEvent: jest.fn(),
      sendPageNavigationEvent: jest.fn(),
    };
    const errorSpy = jest
      .spyOn(rendererLogger, 'error')
      .mockImplementation(() => undefined);
    const debugSpy = jest
      .spyOn(rendererLogger, 'debug')
      .mockImplementation(() => undefined);
    const store = new VotingStore(
      {
        ada: {
          delegateVotes: jest.fn(() => Promise.resolve(Buffer.from('ok'))),
        },
      } as any,
      {} as any,
      analytics as any
    );

    await store.delegateVotes({
      chosenOption: 'abstain',
      passphrase: 'test-passphrase',
      wallet: {
        amount: new BigNumber('123000000'),
        id: 'wallet-1',
        isHardwareWallet: false,
      } as any,
    });

    // The third argument is the derived vote kind, not a delegation target.
    expect(analytics.sendEvent).toHaveBeenCalledWith(
      EventCategories.VOTING,
      'Casted governance vote',
      'abstain'
    );
    expect(analytics.sendEvent.mock.calls[0]).toHaveLength(3);
    const analyticsPayload = jsonStr(analytics.sendEvent.mock.calls);
    expect(analyticsPayload).not.toContain('drep1');
    expect(analyticsPayload).not.toContain('drep_vkh');
    expect(analyticsPayload).not.toContain('drep_script');

    const logged = jsonStrWithErrors([errorSpy.mock.calls, debugSpy.mock.calls]);
    expect(logged).not.toContain('abstain');
    expect(logged).not.toContain(CIP129_DREP);
  });
```

##### Step 6: Verify

```bash
node_modules/.bin/jest --testPathPattern=VotingGovernancePage --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=isSameVoteTarget --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=governance-sanitization --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=governance --no-coverage --runInBand   # 16 suites
node_modules/.bin/tsc --noEmit
yarn lint    # exit 0; the baseline is ~5591 warnings — count must not grow
```

The floor suite must report **26** tests (24 at HEAD + the two added here), all green.
All three files are pre-existing, so run **no** prettier on them (`prettier@2.1.2 --check` is
already red at HEAD on unrelated files; formatting a pre-existing file would drag unrelated
reversions into the diff). Never run `yarn check:all` as a gate — it is red at HEAD for unrelated
reasons (`storybook:build` manager-webpack).

##### Step 7: Commit

```
test(gov): task-147 pin the current-vote flow, HW device states, and the sanitization floor
```

#### Acceptance

- **AC-1** (focused Jest regression suite green) — Step 6's four runs. Step 2's five cases are
  also the slice's only pin on the `drepIndex` → `drepEntry` → badge chain (badge label and
  caption in case 1; the CIP-105 lookup key in case 2), which PRD R-9 owns.
- **AC-2** (HW path green for disconnected / locked / app-not-open) — Step 3's `it.each` over
  `CONNECTING_FAILED` / `CONNECTING` / `LAUNCHING_CARDANO_APP`, plus the no-device
  current-vote case.
- **AC-3** (letter-case vector: same vote, submit stays disabled) — Step 4's three unit vectors
  **and** Step 2's flow case, which types the all-upper-case id into the live form and asserts the
  hint plus the disabled submit. The flow case is what makes "submit stays disabled" executable;
  the unit vectors prove the comparator is case-stable on both the bech32 and the hex side.
- **AC-4** (logger and analytics spies confirm no leaked vote target) — Step 2's logger-spy case
  (four levels, across select → edit → submit → dialog) and Step 5's analytics case.
- **AC-5** (inherited sanitization floor) — Step 5's two additions plus the whole committed floor
  suite re-run green. **Scope note, deliberate:** the literal `'abstain'` *does* appear as the
  analytics vote-kind value on the sentinel path (`VotingStore.ts:196-202`, `:431-435`). That is
  the shipped, reviewed shape — a vote kind, not a target. AC-5 is read as: no DRep identifier in
  any payload, and no sentinel literal in any **logger** payload. Record this reading in the
  task's `statusReason`.

---

### task-148: Same-vote path regression

**Files touched:** `source/renderer/app/stores/VotingStore.spec.ts` — this file only.

**Files deliberately NOT touched.** The `same_vote` path spans six live sites; this task **pins**
them and edits none:

1. `source/renderer/app/stores/VotingStore.ts:62` (the literal inside `expectedInitializeVPDelegationTxErrors`, `:61-64`)
2. `VotingStore.ts:74-95` (`parseApiCode`)
3. `VotingStore.ts:347-360` (the `initializeVPDelegationTx` catch)
4. `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx:89` (the intl map entry)
5. `VotingPowerDelegation.messages.ts:72-76` (`initializeTxErrorSameVote`)
6. `en-US.json:973` / `ja-JP.json:973`

#### Context (verified anchors)

`VotingStore.ts:61-64`:

```ts
export const expectedInitializeVPDelegationTxErrors = [
  'same_vote',
  'no_utxos_available',
  'not_enough_money',
] as const;
```

`parseApiCode` (`:74-95`) recognises an expected code from either
`error instanceof ApiError && expectedCodes.includes(error.code)` **or**
`error instanceof GenericApiError && expectedCodes.includes(error.values.code)`, and otherwise
returns `'generic'`. Production throws `new ApiError(error)` (`api.ts:421`, `:463`, `:488`, …), so
`ApiError` is the faithful fixture.

`initializeVPDelegationTx` (`:281`) calls `selectDelegationCoins` **before** the
`wallet.isHardwareWallet` branch (`:303`), so a rejection there reaches the shared catch for
software wallets too. The catch (`:347-360`) logs only `{ errorCode }` and returns
`{ success: false, errorCode }`.

Renderer surface, `VotingPowerDelegation.tsx:84-92` and `:304-308`:

```tsx
const mapOfTxErrorCodeToIntl: Record<
  InitializeVPDelegationTxError,
  (typeof messages)[keyof typeof messages]
> = {
  generic: messages.initializeTxErrorGeneric,
  same_vote: messages.initializeTxErrorSameVote,
  …
};
```

```tsx
{state.status === 'form-with-error' && (
  <p className={styles.generalError}>
    {intl.formatMessage(mapOfTxErrorCodeToIntl[state.txInitError])}
  </p>
)}
```

`en-US.json:973` renders as:
`This voting power delegation choice has already been successfully recorded in a previous transaction. Please change the registration type or DRep ID in order to proceed.`

`VotingStore.spec.ts` at HEAD: vectors `:62-63`, `hwWallet` `:65-69`, `softwareWallet` `:71-75`,
`buildHardwareWallets` `:84-95`, `buildStore` `:97-108`, the outer describe `:110`, its
`beforeEach` spying `logger.error` `:111-115`, and `describe('initializeVPDelegationTx')`
**`:122-216`** whose last case ends at `:215`.

#### Locked invariants (inline — do not break)

- **The server `same_vote` error must stay reachable behind task-140's client-side disable.**
  The client gate only fires when the chosen target equals the wallet's `currentVote`; the store
  path is reached whenever the ledger disagrees with the renderer's view. Both cases below are
  written from that starting state — a wallet whose `currentVote` does **not** match the chosen
  target — which is exactly what makes them a proof rather than a tautology.
- **Sanitization floor.** The catch logs `{ errorCode }` only; the assertion pins that no DRep id
  reaches the logger payload.
- `expectedInitializeVPDelegationTxErrors` is not modified, re-ordered or re-typed.
- Test names carry no task id and no ALL-CAPS.

#### Resolved judgment calls (do not revisit)

- **The fixture error is `new ApiError({ code: 'same_vote' } as any)`.** `ErrorType.code` is typed
  as a closed `KnownErrorType` union (`ApiError.ts:8-59`) that does not list `same_vote`, so the
  `as any` is required; `ApiError`'s constructor still assigns `code: error.code` verbatim
  (`:100`), which is what `parseApiCode` reads. Passing no second (`logging`) argument means the
  constructor logs nothing.
- **The render assertion stays in this `.ts` file and uses `React.createElement`, not JSX.**
  `tsconfig.json` has no `include`, so spec files are typechecked, and TypeScript rejects JSX in a
  `.ts` file. Do **not** rename the spec to `.tsx`.
- **`WalletsDropdown` and `ItemsDropdown` are mocked to render nothing.** The wallet is selected
  through the `initialFormState` prop, so the dropdowns are never interacted with, and mocking
  them keeps a react-polymorph-heavy widget out of a store spec. The mock form mirrors the
  committed one at `VotingGovernancePage.spec.tsx:34-38`.
- **The store case uses `softwareWallet`.** `selectDelegationCoins` is called before the HW
  branch, so the software wallet exercises the same catch with less setup, and it matches the
  wallet kind the renderer gate applies to.

#### Step-by-Step

##### Step 1: Extend the imports

At the top of `source/renderer/app/stores/VotingStore.spec.ts`, widen the existing store import
(`:4`) and add the render harness:

```ts
import React from 'react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import VotingStore, {
  FundPhase,
  expectedInitializeVPDelegationTxErrors,
} from './VotingStore';
import ApiError from '../domains/ApiError';
import translations from '../i18n/locales/en-US.json';
import { daedalusTheme } from '../themes/daedalus';
import { themeOverrides } from '../themes/overrides';
import VotingPowerDelegation from '../components/voting/voting-governance/VotingPowerDelegation';
```

(The existing `import BigNumber`, `import type { Api }`, `import type { ActionsMap }`,
`import type { CatalystFund }`, `EventCategories`/`noopAnalyticsTracker`, and `logger` imports stay
exactly as they are.)

Directly below the import block add the two widget mocks:

```ts
jest.mock('../components/widgets/forms/WalletsDropdown', () => {
  return function WalletsDropdownMock() {
    return null;
  };
});

jest.mock('../components/widgets/forms/ItemsDropdown', () => {
  return function ItemsDropdownMock() {
    return null;
  };
});
```

##### Step 2: Add the store-level `same_vote` case

Insert as a new `it` inside `describe('initializeVPDelegationTx', …)`, after the existing
"returns a generic error code when the device is not connected" case (`:197-215`) and before that
describe's closing `});` (`:216`). It inherits the `logger.error` spy from the outer
`beforeEach` (`:111-115`):

```ts
    it('surfaces the same_vote server error without logging the vote target', async () => {
      const hardwareWallets = buildHardwareWallets({
        selectDelegationCoins: jest.fn(async () => {
          throw new ApiError({ code: 'same_vote' } as any);
        }),
      });
      const { store } = buildStore(hardwareWallets);

      const result = await store.initializeVPDelegationTx({
        chosenOption: CIP129_KEY,
        wallet: softwareWallet,
      });

      expect(expectedInitializeVPDelegationTxErrors).toContain('same_vote');
      expect(result).toEqual({ success: false, errorCode: 'same_vote' });
      expect(logger.error).toHaveBeenCalledWith(
        'VotingStore: error while initializing VP delegation TX with HW',
        expect.objectContaining({ errorCode: 'same_vote' })
      );

      const errorSpy = jest.spyOn(logger, 'error');
      expect(JSON.stringify(errorSpy.mock.calls)).not.toContain(CIP129_KEY);
    });
```

`jest.spyOn` on an already-spied method returns the existing mock instead of re-wrapping it
(`node_modules/jest-mock/build/index.js:794-796`), so `errorSpy` carries the calls recorded under
the outer `beforeEach` spy. Do not cast `logger.error` to `jest.Mock` — `Logger`'s method type does
not convert cleanly.

##### Step 3: Add the render assertion

Append at the **end** of the file, as a new top-level `describe`:

```ts
const DelegationForm = VotingPowerDelegation as unknown as React.ComponentType<any>;

describe('same-vote server error in the delegation form', () => {
  afterEach(cleanup);

  it('renders the server same_vote copy when the wallet has no matching current vote', async () => {
    const wallet = {
      currentVote: null,
      id: 'sw-wallet-2',
      isHardwareWallet: false,
      name: 'Form Wallet',
    } as any;
    const initiateTransaction = jest.fn(async () => ({
      errorCode: 'same_vote' as const,
      success: false as const,
    }));

    render(
      React.createElement(
        ThemeProvider,
        {
          theme: daedalusTheme,
          skins: SimpleSkins,
          variables: SimpleDefaults,
          themeOverrides,
        },
        React.createElement(
          IntlProvider,
          { locale: 'en-US', messages: translations },
          React.createElement(DelegationForm, {
            getStakePoolById: jest.fn(),
            initialFormState: {
              selectedDRepId: CIP129_KEY,
              selectedWalletId: wallet.id,
              voteType: 'drep',
            },
            initiateTransaction,
            onBrowseDRepsClick: jest.fn(),
            onExternalLinkClick: jest.fn(),
            renderConfirmationDialog: () => null,
            stakePools: [],
            wallets: [wallet],
          })
        )
      )
    );

    const submit = screen.getByRole('button', { name: 'Submit' });
    expect(submit).not.toBeDisabled();
    fireEvent.click(submit);

    expect(
      await screen.findByText(
        'This voting power delegation choice has already been successfully recorded in a previous transaction. Please change the registration type or DRep ID in order to proceed.'
      )
    ).toBeInTheDocument();
    expect(initiateTransaction).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: CIP129_KEY })
    );
  });
});
```

`currentVote: null` is load-bearing: it is what keeps task-140's client-side disable from firing,
so the click really does reach the store contract and the server copy really does render.

##### Step 4: Verify

```bash
node_modules/.bin/jest --testPathPattern=VotingStore --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=governance --no-coverage --runInBand
node_modules/.bin/tsc --noEmit
yarn lint

# the six same_vote sites are untouched
git diff --stat   # only source/renderer/app/stores/VotingStore.spec.ts
grep -n "same_vote" source/renderer/app/stores/VotingStore.ts                    # :62 only
grep -n "same_vote" source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx  # :89 only
```

`VotingStore.spec.ts` is a pre-existing file — run **no** prettier on it. `nix fmt` cannot run in
this container; note it as a pre-merge obligation for the user.

##### Step 5: Commit

```
test(gov): task-148 pin the same_vote server path behind the client-side disable
```

#### Acceptance

- **AC-1** (regression case for the `same_vote` server error path remains green) — Step 2's store
  case plus Step 3's render case; together they cover the code → error-code → copy chain end to end.
- **AC-2** (`VotingStore.expectedInitializeVPDelegationTxErrors.same_vote` is still reachable) —
  Step 2 asserts the constant still contains `'same_vote'` **and** that a thrown
  `ApiError({ code: 'same_vote' })` is mapped through `parseApiCode` to
  `{ success: false, errorCode: 'same_vote' }` rather than collapsing to `'generic'`. Step 3 proves
  the renderer still translates that code, from a starting state the client-side disable does not
  block. No acceptance criterion is left unsatisfied by this row.
