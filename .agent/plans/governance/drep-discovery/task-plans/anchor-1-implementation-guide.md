# anchor-1 — Implementation Guide

> **Phase:** `anchor-1` — "Anchor 1 — Hardened anchor fetch + verify + givenName render" (riskLevel high) |
> **Date:** 2026-07-29 |
> **PRD:** [anchor-1-PRD.md](anchor-1-PRD.md) |
> **Tracker:** [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json) |
> **Review log:** [anchor-1-code-review.md](anchor-1-code-review.md)
>
> Every `file:line` anchor below was verified against branch `feat/drep-discovery`
> at `bf112d9f8` (pre-implementation), working tree clean. Line numbers shift as
> tasks land — **re-anchor by the quoted content, never by the number.** Run every
> command from the worktree root.

This guide is written to the **small-model bar**. A smaller, less capable model must be able to implement each task in
it end-to-end from this guide *alone* — with no access to the PRD, the plan (`governance-drep-discovery-plan.md`), the
designs, the research notes, the tracker JSON, or any orchestrator reasoning. Everything a step needs is inlined here:
the current code is quoted, not pointed at; every exact file path, exported symbol, TypeScript signature, IPC channel
constant, i18n key and both locales' strings are spelled out; every judgment call is pre-resolved under "Resolved
judgment calls (do not revisit)". If a step still appears to need judgment, that is a defect in this guide — resolve it
by following the closest quoted seam, and record the gap in the review log rather than inventing a new shape.

## Table of contents

Task blocks appear below in build order. Each `### ` heading uses the task's tracker title verbatim.

| # | task | heading | subject | primary target |
|---|---|---|---|---|
| 1 | task-152 | [task-152 — Harden openExternal for anchor URLs (HTTPS-only allow-list)](#task-152--harden-openexternal-for-anchor-urls-https-only-allow-list) | HTTPS-only scheme allow-list before `shell.openExternal` | `source/main/ipc/open-external-url.ts` |
| 2 | task-149 | [task-149 — Add hardened anchor fetch service](#task-149--add-hardened-anchor-fetch-service) | bounded raw-bytes fetch + transport guards + sanitization floor | `source/main/governance/AnchorFetchService.ts` |
| 3 | task-150 | [task-150 — Hash-verify, cache, and parse DRep anchor bytes](#task-150--hash-verify-cache-and-parse-drep-anchor-bytes) | Blake2b-256 verify → immutable hash-keyed cache → parse → IPC | `source/main/governance/AnchorVerificationService.ts` |
| 4 | task-151 | [task-151 — Render verified givenName with source label + expose metadata-completeness](#task-151--render-verified-givenname-with-source-label--expose-metadata-completeness) | store enrichment, `verifiedName`, detail render, source labels, the D-5c https link gate | `source/renderer/app/stores/GovernanceStore.ts` |
| 5 | task-172 | [task-172 — Ground DRepCategoryBadge in cohort membership and activate the High value category](#task-172--ground-drepcategorybadge-in-cohort-membership-and-activate-the-high-value-category) | classifier rewrite + High value + tooltip corrections | `source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx` |

## Implementation order

```
152 → 149 → 150 → 151 → 172
```

This order is **dependency-forced by the tracker**, not a preference. Each edge below is quoted from
`.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`:

- **task-152 first.** `"dependencies": []` (`governance-drep-discovery-plan-tasks.json:1662`) — the only anchor-1 task
  with no incoming edge. It is also the gate: locked invariant #3 requires anchor URLs to open only through the
  HTTPS-only-hardened `open-external-url` path, so it must land *before* task-151 renders an anchor URL as a link.
- **task-149 after task-152.** task-149's own edge is `"dependencies": [ "task-104" ]` (`:1599-1600`), and task-104 is
  `complete`, so task-149 is unblocked from the start; it is sequenced second only because task-152 is free and gates
  the render surface. No edge forbids swapping 152/149 — but keep this order so the render gate is in place first.
- **task-150 after task-149.** `"dependencies": [ "task-149" ]` (`:1622-1623`). task-150 consumes S-1's
  `fetchAnchorBytes`, which task-149 creates.
- **task-151 after task-150.** `"dependencies": [ "task-150", "task-116" ]` (`:1640-1642`); task-116 is `complete`.
  task-151 consumes S-2's `resolveVerifiedAnchor` across the IPC seam S-4.
- **task-172 after task-151.** `"dependencies": [ "task-151", "task-118", "task-119" ]` (`:1677-1680`); task-118 and
  task-119 are both `complete`. task-172's classifier consumes the verified-metadata-completeness signal (S-5, S-7)
  that task-151 produces.

## Locked invariants

All fourteen project-wide locked invariants, quoted verbatim from
`.agent/plans/governance/drep-discovery/prompt.md:93-138`. They bind every task in this guide whether or not a task
section repeats them. Where a task section restates one inline, the two must agree; if they ever disagree, **this list
wins**.

1. `prompt.md:98-100` — "**Local-first.** Discovery data comes only from the local node via the main-process
   `GovernanceQueryService`. No hosted explorers, indexers, GovTool, Koios, Blockfrost, or public governance APIs."
   *Bites:* **task-149** — the anchor fetch is the only outbound call the feature makes, and only to the DRep's own
   registered anchor URL. No explorer, indexer or aggregator, ever.
2. `prompt.md:101-105` — "**Sanitization floor (inherited by every slice).** No DRep id, no `abstain` /
   `no_confidence` literal, no CIP-129/CIP-105 bech32 string in any logger, analytics, or electron-store payload —
   re-asserted via the task-111 spy suite in every slice. The task-168 DRep-state snapshot is the one documented
   exception: public on-chain directory data that deliberately bypasses `filterLogData` and must never include the
   user's own vote."
   *Bites:* **task-149** (owns the full discharge — twelve `sensitiveData` additions, the first main-process spy case,
   the docblock renarrow) and **task-151** (a verified name is a DRep identity and must never be logged).
3. `prompt.md:106-111` — "**Anchor transport-security floor.** The full anchor-1 guard set (TLS on, redirects off,
   ≤10s timeouts, ~1 MB cap, JSON content-type allow-list, SSRF + DNS-rebinding mitigation, Blake2b-256 hash-verify
   before parse/cache/render, immutable hash-keyed cache) lands complete in `anchor-1` and is never thinned. No
   anchor-derived content renders without verification + a verified off-chain source label. Anchor URLs open only
   through the HTTPS-only-hardened `open-external-url` path (task-152)."
   *Bites:* **task-152, task-149, task-150, task-151** — this is the phase's whole reason for existing. Nothing in it
   may be relaxed "temporarily".
4. `prompt.md:112-114` — "**No second delegation backend.** Selection supplies a DRep ID to the existing
   `delegateVotes` / `VotingStore` signing paths via React Router `location.state` only. `VotingStore` never reads
   `GovernanceStore` directly."
   *Bites:* **task-151** — the verified name is display-only; it must not reach any signing or delegation path.
5. `prompt.md:115-116` — "**Lovelace losslessness.** `json-bigint` lossless parse → decimal-string IPC → renderer
   `BigNumber` rehydration. Never route raw `JSONbig` objects across IPC or into observables."
   *Bites:* **task-172** — the cohort median is BigNumber arithmetic; never `Number(…)`, `parseInt`, unary `+`, or
   `.toNumber()` on a lovelace value.
6. `prompt.md:117-120` — "**CLI discipline.** Bulk `--all-dreps` once per refresh — per-DRep CLI invocations are
   forbidden. Network flag (`--mainnet` / `--testnet-magic <N>`) derives from node config only, never from
   renderer/IPC input. Socket goes through `CARDANO_NODE_SOCKET_PATH` in `spawn.env`, not argv. Era token `latest`
   with `conway` fallback."
   *Bites:* **task-149, task-150** — the anchor path adds no CLI call at all and must never route through
   `GovernanceQueryService`.
7. `prompt.md:121-124` — "**Default cohort is binding.** Exclude top 35 by voting power; up to the next 200 eligible
   (active, remaining `drepActivity` > 6 epochs, completed metadata when available), randomized. The 6-epoch floor is
   binding in production — fixtures that violate it must not ship. The default cohort IS the \"Recommended\" sort: no
   Recommended tab, no per-card Recommended badge."
   *Bites:* **task-172** — cohort membership is read from `GovernanceStore.defaultCohort`, never re-derived.
8. `prompt.md:125-127` — "**Badges are informational only.** The category badge (slice-5: Primary / Threshold /
   Non-metadata; High value only after anchor-1) never reorders, filters, or overrides the cohort."
   *Bites:* **task-172** — no ordering, filtering, search, favorites or cohort code path may read `getDRepCategory` or
   `DRepCategory`. Flow is one-way: `defaultCohort` → `cohortContext` → classifier.
9. `prompt.md:128-129` — "**No auto-delegation.** Daedalus never picks a delegation. The `noDelegation` state shows
   the CIP-1694 reward-withdrawal warning + CTA."
   *Bites:* **task-172** — a High value badge is a label, never a recommendation the app acts on.
10. `prompt.md:130-131` — "**Byte-equality.** CIP-129, CIP-105, and the signed payload `vote.id` remain byte-equal
    through every identity-display change; on-device DRep ID equals `vote.chosenOption`."
    *Bites:* **task-151** — adding `verifiedName` must not alter any id rendering or the signed payload. Confirmation
    dialog identity is anchor-2 (task-154), not this phase.
11. `prompt.md:132-133` — "**Preliminary copy.** Every new en-US and ja-JP string keeps the leading `!!!` marker.
    Removing `!!!` is a release-end manual review, never a per-slice task."
    *Bites:* **task-151, task-172** — every key either mints or rewords must carry `!!!` in both catalogs.
12. `prompt.md:134` — "**Favorites are per-device** via Electron local store — not per-wallet, not synced."
    *Bites:* **task-172** — favorites is one of the out-of-cohort surfaces the classifier must classify correctly; it
    gains no new persisted field.
13. `prompt.md:135` — "**`Abstain` / `No Confidence` are form-only sentinels**, never DRep directory entries."
    *Bites:* **task-151, task-172** — no anchor fetch, no verified name and no category badge for a sentinel.
14. `prompt.md:136-138` — "**DRep status grounding.** Canonical on-chain status is `active | inactive`
    (`currentEpoch >= expiry`); `expiring` is renderer-derived display state; `retired` is deferred until a distinct
    unregistration signal exists."
    *Bites:* **task-172** — `status` is dropped from the classifier signature (S-8); nothing here invents a new status
    value.

**Convergence rule** (`prompt.md:237-242`, also binding): "Prefer the **smallest truthful change** that satisfies the
task, the plan decision it implements, and the locked invariants. Reuse existing seams over new abstractions: the
`RendererIpcChannel` pattern, `GovernanceQueryService` + `governanceChannel`, the `_shared` governance components
(`DRepIdDisplay`, `DRepSourceLabel`, `DRepStatusBadge`), the existing `delegateVotes` request and `VotingStore` signing
flow, and the existing bech32 helpers (no new bech32 dependency)." **anchor-1 adds ZERO npm dependencies** —
`blake2b`, `blakejs`, `bignumber.js` and `electron-store` are already production dependencies, and Node's `https` and
`dns` are builtins.

**Anchor corrections (corpus vs live repo).** Prefer the live repo everywhere. Three ranges quoted in the planning
corpus are off by a line at `bf112d9f8`, with the text itself unchanged: invariant #5 is `prompt.md:115-116` (corpus
said `:115-117`); invariant #11 is `prompt.md:132-133` (corpus said `:132-134`); the Storybook locale comment is
`storybook/stories/governance/DRepDetail.stories.tsx:43-45` (corpus said `:43-46`) and
`source/main/mithril/mithrilNetworkConfig.ts`'s `fetchText` is `:49-69` (corpus said `:48-69`, which leads with a
blank line). Task sections carry their own corrections for anchors inside their diff.

## Environment and verification commands

All repo work happens in the worktree root. `node_modules` is symlinked in, so tooling works.

Runtime measured at `bf112d9f8`: node **v24.16.0**, jest **27.5.1**, prettier **2.1.2**, TypeScript **4.9.5**,
Electron 41.3.0, React 16.14.0, MobX 5.15.7. `node_modules/.bin/tsc --noEmit` is **clean at baseline**, so any new
TypeScript error is attributable to your change.

```bash
node_modules/.bin/tsc --noEmit                 # clean at baseline
yarn compile                                   # exit 0, ~22s
node_modules/.bin/jest --testPathPattern=<p> --no-coverage --runInBand
yarn lint                                      # exit 0, ~5591 pre-existing warnings
node_modules/.bin/prettier --write <explicit paths>
yarn i18n:manage                               # only when copy changed
```

**Never run these** — each is broken or destructive here, and a red result from one is not a signal about your change:

| Command | Why it is forbidden |
|---|---|
| `yarn jest`, `npx jest`, `yarn test:jest` | node-24 `devEngines.node` npm error. It aborts before Jest starts — not a test failure. Use `node_modules/.bin/jest` directly. |
| `yarn prettier` | the script embeds a repo-wide `"**/*.*"` glob and rewrites ~238 unrelated files. Always pass explicit paths. |
| `git stash` | the stash stack is shared across worktrees. Use `git restore` / `git checkout -- <paths>` instead. |
| `yarn check:all` | red at HEAD for reasons unrelated to anchor-1. |
| `yarn storybook:build` | red at HEAD (storybook manager-webpack JSX-loader gap). `yarn storybook` (dev server) is the real Storybook floor. |
| `prettier` on `source/renderer/app/containers/voting/Governance.tsx` | prettier 2.1.2 cannot parse its line-4 inline type import and **exits 2**. Exclude this file from every prettier invocation. |

Two more environment facts that change what you may claim:

- **`nix` is absent in this devcontainer, so `nix fmt` cannot run.**
  `node_modules/.bin/prettier --write <explicit paths>` is the substitute. **Running `nix fmt` before merge remains an
  outstanding user-owned obligation** and must be recorded as such — never hidden, never reported as done.
- **There is no network and no browser here.** Any live anchor fetch (e.g. to `sipo.tokyo`) and any visual or ja-JP
  overflow pass is **OWED**, never faked green. Every transport guard is proven against mocked `https` / `dns`; every
  anchor body used in a test is a committed fixture with a generated digest. `storybook/` is outside Jest's `roots`
  (`jest.config.js`: `roots: ['<rootDir>/tests', '<rootDir>/source']`), so nothing under `storybook/` is test-provable.
- `tests/jest/governance/GovernanceCliArgvSmoke.spec.ts` **self-skips** because `cardano-cli` is off PATH. That is by
  design, not a broken suite.
- `yarn i18n:manage` writes **both catalogs and `translations/messages.json`**. `git restore` anything it touched that
  was clean at HEAD, unless the run is the one that owns that catalog diff.

**Sanitization re-proof must cite BOTH anchors.** Running only the security suite gives a green that never touched half
of the live surface. Every sanitization claim in this guide runs and cites both commands:

```bash
node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="containers/voting/VotingGovernancePage.spec" --no-coverage --runInBand
```

Baseline for the first: **1 suite / 26 tests, green** (measured at `bf112d9f8`).

## Formatting, commit, and comment conventions

**Code comments.** The default is *none*. Comment only where the logic is not self-evident, and then write 1–3 plain
sentence-case lines stating the invariant, the constraint, or the *why*. Never the *what*, never change history, never
a defence of correctness. **Never task ids, `CAT-*` / `CP-*` labels, review labels, plan names or PR numbers — in
comments OR in test names.** No ALL-CAPS emphasis.

**Commits.** Exactly **one commit per task**, a single Conventional Commits subject line, no body, no
`Co-Authored-By` trailer:

```
<type>(gov): task-NNN <short imperative summary>
```

Task ids **do** belong in commit subjects (they are banned from comments and test names, not from subjects). Commit
only task-relevant files. The task is not done until its commit exists.

**Formatting.** `nix fmt` cannot run here; format with `node_modules/.bin/prettier --write` over the **explicit paths
you changed** — never a glob, never `yarn prettier`, and never
`source/renderer/app/containers/voting/Governance.tsx`.

**i18n.** Run `yarn i18n:manage` **only in a task that changed copy**, and only after the message descriptors are in
place — it extracts from `source/**/*.{ts,tsx}` and writes both catalogs plus `translations/messages.json`. Every new
or reworded en-US and ja-JP string keeps its leading `!!!` marker (invariant #11). Both catalogs must stay
key-identical. `git restore` any file the run touched that your task does not own.

**Docs cite evidence.** Every factual claim carries a `path:line`. Unknowns are stated as unknown. Quote exactly when
the wording matters.

**Storybook.** Never add a local `IntlProvider` and never author per-locale story variants — the global `StoryWrapper`
decorator owns the English/Japanese toggle at the top of the preview window (binding comment at
`storybook/stories/governance/DRepDetail.stories.tsx:43-45`).

## Cross-task seam contracts

These are the exact seams tasks hand to each other. A task section may reference them by number (`S-1` … `S-9`); the
signatures below are the contract. Do not widen, rename or re-shape one without changing this section.

**S-1 — bounded raw-bytes fetch (task-149 → task-150).** `source/main/governance/AnchorFetchService.ts` (new file;
`source/main/governance/` currently contains only `GovernanceQueryService.ts`)

```ts
export interface AnchorFetchOk   { ok: true;  bytes: Buffer; host: string; contentType: string; byteLength: number }
export interface AnchorFetchFail { ok: false; reason: AnchorFetchErrorType }
export type AnchorFetchResult = AnchorFetchOk | AnchorFetchFail;
export interface AnchorTransport { readonly scheme: string; fetch(url: string): Promise<AnchorFetchResult> }
export const httpsAnchorTransport: AnchorTransport;
export function fetchAnchorBytes(url: string): Promise<AnchorFetchResult>;
```

`fetchAnchorBytes` selects a transport by scheme and returns `UnsupportedScheme` for anything unregistered, `ipfs:`
included — the reserved slot is the *interface*, never a stub implementation. Never throws, never parses JSON, never
writes cache (task-149 AC-8). Node builtin `https` + `dns`; `source/main/mithril/mithrilNetworkConfig.ts:49-69` (`fetchText`) is an
**API-shape reference only, not a security model** (no timeout, no size cap, no content-type check).

**S-2 — verify + cache + parse (task-150 → task-151, via IPC).**
`source/main/governance/AnchorVerificationService.ts` (new file)

```ts
export function resolveVerifiedAnchor(anchor: DRepAnchorPresence): Promise<DRepAnchorResult>;
```

Binding order: cache read (re-verify digest) → on miss `fetchAnchorBytes` → Blake2b-256 over the bounded bytes vs
`anchor.hash` → **only then** cache write and `JSON.parse` (task-150 AC-1, AC-2). Never throws.

**S-3 — cache primitives (task-150, and across restarts).** `source/main/governance/anchorCache.ts` (new file)

```ts
export function readVerifiedAnchorBytes(hash: string): Buffer | null;
export function writeVerifiedAnchorBytes(hash: string, bytes: Buffer): void;
```

Both reject any `hash` failing `/^[0-9a-f]{64}$/` before touching the filesystem.

**S-4 — the IPC seam (task-150 → task-151).** `source/common/ipc/api.ts`: `GOVERNANCE_DREP_ANCHOR_CHANNEL`,
`GovernanceDRepAnchorRendererRequest = DRepAnchorPresence`, `GovernanceDRepAnchorMainResponse = DRepAnchorResult`.
`source/renderer/app/ipc/governanceChannel.ts`: `export const governanceDRepAnchorChannel:
RendererIpcChannel<GovernanceDRepAnchorMainResponse, GovernanceDRepAnchorRendererRequest>;`

The supporting response types live in `source/common/types/governance.types.ts` after the shipped
`DRepAnchorPresence` block at `:66-71`:

```ts
export enum AnchorFetchErrorType {
  UnsupportedScheme = 'ANCHOR_UNSUPPORTED_SCHEME', BlockedAddress = 'ANCHOR_BLOCKED_ADDRESS',
  DnsFailed = 'ANCHOR_DNS_FAILED', Redirected = 'ANCHOR_REDIRECTED', HttpStatus = 'ANCHOR_HTTP_STATUS',
  ContentType = 'ANCHOR_CONTENT_TYPE', TooLarge = 'ANCHOR_TOO_LARGE', Timeout = 'ANCHOR_TIMEOUT',
  TlsFailed = 'ANCHOR_TLS_FAILED', Network = 'ANCHOR_NETWORK', HashMismatch = 'ANCHOR_HASH_MISMATCH',
  ParseFailed = 'ANCHOR_PARSE_FAILED', InvalidRequest = 'ANCHOR_INVALID_REQUEST',
}
export interface VerifiedDRepAnchorContent { givenName: string | null }
export type DRepAnchorResult =
  | { status: 'verified'; content: VerifiedDRepAnchorContent; host: string; fetchedAt: number }
  | { status: 'unavailable'; reason: AnchorFetchErrorType };
```

The request is the on-chain anchor pair and **nothing else — no `drepId`**, so main is structurally incapable of
logging a DRep id on this seam (invariant #2); the renderer correlates the response itself. **The handler never
rejects**: 404, 3xx, blocked address, size overflow, hash mismatch, parse failure and malformed request all resolve as
`{ status: 'unavailable', reason }`. `host` is computed in main from the already-validated URL and shipped in the
response; the renderer must not re-parse the URL.

**S-5 — store enrichment (task-151), the verified-completeness producer.**
`source/renderer/app/stores/GovernanceStore.ts`

```ts
export type AnchorEnrichEntry =
  | { state: 'loading';     hash: string }
  | { state: 'verified';    hash: string; givenName: string | null; host: string }
  | { state: 'unavailable'; hash: string; reason: AnchorFetchErrorType };
@observable anchorStateByDRepId: Map<string, AnchorEnrichEntry>;   // absent key == idle
@action fetchAnchorContent(drepId: string, anchor: DRepAnchorPresence): Promise<void>;
private _applyVerifiedNames(entries: AppDRepDirectoryEntry[]): AppDRepDirectoryEntry[];
```

`fetchAnchorContent` no-ops when the map already holds `loading` or a terminal state **for the same hash**; a changed
on-chain hash re-triggers. On `verified` it writes `verifiedName` into both `drepIndex` and `drepList` (rebuilt
together at `GovernanceStore.ts:288-295`). `_applyVerifiedNames` runs at the end of `fetchDRepList` and
`_enrichVotingPower` — both rebuild the list from the IPC payload and would otherwise drop the projection; the map
stays authoritative.

**S-6 — container trigger (task-151).** `source/renderer/app/containers/governance/DRepDetailPage.tsx` adds a second
`reaction` beside the `isNodeInSync` one at `:43-50`:

```ts
reaction(
  () => governanceStore.drepIndex.get(this.props.match.params.drepId)?.anchor ?? null,
  (anchor) => { if (anchor) governanceStore.fetchAnchorContent(drepId, anchor); },
  { fireImmediately: true }
);
```

Deep links mount before the list resolves, so `fireImmediately` plus the reaction covers both orders.

**S-7 — cohort + completeness input (GovernanceStore → task-172).** Declared in `GovernanceStore.ts` beside
`AppDRepDirectoryEntry` (`:20-31`):

```ts
export interface DRepCohortContext {
  memberIds: Set<string> | null;          // null when defaultCohort is null — never an empty Set for that case
  verifiedMetadataIds: Set<string>;       // ids whose anchor passed Blake2b-256 verification and parsed
  medianVotingPower: BigNumber | null;
}
@computed get cohortContext(): DRepCohortContext;
@computed get cohortMedianVotingPower(): BigNumber | null;
```

`memberIds` = `new Set(defaultCohort.map(e => e.drepId))`, or `null` when `defaultCohort` is `null`.
`verifiedMetadataIds` derives from `anchorStateByDRepId` entries with `state === 'verified'` — **never from
`anchor != null`**. The prop name at every call site is `cohort`, threaded store → container → component; a component
never imports the store at runtime.

**S-8 — classifier and source label (task-172, task-151).**

```ts
export type DRepCategory = 'highValue' | 'threshold' | 'primary' | 'nonMetadata';
export function getDRepCategory(
  entry: Pick<AppDRepDirectoryEntry, 'drepId' | 'votingPower' | 'drepActivity'>,
  cohort: DRepCohortContext): DRepCategory;

// DRepSourceLabel.tsx, extending :18
export type DRepSourceLabelVariant = 'on-chain' | 'on-chain-anchor-reference'
  | 'verified-off-chain' | 'unverified-anchor' | 'anchor-unavailable';
type Props = { source: DRepSourceLabelVariant; host?: string; className?: string; intl };
```

`host` is passed only for `'verified-off-chain'`, taken from the IPC response (S-4), and interpolates into the
tooltip. New child component:
`source/renderer/app/components/governance/drep-detail/DRepDetailAnchorContent.tsx`, props
`{ state: AnchorEnrichEntry | null; intl }`, rendered by `DRepDetailAnchorSection` beneath the existing URL / hash /
Source rows (`DRepDetailAnchorSection.tsx:49-79`), replacing nothing.

**S-9 — the https link gate (task-151, D-5c).** `DRepDetailAnchorSection` gains
`onOpenExternalLink: (url: string) => void`, threaded `DRepDetailPage` → `DRepDetail` → `DRepDetailAnchorSection`
from `stores.app.openExternalLink`. The URL row renders an `<a href target="_blank" rel="noopener noreferrer">` only
when `new URL(anchor.url).protocol === 'https:'`; otherwise the inert `<dd>` text of today. **task-152 builds none of
this** — its AC-3 gates anchor-URL rendering *on* the hardening landing, so it touches no governance component.

**`'unverified-anchor'` has no production emitter in anchor-1:** a hash mismatch maps to *Anchor unavailable*, and
main verifies before responding, so the renderer never sees fetched-but-unverified content. The variant and its copy
are minted anyway because the design's five-label set is the contract and Storybook renders it — stated here so a
reviewer does not hunt for the missing path.

### task-152 — Harden openExternal for anchor URLs (HTTPS-only allow-list)

Tracker row: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json:1655-1669`.
`dependencies: []` — this is the **first** task of `anchor-1` and nothing else may land before it.

**Files touched (five, no more):**

1. `source/renderer/app/utils/network.ts` — edit `getNetworkExplorerUrl` (the one real non-https producer).
2. `tests/common/unit/networks.spec.ts` — append two `describe` blocks to the **existing** spec.
3. `source/main/ipc/open-external-url.ts` — the hardening itself.
4. `source/main/ipc/open-external-url.spec.ts` — **new file**, the first spec this module has ever had.
5. `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json` — the task-152 row.

**Files deliberately NOT touched:** `source/main/ipc/index.ts` (the exported channel name is unchanged, so
`:22` and `:52-53` stay as they are), `source/renderer/app/ipc/open-external-url.ts`,
`source/renderer/app/stores/AppStore.ts`, `source/common/ipc/api.ts`, and **every** file under
`source/renderer/app/components/governance/`.

#### Context

`source/main/ipc/open-external-url.ts` is 15 lines and does no validation at all. Entire current file:

```ts
import { shell } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { OPEN_EXTERNAL_URL_CHANNEL } from '../../common/ipc/api';
import type {
  OpenExternalUrlMainResponse,
  OpenExternalUrlRendererRequest,
} from '../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const openExternalUrlChannel: MainIpcChannel<
  OpenExternalUrlRendererRequest,
  OpenExternalUrlMainResponse
> = new MainIpcChannel(OPEN_EXTERNAL_URL_CHANNEL);
openExternalUrlChannel.onReceive((url: OpenExternalUrlRendererRequest) =>
  shell.openExternal(url) ? Promise.resolve() : Promise.reject()
);
```

The wire contract, `source/common/ipc/api.ts:180-182` — the request is an unconstrained `string`:

```ts
export const OPEN_EXTERNAL_URL_CHANNEL = 'OPEN_EXTERNAL_URL_CHANNEL';
export type OpenExternalUrlRendererRequest = string;
export type OpenExternalUrlMainResponse = void;
```

Registration, `source/main/ipc/index.ts:22` and `:52-53` — a bare expression reference, not a handler call:

```ts
import { openExternalUrlChannel } from './open-external-url';
// …
  // eslint-disable-next-line no-unused-expressions
  openExternalUrlChannel;
```

Renderer client, `source/renderer/app/ipc/open-external-url.ts:8-12` (note the **reversed** type-param order
vs main). Its sole consumer is `source/renderer/app/stores/AppStore.ts:8` (import) and `:80-83`:

```ts
  openExternalLink(url: string, event?: MouseEvent): void {
    if (event) event.preventDefault();
    openExternalUrlChannel.send(url);
  }
```

`send` is fire-and-forget — `AppStore` ignores the returned promise — so a main-side rejection is never
surfaced to the user. **`openExternalLink` is app-wide, not governance-scoped:** 104 references across 53
files under `source/` at HEAD (measured). This hardening changes behaviour for every one of them.

`MainIpcChannel.onReceive(handler)` (`source/main/ipc/lib/MainIpcChannel.ts`) delegates to
`IpcChannel.onReceive` (`source/common/ipc/lib/IpcChannel.ts`), which wraps the handler in
`try { … } catch (error) { event.sender.send(this._responseChannel, false, error) }`. A thrown/rejected
handler is therefore transported as a failed response and nothing more.

The **only** non-https URL producer that reaches `openExternalLink`, `source/renderer/app/utils/network.ts:36-43`:

```ts
export const getNetworkExplorerUrl = (network: string): string => {
  const protocol =
    network === MAINNET || network === TESTNET || network === DEVELOPMENT
      ? 'https://'
      : 'http://';
  const uri = getNetworkExplorerUri(network);
  return `${protocol}${uri}`;
};
```

Measured at HEAD: `mainnet`/`testnet`/`development` → `https://…`, but `staging` → `http://explorer.staging.cardano.org`
and every unlisted network (`preprod`, `preview`, `shelley_qa`, `vasil_dev`, `selfnode`) →
`http://explorer.cardano.org`. The unlisted branch already returns the *mainnet* host (`network.ts:34`), so
`http://explorer.cardano.org` was wrong before this task existed. That URL reaches `openExternalLink` through
`StakingRewardsPage.tsx:39-42`, `paper-wallet-certificate/InstructionsDialog.tsx:145`,
`paper-wallet-certificate/CompletionDialog.tsx:123`, and via `getNetworkExplorerUrlByType` (`network.ts:70-72`)
through `WalletSummaryPage.tsx:134` and `WalletTransactionsPage.tsx:46`.

The anchor URL that motivates all this is still inert text and must stay that way in this commit —
`source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection.tsx:55-57`:

```tsx
            {/* Deliberately inert text: no anchor may be fetched, rendered as
                a link, or opened before the hardened anchor pipeline lands. */}
            <dd className={styles.anchorValue}>{anchor.url}</dd>
```

An existing jest spec already owns this module's siblings: `tests/common/unit/networks.spec.ts`, 4 tests,
green at HEAD (measured). `yarn lint` covers `source storybook utils` only — **not** `tests/`.

#### Locked invariants this change must not break

- **#3 Anchor transport-security floor** (`prompt.md:106-111`, verbatim): "The full anchor-1 guard set (TLS on,
  redirects off, ≤10s timeouts, ~1 MB cap, JSON content-type allow-list, SSRF + DNS-rebinding mitigation,
  Blake2b-256 hash-verify before parse/cache/render, immutable hash-keyed cache) lands complete in `anchor-1`
  and is never thinned. No anchor-derived content renders without verification + a verified off-chain source
  label. **Anchor URLs open only through the HTTPS-only-hardened `open-external-url` path (task-152).**"
- **#2 Sanitization floor** (`prompt.md:101-105`): no DRep id, no `abstain` / `no_confidence` literal, no
  CIP-129/CIP-105 bech32 string in any logger, analytics or electron-store payload. `filterLogData`
  (`source/common/utils/logging.ts:21-74`) is **renderer-only** — `rg filterLogData source/main` returns zero
  hits and `source/main/utils/logging.ts:26-33` forwards `data` to electron-log untouched — so on this
  main-process seam the discipline is hand-enforced: log the scheme, never the URL, never the error object.
- **Convergence rule** (`prompt.md:237-242`): reuse the existing seams. No new package, no new channel, no
  governance-local wrapper around `shell.openExternal`.
- **Comments:** default is none; only where the logic is not self-evident, then 1–3 plain sentence-case lines
  stating the invariant or the why. Never task ids, review labels, ALL-CAPS or change history — in comments
  **or** in test names.
- **Commit:** exactly one, one Conventional Commits subject line, no body, no `Co-Authored-By`.

#### Resolved judgment calls (do not revisit)

- **The allow-list is exactly `https:` — nothing wider.** Not `http:`, not `mailto:`, not `ipfs:`.
- **Rejection stays silent to the renderer.** `onReceive` keeps returning a rejected promise and
  `AppStore.openExternalLink` keeps `send()`. Converting `send` → `request` would be an app-wide change across
  every caller for zero benefit — they all already ignore the result. Do **not** touch `AppStore.ts`.
- **Main logs the scheme, never the URL:** exactly
  `logger.warn('Open external URL: rejected non-https scheme', { scheme })`, where `scheme` is
  `new URL(url).protocol` or the literal `'unparseable'` when `new URL` throws.
- **The renderer gates the offer, main gates the action, and the two guards stay independent.** Rendering the
  anchor URL as a link (only when it parses as `https:`) is **task-151's Step 9**, not this task's. AC-3 says
  anchor-URL rendering stays *gated on* this hardening landing, so a task-152 commit that rendered the link would
  fail its own criterion. This commit adds no link anywhere and touches no governance component.
- **The audit's one real hit is fixed in the same commit.** `getNetworkExplorerUrl` is changed to always emit
  `https://`. Landing the guard without this silently breaks explorer links on every non-mainnet/testnet build.
  The targets are public explorer hostnames (`source/renderer/app/config/urlsConfig.ts:3-5`), never loopback.
- **Case handling is free.** The WHATWG `URL` parser lowercases the scheme, so `JavaScript:alert(1)` yields
  `protocol === 'javascript:'` and `HTTPS://example.com` yields `'https:'`. Do not add a `toLowerCase()` pass.
- **`shell.openExternal` receives the original string, not a re-serialised URL.** Round-tripping through
  `parsed.toString()` would normalise inputs that work today; the guard is the only behaviour change.
- **The new explorer test extends `tests/common/unit/networks.spec.ts`** rather than creating
  `source/renderer/app/utils/network.spec.ts` — that file already owns `getNetworkExplorerUri`, and keeping the
  spec under `tests/` leaves the `yarn lint` warning baseline untouched by it.
- **Spec location for the channel:** `source/main/ipc/open-external-url.spec.ts`, colocated, with
  `@jest-environment node`. Precedent: `source/main/ipc/{chainStorageChannel,mithrilBootstrapChannel,mithrilPartialSyncChannel}.spec.ts`.
  A new spec under `source/` **does** move the `yarn lint` warning baseline; that is expected, not a regression.

#### Step 1: Audit every caller for a non-https URL before tightening anything

Run all three from the repo root and read the output before editing:

```bash
grep -rn "openExternalLink\|openExternalUrlChannel" source/ --include=*.ts --include=*.tsx | wc -l
grep -rn "http://" source/ --include=*.ts --include=*.tsx
grep -rn "mailto:" source/ --include=*.ts --include=*.tsx
grep -rn "getNetworkExplorerUrl" source/ --include=*.ts --include=*.tsx
```

Measured at HEAD: **104** call-site references across 53 files; **9** `http://` literals; **0** `mailto:` hits;
**13** `getNetworkExplorerUrl` references. Classify every `http://` hit exactly as follows — the nine at HEAD
are already classified, so a tenth means the tree has moved and you must classify it yourself:

| Hit | Disposition |
|---|---|
| `source/renderer/app/utils/network.ts:40` | **The one real producer.** Fixed in Step 2. |
| `source/renderer/app/components/static/About.tsx:136` | Cosmetic only — `:135` opens `'https://daedaluswallet.io'`; `:136` is the visible `label`. Leave it; record it. |
| `source/renderer/app/analytics/MatomoClient.ts:65` | Synthetic analytics host, never passed to `openExternalLink`. Leave. |
| `source/renderer/app/config/urlsConfig.ts:9` | A comment about build-time newsfeed overrides. Leave. |
| `.../wallet-token-picker/WalletTokenPicker.stories.tsx:27` | Storybook fixture. Leave. |
| `source/main/windows/main.ts:84`, `CardanoSelfnodeLauncher.ts:35`, `config.ts:185`, `mithrilCommandRunner.spec.ts:34` | Loopback / dev-server URLs in the main process; never reach this channel. Leave. |

Rules for anything the audit turns up that is **not** in that table:

- An `http:` URL that genuinely reaches `openExternalLink` and points at a public host that serves https →
  change the producer to `https://` in **this** commit and pin it with a test, exactly as Step 2 does.
- An `http:` URL pointing at loopback or a non-https-capable host → **stop and record it as a blocker**; do not
  widen the allow-list and do not land the guard until it is resolved.
- A `mailto:` URL reaching `openExternalLink` → **stop**. The allow-list is not widened. Record it.

Zero `mailto:` callers exist at HEAD, so no `mailto:` handling is written in this task.

#### Step 2: Make `getNetworkExplorerUrl` always emit https

In `source/renderer/app/utils/network.ts`, replace the whole `getNetworkExplorerUrl` body (`:36-43`) with:

```ts
export const getNetworkExplorerUrl = (network: string): string => {
  const uri = getNetworkExplorerUri(network);
  return `https://${uri}`;
};
```

Do not touch `getNetworkExplorerUri`, `getNetworkExplorerUrlByType`, `getNewsURL` or `getNewsHashURL`. The
`MAINNET` / `STAGING` / `TESTNET` / `DEVELOPMENT` imports at `:14-19` all remain referenced elsewhere in the
file (`:22`, `:26`, `:30`, `:54`, `:78-90`, `:100-113`), so the import block is unchanged.

#### Step 3: Extend `tests/common/unit/networks.spec.ts`

Widen the existing import on `:1` and add the two new `describe` blocks at the end of the file. Leave the four
existing cases untouched.

```ts
import {
  getNetworkExplorerUri,
  getNetworkExplorerUrl,
  getNetworkExplorerUrlByType,
} from '../../../source/renderer/app/utils/network';
import {
  MAINNET,
  STAGING,
  TESTNET,
  DEVELOPMENT,
} from '../../../source/common/types/environment.types';
```

Appended verbatim after the closing `});` of the last existing `describe`:

```ts
describe('Function getNetworkExplorerUrl returns:', () => {
  it.each([MAINNET, TESTNET, DEVELOPMENT, STAGING, 'preprod', 'selfnode'])(
    'an https url for %s',
    (network) => {
      expect(getNetworkExplorerUrl(network).startsWith('https://')).toBe(true);
    }
  );
  it('the staging explorer host over https', () => {
    expect(getNetworkExplorerUrl(STAGING)).toBe(
      'https://explorer.staging.cardano.org'
    );
  });
});
describe('Function getNetworkExplorerUrlByType returns:', () => {
  it('an https url for a network outside the localised set', () => {
    expect(getNetworkExplorerUrlByType('tx', 'abc', STAGING, 'en-US')).toBe(
      'https://explorer.staging.cardano.org/txabc'
    );
  });
});
```

`'https://explorer.staging.cardano.org/txabc'` is the exact post-fix output: `STAGING` is outside the
`MAINNET || TESTNET` branch at `network.ts:54`, so there is no locale prefix and no query-string prefix.

#### Step 4: Rewrite `source/main/ipc/open-external-url.ts`

Replace the whole file with exactly this. It is prettier-2.1.2-clean and eslint-error-free as written:

```ts
import { shell } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { OPEN_EXTERNAL_URL_CHANNEL } from '../../common/ipc/api';
import type {
  OpenExternalUrlMainResponse,
  OpenExternalUrlRendererRequest,
} from '../../common/ipc/api';
import { logger } from '../utils/logging';

const ALLOWED_EXTERNAL_URL_PROTOCOL = 'https:';

// The URL parser lowercases the scheme, so case-variant input needs no extra handling.
const externalUrlScheme = (url: string): string => {
  try {
    return new URL(url).protocol;
  } catch {
    return 'unparseable';
  }
};

export const isAllowedExternalUrl = (url: string): boolean =>
  externalUrlScheme(url) === ALLOWED_EXTERNAL_URL_PROTOCOL;

export const handleOpenExternalUrl = (
  url: OpenExternalUrlRendererRequest
): Promise<OpenExternalUrlMainResponse> => {
  if (!isAllowedExternalUrl(url)) {
    logger.warn('Open external URL: rejected non-https scheme', {
      scheme: externalUrlScheme(url),
    });
    return Promise.reject(new Error('Rejected non-https external URL'));
  }
  return shell.openExternal(url) ? Promise.resolve() : Promise.reject();
};

// IpcChannel<Incoming, Outgoing>
export const openExternalUrlChannel: MainIpcChannel<
  OpenExternalUrlRendererRequest,
  OpenExternalUrlMainResponse
> = new MainIpcChannel(OPEN_EXTERNAL_URL_CHANNEL);
openExternalUrlChannel.onReceive(handleOpenExternalUrl);
```

Four things that are deliberate and must not be "improved":

1. The allowed branch keeps the original `shell.openExternal(url) ? Promise.resolve() : Promise.reject()`
   expression verbatim. Do not `await` it — that would change the behaviour of the success path, which is out
   of scope.
2. The guard runs **before** `shell.openExternal` is referenced at all, which is what AC-1 asks for.
3. The rejection reason is a fixed `Error` whose message contains no URL, so nothing leaks through the
   `event.sender.send(this._responseChannel, false, error)` path in `IpcChannel.onReceive`.
4. `handleOpenExternalUrl` and `isAllowedExternalUrl` are exported so the spec can drive them directly; the
   channel wiring stays a one-liner and the exported channel name is unchanged, so `index.ts` needs no edit.

#### Step 5: Create `source/main/ipc/open-external-url.spec.ts`

New file, exactly this. `no-script-url` is an **error**-level rule in `.eslintrc`, and every `javascript:`
string literal trips it — the file-level disable on line 4 is required or `yarn lint` exits non-zero.

```ts
/**
 * @jest-environment node
 */
/* eslint-disable no-script-url */
import type {} from './open-external-url';

const mockChannels: Array<{ onReceive: jest.Mock }> = [];

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => {
    const channel = { onReceive: jest.fn() };
    mockChannels.push(channel);
    return channel;
  }),
}));

jest.mock('electron', () => ({
  shell: { openExternal: jest.fn(() => Promise.resolve()) },
}));

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));

const { shell } = jest.requireMock('electron');
const { logger } = jest.requireMock('../utils/logging');

const loadModule = () => {
  mockChannels.length = 0;
  let moduleExports;
  jest.isolateModules(() => {
    moduleExports = require('./open-external-url');
  });
  return moduleExports as typeof import('./open-external-url');
};

describe('open-external-url', () => {
  it('opens an https url', async () => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(
      handleOpenExternalUrl('https://example.com/anchor.jsonld')
    ).resolves.toBeUndefined();
    expect(shell.openExternal).toHaveBeenCalledWith(
      'https://example.com/anchor.jsonld'
    );
  });

  it('opens an https url written with an uppercase scheme', async () => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(
      handleOpenExternalUrl('HTTPS://example.com/')
    ).resolves.toBeUndefined();
    expect(shell.openExternal).toHaveBeenCalledTimes(1);
  });

  it.each([
    ['javascript', 'javascript:alert(document.cookie)'],
    ['file', 'file:///etc/passwd'],
    ['data', 'data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg=='],
    ['http', 'http://example.com/anchor.jsonld'],
    ['mixed-case javascript', 'JavaScript:alert(1)'],
    ['unparseable input', 'not a url'],
  ])('rejects %s without reaching the shell', async (_name, url) => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(handleOpenExternalUrl(url)).rejects.toThrow(
      'Rejected non-https external URL'
    );
    expect(shell.openExternal).not.toHaveBeenCalled();
  });

  it('logs the rejected scheme and nothing else', async () => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(
      handleOpenExternalUrl('javascript:alert(1)')
    ).rejects.toThrow();
    expect(logger.warn).toHaveBeenCalledWith(
      'Open external URL: rejected non-https scheme',
      { scheme: 'javascript:' }
    );
  });

  it('logs an unparseable marker when the input is not a url', async () => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(handleOpenExternalUrl('not a url')).rejects.toThrow();
    expect(logger.warn).toHaveBeenCalledWith(
      'Open external URL: rejected non-https scheme',
      { scheme: 'unparseable' }
    );
  });

  it('omits the rejected url from the log payload', async () => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(
      handleOpenExternalUrl('http://user:pw@internal.example/secret')
    ).rejects.toThrow();
    const payload = JSON.stringify(logger.warn.mock.calls);
    expect(payload).not.toContain('internal.example');
    expect(payload).not.toContain('secret');
  });

  it('registers the hardened handler on the channel', () => {
    const moduleExports = loadModule();
    expect(mockChannels).toHaveLength(1);
    expect(mockChannels[0].onReceive).toHaveBeenCalledWith(
      moduleExports.handleOpenExternalUrl
    );
  });

  it('accepts only the https scheme', () => {
    const { isAllowedExternalUrl } = loadModule();
    expect(isAllowedExternalUrl('https://example.com')).toBe(true);
    expect(isAllowedExternalUrl('http://example.com')).toBe(false);
    expect(isAllowedExternalUrl('ipfs://QmHash')).toBe(false);
    expect(isAllowedExternalUrl('')).toBe(false);
  });
});
```

Why the mocks are shaped this way: `IpcChannel` enforces one instance per channel name and throws on a second
construction, and its real `onReceive` calls `receiver.on(...)` on electron's `ipcMain`. Mocking
`./lib/MainIpcChannel` (the shape `chainStorageChannel.spec.ts` and `mithrilBootstrapChannel.spec.ts` already
use) sidesteps both, and `jest.isolateModules` lets each case re-load the module cleanly. The array must be
named with a `mock` prefix — jest's factory hoisting rejects any other out-of-scope reference.

#### Step 6: Confirm nothing renders an anchor link in this commit (AC-3)

```bash
git status --porcelain
git diff --name-only
```

Both must list only the five files named at the top of this section. In particular
`source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection.tsx` must be absent — its
`:55-57` inert-text block and its comment stay exactly as they are. **task-151 Step 9 turns the anchor URL into
an https-gated link**, after this hardening is on disk; do not do it here and do not leave a placeholder for it.

#### Step 7: Format, typecheck, lint

`nix fmt` is unavailable here and stays a user-owned pre-merge obligation.

```bash
node_modules/.bin/prettier --write \
  source/main/ipc/open-external-url.ts \
  source/main/ipc/open-external-url.spec.ts \
  source/renderer/app/utils/network.ts \
  tests/common/unit/networks.spec.ts
```

#### Step 8: Update the tracker row and commit

In `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`, on the task-152 object
at `:1655-1669`: set `"status": "complete"`, and add `"statusReason"`, `"evidence"` and `"updatedAt"` in the
key order used by the completed sibling rows (`id, title, description, status, statusReason, evidence,
updatedAt, priority, estimatedHours, dependencies, targetPath, acceptanceCriteria`). `updatedAt` is
`YYYY-MM-DD`. `evidence` is an array of repo-relative paths, source files first, then plan docs:

```json
"evidence": [
  "source/main/ipc/open-external-url.ts",
  "source/main/ipc/open-external-url.spec.ts",
  "source/renderer/app/utils/network.ts",
  "tests/common/unit/networks.spec.ts",
  ".agent/plans/governance/drep-discovery/task-plans/anchor-1-implementation-guide.md"
]
```

`statusReason` states, in prose: the measured test counts, that the audit found exactly one real non-https
producer and that it was fixed in the same commit, and that no anchor URL is rendered as a link by this change.

One commit, one subject line, no body, no trailer:

```
fix(gov): task-152 restrict open-external-url to the https scheme
```

#### Verify

Run from the repo root. Every number below is measured, not estimated.

```bash
# 1. New channel spec: 1 suite, 13 tests, all green.
node_modules/.bin/jest --testPathPattern="source/main/ipc/open-external-url" --no-coverage --runInBand

# 2. Explorer-url spec: 4 tests at HEAD -> 12 tests after Step 3, all green.
node_modules/.bin/jest --testPathPattern="tests/common/unit/networks" --no-coverage --runInBand

# 3. Typecheck: exit 0. Clean at HEAD, so any error is attributable to this task.
node_modules/.bin/tsc --noEmit

# 4. Full typecheck incl. sass typedefs: exit 0, ~22s.
yarn compile

# 5. Lint: exit 0. The warning count moves up because a new spec landed under source/;
#    the error count must stay at 0.
yarn lint

# 6. Formatting: "All matched files use Prettier code style!"
node_modules/.bin/prettier --check \
  source/main/ipc/open-external-url.ts \
  source/main/ipc/open-external-url.spec.ts \
  source/renderer/app/utils/network.ts \
  tests/common/unit/networks.spec.ts

# 7. Sanitization floor, both anchors. This task adds a main-process logger sink
#    (`{ scheme }`), so cv-2 F-31's two-anchor rule binds it like every other row.
node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" --no-coverage --runInBand
# expect: 1 suite, 26 tests, unchanged from baseline
node_modules/.bin/jest --testPathPattern="containers/voting/VotingGovernancePage.spec" --no-coverage --runInBand
# expect: 1 suite, 27 tests, unchanged from baseline

# 8. Nothing outside the five files changed.
git status --porcelain
```

If run 1 reports fewer than 13 tests, the `it.each` table lost a row. If run 2 reports 4, Step 3 was not saved.

#### Acceptance

| AC (verbatim from the tracker, `:1665-1667`) | How the steps above discharge it |
|---|---|
| "openExternal rejects any URL whose scheme is not https before calling shell.openExternal." | Step 4 puts `if (!isAllowedExternalUrl(url)) { … return Promise.reject(…) }` ahead of every reference to `shell.openExternal` in `handleOpenExternalUrl`, and `isAllowedExternalUrl` compares `new URL(url).protocol` against the single constant `ALLOWED_EXTERNAL_URL_PROTOCOL = 'https:'`. Unparseable input maps to `'unparseable'` and is therefore also rejected. Step 5's `it.each` block proves the rejection for six inputs and asserts `shell.openExternal` was **not** called in each; the `registers the hardened handler on the channel` case proves the guarded function is the one actually wired to `onReceive`, so the guard is on the wire and not just in an unused export. **Green.** |
| "Jest coverage asserts rejection of javascript:, file:, and data: URLs." | Step 5's `it.each` table names `javascript:alert(document.cookie)`, `file:///etc/passwd` and `data:text/html;base64,…` as its first three rows, each asserting `rejects.toThrow('Rejected non-https external URL')` **and** `expect(shell.openExternal).not.toHaveBeenCalled()`. `JavaScript:alert(1)` additionally pins the parser's scheme-lowercasing so a case-variant bypass cannot regress. Verify run 1: 1 suite / 13 tests. **Green.** |
| "Anchor URL rendering remains gated on this hardening landing." | task-152 has `dependencies: []` and is the first task of the phase, so the guard is on disk before any anchor-render task starts. Step 6 proves this commit renders no anchor link: `git status --porcelain` lists only the five files, and `DRepDetailAnchorSection.tsx:55-57` still emits `<dd className={styles.anchorValue}>{anchor.url}</dd>` under its inert-text comment. The link itself, gated on the URL parsing as `https:`, lands in **task-151 Step 9** — fourth in the build order, three commits after this one. **Green.** |

**OWED — not provable in this environment, never to be reported green:**

1. `nix fmt` before merge — `nix` is absent; `node_modules/.bin/prettier --write` on the four explicit paths is
   the substitute, and the obligation stays open.
2. That `explorer.staging.cardano.org` actually serves https. There is no network here, so the Step 2 change is
   verified as a code property (the scheme emitted) and not as a reachable endpoint. A runtime check on a
   staging build is owed before release.

**Note for the risks section (implement as specified regardless):** with the rejection kept fire-and-forget,
`AppStore.openExternalLink` (`AppStore.ts:80-83`) ignores the promise `RendererIpcChannel.send` returns, so a
blocked URL now produces an unhandled promise rejection in the renderer console instead of a visible error.
That is a console-noise consequence, not a crash, and after Step 2 no shipped caller produces a non-https URL —
but it is the observable cost of the silent-rejection decision and should be recorded, not hidden.

### task-149 — Add hardened anchor fetch service

Tracker row: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json:1592-1614`.
Mode: `autonomous`. Depends on `task-104` (complete). `targetPath: source/main/`. Nine acceptance criteria.

This task builds the **transport** half of the anchor pipeline: a main-process service that returns **bounded raw
bytes plus transport metadata and nothing else**. It does not parse JSON, does not hash-verify, does not write a
cache — those are task-150. It also lands the first main-process sanitization-floor assertion.

**Add zero npm packages.** Everything here is a Node builtin (`https`, `dns`, `net`) plus what already ships.

#### Context

**There is no existing hardened fetch to copy.** The nearest main-process HTTP helper is `fetchText` in
`source/main/mithril/mithrilNetworkConfig.ts`. Line 1 is `import https from 'https';`, and the helper spans
`:49-69` verbatim:

```ts
export function fetchText(url: string): Promise<string> {
  return new Promise((resolve, reject) => {
    const request = https.request(url, (response) => {
      const { statusCode } = response;
      if (!statusCode || statusCode < 200 || statusCode >= 300) {
        response.resume();
        reject(new Error(`Request failed with status ${statusCode}`));
        return;
      }

      let data = '';
      response.on('data', (chunk) => {
        data += chunk.toString();
      });
      response.on('end', () => resolve(data.trim()));
    });

    request.on('error', reject);
    request.end();
  });
}
```

Use it as an **API-shape reference only. It is not a security model**: no timeout, no size cap, no content-type
check, no SSRF guard, no address pinning, and it rejects with a raw `Error`. Every one of those gaps is an
acceptance criterion below.

> Anchor correction, live repo preferred: the planning corpus cites this helper at `mithrilNetworkConfig.ts:48-69`.
> At `bf112d9f8` the `export function fetchText` line is **49**, not 48. Everything else in that citation holds.

**Dependency ground truth.** `axios@1.7.7` is a **devDependency** (`package.json:112`) and is not shippable in the
main process. Node `dns` is imported **nowhere** in `source/` today (`grep -rn "from 'dns'" source/` → zero hits) —
this task is its first use. `@types/node` is pinned at **14.18.1** (`package.json:99`), and its
`http.ClientRequestArgs` does carry `lookup?: LookupFunction | undefined` (`node_modules/@types/node/http.d.ts`),
so the custom-lookup branch of AC-7 typechecks without a cast.

**Shared types — the anchor block ends at `:71`.** `source/common/types/governance.types.ts:66-71`:

```ts
export interface DRepAnchorPresence {
  /** The raw anchor URL recorded on-chain. */
  url: string;
  /** Blake2b-256 hex digest of the anchor content. */
  hash: string;
}
```

The file is 120 lines; `GovernanceQueryErrorType` occupies `:105-114`. There is no anchor error enum yet.

**Main-process logging has no filter.** `source/main/utils/logging.ts:26-33`:

```ts
const logToLevel =
  (level: string) =>
  (message: string, data: Record<string, any> | null | undefined) =>
    log[level](formatContext({ ...messageContext, level }), {
      message,
      data: toJS(data),
      environmentData,
    });
```

`export const logger: Logger = {...}` follows at `:35-40`. `filterLogData` is **renderer-only**:
`grep -rn filterLogData source/main` returns only the comment at `source/main/utils/setupLogging.ts:180`. Every
real call site is `source/renderer/app/api/api.ts` (44 references repo-wide, all under `source/renderer`). So the
main process has **no automatic redaction** and must enforce its discipline by hand.

**The redaction list this task widens.** `source/common/utils/logging.ts:21-74` is `filterLogData`; the
`sensitiveData` array is `:24-49`, its governance entries are `:45-48` (`'drepId'`, `'dRepId'`, `'vote'`,
`'voting'`), the match is **exact string equality** at `:59` (`sensitiveData.includes(key)`), and a hit deletes the
whole subtree (`:59-61`) inside the hand-rolled `redact` closure at `:51-71`.

**The floor suite.** `tests/jest/security/governance-sanitization.spec.ts` — 556 lines, **26 tests, all green
(measured)**. Docblock `:1-9`; four hoisted `jest.mock` factories `:10-34`; imports `:36-44`; bech32 vectors
`CIP129_DREP` / `CIP105_KEY` / `CIP105_SCRIPT` at `:46-56`; `jsonStr` `:58-60`; `jsonStrWithErrors` `:65-69`
(expands non-enumerable `Error.message`/`.stack` — use it, never bare `JSON.stringify`, on any error path);
describes at `:71`, `:227`, `:520`. The `filterLogData` describe closes at `:225`. **The file has no
main-process case at all** — this task adds the first.

The docblock contradicts the file. `:4-5` reads:

```
 * Asserts that no DRep ID, abstain/no_confidence literal, or CIP-129/CIP-105
 * bech32 string reaches any logger call or analytics payload.
```

but `:500-504` in the same file *requires* `analytics.sendEvent(EventCategories.VOTING, 'Casted governance vote',
'abstain')`. Step 6 renarrows it.

**The floor has two anchors, not one.** The second is
`source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx:852-876`, the test
`'keeps the vote target out of renderer logger payloads across the flow'` (its describe closes at `:877`).
**A sanitization claim that cites only the security suite is a false green.** Both run in Verify.

**Main-process spec precedent to mirror.** `tests/jest/governance/GovernanceQueryService.spec.ts:1-32` — an
`@jest-environment node` docblock, then a hoisted `jest.mock` with a `requireActual` spread:

```ts
/**
 * ...
 * @jest-environment node
 */
import { EventEmitter } from 'events';
...
jest.mock('child_process', () => {
  const actual = jest.requireActual('child_process');
  return {
    ...actual,
    spawn: jest.fn(),
  };
});

const mockSpawn = childProcess.spawn as jest.Mock;
```

Mirror this shape for `https` and `dns`.

**Pre-existing sinks that are NOT this task's diff.** `GovernanceQueryService.ts:523-526` logs `{ index, error: err }`;
`governanceChannel.ts:58-60` logs `{ error: snapshotError }`; `:64` and `:77` log `{ error }` whose `details` holds
trimmed cardano-cli stderr. Do not retro-fix them here. They are recorded under OWED.

**Environment gotchas, measured in this worktree.**

- `setImmediate` is **undefined** under `jest-environment-jsdom` 27. Use `process.nextTick` in every async fake.
  (The security suite is jsdom; only the new service spec carries `@jest-environment node`.)
- `jest.spyOn(https, 'request')` and `jest.spyOn(dns.promises, 'lookup')` both work under jsdom with the
  `esModuleInterop` default import — verified.
- Importing `source/main/utils/logging` under jsdom and spying on its `logger` works — verified.
- `yarn lint` covers `source storybook utils` only (`package.json:43`), not `tests/`. A new file under
  `source/main/governance/` moves the warning baseline; a new spec under `tests/jest/` does not.
- `no-bitwise` is off (`.eslintrc`), so the prefix arithmetic below is lint-clean.

#### Locked invariants this change must not break

Inlined in full — do not follow a reference.

- **#3 Anchor transport-security floor.** The full guard set — TLS on, redirects off, ≤10 s connect **and** total
  timeout, ~1 MB cap, JSON content-type allow-list (`application/json`, `application/ld+json`), SSRF guard over
  RFC 1918 / loopback / link-local / `0.0.0.0/8` / ULA / IPv6 reserved, DNS-rebinding mitigation binding the
  validated resolved IP to the actual TCP connection, Blake2b-256 verify **before** parse/cache/render, immutable
  hash-keyed cache — **lands complete in `anchor-1` and is never thinned.** task-149 owns everything up to and
  excluding the hash verify. No guard may be made optional, configurable, or bypassable by a test hook.
- **#2 Sanitization floor.** No DRep id, no `abstain` / `no_confidence` literal, no CIP-129/CIP-105 bech32 string in
  any logger, analytics, or electron-store payload. anchor-1 widens it to **anchor URLs, hosts, resolved IPs and
  verified names**. `logDRepStateSnapshot` (`source/main/utils/setupLogging.ts:178-183`) stays the one documented
  exception.
- **#1 local-first.** The anchor fetch is the **only** outbound network call this feature makes, and only to the
  DRep's own registered anchor URL. No explorer, no indexer, no aggregator, no telemetry beacon.
- **#11 preliminary copy.** Every new en-US and ja-JP string keeps its leading `!!!`. **task-149 mints no i18n
  keys** — it is a main-process service with no user-facing copy. If you find yourself writing a message id here,
  stop: the copy belongs to task-151.
- **Convergence rule.** Reuse existing seams; add no package at all.
- **Comments and commits.** Comment only where the logic is not self-evident, then 1-3 plain sentence-case lines
  stating the invariant or the why — never task ids, review labels, ALL-CAPS, or change history, in comments **or**
  test names. One commit, one Conventional Commits subject line, no body, no `Co-Authored-By`.

#### Resolved judgment calls (do not revisit)

1. **The service file is `source/main/governance/AnchorFetchService.ts`**, beside the existing
   `GovernanceQueryService.ts` (today the only file in that directory). Plain exported functions, **not** a
   singleton class: unlike `GovernanceQueryService` it holds no node/CLI configuration and needs no lifecycle.
2. **Exported surface is fixed by the cross-task seam (S-1)** and task-150 codes against it verbatim:
   ```ts
   export interface AnchorFetchOk   { ok: true;  bytes: Buffer; host: string; contentType: string; byteLength: number }
   export interface AnchorFetchFail { ok: false; reason: AnchorFetchErrorType }
   export type AnchorFetchResult = AnchorFetchOk | AnchorFetchFail;
   export interface AnchorTransport { readonly scheme: string; fetch(url: string): Promise<AnchorFetchResult> }
   export const httpsAnchorTransport: AnchorTransport;
   export function fetchAnchorBytes(url: string): Promise<AnchorFetchResult>;
   ```
   **It never throws and never rejects.** Every outcome is a resolved discriminated union.
3. **`https:` only.** `http:` returns `UnsupportedScheme`, exactly like `ipfs:` — a plaintext transport has no TLS
   to keep verification on, and task-152 hardens `open-external-url` to https-only in the same slice.
4. **The IPFS slot is the *interface*, never a stub.** `AnchorTransport` is the slot. Register nothing for `ipfs:`;
   an `ipfs:` URL falls through the transport registry to `UnsupportedScheme`. Do not add a placeholder
   implementation, a TODO, or a throwing stub.
5. **Error transport is enum-only.** task-149 adds the whole `AnchorFetchErrorType` enum (D-2) to
   `governance.types.ts` in one edit; `HashMismatch` and `ParseFailed` are declared here but first used by
   task-150. No free text crosses any boundary. The anchor path **must never construct a `GovernanceQueryError`**
   (`GovernanceQueryService.ts:23-32`) and must never route through `GovernanceQueryService` — that keeps it
   structurally out of reach of `_shouldRetryWithConway`, which string-matches the word **"latest"** in failure
   text and would spuriously retry. **Do not use the word "latest" in any string this task adds.**
6. **3xx and 404 produce the same graceful empty state** (AC-2): both are `ok: false`, both carry no bytes and no
   host, both flow to task-150's `{ status: 'unavailable' }` and render as *Anchor unavailable*. They carry
   **distinct diagnostic reasons** (`Redirected` vs `HttpStatus`) that never leave the main process. `https.request`
   does not auto-follow redirects, but relying on that is not a guard — reject 3xx **explicitly** and never read the
   `location` header.
7. **AC-7 uses the custom-cached-lookup branch, not IP-literal connect.** Pass a `lookup` function that always calls
   back with the address the SSRF guard validated, while `hostname` and `servername` stay the original host so TLS
   SNI and certificate hostname verification still run against the real name. IP-literal connect would force
   hand-rolling the `Host` header and disabling SNI.
8. **Every resolved address must pass the guard, not just the pinned one.** `dns.promises.lookup(host, { all: true })`
   and reject the whole fetch if *any* returned address is blocked. A host that resolves to one public and one
   private address is an attack, not a fallback.
9. **Two independent timeouts, both 10 000 ms** (AC-3): `options.timeout` (socket/connect inactivity, surfaced as
   the request's `'timeout'` event) **and** a wall-clock `setTimeout` total budget armed when the request starts.
   Either firing aborts the request and resolves `Timeout`.
10. **Two independent size guards** (AC-4): reject a declared `content-length` above the cap before reading a byte,
    **and** abort mid-stream the moment accumulated length exceeds the cap. The header is advisory; the stream
    counter is the guard.
11. **`rejectUnauthorized` is never mentioned in the source file** (AC-1). Not `true`, not a variable, not a
    comment-toggle. Omitting it is what keeps the Node default (verification on), and the spec asserts the option is
    `undefined` on the outgoing request.
12. **Main-process logging discipline (binding for every line this task adds).** Log only enum values and
    identifier-free scalars: `{ errorType }`, `{ byteLength }`. **Never `{ error }`, never `err.message`, never
    `err.stack`, never a URL, host, hostname, resolved IP, or DRep id.** This mirrors the shipped renderer
    discipline at `GovernanceStore.ts:262-265` and `:302-304`. The host the renderer needs travels in the **return
    value**, not in a log line.
13. **`sensitiveData` gains twelve names, not two** (cv-2 F-31 forbids a two-key patch): F-9's seven
    (`drepIdentity`, `currentVote`, `votingTarget`, `chosenOption`, `raw`, `cip105`, `credentialHex`), **plus
    `cip129`** (F-9 omits it although `DRepIdentity` carries it, `governance.types.ts:20-31`), **plus the four
    anchor-shaped names** (`anchorUrl`, `anchorContent`, `givenName`, `verifiedName`). Accepted tradeoff, stated
    once: `raw` is generic and a hit deletes the whole subtree, so unrelated `{ raw: … }` diagnostics lose detail —
    a verbosity loss, not a break. Checked: no fixture or spec in the repo depends on any of the twelve surviving
    `filterLogData` (`tests/mocks/wallets/wallet-voting-drep.json` has zero collisions).
14. **No filesystem access of any kind in this task.** No cache read, no cache write, no temp file. `anchorCache.ts`
    is task-150's file (S-3). The spec asserts this.

#### Step 1 — Add the anchor error enum to the shared types

Edit `source/common/types/governance.types.ts`. Insert immediately **after** the `DRepAnchorPresence` interface
closes at `:71`, before the `// ---- Wallet Governance Status ----` comment:

```ts
// ---- Anchor Fetch (transport outcomes, anchor-1) ----

export enum AnchorFetchErrorType {
  UnsupportedScheme = 'ANCHOR_UNSUPPORTED_SCHEME',
  BlockedAddress = 'ANCHOR_BLOCKED_ADDRESS',
  DnsFailed = 'ANCHOR_DNS_FAILED',
  Redirected = 'ANCHOR_REDIRECTED',
  HttpStatus = 'ANCHOR_HTTP_STATUS',
  ContentType = 'ANCHOR_CONTENT_TYPE',
  TooLarge = 'ANCHOR_TOO_LARGE',
  Timeout = 'ANCHOR_TIMEOUT',
  TlsFailed = 'ANCHOR_TLS_FAILED',
  Network = 'ANCHOR_NETWORK',
  HashMismatch = 'ANCHOR_HASH_MISMATCH',
  ParseFailed = 'ANCHOR_PARSE_FAILED',
  InvalidRequest = 'ANCHOR_INVALID_REQUEST',
}
```

Add nothing else to this file. `VerifiedDRepAnchorContent` and `DRepAnchorResult` belong to task-150; the
`GOVERNANCE_DREP_ANCHOR_CHANNEL` contract in `source/common/ipc/api.ts` belongs to task-150. task-149 ships **no IPC
channel** — the service is called by task-150's verification service, not by a renderer.

#### Step 2 — Create the service file: header, constants and seam types

Create `source/main/governance/AnchorFetchService.ts`. Start with exactly:

```ts
import https from 'https';
import dns from 'dns';
import net from 'net';
import type { ClientRequest, IncomingMessage } from 'http';
import { AnchorFetchErrorType } from '../../common/types/governance.types';
import { logger } from '../utils/logging';

export const ANCHOR_MAX_BYTES = 1024 * 1024;
export const ANCHOR_TIMEOUT_MS = 10000;
export const ANCHOR_ALLOWED_CONTENT_TYPES = [
  'application/json',
  'application/ld+json',
];

export interface AnchorFetchOk {
  ok: true;
  bytes: Buffer;
  host: string;
  contentType: string;
  byteLength: number;
}

export interface AnchorFetchFail {
  ok: false;
  reason: AnchorFetchErrorType;
}

export type AnchorFetchResult = AnchorFetchOk | AnchorFetchFail;

export interface AnchorTransport {
  readonly scheme: string;
  fetch(url: string): Promise<AnchorFetchResult>;
}

const TLS_ERROR_CODES = [
  'CERT_HAS_EXPIRED',
  'DEPTH_ZERO_SELF_SIGNED_CERT',
  'SELF_SIGNED_CERT_IN_CHAIN',
  'UNABLE_TO_GET_ISSUER_CERT_LOCALLY',
  'UNABLE_TO_VERIFY_LEAF_SIGNATURE',
  'ERR_TLS_CERT_ALTNAME_INVALID',
  'ERR_TLS_HANDSHAKE_TIMEOUT',
  'ERR_SSL_WRONG_VERSION_NUMBER',
];

const DNS_ERROR_CODES = ['ENOTFOUND', 'EAI_AGAIN'];
```

#### Step 3 — Add the SSRF address guard (AC-6)

Append to the same file. The prefix tables are the whole of AC-6 plus the ranges that tunnel to it.

```ts
const BLOCKED_IPV4_PREFIXES: Array<[string, number]> = [
  ['0.0.0.0', 8],
  ['10.0.0.0', 8],
  ['100.64.0.0', 10],
  ['127.0.0.0', 8],
  ['169.254.0.0', 16],
  ['172.16.0.0', 12],
  ['192.0.0.0', 24],
  ['192.168.0.0', 16],
  ['198.18.0.0', 15],
  ['224.0.0.0', 4],
  ['240.0.0.0', 4],
];

// 2002::/16 and 2001::/32 can encapsulate an arbitrary IPv4 destination, so
// they are blocked alongside the ranges that are reserved outright.
const BLOCKED_IPV6_PREFIXES: Array<[string, number]> = [
  ['::', 128],
  ['::1', 128],
  ['64:ff9b::', 96],
  ['100::', 64],
  ['2001::', 32],
  ['2001:db8::', 32],
  ['2002::', 16],
  ['fc00::', 7],
  ['fe80::', 10],
  ['ff00::', 8],
];

function ipv4ToBytes(input: string): Uint8Array | null {
  if (!net.isIPv4(input)) return null;
  return Uint8Array.from(input.split('.').map((part) => Number(part)));
}

function ipv6ToBytes(input: string): Uint8Array | null {
  const address = input.split('%')[0];
  if (!net.isIPv6(address)) return null;
  let text = address;
  if (text.lastIndexOf('.') !== -1) {
    const colon = text.lastIndexOf(':');
    const embedded = ipv4ToBytes(text.slice(colon + 1));
    if (!embedded) return null;
    text = `${text.slice(0, colon + 1)}${(
      (embedded[0] << 8) |
      embedded[1]
    ).toString(16)}:${((embedded[2] << 8) | embedded[3]).toString(16)}`;
  }
  const [left, right = ''] = text.split('::');
  const leftGroups = left === '' ? [] : left.split(':');
  const rightGroups = right === '' ? [] : right.split(':');
  const groups = text.includes('::')
    ? [
        ...leftGroups,
        ...new Array(8 - leftGroups.length - rightGroups.length).fill('0'),
        ...rightGroups,
      ]
    : leftGroups;
  if (groups.length !== 8) return null;
  const bytes = new Uint8Array(16);
  for (let index = 0; index < 8; index += 1) {
    const value = parseInt(groups[index], 16);
    if (Number.isNaN(value)) return null;
    bytes[index * 2] = value >> 8;
    bytes[index * 2 + 1] = value & 0xff;
  }
  return bytes;
}

function isInPrefix(
  bytes: Uint8Array,
  prefix: Uint8Array,
  prefixLength: number
): boolean {
  const fullBytes = Math.floor(prefixLength / 8);
  for (let index = 0; index < fullBytes; index += 1) {
    if (bytes[index] !== prefix[index]) return false;
  }
  const remainingBits = prefixLength % 8;
  if (remainingBits === 0) return true;
  const mask = (0xff << (8 - remainingBits)) & 0xff;
  return (bytes[fullBytes] & mask) === (prefix[fullBytes] & mask);
}

// Anything that is not a parseable public address is blocked, so a new address
// form can never default to allowed.
export function isBlockedAnchorAddress(address: string): boolean {
  if (net.isIPv4(address)) {
    const bytes = ipv4ToBytes(address);
    if (!bytes) return true;
    return BLOCKED_IPV4_PREFIXES.some(([prefix, length]) => {
      const prefixBytes = ipv4ToBytes(prefix);
      return prefixBytes != null && isInPrefix(bytes, prefixBytes, length);
    });
  }
  if (net.isIPv6(address)) {
    const bytes = ipv6ToBytes(address);
    if (!bytes) return true;
    const mapped = ipv6ToBytes('::ffff:0:0');
    if (mapped != null && isInPrefix(bytes, mapped, 96)) {
      return isBlockedAnchorAddress(
        `${bytes[12]}.${bytes[13]}.${bytes[14]}.${bytes[15]}`
      );
    }
    return BLOCKED_IPV6_PREFIXES.some(([prefix, length]) => {
      const prefixBytes = ipv6ToBytes(prefix);
      return prefixBytes != null && isInPrefix(bytes, prefixBytes, length);
    });
  }
  return true;
}
```

#### Step 4 — Add the failure helper and error classifier (AC-9)

`fail()` is the **single logging point** for every rejection, which is what makes the sanitization assertion
provable in one place.

```ts
function fail(reason: AnchorFetchErrorType): AnchorFetchFail {
  logger.warn('Anchor fetch: request rejected', { errorType: reason });
  return { ok: false, reason };
}

function classifyTransportError(error: unknown): AnchorFetchErrorType {
  const code = (error as { code?: string })?.code ?? '';
  if (TLS_ERROR_CODES.includes(code)) return AnchorFetchErrorType.TlsFailed;
  if (DNS_ERROR_CODES.includes(code)) return AnchorFetchErrorType.DnsFailed;
  if (code === 'ETIMEDOUT') return AnchorFetchErrorType.Timeout;
  return AnchorFetchErrorType.Network;
}
```

The classifier reads `error.code` and **discards the error object**. Nothing else in this file may touch
`error.message` or `error.stack`.

#### Step 5 — Add the bounded request (AC-1, AC-2, AC-3, AC-4, AC-5, AC-7, AC-8)

```ts
function requestAnchorBytes(
  parsed: URL,
  host: string,
  pinned: dns.LookupAddress,
  budgetMs: number
): Promise<AnchorFetchResult> {
  return new Promise((resolve) => {
    const chunks: Buffer[] = [];
    let received = 0;
    let settled = false;
    let request: ClientRequest | null = null;
    let totalTimer: ReturnType<typeof setTimeout>;

    const rejectOnce = (reason: AnchorFetchErrorType) => {
      if (settled) return;
      settled = true;
      clearTimeout(totalTimer);
      if (request) request.destroy();
      resolve(fail(reason));
    };

    const resolveOnce = (result: AnchorFetchOk) => {
      if (settled) return;
      settled = true;
      clearTimeout(totalTimer);
      logger.info('Anchor fetch: anchor bytes retrieved', {
        byteLength: result.byteLength,
      });
      resolve(result);
    };

    // budgetMs is what is left of the one wall-clock budget after DNS, so the
    // ten seconds cover resolution and transfer together, not each in turn.
    if (budgetMs <= 0) {
      resolve(fail(AnchorFetchErrorType.Timeout));
      return;
    }

    totalTimer = setTimeout(
      () => rejectOnce(AnchorFetchErrorType.Timeout),
      budgetMs
    );

    // The socket is forced onto the address the guard validated, so a second
    // DNS answer cannot redirect the connection after the check.
    const lookup: net.LookupFunction = (_hostname, _options, callback) =>
      callback(null, pinned.address, pinned.family);

    const options: https.RequestOptions = {
      protocol: 'https:',
      hostname: host,
      port: parsed.port || 443,
      path: `${parsed.pathname}${parsed.search}`,
      method: 'GET',
      headers: { accept: ANCHOR_ALLOWED_CONTENT_TYPES.join(', ') },
      lookup,
      timeout: budgetMs,
      ...(net.isIP(host) === 0 ? { servername: host } : {}),
    };

    request = https.request(options, (response: IncomingMessage) => {
      const statusCode = response.statusCode ?? 0;
      if (statusCode >= 300 && statusCode < 400) {
        response.destroy();
        rejectOnce(AnchorFetchErrorType.Redirected);
        return;
      }
      if (statusCode < 200 || statusCode >= 300) {
        response.destroy();
        rejectOnce(AnchorFetchErrorType.HttpStatus);
        return;
      }

      const contentType = String(response.headers['content-type'] ?? '')
        .split(';')[0]
        .trim()
        .toLowerCase();
      if (!ANCHOR_ALLOWED_CONTENT_TYPES.includes(contentType)) {
        response.destroy();
        rejectOnce(AnchorFetchErrorType.ContentType);
        return;
      }

      const declaredLength = Number(response.headers['content-length']);
      if (Number.isFinite(declaredLength) && declaredLength > ANCHOR_MAX_BYTES) {
        response.destroy();
        rejectOnce(AnchorFetchErrorType.TooLarge);
        return;
      }

      response.on('data', (chunk: Buffer) => {
        received += chunk.length;
        if (received > ANCHOR_MAX_BYTES) {
          response.destroy();
          rejectOnce(AnchorFetchErrorType.TooLarge);
          return;
        }
        chunks.push(Buffer.from(chunk));
      });
      response.on('error', (error) =>
        rejectOnce(classifyTransportError(error))
      );
      response.on('end', () => {
        const bytes = Buffer.concat(chunks, received);
        resolveOnce({
          ok: true,
          bytes,
          host,
          contentType,
          byteLength: bytes.length,
        });
      });
    });

    request.on('timeout', () => rejectOnce(AnchorFetchErrorType.Timeout));
    request.on('error', (error) => rejectOnce(classifyTransportError(error)));
    request.end();
  });
}
```

Notes the implementer must not "improve" away: the `location` header is never read; `rejectUnauthorized` never
appears; `servername` is omitted for an IP-literal host because TLS forbids SNI with an IP; a missing
`content-length` yields `Number('')` → `0`, which correctly passes the declared-size check; the returned `bytes`
are raw and never touched by `JSON.parse` (AC-8).

#### Step 6 — Add the https transport, the reserved registry and the entry point (AC-3, AC-6)

```ts
const TIMEOUT_SENTINEL = Symbol('anchor-timeout');

// dns.promises.lookup has no timeout of its own, so the wall-clock budget is
// armed here and the resolver races against it; what is left funds the request.
function lookupWithinBudget(
  host: string,
  budgetMs: number
): Promise<dns.LookupAddress[] | typeof TIMEOUT_SENTINEL> {
  let timer: ReturnType<typeof setTimeout>;
  const expiry = new Promise<typeof TIMEOUT_SENTINEL>((resolve) => {
    timer = setTimeout(() => resolve(TIMEOUT_SENTINEL), budgetMs);
  });
  return Promise.race([
    dns.promises.lookup(host, { all: true }),
    expiry,
  ]).finally(() => clearTimeout(timer));
}

async function fetchOverHttps(url: string): Promise<AnchorFetchResult> {
  const deadline = Date.now() + ANCHOR_TIMEOUT_MS;

  let parsed: URL;
  try {
    parsed = new URL(url);
  } catch (error) {
    return fail(AnchorFetchErrorType.InvalidRequest);
  }
  if (parsed.protocol !== 'https:') {
    return fail(AnchorFetchErrorType.UnsupportedScheme);
  }
  const host = parsed.hostname.replace(/^\[|\]$/g, '');
  if (host === '') return fail(AnchorFetchErrorType.InvalidRequest);

  let addresses: dns.LookupAddress[];
  if (net.isIP(host) !== 0) {
    addresses = [{ address: host, family: net.isIP(host) }];
  } else {
    try {
      const resolved = await lookupWithinBudget(host, ANCHOR_TIMEOUT_MS);
      if (resolved === TIMEOUT_SENTINEL) {
        return fail(AnchorFetchErrorType.Timeout);
      }
      addresses = resolved;
    } catch (error) {
      return fail(AnchorFetchErrorType.DnsFailed);
    }
  }
  if (addresses.length === 0) return fail(AnchorFetchErrorType.DnsFailed);
  // One blocked answer fails the whole fetch: a split resolution is an attack,
  // not a fallback.
  if (addresses.some((entry) => isBlockedAnchorAddress(entry.address))) {
    return fail(AnchorFetchErrorType.BlockedAddress);
  }

  try {
    return await requestAnchorBytes(
      parsed,
      host,
      addresses[0],
      deadline - Date.now()
    );
  } catch (error) {
    return fail(classifyTransportError(error));
  }
}

export const httpsAnchorTransport: AnchorTransport = {
  scheme: 'https:',
  fetch: fetchOverHttps,
};

// The IPFS slot is this interface. No ipfs: entry is registered, so an ipfs URL
// resolves to UnsupportedScheme rather than a partial implementation.
const TRANSPORTS: Record<string, AnchorTransport> = {
  [httpsAnchorTransport.scheme]: httpsAnchorTransport,
};

export async function fetchAnchorBytes(
  url: string
): Promise<AnchorFetchResult> {
  let scheme: string;
  try {
    scheme = new URL(url).protocol;
  } catch (error) {
    return fail(AnchorFetchErrorType.InvalidRequest);
  }
  const transport = TRANSPORTS[scheme];
  if (!transport) return fail(AnchorFetchErrorType.UnsupportedScheme);
  return transport.fetch(url);
}
```

`hostname.replace(/^\[|\]$/g, '')` is required: `new URL('https://[::1]/x').hostname` is `'[::1]'`, brackets
included, and neither `net.isIP` nor the guard accepts the bracketed form.

The service file is now complete. Note again: nothing in it imports `fs`, `path`, `blake2b`, or `blakejs`.

#### Step 7 — Extend the sanitization key list

Edit `source/common/utils/logging.ts`. In `sensitiveData`, immediately after `'voting',` at `:48` and before the
closing `];` at `:49`, insert:

```ts
    // Governance identity and anchor redaction. A verified anchor name
    // identifies a DRep as precisely as a bech32 id, and an anchor URL
    // identifies the DRep whose detail page the user is viewing.
    'drepIdentity',
    'currentVote',
    'votingTarget',
    'chosenOption',
    'raw',
    'cip105',
    'cip129',
    'credentialHex',
    'anchorUrl',
    'anchorContent',
    'givenName',
    'verifiedName',
```

Change nothing else in that file: the exact-match semantics at `:59` and the subtree deletion at `:59-61` stay
as they are.

#### Step 8 — Renarrow the floor-suite docblock

Edit `tests/jest/security/governance-sanitization.spec.ts`. Replace lines `:4-5`:

```
 * Asserts that no DRep ID, abstain/no_confidence literal, or CIP-129/CIP-105
 * bech32 string reaches any logger call or analytics payload.
```

with:

```
 * Asserts that no DRep ID, CIP-129/CIP-105 bech32 string, anchor URL or verified
 * anchor content reaches any logger call, and that no vote target reaches an
 * analytics payload. The derived vote kind is a sanctioned analytics dimension;
 * the vote target never is.
```

Leave `:1-3` and `:6-9` untouched. This resolves the contradiction with `:500-504` in the same pass that edits the
file, which is where cv-2 assigned it.

#### Step 9 — Add eight domain-shaped `filterLogData` cases

Still in `tests/jest/security/governance-sanitization.spec.ts`. Append these eight `it` blocks inside the
`describe('Governance sanitization — filterLogData')` opened at `:71`, immediately **before** its closing `});` at
`:225`. A two-key patch is explicitly forbidden; these cases must be domain-shaped.

1. `it('removes a nested drepIdentity object entirely', …)` — input
   `{ context: { drepIdentity: { raw: CIP129_DREP, cip129: CIP129_DREP, cip105: CIP105_KEY, credentialHex: 'a1b2' } } }`;
   assert `jsonStr(result)` contains none of the three vectors and none of `'a1b2'`.
2. `it('removes the standalone raw, cip129, cip105 and credentialHex keys', …)` — input
   `{ raw: CIP129_DREP, cip129: CIP129_DREP, cip105: CIP105_SCRIPT, credentialHex: 'a1b2' }`; assert the result has
   no own keys.
3. `it('removes a currentVote object carrying a vote kind and a drep id', …)` — input
   `{ currentVote: { voteKind: 'abstain', drepId: CIP129_DREP } }`; assert the serialized result contains neither
   `'abstain'` nor the vector.
4. `it('removes votingTarget', …)` — input `{ votingTarget: CIP129_DREP }`.
5. `it('removes chosenOption', …)` — input `{ chosenOption: 'no_confidence' }`; assert `'no_confidence'` is absent.
6. `it('removes anchorUrl', …)` — input
   `{ anchorUrl: 'https://anchor.example.org/profile.jsonld' }`; assert `'anchor.example.org'` is absent.
7. `it('removes givenName, verifiedName and a nested anchorContent object', …)` — input
   `{ givenName: 'Sample DRep', verifiedName: 'Sample DRep', anchorContent: { givenName: 'Sample DRep' } }`; assert
   `'Sample DRep'` is absent.
8. `it('retains a sensitive-looking value under a key that is not on the list', …)` — input
   `{ note: CIP129_DREP }`; assert the value **survives**, with a two-line comment stating that
   `sensitiveData.includes(key)` at `source/common/utils/logging.ts:59` is exact string equality, so the filter
   redacts by key name and never by value shape. This documents the limit rather than pretending it away.

#### Step 10 — Add the first main-process spy case (AC-9)

Append a new top-level `describe` at the **end** of `tests/jest/security/governance-sanitization.spec.ts`, after the
analytics-URL-masking describe closes. It contains exactly one `it`.

Use `jest.spyOn`, **not** a hoisted `jest.mock`, for `https` and `dns` here: the file already has four hoisted
factories for renderer modules and 26 tests that must stay green, and a scoped spy has no blast radius. Add these
imports beside the existing import block at `:36-44`:

```ts
import https from 'https';
import dns from 'dns';
import { EventEmitter } from 'events';
import { logger as mainLogger } from '../../../source/main/utils/logging';
import { fetchAnchorBytes } from '../../../source/main/governance/AnchorFetchService';
```

The case:

```ts
describe('Governance sanitization — main-process anchor fetch', () => {
  const ANCHOR_URL = `https://anchor.example.org/${CIP129_DREP}.jsonld`;

  class FakeRequest extends EventEmitter {
    end = jest.fn();
    destroy = jest.fn();
  }
  class FakeResponse extends EventEmitter {
    statusCode = 200;
    headers: Record<string, string> = { 'content-type': 'text/html' };
    destroy = jest.fn();
  }

  it('keeps anchor URLs, hosts, DRep ids and raw errors out of anchor fetch logger payloads', async () => {
    const spies = [
      jest.spyOn(mainLogger, 'debug').mockImplementation(() => undefined),
      jest.spyOn(mainLogger, 'info').mockImplementation(() => undefined),
      jest.spyOn(mainLogger, 'warn').mockImplementation(() => undefined),
      jest.spyOn(mainLogger, 'error').mockImplementation(() => undefined),
    ];
    const requestSpy = jest.spyOn(https, 'request');
    const lookupSpy = jest.spyOn(dns.promises, 'lookup');
    const SENTINEL = 'sentinel-error-detail';

    // unsupported scheme, malformed input
    await fetchAnchorBytes(`ipfs://${CIP129_DREP}`);
    await fetchAnchorBytes('not a url');
    await fetchAnchorBytes(`http://anchor.example.org/${CIP105_KEY}.jsonld`);

    // dns failure carrying an error whose message names the host
    lookupSpy.mockRejectedValue(
      Object.assign(new Error(`${SENTINEL} anchor.example.org`), {
        code: 'ENOTFOUND',
      })
    );
    await fetchAnchorBytes(ANCHOR_URL);

    // blocked address
    lookupSpy.mockResolvedValue([{ address: '169.254.169.254', family: 4 }] as any);
    await fetchAnchorBytes(ANCHOR_URL);

    lookupSpy.mockResolvedValue([{ address: '93.184.216.34', family: 4 }] as any);

    // transport error
    const failingRequest = new FakeRequest();
    requestSpy.mockImplementation(((): any => {
      process.nextTick(() =>
        failingRequest.emit(
          'error',
          Object.assign(new Error(`${SENTINEL} ${ANCHOR_URL}`), {
            code: 'CERT_HAS_EXPIRED',
          })
        )
      );
      return failingRequest;
    }) as any);
    await fetchAnchorBytes(ANCHOR_URL);

    // redirect, rejected content type, oversized body
    const drive = (response: FakeResponse, after?: () => void) => {
      const request = new FakeRequest();
      requestSpy.mockImplementation(((_options: any, callback: any): any => {
        process.nextTick(() => {
          callback(response);
          if (after) process.nextTick(after);
        });
        return request;
      }) as any);
    };

    const redirect = new FakeResponse();
    redirect.statusCode = 302;
    redirect.headers = { location: `https://evil.example.net/${CIP129_DREP}` };
    drive(redirect);
    await fetchAnchorBytes(ANCHOR_URL);

    const wrongType = new FakeResponse();
    drive(wrongType);
    await fetchAnchorBytes(ANCHOR_URL);

    const oversized = new FakeResponse();
    oversized.headers = {
      'content-type': 'application/json',
      'content-length': String(2 * 1024 * 1024),
    };
    drive(oversized);
    await fetchAnchorBytes(ANCHOR_URL);

    const logged = jsonStrWithErrors(spies.map((spy) => spy.mock.calls));
    expect(logged).not.toContain(ANCHOR_URL);
    expect(logged).not.toContain('anchor.example.org');
    expect(logged).not.toContain('evil.example.net');
    expect(logged).not.toContain('93.184.216.34');
    expect(logged).not.toContain('169.254.169.254');
    expect(logged).not.toContain(CIP129_DREP);
    expect(logged).not.toContain(CIP105_KEY);
    expect(logged).not.toContain('drep1');
    expect(logged).not.toContain('drep_vkh');
    expect(logged).not.toContain(SENTINEL);
    expect(logged).toContain('ANCHOR_TLS_FAILED');

    spies.forEach((spy) => spy.mockRestore());
    requestSpy.mockRestore();
    lookupSpy.mockRestore();
  });
});
```

The `expect(logged).toContain('ANCHOR_TLS_FAILED')` line is deliberate: it proves the assertions above are green
because the payloads are clean, not because nothing was logged.

`jsonStrWithErrors` (`:65-69`) expands `Error.message` and `.stack`, which bare `JSON.stringify` drops — that is why
the sentinel assertion is meaningful. Use `process.nextTick`, never `setImmediate`: this file runs under jsdom,
where `setImmediate` is undefined.

#### Step 11 — Create the service spec

Create `tests/jest/governance/AnchorFetchService.spec.ts`. Open it exactly like
`tests/jest/governance/GovernanceQueryService.spec.ts:1-32`: a docblock ending in `@jest-environment node`, then
hoisted `jest.mock` factories with `requireActual` spreads.

```ts
/**
 * Transport-security floor for the anchor fetch service: scheme allow-list,
 * SSRF address guard, DNS-rebinding pinning, TLS, redirects, timeouts, the
 * response-size cap and the content-type allow-list.
 *
 * Uses deterministic jest.mock over https.request and dns.promises.lookup — no
 * real socket is opened.
 *
 * @jest-environment node
 */
import { EventEmitter } from 'events';
import https from 'https';
import dns from 'dns';
import fs from 'fs';
import {
  fetchAnchorBytes,
  isBlockedAnchorAddress,
  httpsAnchorTransport,
  ANCHOR_MAX_BYTES,
  ANCHOR_TIMEOUT_MS,
} from '../../../source/main/governance/AnchorFetchService';
import { AnchorFetchErrorType } from '../../../source/common/types/governance.types';

jest.mock('https', () => ({
  ...jest.requireActual('https'),
  request: jest.fn(),
}));

jest.mock('dns', () => {
  const actual = jest.requireActual('dns');
  return {
    ...actual,
    promises: { ...actual.promises, lookup: jest.fn() },
  };
});

const mockRequest = https.request as unknown as jest.Mock;
const mockLookup = dns.promises.lookup as unknown as jest.Mock;
const ANCHOR_URL = 'https://anchor.example.org/profile.jsonld';

class FakeResponse extends EventEmitter {
  statusCode = 200;
  headers: Record<string, string> = { 'content-type': 'application/json' };
  destroy = jest.fn();
}

class FakeRequest extends EventEmitter {
  end = jest.fn();
  destroy = jest.fn();
}

function primeTransport(response: FakeResponse | null, body?: string) {
  const request = new FakeRequest();
  mockRequest.mockImplementation((_options: any, callback: any) => {
    if (response) {
      process.nextTick(() => {
        callback(response);
        process.nextTick(() => {
          if (body !== undefined) {
            response.emit('data', Buffer.from(body));
            response.emit('end');
          }
        });
      });
    }
    return request;
  });
  return request;
}

beforeEach(() => {
  mockLookup.mockResolvedValue([{ address: '93.184.216.34', family: 4 }]);
});
```

Then ten describes, **39 tests total**. Every named case must exist; merging cases is not allowed.

**`describe('Anchor fetch service — transport selection')` — 4 tests**
1. `'exposes the https transport under the https scheme'` — `httpsAnchorTransport.scheme` is `'https:'`.
2. `'rejects an http url without opening a socket'` — `UnsupportedScheme`; `mockRequest` not called.
3. `'rejects an ipfs url because no transport is registered for it'` — `UnsupportedScheme`.
4. `'rejects a malformed url'` — `fetchAnchorBytes('not a url')` → `InvalidRequest`.

**`describe('Anchor fetch service — address guard')` — 10 tests**, all unit-level over `isBlockedAnchorAddress`.
5. `'blocks RFC 1918 ranges'` — `10.0.0.1`, `172.16.5.5`, `172.31.255.255`, `192.168.1.1`.
6. `'blocks loopback'` — `127.0.0.1`, `127.10.0.1`.
7. `'blocks link-local including the cloud metadata address'` — `169.254.0.1`, `169.254.169.254`.
8. `'blocks the 0.0.0.0/8 range'` — `0.0.0.0`, `0.1.2.3`.
9. `'blocks shared, protocol-assignment, benchmarking, multicast and reserved v4 ranges'` — `100.64.0.1`,
   `192.0.0.1`, `198.18.0.1`, `224.0.0.1`, `255.255.255.255`.
10. `'blocks the IPv6 unspecified and loopback addresses'` — `::`, `::1`.
11. `'blocks IPv6 unique local and link-local addresses'` — `fc00::1`, `fd00::1`, `fe80::1`.
12. `'blocks IPv6 documentation, 6to4, Teredo, NAT64 and multicast ranges'` — `2001:db8::1`, `2002:7f00:1::`,
    `2001:0:53aa::1`, `64:ff9b::7f00:1`, `ff02::1`.
13. `'blocks an IPv4-mapped IPv6 address wrapping a private address and allows one wrapping a public address'` —
    `::ffff:127.0.0.1` blocked, `::ffff:93.184.216.34` allowed.
14. `'allows public addresses and blocks anything that is not an IP'` — `93.184.216.34`, `8.8.8.8`, `172.32.0.1`,
    `2606:4700:4700::1111` allowed; `'not-an-ip'`, `''` blocked.

**`describe('Anchor fetch service — DNS resolution and rebinding')` — 5 tests**
15. `'rejects a host that resolves to a private address before opening a socket'` — `BlockedAddress`; `mockRequest`
    not called.
16. `'rejects when any resolved address is private'` — `[93.184.216.34, 10.0.0.5]` → `BlockedAddress`.
17. `'maps a resolver failure'` — `mockLookup.mockRejectedValue({ code: 'ENOTFOUND' })` → `DnsFailed`.
18. `'maps an empty resolution'` — `mockLookup.mockResolvedValue([])` → `DnsFailed`.
19. `'pins the validated address through a custom lookup while the hostname and servername stay the original host'` —
    read `mockRequest.mock.calls[0][0]`; assert `hostname` and `servername` are `'anchor.example.org'`, `path` is
    `'/profile.jsonld'`, and calling `options.lookup('anchor.example.org', {}, cb)` invokes
    `cb(null, '93.184.216.34', 4)`.

**`describe('Anchor fetch service — TLS')` — 2 tests**
20. `'never sets rejectUnauthorized on the outgoing request'` — assert `options.rejectUnauthorized` is `undefined`.
21. `'maps certificate errors'` — emit `'error'` with `code: 'CERT_HAS_EXPIRED'` → `TlsFailed`; repeat for
    `'ERR_TLS_CERT_ALTNAME_INVALID'`.

**`describe('Anchor fetch service — redirects and status codes')` — 4 tests**
22. `'rejects a 302 and never follows the location header'` — `Redirected`; `mockRequest` called exactly once;
    `response.destroy` called.
23. `'rejects 301, 307 and 308 the same way'` — all three → `Redirected`.
24. `'rejects a 404 with the same result shape as a redirect'` — assert the 404 result and the 302 result have the
    same keys, both `ok: false`, both without `bytes` or `host`.
25. `'rejects a 500'` — `HttpStatus`.

**`describe('Anchor fetch service — timeouts')` — 3 tests**
26. `'sets a socket timeout no larger than the ten second budget'` — `options.timeout` is `> 0` and
    `<= ANCHOR_TIMEOUT_MS`, and `ANCHOR_TIMEOUT_MS` is `10000`. The value is the *remaining* budget after DNS, so
    assert the bound, never equality — a stray equality assertion here is what would hide a regression that
    re-armed the clock per phase.
27. `'maps a socket timeout event and destroys the request'` — emit `'timeout'` → `Timeout`; `request.destroy`
    called.
28. `'aborts a response that never ends once the total budget elapses'` — with `jest.useFakeTimers()`, invoke the
    response callback but emit no `'end'`, flush microtasks with
    `for (let tick = 0; tick < 10; tick += 1) await Promise.resolve();`, then
    `jest.advanceTimersByTime(ANCHOR_TIMEOUT_MS)` → `Timeout`; `request.destroy` called; `jest.useRealTimers()` at
    the end. The microtask flush is required because the resolved lookup is awaited before the request timer is
    armed; the budget itself already started in `fetchOverHttps`, so nothing here depends on when the timer lands.

**`describe('Anchor fetch service — response size cap')` — 3 tests**
29. `'rejects a declared content-length above the cap before reading a body'` —
    `'content-length': String(ANCHOR_MAX_BYTES + 1)` → `TooLarge`; no `'data'` handler ever fires.
30. `'aborts mid-stream when the body overflows the cap'` — emit `Buffer.alloc(ANCHOR_MAX_BYTES)` then
    `Buffer.alloc(1)` → `TooLarge`; both `response.destroy` and `request.destroy` called.
31. `'accepts a body exactly at the cap'` — emit `Buffer.alloc(ANCHOR_MAX_BYTES)` then `'end'` → `ok: true` with
    `byteLength === ANCHOR_MAX_BYTES`; and `ANCHOR_MAX_BYTES` is `1024 * 1024`.

**`describe('Anchor fetch service — content type')` — 4 tests**
32. `'accepts application/json'`.
33. `'accepts application/ld+json with parameters'` — `'application/ld+json; charset=utf-8'` → `ok: true` and
    `contentType === 'application/ld+json'`.
34. `'rejects a disallowed content type'` — `'text/html; charset=utf-8'` → `ContentType`; `response.destroy` called.
35. `'rejects a missing content type'` — `headers: {}` → `ContentType`.

**`describe('Anchor fetch service — result shape')` — 3 tests**
36. `'returns bounded raw bytes and transport metadata only'` — assert deep equality with
    `{ ok: true, bytes: Buffer.from(body), host: 'anchor.example.org', contentType: 'application/json', byteLength: body.length }`
    — no extra key.
37. `'never parses the body'` — drive a body of `'{"body":'` (invalid JSON); the result is `ok: true` and
    `result.bytes.toString()` is byte-identical to the input.
38. `'writes nothing to the filesystem on any path'` — `jest.spyOn(fs, 'writeFileSync')` and
    `jest.spyOn(fs.promises, 'writeFile')`; drive one success and one failure; assert neither spy was called.

**`describe('Anchor fetch service — DNS budget')` — 1 test**
39. `'aborts when DNS resolution never settles'` — with `jest.useFakeTimers()`, `mockLookup.mockReturnValue(new
    Promise(() => {}))`, call `fetchAnchorBytes(ANCHOR_URL)` without awaiting, flush microtasks with
    `for (let tick = 0; tick < 10; tick += 1) await Promise.resolve();`, then
    `jest.advanceTimersByTime(ANCHOR_TIMEOUT_MS)` and await the call → `{ ok: false, reason: Timeout }`; assert
    `mockRequest` was **not** called; `jest.useRealTimers()` at the end. This is the case that proves the ≤10 s
    budget covers resolution as well as transfer — without it a hostile resolver is bounded only by the OS default.

#### Step 12 — Format, then commit

`nix fmt` is unavailable here and stays a user-owned pre-merge obligation.

```bash
cd /home/node/.claude/jobs/f104125f/wt-anchor-1
node_modules/.bin/prettier --write \
  source/main/governance/AnchorFetchService.ts \
  source/common/types/governance.types.ts \
  source/common/utils/logging.ts \
  tests/jest/governance/AnchorFetchService.spec.ts \
  tests/jest/security/governance-sanitization.spec.ts
```

Commit subject, exactly one line, no body, no trailer:

```
feat(gov): task-149 add the hardened anchor fetch service
```

#### Verify

```bash
cd /home/node/.claude/jobs/f104125f/wt-anchor-1

# 1. Typecheck. Baseline is clean, so any error is attributable to this task.
node_modules/.bin/tsc --noEmit                 # expect exit 0
yarn compile                                   # expect exit 0, ~22s

# 2. The new service spec.
node_modules/.bin/jest --testPathPattern="tests/jest/governance/AnchorFetchService" \
  --no-coverage --runInBand
# expect: 1 suite passed, 39 tests passed

# 3. Sanitization floor, anchor 1 of 2 — the security suite.
node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" \
  --no-coverage --runInBand
# expect: 1 suite passed, 26 -> 35 tests passed (measured baseline: 26)

# 4. Sanitization floor, anchor 2 of 2 — MANDATORY. Citing only step 3 is a false green.
node_modules/.bin/jest --testPathPattern="containers/voting/VotingGovernancePage.spec" \
  --no-coverage --runInBand
# expect: 1 suite passed, 27 tests passed, unchanged from baseline

# 5. Regression sweep for the twelve new sensitiveData names.
node_modules/.bin/jest --testPathPattern="tests/jest" --no-coverage --runInBand
# baseline: 11 suites (1 skipped), 158 total (146 passed, 12 skipped)
# expect:   12 suites (1 skipped), 206 total (194 passed, 12 skipped)  = +39 service, +9 sanitization
node_modules/.bin/jest --testPathPattern="source/renderer/app/api" --no-coverage --runInBand
# expect: 1 suite passed, 6 tests passed, unchanged from baseline

# 6. Lint. Baseline is exit 0 with ~5591 pre-existing warnings.
yarn lint                                      # expect exit 0
# The warning count moves because AnchorFetchService.ts is a new file under source/;
# the new spec under tests/ does not move it (lint covers `source storybook utils` only).

# 7. Structural proof of the no-parse / no-cache / no-network-library rules.
grep -n "JSON.parse\|require('fs')\|from 'fs'\|blake2b\|blakejs\|axios\|rejectUnauthorized\|latest" \
  source/main/governance/AnchorFetchService.ts
# expect: no output
grep -rn "from 'dns'\|from 'https'" source/main/governance/AnchorFetchService.ts
# expect: exactly the two builtin imports
```

`tests/jest/governance/GovernanceCliArgvSmoke.spec.ts` self-skips because `cardano-cli` is off PATH — that is the
1 skipped suite / 12 skipped tests in step 5, not a failure.

#### Acceptance

| AC | Verbatim criterion | How the steps above discharge it | Status |
|---|---|---|---|
| AC-1 | "TLS default verification remains on; no `rejectUnauthorized: false`." | Step 5 builds `https.RequestOptions` without any `rejectUnauthorized` key, so Node's default (verification on) applies. Step 11 case 20 asserts `options.rejectUnauthorized` is `undefined` on the outgoing request; case 21 asserts certificate error codes map to `TlsFailed` rather than being swallowed. Verify step 7 greps the source for the literal. | **Green offline** |
| AC-2 | "Redirects are disabled by default; 3xx surfaces as the same graceful empty state as 404." | Step 5 rejects `300 ≤ status < 400` explicitly with `Redirected`, destroys the response, and never reads `location`; `https.request` is never asked to follow. Step 11 cases 22-24 assert one request call for a 302, identical rejection for 301/307/308, and that the 404 result has the same key set and `ok: false` shape as the 302 result — no bytes, no host, both flowing to task-150's single `unavailable` state. | **Green offline** |
| AC-3 | "Per-request connect+total timeout is <= 10 seconds." | Step 2 fixes `ANCHOR_TIMEOUT_MS = 10000`. **Step 6 starts the clock first**: `const deadline = Date.now() + ANCHOR_TIMEOUT_MS` is taken before `lookupWithinBudget`, which races `dns.promises.lookup` (a call with no timeout of its own) against the same budget and returns `Timeout` if the resolver never answers. Only `deadline - Date.now()` is handed to Step 5, which arms both `options.timeout` and the wall-clock `setTimeout` from that remainder, so resolution and transfer share one ≤10 s budget rather than getting one each. Step 11 cases 26-28 assert the bound on the option value, the socket-timeout path and the total-budget path under fake timers; case 39 drives a lookup that never settles and asserts `Timeout` with `mockRequest` never called. | **Green offline** |
| AC-4 | "Hard response-size cap is about 1 MB; abort on overflow." | Step 2 fixes `ANCHOR_MAX_BYTES = 1024 * 1024`. Step 5 rejects an oversized declared `content-length` before reading a byte and aborts mid-stream (`response.destroy()` + `request.destroy()`) the moment the accumulated length exceeds the cap. Step 11 cases 29-31 assert all three, including the exactly-at-cap success. | **Green offline** |
| AC-5 | "Content-type allow-list includes application/json and application/ld+json; reject otherwise." | Step 2 declares `ANCHOR_ALLOWED_CONTENT_TYPES` as exactly those two. Step 5 strips parameters at `;`, trims, lowercases, and rejects anything else — including a missing header — with `ContentType`, before any body byte is buffered. Step 11 cases 32-35. | **Green offline** |
| AC-6 | "SSRF guard rejects RFC 1918, loopback, link-local, 0.0.0.0/8, ULA, and IPv6 reserved ranges." | Step 3's `BLOCKED_IPV4_PREFIXES` / `BLOCKED_IPV6_PREFIXES` cover every named category plus multicast, reserved, shared address space, NAT64, 6to4 and Teredo; unparseable input defaults to blocked. Step 6 rejects the fetch if **any** resolved address is blocked, before a socket opens. Step 11 cases 5-14 pin each category and cases 15-16 pin the end-to-end rejection. | **Green offline** |
| AC-7 | "DNS-rebinding mitigation binds the validated resolved IP to the actual TCP connection through either IP-literal connect with original Host/servername or a custom cached lookup option." | Step 5 supplies a `lookup` that unconditionally calls back with the guard-validated `pinned.address` / `pinned.family`, while `hostname` and `servername` stay the original host so SNI and certificate hostname verification are unaffected — the custom-cached-lookup branch. Step 11 case 19 invokes the supplied `lookup` and asserts the pinned tuple, plus the unchanged hostname and servername. | **Green offline** |
| AC-8 | "Fetch result is bounded raw bytes plus transport metadata only; no JSON parsing and no cache writes occur in this task." | The `AnchorFetchOk` shape in Step 2 is exactly `{ ok, bytes, host, contentType, byteLength }`. The service imports no `fs`, no `path`, no hash library (Step 2 import list), and calls `JSON.parse` nowhere. Step 11 case 36 asserts deep equality with no extra key, case 37 drives an invalid-JSON body through to a successful byte-identical result, and case 38 asserts no filesystem write on any path. Verify step 7 greps the source. | **Green offline** |
| AC-9 | "INHERITED sanitization floor, main-process side: a task-111-style spy case asserts this service's own logger payloads carry no anchor URL, no DRep id and no raw error object on any failure path. …" | Steps 4-6 route **every** rejection through the single `fail()` logging point, which emits `{ errorType }` only, and the success line emits `{ byteLength }` only; `classifyTransportError` reads `error.code` and discards the object. Step 7 extends `sensitiveData` by twelve names (F-9's seven + `cip129` + the four anchor-shaped names) — not a two-key patch. Step 8 renarrows the contradictory docblock at `:4-5` in the same edit pass. Step 9 adds eight domain-shaped `filterLogData` cases including the negative case documenting exact-key matching. Step 10 adds the file's first main-process spy case, driving unsupported-scheme, malformed-URL, non-https, DNS-failure, blocked-address, TLS-error, redirect, content-type and oversize paths and asserting the serialized payloads contain no anchor URL, no host, no resolved IP, no bech32 vector and no error sentinel — with a positive `ANCHOR_TLS_FAILED` assertion proving the payloads were non-empty. **Both floor anchors run in Verify** (steps 3 and 4); citing only the security suite would be a false green. | **Green offline** |

**Discharged before start — no work scheduled.** Nothing in task-149. (task-151's AC-5/AC-6/AC-7 are the
discharged-before-start set for this phase; see that task's section.)

**OWED — never report these green.**

1. **`nix fmt` before merge.** `nix` is absent in this devcontainer; `node_modules/.bin/prettier --write` on
   explicit paths is the substitute. User-owned obligation.
2. **A live anchor fetch.** There is no network here. Every guard in this task is proven against mocked
   `https.request` and `dns.promises.lookup`. No end-to-end fetch against a real anchor URL — including the SIPO and
   Cardano Academy vectors — has been executed, and the TLS-verification default has therefore never been exercised
   against a real certificate chain.
3. **Residual, outside this task's diff.** Three main-process whole-error sinks stay unhardened:
   `GovernanceQueryService.ts:523-526` (`{ index, error: err }`), `governanceChannel.ts:58-60`
   (`{ error: snapshotError }`), and `:64` / `:77` (`{ error }` whose `details` holds trimmed cardano-cli stderr).
   Pre-existing and already assessed by cv-2; a later hardening row owns them.

**Corpus-vs-repo corrections recorded by this task.**

- `mithrilNetworkConfig.ts` `fetchText` is at **`:49-69`**, not `:48-69` as the planning corpus cites. Live repo
  preferred; nothing else in that citation moves.
- The second sanitization anchor test in
  `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` spans **`:852-876`**; `:877` is the
  enclosing describe's closing brace. The corpus cites `:852-877`.

**Note to the decisions owner (implement as specified regardless).** D-2 declares `AnchorFetchErrorType`,
`VerifiedDRepAnchorContent` and `DRepAnchorResult` as one block in `governance.types.ts` without splitting them by
task. This section assigns only the enum to task-149, because the other two describe post-verification content that
task-149's AC-8 forbids it from producing. If a reviewer wants all three landed here, the enum-only split is the
smaller diff and does not block task-150.

### task-150 — Hash-verify, cache, and parse DRep anchor bytes

Tracker object: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json:1615-1632`.
`status: pending` · `priority: high` · `estimatedHours: 8` · `dependencies: ["task-149"]` · `targetPath: source/main/` ·
interaction mode `autonomous`.

**task-149 must be landed before you start.** This task consumes `fetchAnchorBytes`, `AnchorFetchResult` and
`AnchorFetchErrorType` from it and adds nothing to the transport.

#### Context

**The on-chain hash source.** `DRepAnchorPresence` is the `(url, hash)` pair read from the ledger; the `hash` field is
the digest task-150 verifies against. Verbatim, `source/common/types/governance.types.ts:64-71`:

```ts
// ---- Anchor Presence (on-chain reference only, NO fetch in slice-1) ----

export interface DRepAnchorPresence {
  /** The raw anchor URL recorded on-chain. */
  url: string;
  /** Blake2b-256 hex digest of the anchor content. */
  hash: string;
}
```

It reaches the renderer on `DRepDirectoryEntry.anchor` (`:51-62`, field at `:61`), populated by
`GovernanceQueryService._parseAnchor` — called at `source/main/governance/GovernanceQueryService.ts:516`
(`const anchor = this._parseAnchor(state, index);`), defined at `:680-712`. Its final line, `:711`, is
`return { url: urlRaw, hash: hashRaw };`, and `:687-691` shows the accepted cardano-cli spellings:

```ts
    const urlRaw = anchor.url ?? anchor.anchorUrl ?? anchor['anchor-url'];
    const hashRaw =
      anchor.hash ??
      anchor.anchorHash ??
      anchor['anchor-hash'] ??
      anchor.dataHash;
```

The hash arrives **exactly as the CLI printed it** — `_parseAnchor` does no case-folding and no `/^[0-9a-f]{64}$/`
check. task-150 owns normalisation and validation.

**A committed real-world pair**, `.agent/plans/governance/drep-discovery/research/drep-state-preprod-epoch295-sample.json:2852-2855`:

```json
            "anchor": {
                "dataHash": "9e8cb2b0f4c2ddbd9dea316b44680d8a989743868aeb40c1e6959982452f38e1",
                "url": "https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld"
            },
```

*Anchor correction:* the planning corpus cites this object as `:2852-2856`. Live, the object is `:2852-2855`
(`"dataHash"` at `:2853`, `"url"` at `:2854`); `:2856` is `"deposit": 500000000,`, outside the anchor. Use `:2852-2855`.
The body those bytes hash to is **not** committed anywhere and there is no network here — this pair drives cache-key
derivation and the hash-mismatch path only, never a real-content assertion.

**Blake2b-256 primitives — both already production dependencies, add nothing.** `package.json:207` `"blake2b": "2.1.3"`,
`:208` `"blakejs": "1.1.0"`.

- Main-process precedent, `source/main/utils/restoreKeystore.ts:2` — `import * as blake2b from 'blake2b';` — used at
  `:68`: `return blake2b(20).update(xpub).digest('hex');`
- Renderer one-shot precedent, `source/renderer/app/utils/crypto.ts:3` — `import { blake2b } from 'blakejs';` — used at
  `:109`: `export const blake2b224 = (data: Buffer): Buffer => blake2b(data, null, 28);`

**Use `blake2b` (the streaming one).** One line of justification, and it is the whole reason: it is the existing
main-process precedent, and `blake2b(32).update(bytes).digest('hex')` returns lowercase hex directly from a
size-capped byte stream with no `Buffer` round-trip. `blakejs` stays in the guide only as the independent cross-check in
Step 9. Neither package ships typings and `tsconfig.json` sets `"noImplicitAny": false`, so the `import * as` form at
`restoreKeystore.ts:2` type-checks today; copy it exactly.

**The IPC recipe you are copying.** `source/main/ipc/governanceChannel.ts:16-19` constructs a channel:

```ts
const governanceDRepListChannel: MainIpcChannel<
  GovernanceDRepListRendererRequest,
  GovernanceDRepListMainResponse
> = new MainIpcChannel(GOVERNANCE_DREP_LIST_CHANNEL);
```

and `:47-48` opens the handler factory: `export const handleGovernanceRequests = () => { governanceDRepListChannel.onRequest(async (_request) => {`.
`MainIpcChannel.onRequest` (`source/main/ipc/lib/MainIpcChannel.ts:38-43`) takes `(arg0: Incoming) => Promise<Outgoing>`.
Registration is `source/main/ipc/index.ts:29` (import) + `:51` (`handleGovernanceRequests();`).
Renderer barrel `source/renderer/app/ipc/governanceChannel.ts:18-21` (file is 21 lines) shows the **reversed** type-param
order:

```ts
export const governanceDRepStakeChannel: RendererIpcChannel<
  GovernanceDRepStakeMainResponse,
  GovernanceDRepStakeRendererRequest
> = new RendererIpcChannel(GOVERNANCE_DREP_STAKE_CHANNEL);
```

**Cache location precedent.** `source/main/config.ts:128` — `export const stateDirectoryPath = stateDir;`.
Sibling directory users: `source/main/utils/downloadManager.ts:36` (`` const downloadsDirectory = `${stateDirectoryPath}/Downloads`; ``)
and `source/main/utils/chainStorageManager.ts:78-80` (`constructor(daedalusStateDirectoryPath: string = stateDirectoryPath)`
→ `path.join(this._stateDirectoryPath, CHAIN_DIRECTORY_NAME)`).

**`source/main/config.ts` throws outside an Electron launcher** — `:19` `const isStartedByLauncher = !!LAUNCHER_CONFIG;`,
`:22` `if (!isStartedByLauncher) {`, `:40` `` throw new Error(`${dialogTitle}\n\n${dialogMessage}\n`); `` — so every
spec that reaches it must mock it. Precedent, verbatim
`tests/jest/governance/logDRepStateSnapshot.spec.ts:7-19`:

```ts
// main/config boots launcher configuration and throws outside an Electron
// launcher, so the log folder is redirected to a temp dir instead. jest.mock
// calls hoist above the imports, so the factories apply before setupLogging
// resolves its dependencies (same pattern as GovernanceStore.spec.ts).
jest.mock('../../../source/main/config', () => {
  const nodeOs = require('os');
  const nodePath = require('path');
  const base = nodePath.join(nodeOs.tmpdir(), 'drep-snapshot-spec');
  return {
    appLogsFolderPath: base,
    pubLogsFolderPath: nodePath.join(base, 'pub'),
  };
});
```

**Main-process logging has no sanitizer.** `source/main/utils/logging.ts:26-33` forwards the payload untouched:

```ts
const logToLevel =
  (level: string) =>
  (message: string, data: Record<string, any> | null | undefined) =>
    log[level](formatContext({ ...messageContext, level }), {
      message,
      data: toJS(data),
      environmentData,
    });
```

`filterLogData` is renderer-only (`rg filterLogData source/main` → zero hits). Discipline here is hand-enforced.

**Prettier formats `tests/**/*.json`.** `.prettierignore` un-ignores `!tests/` and then `!*.json`. A fixture whose bytes
prettier later rewrites invalidates its committed digest — Step 8 orders format-then-hash for exactly this reason.
`.txt`, `.hash` and `.md` under `tests/` are **not** prettier targets.

#### Locked invariants this change must not break

Inlined verbatim from `.agent/plans/governance/drep-discovery/prompt.md`.

> **3. Anchor transport-security floor.** The full anchor-1 guard set (TLS on, redirects off, ≤10s timeouts, ~1 MB cap,
> JSON content-type allow-list, SSRF + DNS-rebinding mitigation, Blake2b-256 hash-verify before parse/cache/render,
> immutable hash-keyed cache) lands complete in `anchor-1` and is never thinned. No anchor-derived content renders
> without verification + a verified off-chain source label. Anchor URLs open only through the HTTPS-only-hardened
> `open-external-url` path (task-152). (`prompt.md:106-111`)

> **2. Sanitization floor (inherited by every slice).** No DRep id, no `abstain` / `no_confidence` literal, no
> CIP-129/CIP-105 bech32 string in any logger, analytics, or electron-store payload — re-asserted via the task-111 spy
> suite in every slice. The task-168 DRep-state snapshot is the one documented exception: public on-chain directory data
> that deliberately bypasses `filterLogData` and must never include the user's own vote. (`prompt.md:101-105`)

> **1. Local-first.** Discovery data comes only from the local node via the main-process `GovernanceQueryService`. No
> hosted explorers, indexers, GovTool, Koios, Blockfrost, or public governance APIs. (`prompt.md:98-100`)

**Convergence rule** (`prompt.md:236-242`): reuse the existing seams — `MainIpcChannel` / `RendererIpcChannel`,
`DRepAnchorPresence`, the existing governance IPC block. **Add zero npm packages.**

**The order is load-bearing and cannot be reordered.** State it once, obey it everywhere:

> fetch bytes → verify the Blake2b-256 digest against the on-chain hash → **only then** parse → **only then** cache →
> **only then** render.

Nothing downstream of "verify" may run on bytes whose digest did not match: no `JSON.parse`, no cache write, no IPC
`verified` response. A cache read is allowed *before* a fetch, but a cache hit re-runs the same digest check before its
bytes are parsed, so no path reaches the parser unverified.

**Cache-content note, state it inline in the PR and in `tests/mocks/governance/README.md`:** the cache holds **public
on-chain-referenced directory data** — an anchor body every node operator can fetch — keyed by a content hash. It is not
sensitive. Invariant 2 nonetheless still forbids any DRep id, `abstain` / `no_confidence` literal, or CIP-129/CIP-105
bech32 string reaching a logger, an analytics payload, or an electron-store payload. task-150 satisfies that
structurally: the IPC request carries `DRepAnchorPresence` and **no `drepId`**, so main is incapable of logging one on
this seam, and the cache is on-disk under `stateDirectoryPath`, never in electron-store.

#### Resolved judgment calls (do not revisit)

1. **Cache location** — `<stateDirectoryPath>/DRep-anchor-cache/<hash>.json`, one file per verified anchor hash. Never
   `electron-store`: `electron-store@8.0.1` holds one JSON blob in memory and rewrites the whole file on every `set`,
   which is the wrong shape for an unbounded set of up-to-1 MB payloads, and it is a single shared instance
   (`source/main/ipc/electronStoreConversation.ts:12`) whose keys are network-prefixed (`:19`) and schema-bound to
   `STORAGE_KEYS` (`:5-8`) — a hash-keyed namespace does not fit it.
2. **Key derivation** — the key is `DRepAnchorPresence.hash`, trimmed and lowercased, then validated against
   `/^[0-9a-f]{64}$/` **before any `path.join`**. A hash failing that regex returns `AnchorFetchErrorType.InvalidRequest`
   and never touches the filesystem or the network. This is the path-traversal guard and it needs its own named test.
3. **The cache stores the verified raw bytes, not the parsed object.** anchor-2 extracts more CIP-119 fields with no
   format change, and a cache hit can be re-verified against its own filename.
4. **Immutability** — `fs.writeFileSync(p, bytes, { flag: 'wx' })`. `EEXIST` is success (another writer won the race).
   Entries are never rewritten in place.
5. **A cache hit re-runs Blake2b-256 over the file bytes**; a mismatch is treated as a miss (delete the file, refetch).
   Defence in depth against a tampered or truncated file.
6. **Staleness (AC-4) holds structurally, not by a check.** A changed on-chain hash is a *different filename*, so no code
   path can serve stale content for a new hash. Do **not** add a redundant staleness branch — it would imply the property
   is conditional.
7. **Bound** — `ANCHOR_CACHE_MAX_ENTRIES = 500`, `ANCHOR_CACHE_MAX_BYTES = 32 * 1024 * 1024`. On write, delete
   oldest-`mtime`-first until both hold. FIFO by write time, **no read-touch**: entries are immutable, so an LRU refresh
   would add a write per read for no correctness gain. The sweep runs on the main-process event loop, so it is skipped
   entirely below `ANCHOR_CACHE_SWEEP_FLOOR` files: no entry exceeds the fetch layer's ~1 MB cap, so neither bound can
   be exceeded under `ANCHOR_CACHE_MAX_BYTES / ANCHOR_MAX_BYTES` entries and the ordinary three-entry cache stats
   nothing at all.
8. **Restart** — the cache survives, which is the point. **No in-memory mirror** (a second layer needs its own
   invalidation). An in-flight dedup map keyed by hash, same shape as `GovernanceQueryService.ts:78-79`, collapses
   concurrent requests for one anchor to one fetch.
9. **The IPC handler never rejects.** 404, 3xx, blocked address, size overflow, hash mismatch, parse failure and a
   malformed request all resolve as `{ status: 'unavailable', reason }`. One renderer code path. If a later row ever adds
   a rejection here it MUST re-throw the plain marked object `{ __governanceError: true, type, message, details }` per
   `source/main/ipc/governanceChannel.ts:26-45` — `Error` instances flatten to `{ name, message }` under structured clone
   and lose `details`.
10. **`host` is computed in main, exactly once**, as `new URL(anchor.url).hostname` at the top of
    `resolveVerifiedAnchor`, and used on both the cache-hit and fetch paths. The renderer must not re-parse the URL for
    its "Fetched from {host}" tooltip. One derivation is the only way the two paths agree — the cache-hit path has no
    transport result to take a host from. This parse makes **no security decision**: scheme enforcement stays entirely in
    task-149's `fetchAnchorBytes`.
11. **`givenName` is compulsory.** Verified bytes whose `body.givenName` is absent, non-string (and not a JSON-LD
    `{"@value": "…"}` wrapper), or blank resolve as `{ status: 'unavailable', reason: ParseFailed }`. A verified state
    with no name is exactly the partial content AC-3 forbids. `VerifiedDRepAnchorContent.givenName` keeps its
    `string | null` type because the seam contract fixes it and anchor-2 may relax the rule.
12. **Every anchor error is an enum value.** No free text crosses this seam, and the anchor path must never construct a
    `GovernanceQueryError` (`GovernanceQueryService.ts:23`) or route through `GovernanceQueryService` — that keeps the
    word `latest` structurally out of the text match in `_shouldRetryWithConway`
    (`source/main/governance/GovernanceQueryService.ts:308`, called at `:297`).
13. **Main-process log discipline** (`filterLogData` has no main counterpart): log only enum values and identity-free
    scalars — `{ status }`, `{ reason }`, `{ errorCode }`, `{ bytes }`. **Never** `{ error }`, `err.message`,
    `err.stack`, a URL, a host, a resolved IP, a DRep id — **and never the anchor hash**, which is a per-DRep correlator:
    logging it while the user is on that DRep's detail page reveals which DRep they viewed, exactly as the URL would.
14. **task-150 mints zero i18n strings.** All source-label and anchor-content copy is task-151's. Do **not** run
    `yarn i18n:manage` in this task.

#### Step 1 — Add the verified-content response types

Edit `source/common/types/governance.types.ts`. Append immediately after the `DRepAnchorPresence` block (which ends at
`:71`) and after the `AnchorFetchErrorType` enum task-149 added there:

```ts
/** CIP-119 fields extracted from anchor bytes that passed Blake2b-256 verification. */
export interface VerifiedDRepAnchorContent {
  givenName: string | null;
}

export type DRepAnchorResult =
  | {
      status: 'verified';
      content: VerifiedDRepAnchorContent;
      host: string;
      fetchedAt: number;
    }
  | { status: 'unavailable'; reason: AnchorFetchErrorType };
```

If `AnchorFetchErrorType` is missing, task-149 is not landed — stop and land it first. The members task-150 uses are
`HashMismatch`, `ParseFailed` and `InvalidRequest`.

#### Step 2 — Create the immutable hash-keyed cache

Create `source/main/governance/anchorCache.ts`:

```ts
import fs from 'fs';
import path from 'path';
import { stateDirectoryPath } from '../config';
import { ANCHOR_MAX_BYTES } from './AnchorFetchService';
import { logger } from '../utils/logging';

const ANCHOR_CACHE_DIRECTORY_NAME = 'DRep-anchor-cache';
const ANCHOR_HASH_PATTERN = /^[0-9a-f]{64}$/;

export const ANCHOR_CACHE_MAX_ENTRIES = 500;
export const ANCHOR_CACHE_MAX_BYTES = 32 * 1024 * 1024;

// Below this many files neither bound can bite, because no entry exceeds the
// fetch layer's ~1 MB cap. Derived, not typed, so the two caps cannot drift.
const ANCHOR_CACHE_SWEEP_FLOOR = Math.floor(
  ANCHOR_CACHE_MAX_BYTES / ANCHOR_MAX_BYTES
);

export const isValidAnchorHash = (hash: unknown): hash is string =>
  typeof hash === 'string' && ANCHOR_HASH_PATTERN.test(hash);

export const anchorCacheDirectoryPath = (): string =>
  path.join(stateDirectoryPath, ANCHOR_CACHE_DIRECTORY_NAME);

const entryPath = (hash: string): string =>
  path.join(anchorCacheDirectoryPath(), `${hash}.json`);

export function readVerifiedAnchorBytes(hash: string): Buffer | null {
  if (!isValidAnchorHash(hash)) return null;
  try {
    return fs.readFileSync(entryPath(hash));
  } catch {
    return null;
  }
}

export function deleteVerifiedAnchorBytes(hash: string): void {
  if (!isValidAnchorHash(hash)) return;
  try {
    fs.unlinkSync(entryPath(hash));
  } catch {
    // A missing or already-removed entry is the desired end state.
  }
}

export function writeVerifiedAnchorBytes(hash: string, bytes: Buffer): void {
  if (!isValidAnchorHash(hash)) return;
  const directoryPath = anchorCacheDirectoryPath();
  try {
    fs.mkdirSync(directoryPath, { recursive: true });
    fs.writeFileSync(entryPath(hash), bytes, { flag: 'wx' });
  } catch (err) {
    const errorCode = (err as NodeJS.ErrnoException).code ?? 'UNKNOWN';
    // Entries are immutable, so a concurrent writer that won the race left
    // byte-identical content behind.
    if (errorCode !== 'EEXIST') {
      logger.warn('Anchor cache: write failed', { errorCode });
      return;
    }
  }
  enforceCacheBound(directoryPath);
}

type CacheEntryStat = { filePath: string; mtimeMs: number; size: number };

function enforceCacheBound(directoryPath: string): void {
  let fileNames: string[];
  try {
    fileNames = fs.readdirSync(directoryPath);
  } catch {
    return;
  }
  // Every entry is capped at ~1 MB upstream, so below this many files neither
  // bound can be exceeded and the sweep would stat the whole directory for
  // nothing. This is the common case: a wallet holds a handful of anchors.
  if (fileNames.length <= ANCHOR_CACHE_SWEEP_FLOOR) return;

  const entries: CacheEntryStat[] = [];
  fileNames.forEach((name) => {
    if (
      !name.endsWith('.json') ||
      !isValidAnchorHash(path.basename(name, '.json'))
    ) {
      return;
    }
    const filePath = path.join(directoryPath, name);
    try {
      const stats = fs.statSync(filePath);
      entries.push({ filePath, mtimeMs: stats.mtimeMs, size: stats.size });
    } catch {
      // The entry vanished between readdir and stat.
    }
  });
  entries.sort((a, b) => a.mtimeMs - b.mtimeMs);

  let count = entries.length;
  let totalBytes = entries.reduce((sum, entry) => sum + entry.size, 0);

  entries.forEach((entry) => {
    if (count <= ANCHOR_CACHE_MAX_ENTRIES && totalBytes <= ANCHOR_CACHE_MAX_BYTES) {
      return;
    }
    try {
      fs.unlinkSync(entry.filePath);
      count -= 1;
      totalBytes -= entry.size;
    } catch {
      // Another process removed it first; the bound still converges.
    }
  });
}
```

#### Step 3 — Create the verification service

Create `source/main/governance/AnchorVerificationService.ts`:

```ts
import * as blake2b from 'blake2b';
import { AnchorFetchErrorType } from '../../common/types/governance.types';
import type {
  DRepAnchorPresence,
  DRepAnchorResult,
  VerifiedDRepAnchorContent,
} from '../../common/types/governance.types';
import { fetchAnchorBytes } from './AnchorFetchService';
import {
  deleteVerifiedAnchorBytes,
  isValidAnchorHash,
  readVerifiedAnchorBytes,
  writeVerifiedAnchorBytes,
} from './anchorCache';

const ANCHOR_DIGEST_BYTES = 32;
const GIVEN_NAME_MAX_LENGTH = 80;

const inFlightByHash = new Map<string, Promise<DRepAnchorResult>>();

export const anchorDigest = (bytes: Buffer): string =>
  blake2b(ANCHOR_DIGEST_BYTES).update(bytes).digest('hex');

const unavailable = (reason: AnchorFetchErrorType): DRepAnchorResult => ({
  status: 'unavailable',
  reason,
});

function readCip119String(raw: unknown): string | null {
  let value: string | null = null;
  if (typeof raw === 'string') {
    value = raw;
  } else if (raw !== null && typeof raw === 'object') {
    const wrapped = (raw as Record<string, unknown>)['@value'];
    if (typeof wrapped === 'string') value = wrapped;
  }
  if (value === null) return null;
  const trimmed = value.trim();
  if (trimmed === '') return null;
  return trimmed.slice(0, GIVEN_NAME_MAX_LENGTH);
}

function parseVerifiedContent(bytes: Buffer): VerifiedDRepAnchorContent | null {
  let parsed: unknown;
  try {
    parsed = JSON.parse(bytes.toString('utf8'));
  } catch {
    return null;
  }
  if (parsed === null || typeof parsed !== 'object' || Array.isArray(parsed)) {
    return null;
  }
  const body = (parsed as Record<string, unknown>).body;
  if (body === null || typeof body !== 'object' || Array.isArray(body)) {
    return null;
  }
  const givenName = readCip119String((body as Record<string, unknown>).givenName);
  if (givenName === null) return null;
  return { givenName };
}

// Fetch, verify, parse, cache, respond. The digest check gates every step that
// follows it: unverified bytes never reach JSON.parse and never reach the cache.
async function resolveFromCacheOrFetch(
  url: string,
  hash: string,
  host: string
): Promise<DRepAnchorResult> {
  const cached = readVerifiedAnchorBytes(hash);
  if (cached !== null) {
    if (anchorDigest(cached) === hash) {
      const content = parseVerifiedContent(cached);
      if (content === null) return unavailable(AnchorFetchErrorType.ParseFailed);
      return { status: 'verified', content, host, fetchedAt: Date.now() };
    }
    deleteVerifiedAnchorBytes(hash);
  }

  const fetched = await fetchAnchorBytes(url);
  if (!fetched.ok) return unavailable(fetched.reason);

  if (anchorDigest(fetched.bytes) !== hash) {
    return unavailable(AnchorFetchErrorType.HashMismatch);
  }

  writeVerifiedAnchorBytes(hash, fetched.bytes);

  const content = parseVerifiedContent(fetched.bytes);
  if (content === null) return unavailable(AnchorFetchErrorType.ParseFailed);

  return { status: 'verified', content, host, fetchedAt: Date.now() };
}

export function resolveVerifiedAnchor(
  anchor: DRepAnchorPresence
): Promise<DRepAnchorResult> {
  const hash =
    typeof anchor?.hash === 'string' ? anchor.hash.trim().toLowerCase() : '';
  const url = typeof anchor?.url === 'string' ? anchor.url.trim() : '';
  if (!isValidAnchorHash(hash) || url === '') {
    return Promise.resolve(unavailable(AnchorFetchErrorType.InvalidRequest));
  }

  let host: string;
  try {
    host = new URL(url).hostname;
  } catch {
    return Promise.resolve(unavailable(AnchorFetchErrorType.InvalidRequest));
  }

  const inFlight = inFlightByHash.get(hash);
  if (inFlight) return inFlight;

  const pending = resolveFromCacheOrFetch(url, hash, host).finally(() => {
    inFlightByHash.delete(hash);
  });
  inFlightByHash.set(hash, pending);
  return pending;
}
```

`resolveVerifiedAnchor` never throws: every `fs` call is already caught in `anchorCache`, `JSON.parse` is wrapped,
`blake2b` over a `Buffer` cannot throw, and `fetchAnchorBytes` never throws by task-149's contract.

#### Step 4 — Add the IPC contract

Edit `source/common/ipc/api.ts`.

**4a.** Extend the governance-types import at `:85-88` to:

```ts
import type {
  DRepListQueryPayload,
  DRepStakeQueryPayload,
  DRepAnchorPresence,
  DRepAnchorResult,
} from '../types/governance.types';
```

**4b.** Append after `:666` (`export type GovernanceDRepStakeMainResponse = DRepStakeQueryPayload;` — currently the last
line of the file):

```ts

export const GOVERNANCE_DREP_ANCHOR_CHANNEL = 'GOVERNANCE_DREP_ANCHOR_CHANNEL';
export type GovernanceDRepAnchorRendererRequest = DRepAnchorPresence;
export type GovernanceDRepAnchorMainResponse = DRepAnchorResult;
```

The request is the on-chain anchor pair and **nothing else — no `drepId`**. The renderer correlates the response itself.

#### Step 5 — Create the main handler

Create `source/main/ipc/governanceAnchorChannel.ts`:

```ts
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GOVERNANCE_DREP_ANCHOR_CHANNEL } from '../../common/ipc/api';
import type {
  GovernanceDRepAnchorRendererRequest,
  GovernanceDRepAnchorMainResponse,
} from '../../common/ipc/api';
import { AnchorFetchErrorType } from '../../common/types/governance.types';
import { resolveVerifiedAnchor } from '../governance/AnchorVerificationService';
import { logger } from '../utils/logging';

const governanceDRepAnchorChannel: MainIpcChannel<
  GovernanceDRepAnchorRendererRequest,
  GovernanceDRepAnchorMainResponse
> = new MainIpcChannel(GOVERNANCE_DREP_ANCHOR_CHANNEL);

// The request carries an anchor URL, so nothing from it may be logged; the
// response is enum-shaped and the handler never rejects.
export const handleGovernanceAnchorRequests = () => {
  governanceDRepAnchorChannel.onRequest(async (anchor) => {
    let result: GovernanceDRepAnchorMainResponse;
    try {
      result = await resolveVerifiedAnchor(anchor);
    } catch {
      result = {
        status: 'unavailable',
        reason: AnchorFetchErrorType.InvalidRequest,
      };
    }
    logger.info('Governance IPC: anchor resolution finished', {
      status: result.status,
      reason: result.status === 'unavailable' ? result.reason : undefined,
    });
    return result;
  });
};
```

#### Step 6 — Register the handler

Edit `source/main/ipc/index.ts`. Add the import immediately after `:29`:

```ts
import { handleGovernanceAnchorRequests } from './governanceAnchorChannel';
```

and the call immediately after `:51` (`handleGovernanceRequests();`):

```ts
  handleGovernanceAnchorRequests();
```

Skipping this call is invisible to every other gate in this slice: `tsc --noEmit`, `yarn lint` and all six specs stay
green while the channel is dead at runtime, because task-151's container tests mock the store. Verify step 9 greps for
it, and Step 6b pins the handler's own contract.

#### Step 6b — Pin the handler contract (`source/main/ipc/governanceAnchorChannel.spec.ts`)

New colocated spec, `@jest-environment node`, mocking `./lib/MainIpcChannel` the same way
`source/main/ipc/open-external-url.spec.ts` does — capture the constructed channel in a `mock`-prefixed array, take
the function passed to `onRequest`, and drive it directly. Also `jest.mock('../governance/AnchorVerificationService',
() => ({ resolveVerifiedAnchor: jest.fn() }))` and the four-method `logger` mock. Two cases:

1. `'returns the verification result unchanged'` — `resolveVerifiedAnchor` resolves
   `{ status: 'verified', content: { givenName: 'Daedalus Test DRep' }, host: 'anchor.example.org', fetchedAt: 1 }`;
   the handler resolves the same object.
2. `'resolves as unavailable when the verification service throws'` — `resolveVerifiedAnchor` rejects with
   `new Error('boom')`; the handler **resolves** (never rejects) `{ status: 'unavailable', reason:
   AnchorFetchErrorType.InvalidRequest }`, and no logger payload contains `'boom'`.

Case 2 is the only test of D-2's central promise — that the handler never rejects — because Step 10 case 13 drives
`resolveVerifiedAnchor`, not the handler.

#### Step 7 — Add the renderer channel export

Edit `source/renderer/app/ipc/governanceChannel.ts`.

Add `GOVERNANCE_DREP_ANCHOR_CHANNEL` to the value import at `:2-5` and
`GovernanceDRepAnchorMainResponse`, `GovernanceDRepAnchorRendererRequest` to the type import at `:6-11`, then append
after `:21`:

```ts

export const governanceDRepAnchorChannel: RendererIpcChannel<
  GovernanceDRepAnchorMainResponse,
  GovernanceDRepAnchorRendererRequest
> = new RendererIpcChannel(GOVERNANCE_DREP_ANCHOR_CHANNEL);
```

Note the type-param order is **reversed** relative to main. That asymmetry is the repo's existing shape; do not
"fix" it.

#### Step 8 — Commit the offline fixtures, format first, hash second

There is no network here and no CIP-119 body is committed anywhere in the repo, so the fixture is synthetic and
clearly labelled as such.

**8a.** Create `tests/mocks/governance/anchor-cip119-sample.json`:

```json
{
  "@context": {
    "CIP119": "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/README.md#",
    "givenName": "CIP119:givenName",
    "objectives": "CIP119:objectives",
    "motivations": "CIP119:motivations",
    "qualifications": "CIP119:qualifications"
  },
  "hashAlgorithm": "blake2b-256",
  "body": {
    "givenName": "Daedalus Test DRep",
    "objectives": "Synthetic fixture objectives for offline anchor verification tests.",
    "motivations": "Synthetic fixture motivations for offline anchor verification tests.",
    "qualifications": "Synthetic fixture qualifications for offline anchor verification tests."
  }
}
```

**8b.** Create `tests/mocks/governance/anchor-malformed.txt` with exactly `{"body":` and a trailing newline. The `.txt`
extension keeps editor/lint JSON validation off it, and `.prettierignore` does not format it.

**8c.** Format the JSON fixture **before** hashing it — prettier formats `tests/**/*.json`, so hashing first would
commit a digest of bytes that no longer exist:

```bash
cd /home/node/.claude/jobs/f104125f/wt-anchor-1
node_modules/.bin/prettier --write tests/mocks/governance/anchor-cip119-sample.json
```

**8d.** Generate `tests/mocks/governance/anchor-cip119-sample.hash` — one lowercase 64-hex line plus `\n`, **generated,
never typed**:

```bash
cd /home/node/.claude/jobs/f104125f/wt-anchor-1
node -e "const fs=require('fs');const blake2b=require('blake2b');\
const d=fs.readFileSync('tests/mocks/governance/anchor-cip119-sample.json');\
process.stdout.write(blake2b(32).update(d).digest('hex')+'\n')" \
  > tests/mocks/governance/anchor-cip119-sample.hash
```

Committing the literal rather than recomputing it inside the test pins the algorithm — blake2b-256, unkeyed, no
personalization — against a constant instead of against itself.

**8e.** Create `tests/mocks/governance/README.md`:

```markdown
# Governance test fixtures

`anchor-cip119-sample.json` is a **synthetic** CIP-119 anchor body with an abbreviated `@context`. It is not a copy of
any real DRep's metadata.

`anchor-cip119-sample.hash` is its Blake2b-256 digest, generated from the committed bytes with
`blake2b(32).update(bytes).digest('hex')`. Regenerate it whenever the JSON changes, and always after prettier has
formatted the JSON.

`anchor-malformed.txt` is bytes that fetch cleanly but do not parse.

The real CIP-119 vectors are `https://sipo.tokyo/drep/SIPO.jsonld` (mainnet) and the Cardano Academy preprod
`.jsonld`. The real on-chain `(url, hash)` pair used for cache-key and hash-mismatch tests is at
`.agent/plans/governance/drep-discovery/research/drep-state-preprod-epoch295-sample.json:2852-2855`. No offline copy of
either body exists in this repo.

Cached anchor bodies are public on-chain-referenced directory data keyed by a content hash. No DRep id is stored with
them and none reaches a logger.
```

If task-149 already created any of these four files, verify the digest still reproduces (re-run 8d and `git diff` the
`.hash`) and leave them in place rather than recreating them.

#### Step 9 — Write `tests/jest/governance/anchorCache.spec.ts` (9 cases)

Header: docblock ending in `@jest-environment node`, then the hoisted config mock (copy the shape at
`logDRepStateSnapshot.spec.ts:7-19`, using `nodePath.join(nodeOs.tmpdir(), 'anchor-cache-spec')` as the state dir and
exporting `stateDirectoryPath` from the factory). `beforeEach` removes the cache directory recursively
(`fs.rmSync(dir, { recursive: true, force: true })`) and `jest.restoreAllMocks()`.

Helper: `const hashOf = (n: number) => n.toString(16).padStart(64, '0');`

1. `writeVerifiedAnchorBytes` then `readVerifiedAnchorBytes` returns the identical bytes, and the file lands at
   `<stateDir>/DRep-anchor-cache/<hash>.json`.
2. A second write for the same hash with different bytes leaves the original file byte-identical (immutability;
   `EEXIST` is success and does not log a warning — assert `logger.warn` was not called).
3. `readVerifiedAnchorBytes` returns `null` for a hash with no file.
4. `readVerifiedAnchorBytes('../../../etc/passwd')` returns `null` and `jest.spyOn(fs, 'readFileSync')` was **not**
   called.
5. `writeVerifiedAnchorBytes('../../../etc/passwd', bytes)` is a no-op: `jest.spyOn(fs, 'writeFileSync')` and
   `jest.spyOn(fs, 'mkdirSync')` were **not** called.
6. An uppercase 64-hex hash is rejected by both primitives (read returns `null`, write does not call `fs.writeFileSync`)
   — keys are lowercase-normalised before they reach the cache.
7. `deleteVerifiedAnchorBytes` removes the entry and a subsequent read returns `null`.
8. Eviction: pre-create `ANCHOR_CACHE_MAX_ENTRIES + 1` files directly with `fs.writeFileSync` under `hashOf(i)`, set
   ascending mtimes with `fs.utimesSync` so ordering is deterministic, then call `writeVerifiedAnchorBytes` once for a
   new hash. Assert the directory holds at most `ANCHOR_CACHE_MAX_ENTRIES` files, that `hashOf(0).json` (oldest) is
   gone, and that the newly written entry is still readable.
9. Restart survival: write an entry, `jest.resetModules()`, re-`require` the module (same mocked state dir), and read
   the entry back byte-identically.

#### Step 10 — Write `tests/jest/governance/AnchorVerificationService.spec.ts` (13 cases)

**This spec is the point of task-150.** The ordering assertions are what prove the invariant.

Header: docblock ending in `@jest-environment node`; the same hoisted `source/main/config` mock (state dir
`anchor-verification-spec`); plus

```ts
jest.mock('../../../source/main/governance/AnchorFetchService', () => ({
  fetchAnchorBytes: jest.fn(),
}));
```

Then `const mockFetchAnchorBytes = fetchAnchorBytes as jest.Mock;`.

Fixture loading:

```ts
const FIXTURE_DIR = path.join(__dirname, '../../mocks/governance');
const SAMPLE_BYTES = fs.readFileSync(path.join(FIXTURE_DIR, 'anchor-cip119-sample.json'));
const SAMPLE_HASH = fs.readFileSync(path.join(FIXTURE_DIR, 'anchor-cip119-sample.hash'), 'utf8').trim();
const MALFORMED_BYTES = fs.readFileSync(path.join(FIXTURE_DIR, 'anchor-malformed.txt'));
const ONCHAIN_URL = 'https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld';
const OTHER_HASH = '9e8cb2b0f4c2ddbd9dea316b44680d8a989743868aeb40c1e6959982452f38e1';
const okResult = (bytes: Buffer) => ({ ok: true, bytes, host: 'raw.githubusercontent.com', contentType: 'application/json', byteLength: bytes.length });
```

`beforeEach` clears the cache directory, resets `mockFetchAnchorBytes` and `jest.restoreAllMocks()`.

1. **Verified path.** `mockFetchAnchorBytes` resolves `okResult(SAMPLE_BYTES)`; `resolveVerifiedAnchor({ url:
   ONCHAIN_URL, hash: SAMPLE_HASH })` returns `{ status: 'verified', content: { givenName: 'Daedalus Test DRep' },
   host: 'raw.githubusercontent.com' }` with a numeric `fetchedAt`.
2. **Algorithm cross-check** (free, offline, no network needed): `anchorDigest(SAMPLE_BYTES)`,
   `blakejs.blake2bHex(SAMPLE_BYTES, null, 32)` and the committed `SAMPLE_HASH` are all equal. Agreement between two
   independent implementations rules out a keying or personalization mistake.
3. **A wrong-hash blob never reaches the parser.** `mockFetchAnchorBytes` resolves `okResult(SAMPLE_BYTES)` but the
   request carries `OTHER_HASH`. Install `const parseSpy = jest.spyOn(JSON, 'parse');` **before** the call. Assert the
   result is `{ status: 'unavailable', reason: AnchorFetchErrorType.HashMismatch }` **and** `parseSpy` was not called
   **and** `jest.spyOn(fs, 'writeFileSync')` was not called.
4. **A hash mismatch writes nothing to cache.** After case 3's setup, assert
   `readVerifiedAnchorBytes(OTHER_HASH) === null` and that the cache directory contains no `${OTHER_HASH}.json`.
5. **Parse failure after successful verification is graceful.** `mockFetchAnchorBytes` resolves
   `okResult(MALFORMED_BYTES)` and the request hash is `anchorDigest(MALFORMED_BYTES)` computed in the test. Assert
   `{ status: 'unavailable', reason: AnchorFetchErrorType.ParseFailed }`, and that the result object has **no**
   `content` property (`expect('content' in result).toBe(false)`) — no partial render is possible.
6. **`body` present, `givenName` absent** → `ParseFailed`. Build the bytes inline as
   `Buffer.from(JSON.stringify({ body: {} }))` and hash them in the test.
7. **A cache hit does not issue a request.** Seed the cache with `writeVerifiedAnchorBytes(SAMPLE_HASH, SAMPLE_BYTES)`,
   then resolve. Assert `mockFetchAnchorBytes` was **not** called and the result is `verified` with the fixture's
   `givenName`.
8. **A changed on-chain hash invalidates.** Seed the cache under `SAMPLE_HASH`, then resolve the **same URL** with
   `OTHER_HASH` while `mockFetchAnchorBytes` resolves `{ ok: false, reason: AnchorFetchErrorType.HttpStatus }`. Assert
   `mockFetchAnchorBytes` **was** called once and the result is `{ status: 'unavailable', reason: HttpStatus }` — the
   stale `SAMPLE_HASH` bytes are never served for the new hash. Also assert the `SAMPLE_HASH` entry still exists on disk
   (a different key is untouched, not evicted by the miss).
9. **A tampered cache file is treated as a miss.** Write `Buffer.from('tampered')` directly to
   `<cacheDir>/${SAMPLE_HASH}.json` with `fs.writeFileSync`, then resolve with `SAMPLE_HASH` while
   `mockFetchAnchorBytes` resolves `okResult(SAMPLE_BYTES)`. Assert `mockFetchAnchorBytes` was called, the file no longer
   contains `tampered`, and the result is `verified`.
10. **Transport failures pass through unchanged.** For each of `AnchorFetchErrorType.TooLarge`, `.Timeout` and
    `.BlockedAddress`, `mockFetchAnchorBytes` resolves `{ ok: false, reason }` and the result is
    `{ status: 'unavailable', reason }`. (One `it` with a loop, or `it.each` — one test either way.)
11. **An invalid on-chain hash never touches disk or network.** For `'../../etc/passwd'`, `'ZZZZ'`, `''` and
    `SAMPLE_HASH.toUpperCase() + 'x'`, the result is `{ status: 'unavailable', reason: InvalidRequest }`,
    `mockFetchAnchorBytes` was not called, and `jest.spyOn(fs, 'readFileSync')` / `jest.spyOn(fs, 'writeFileSync')` were
    not called.
12. **Concurrent resolutions collapse to one fetch.** Call `resolveVerifiedAnchor` twice with the same `(url, hash)`
    without awaiting between, `await Promise.all([...])`, and assert `mockFetchAnchorBytes` was called exactly once and
    both results are `verified`.
13. **No logger payload carries an anchor URL, host, hash or DRep id.** Spy on all four levels of the main logger
    (`jest.spyOn(mainLogger, 'info' | 'warn' | 'error' | 'debug')`), drive the verified, `HashMismatch`, `ParseFailed`
    and `InvalidRequest` paths through `handleGovernanceAnchorRequests`' resolver (call `resolveVerifiedAnchor` plus one
    direct `writeVerifiedAnchorBytes` failure), serialize every captured payload with `JSON.stringify`, and assert the
    string contains none of `ONCHAIN_URL`, `'raw.githubusercontent.com'`, `SAMPLE_HASH`, `OTHER_HASH`, or a
    `drep1`-prefixed sentinel.

#### Verify

```bash
cd /home/node/.claude/jobs/f104125f/wt-anchor-1

# 1. The committed digest still matches the committed bytes (must print nothing).
node -e "const fs=require('fs');const blake2b=require('blake2b');\
const d=fs.readFileSync('tests/mocks/governance/anchor-cip119-sample.json');\
const want=fs.readFileSync('tests/mocks/governance/anchor-cip119-sample.hash','utf8').trim();\
const got=blake2b(32).update(d).digest('hex');\
if(got!==want){console.error('DIGEST DRIFT',got,want);process.exit(1)}"

# 2. Typecheck — exit 0, ~22s. Clean at HEAD, so any error here is yours.
yarn compile

# 3. The two new suites.
node_modules/.bin/jest --testPathPattern="tests/jest/governance/anchorCache" --no-coverage --runInBand
#   expect: 1 suite, 9 tests, all green
node_modules/.bin/jest --testPathPattern="tests/jest/governance/AnchorVerificationService" --no-coverage --runInBand
#   expect: 1 suite, 13 tests, all green
node_modules/.bin/jest --testPathPattern="source/main/ipc/governanceAnchorChannel" --no-coverage --runInBand
#   expect: 1 suite, 2 tests, all green

# 3b. The handler is actually registered. Without this the channel is dead at
#     runtime and every other gate in this slice still passes.
grep -n "handleGovernanceAnchorRequests" source/main/ipc/index.ts
#   expect: exactly two hits - the import and the call

# 4. The whole governance directory.
node_modules/.bin/jest --testPathPattern="tests/jest/governance" --no-coverage --runInBand
#   measured baseline at bf112d9f8, BEFORE task-149: 5 passed suites + 1 skipped (6 total),
#   98 passed + 12 skipped (110 total). task-150 adds exactly 2 suites / 22 tests on top of
#   whatever task-149 left, so expect >= 7 passed suites and >= 120 passing tests.

# 5. Sanitization floor — cv-2 F-31 requires BOTH anchors, cited together.
node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" --no-coverage --runInBand
#   expect: 1 suite; 26 tests at HEAD, more once task-149's cases land; all green
node_modules/.bin/jest --testPathPattern="containers/voting/VotingGovernancePage.spec" --no-coverage --runInBand
#   expect: 1 suite, all green

# 6. Lint — exit 0. The warning baseline (~5591) moves because the new source files
#    live under source/; that is expected, not a regression.
yarn lint

# 7. Format. nix fmt is unavailable here and stays a user-owned pre-merge obligation.
node_modules/.bin/prettier --write \
  source/main/governance/anchorCache.ts \
  source/main/governance/AnchorVerificationService.ts \
  source/main/ipc/governanceAnchorChannel.ts \
  source/main/ipc/governanceAnchorChannel.spec.ts \
  source/main/ipc/index.ts \
  source/common/ipc/api.ts \
  source/common/types/governance.types.ts \
  source/renderer/app/ipc/governanceChannel.ts \
  tests/jest/governance/anchorCache.spec.ts \
  tests/jest/governance/AnchorVerificationService.spec.ts \
  tests/mocks/governance/anchor-cip119-sample.json

# 8. If step 7 rewrote the fixture, re-run step 1 and regenerate the .hash before committing.
```

Do **not** run `yarn i18n:manage` — task-150 mints no copy.

Commit (one subject line, no body, no trailer):

```
feat(gov): task-150 verify, cache and parse DRep anchor bytes
```

#### Acceptance

| AC | Verbatim criterion | How the steps above discharge it | Status |
|---|---|---|---|
| AC-1 | "Blake2b-256 hash verification runs on bounded raw bytes before JSON parsing." | Step 3: `resolveFromCacheOrFetch()` calls `anchorDigest(fetched.bytes) !== hash` and returns `HashMismatch` before `parseVerifiedContent` is ever reached; the cache-hit branch re-runs `anchorDigest(cached)` before parsing too. The bytes are bounded because they come only from task-149's `fetchAnchorBytes` (~1 MB cap). Proven by Step 10 case 3 — a `jest.spyOn(JSON, 'parse')` that must not fire — and case 2 pins the algorithm against the committed digest and a second implementation. | **green, provable offline** |
| AC-2 | "Only hash-verified bytes/content are written to immutable cache keyed by anchor hash." | Step 3: `writeVerifiedAnchorBytes` is called only after the digest comparison passes, and it is the sole cache writer. Step 2: the key is the validated lowercase 64-hex hash, the filename is `<hash>.json`, the write uses `{ flag: 'wx' }` and never rewrites in place. Proven by Step 10 cases 3-4 (mismatch writes nothing; `fs.writeFileSync` spy silent) and Step 9 case 2 (a second write leaves the original bytes untouched). | **green, provable offline** |
| AC-3 | "Parse failures surface as graceful anchor-unavailable states without rendering partial content." | Step 3: `parseVerifiedContent` returns `null` on a `JSON.parse` throw, a non-object document, a missing/non-object `body`, or a missing/blank `givenName`; every one maps to `{ status: 'unavailable', reason: ParseFailed }`, a variant that structurally has no `content` field. Step 5: the IPC handler never rejects, so the renderer sees one shape. Proven by Step 10 cases 5-6, including `expect('content' in result).toBe(false)`. The *render* half is task-151's; task-150 guarantees the renderer is never handed partial content. | **green for the main-process half; the render assertion is task-151's** |
| AC-4 | "Stale cached content for a changed on-chain anchor hash is not served." | Step 2 + resolved judgment call 6: a changed on-chain hash is a different filename, so no code path can read the old entry for the new hash — the property is structural, not a branch. Step 3's cache-hit branch additionally re-verifies the file's own digest and deletes on mismatch. Proven by Step 10 case 8 (seed under hash A, resolve under hash B → fetch is issued and A's content is never returned) and case 9 (tampered file treated as a miss). | **green, provable offline** |

**OWED — not green, and must not be reported as green:**

1. **A live anchor fetch.** There is no network in this devcontainer. Every path above is proven against a mocked
   `fetchAnchorBytes`; nothing here proves behaviour against a real anchor host.
2. **The real SIPO / Cardano Academy CIP-119 body bytes.** No CIP-119 body is committed anywhere in the repo and the
   digest at `drep-state-preprod-epoch295-sample.json:2853` cannot be reproduced offline. The real pair drives the
   cache-key and mismatch paths in mechanism only; the content half is task-151 AC-4's OWED item and is carried forward.
3. **`nix fmt` before merge.** `nix` is absent here; `node_modules/.bin/prettier --write <explicit paths>` is the
   substitute and the real `nix fmt` run is a user-owned obligation.

**Notes on the decisions this section implements** (recorded, not re-opened):

- The decisions doc gives `readVerifiedAnchorBytes` / `writeVerifiedAnchorBytes` as "rejecting" an invalid hash. Their
  fixed return types (`Buffer | null`, `void`) admit no throw, so this section implements rejection as
  `null` / no-op, and surfaces `InvalidRequest` from `resolveVerifiedAnchor`, which is the only caller that can express
  it. The observable contract — "never touches the filesystem" — is unchanged and directly tested.
- `AnchorFetchOk.host` is deliberately not used: `host` is derived once from `anchor.url`, because the cache-hit path
  has no transport result and two derivations could disagree.

### task-151 — Render verified givenName with source label + expose metadata-completeness

`estimatedHours: 4.25` · `priority: high` · `dependencies: ["task-150", "task-116"]` · `targetPath: source/renderer/app/components/` · mode: **autonomous**.

#### Context

**Path correction, read this first.** The governance components live under `source/renderer/app/components/governance/`, **not** `.../components/voting/governance/`. Every path below is verified in this worktree.

**Preconditions this task consumes from task-150 (verify before Step 1; if any is missing, stop and finish task-150):**

```bash
grep -n "GOVERNANCE_DREP_ANCHOR_CHANNEL" source/common/ipc/api.ts
grep -n "AnchorFetchErrorType\|VerifiedDRepAnchorContent\|DRepAnchorResult" source/common/types/governance.types.ts
grep -n "governanceDRepAnchorChannel" source/renderer/app/ipc/governanceChannel.ts
ls tests/mocks/governance/anchor-cip119-sample.json tests/mocks/governance/anchor-cip119-sample.hash
```

The shapes task-150 leaves behind (do not redefine them):

```ts
// source/common/types/governance.types.ts
export interface VerifiedDRepAnchorContent { givenName: string | null }
export type DRepAnchorResult =
  | { status: 'verified'; content: VerifiedDRepAnchorContent; host: string; fetchedAt: number }
  | { status: 'unavailable'; reason: AnchorFetchErrorType };

// source/renderer/app/ipc/governanceChannel.ts
export const governanceDRepAnchorChannel: RendererIpcChannel<
  GovernanceDRepAnchorMainResponse,      // = DRepAnchorResult
  GovernanceDRepAnchorRendererRequest    // = DRepAnchorPresence
>;
```

**`source/common/types/governance.types.ts:51-62` — the wire entry, five fields, no name:**

```ts
export interface DRepDirectoryEntry {
  /** CIP-129 bech32-encoded DRep ID derived from on-chain credential. */
  drepId: DRepId;
  /** Voting power in lovelace as a decimal string; null when no stake is available. */
  votingPower: Lovelace | null;
  /** Active / Inactive. */
  status: DRepStatus;
  /** Remaining epochs until expiry (null if unknown). */
  drepActivity: DrepActivity;
  /** Anchor presence (URL + hash) from on-chain data. No fetch performed in slice-1. */
  anchor: DRepAnchorPresence | null;
}
```

`:66-71` is `DRepAnchorPresence { url: string; hash: string }` and stays the on-chain reference pair.

**`source/renderer/app/stores/GovernanceStore.ts:20-31` — the app entry, also five fields, no name:**

```ts
export interface AppDRepDirectoryEntry {
  /** Bech32-encoded DRep ID. */
  drepId: string;
  /** Voting power in lovelace as BigNumber, or null if ranking unavailable. */
  votingPower: BigNumber | null;
  /** Active / Inactive. */
  status: DRepDirectoryEntry['status'];
  /** Remaining epochs until expiry (null if unknown). */
  drepActivity: DRepDirectoryEntry['drepActivity'];
  /** Anchor presence (URL + hash) from on-chain. No fetch performed in slice-1. */
  anchor: DRepAnchorPresence | null;
}
```

**`GovernanceStore.ts:379-387` — the only renderer construction site:**

```ts
  private _rehydrateDReps(raw: DRepDirectoryEntry[]): AppDRepDirectoryEntry[] {
    return raw.map((entry) => ({
      drepId: entry.drepId,
      votingPower: entry.votingPower ? new BigNumber(entry.votingPower) : null,
      status: entry.status,
      drepActivity: entry.drepActivity,
      anchor: entry.anchor,
    }));
  }
```

**`source/main/governance/GovernanceQueryService.ts:518` — the only main construction site** (inside `_parseDRepState`, declared `: DRepDirectoryEntry[]` at `:458-461`):

```ts
        return { drepId, votingPower, status, drepActivity, anchor };
```

**The two list-rebuild sites the name projection must survive** — `GovernanceStore.ts:251-259` (`fetchDRepList`) and `:288-299` (`_enrichVotingPower`); both do `this.drepList = entries; this.drepIndex = new Map(entries.map((e) => [e.drepId, e]));`.

**`source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx:18-35` — the whole body that changes:**

```ts
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

  return <span className={className}>{intl.formatMessage(message)}</span>;
}
```

**`.../drep-detail/DRepDetailAnchorSection.tsx:35-38` (props) and `:49-79` (body).** `:55-56` carries the gating comment this phase retires:

```
            {/* Deliberately inert text: no anchor may be fetched, rendered as
                a link, or opened before the hardened anchor pipeline lands. */}
```

**`.../drep-detail/DRepDetail.tsx:41-48` (props) and `:98-116` (body).** The body order is backLink → `<h1>` title → `.header` (`DRepIdDisplay` `:103`, `DRepCategoryBadge` `:104`) → `<DRepDetailOnchainSection ... />` `:106-109` → `<DRepDetailAnchorSection anchor={entry.anchor} />` `:110` → `<DRepDetailActions ... />` `:111-114`.

**`.../drep-detail/DRepDetailOnchainSection.tsx` — exactly four `fieldRow`s, unchanged by this task:** Status `:94`, Expires in `:102`, Voting power `:114`, Current votes `:137` (value = `governance.drepDetail.votePositions.unavailable`).

**Container `source/renderer/app/containers/governance/DRepDetailPage.tsx`** — `:43-50` holds the single existing `reaction` on `isNodeInSync`, disposed at `:53-58`; `:89-96` is the `<DRepDetail>` element.

**Design contracts, quoted, not paraphrased.**

`designs/shared-design-tokens.md:51-53` (§2, three unshipped labels):

> | **Verified off-chain content** | Anchor-derived fields after hash verification (`anchor-1` onward) | small pill, `--source-verified-fg`, check-shield icon | "Fetched from {host}, hash-matched the on-chain anchor hash." |
> | **Unverified anchor** | Anchor *content* has been fetched but not yet hash-verified (transitional state during the `anchor-1` fetch pipeline). Never applied to the raw URL/hash pair on-chain — that uses **On-chain anchor reference**. | small pill, `--source-unverified-fg`, dashed-circle icon | "Anchor content fetched but not yet hash-verified. Treat as untrusted." |
> | **Anchor unavailable** | Fetch or hash check failed | small pill, `--source-warning-fg`, warning triangle | "The anchor URL could not be retrieved or did not match the on-chain hash. Off-chain profile is not shown." |

Their message ids are inventoried at `shared-design-tokens.md:205-207`.

`designs/drep-discovery-design.md:216`:

> In anchor-1 (givenName) and anchor-2 (remaining fields), after `GovernanceQueryService` + anchor fetch verify the content, the section adds a child `DRepDetailAnchorContent` rendering `givenName`, `objectives`, `motivations`, `qualifications`, `references[Link|Identity]`, `paymentAddress`. Each rendered field carries the `Verified off-chain content` label. `DRepCard` does **not** render verified anchor content even after anchor-1/anchor-2 (cards stay on-chain-only) — the verified enrichment surfaces in detail and favorites only.

`governance-drep-discovery-plan.md:335` (risk row, binding on the copy):

> Hash verification proves only that the registrant authored the blob — which an impersonator satisfies exactly — so it is not on its own a mitigation for a claimed identity.

`research/external-research.md:59`: `givenName` is "**Compulsory** in metadata", "≤80 chars, no markdown", "Display as DRep name once verified; truncate with ellipsis + tooltip beyond ~32 chars in cards." `:69-71` — reject outright: aggregate trust scores / star ratings, inline social embeds, "Identity verification claims as facts."

`governance-drep-discovery-plan.md:165`: "Directory cards and search are **DRep-ID-only** in v1. Verified `givenName` (CIP-119) appears only in the detail view (anchor-1) and confirmation."

**Measured baselines at `bf112d9f8` (worktree clean):**

| Suite | Tests | Snapshots |
|---|---|---|
| `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx` | 12 | 1 |
| `tests/jest/governance/GovernanceStore.spec.ts` | 35 | 0 |
| `tests/jest/governance/GovernanceQueryService.spec.ts` | 38 | 0 |
| `tests/jest/governance/logDRepStateSnapshot.spec.ts` | 4 | 0 |
| `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` | 4 | 0 |
| `--testPathPattern="(components/governance\|CurrentVoteSummary)"` (5 suites) | 101 | 8 |

i18n: both catalogs hold **84** `governance.*` keys, the key sets are identical, and all 168 strings carry `!!!`. The three `sourceLabel` keys and every `anchorContent` key are **absent** in both catalogs.

#### Locked invariants this change must not break

- **#3 Anchor transport-security floor** (`prompt.md:106-111`): TLS on, redirects off, ≤10 s connect+total timeout, ~1 MB cap, JSON content-type allow-list, SSRF + DNS-rebinding mitigation, Blake2b-256 hash-verify before parse/cache/render, immutable hash-keyed cache. **"No anchor-derived content renders without verification + a verified off-chain source label. Anchor URLs open only through the HTTPS-only-hardened `open-external-url` path (task-152)."** The renderer must therefore render `givenName` only from a `state: 'verified'` entry and must never fetch, parse or hash anything itself.
- **#2 Sanitization floor** (`prompt.md:101-105`): no DRep id, no `abstain`/`no_confidence` literal, no CIP-129/CIP-105 bech32 string in any logger, analytics or electron-store payload. anchor-1 widens it to anchor URLs, hosts and verified names. `logDRepStateSnapshot` (`source/main/utils/setupLogging.ts:178-183`) is the one documented exception and its payload must stay provably free of a verified name.
- **#1 local-first** (`prompt.md:98-100`): the anchor fetch is the only outbound call the feature makes and it happens in main. No explorer, indexer or aggregator; no renderer-side `fetch`.
- **#7 default cohort is binding** (`prompt.md:120-124`): "up to the next 200 eligible (active, remaining `drepActivity` > 6 epochs, completed metadata when available)". This task produces the completeness signal; it must not change `defaultCohort` itself.
- **#8 badges informational only** (`prompt.md:125-127`): the category badge never reorders, filters or overrides the cohort. `GovernanceStore` must not import anything from `DRepCategoryBadge`.
- **#11 preliminary copy** (`prompt.md:132-133`): every new en-US and ja-JP string keeps the leading `!!!`. Removing `!!!` is a release-end manual review, never a per-slice task.
- **Convergence rule** (`prompt.md:237-242`): reuse `RendererIpcChannel` and the `_shared` governance components. Zero new npm packages.

#### Resolved judgment calls (do not revisit)

1. **Both interfaces gain `verifiedName: string | null`** — required, never optional, matching `votingPower: Lovelace | null` (`governance.types.ts:55`) and `anchor: ... | null` (`:61`). An optional property lets a construction site omit it silently. The name is **`verifiedName`, not `givenName`**: the field may only ever hold a Blake2b-256-verified value, and naming it after the CIP-119 source field invites a future writer to fill it from unverified parse output. The CIP-119 → app mapping is documented once, at the parse boundary in main.
2. **Main sets it to `null` unconditionally.** The bulk `drep-state` query never fetches an anchor, so `GovernanceQueryService.ts:518` writes `verifiedName: null`. The field exists on the wire type to preserve the documented equivalence of the two interfaces (`GovernanceStore.ts:16-19`) and to reserve the slot for the deferred bulk-prefetch name-search phase without a breaking wire change.
3. **`entry.verifiedName` is a projection, not the source of truth.** `GovernanceStore.anchorStateByDRepId` is authoritative; `_applyVerifiedNames` re-applies the projection after every list rebuild.
4. **Completeness = `state === 'verified'`**, not "verified and has a name". A verified blob with a null `givenName` is still completed metadata.
5. **The store logs nothing at all on the anchor path.** A DRep id and a host are both forbidden payloads, and the only other datum is the reason enum, which is worthless without the id. This mirrors the shipped favorites rule at `GovernanceStore.ts:327-331` ("logging here is forbidden because the payload holds DRep ids"). `filterLogData` is renderer-only and matches key names by exact string equality, so it is not a substitute for not logging.
6. **`'unverified-anchor'` has no production emitter in anchor-1.** Main verifies before responding, and a hash mismatch maps to *Anchor unavailable* per `shared-design-tokens.md:53`. The variant and its copy are minted anyway because §2's five-label set is the design contract and Storybook renders it. Do not hunt for the missing path and do not add one.
7. **Tooltips are added only for the three new variants.** `on-chain` and `on-chain-anchor-reference` have no tooltip key in the catalogs and gain none here, so their rendered markup stays byte-identical and `source/renderer/app/components/voting/voting-governance/__snapshots__/CurrentVoteSummary.spec.tsx.snap` (four `<span class="sourceLabel">!!!On-chain</span>` blocks at `:27-31`, `:114-118`, `:199-203`, `:355-359`) must not change. If that snapshot moves, the tooltip guard is wrong.
8. **`host` is never re-parsed in the renderer.** It arrives on the IPC response, computed in main from the already-validated URL. A second parser is a second chance to disagree with the one that made the security decision.
9. **The name is clamped to 80 characters at the store boundary** with a trailing `…`, per CIP-119's `≤80 chars` (`research/external-research.md:59`). Nothing on the wire enforces the CIP limit, so a hostile anchor could otherwise ship an arbitrarily long "name" into the detail view. No truncation below 80 in the detail view — the ~32-char truncation in `research/external-research.md:59` is scoped to **cards**, and `DRepCard` renders no verified content at all (`drep-discovery-design.md:216`).
10. **No `dangerouslySetInnerHTML`, no markdown rendering, no linkification** of any anchor-derived string. React text nodes escape by default; that is the defence.
11. **Copy must not imply identity assurance.** No trust score, no star rating, no badge that reads as "verified identity", no social embed. The `anchorContent.caption` string added below states in the UI that the name is the DRep's own claim and that Daedalus does not verify identity — a direct discharge of `plan.md:335` and `research/external-research.md:71`. This one key is an addition beyond the seam-contract inventory; it is additive, not a substitution.
12. **`DRepCard`, search, sort and filter never read `verifiedName`.** `plan.md:165` keeps directory and search DRep-ID-only in v1.
13. **AC-5, AC-6 and AC-7 are discharged before start — schedule no work for them.** Verified in this worktree: `designs/drep-discovery-design.md:92` is now `│ │ Current votes: 2 Yes · 1 No · 0 Abstain (this epoch)     │  │`; `grep -rn "Registered: epoch" designs/` returns exactly one hit, `:106`, which is the prose correction, not a wireframe row. `:106` reads: "The On-chain box lists exactly the fields `DRepDetailOnchainSection` renders: Status, Expires in, Voting power, Current votes. There is deliberately no `Registered: epoch N` row — no local `drep-state` output carries a registration epoch, so restoring that row would need a new on-chain data source, not a UI change. … the row itself is not deferred and must stay in this box." Live code agrees: four `fieldRow`s at `DRepDetailOnchainSection.tsx:94,102,114,137`. **Corpus-vs-repo correction to record:** the AC text anchors `:92`/`:93` have drifted by one line — `:93` is now the closing box border. Prefer the live repo. Do not edit the wireframe again and do not restore anything.
14. **The `sensitiveData` extension is task-149's, not this task's.** `givenName`, `verifiedName`, `anchorUrl` and `anchorContent` are added to `source/common/utils/logging.ts:24-49` by task-149. Do not touch that file here.
15. **`cohortContext` / `cohortMedianVotingPower` / the classifier rewrite are task-172's.** This task exposes `verifiedMetadataIds` and stops. task-172 wires `DRepCohortContext.verifiedMetadataIds = this.verifiedMetadataIds`.
16. **This task owns the https link gate (D-5c).** task-152 hardened `open-external-url` and deliberately touched no governance component — its AC-3 requires anchor-URL rendering to stay *gated on* that landing, so it could not also ship the render. The gate is therefore Step 9's: the anchor URL becomes an `<a target="_blank" rel="noopener noreferrer">` routed through `AppStore.openExternalLink` **only** when `new URL(anchor.url).protocol === 'https:'`, and otherwise stays the inert `<dd>` it is today. A link that silently does nothing is worse than no link; the renderer gates the offer, main gates the action, and neither guard may be dropped because the other exists. The two-line gating comment at `DRepDetailAnchorSection.tsx:55-56` is retired by the same step.

#### Step 1 — Add `verifiedName` to the wire type

In `source/common/types/governance.types.ts`, inside `DRepDirectoryEntry` (`:51-62`), append after the `anchor` field:

```ts
  /** CIP-119 body.givenName, only ever set from Blake2b-256-verified anchor content. */
  verifiedName: string | null;
```

#### Step 2 — Set it in main

In `source/main/governance/GovernanceQueryService.ts`, replace the return at `:518`:

```ts
        return { drepId, votingPower, status, drepActivity, anchor };
```

with

```ts
        // The bulk drep-state query never fetches an anchor; the verified name
        // is filled in the renderer from the per-DRep anchor channel.
        return {
          drepId,
          votingPower,
          status,
          drepActivity,
          anchor,
          verifiedName: null,
        };
```

#### Step 3 — Add `verifiedName` to the app type and the rehydrator

In `source/renderer/app/stores/GovernanceStore.ts`, inside `AppDRepDirectoryEntry` (`:20-31`), append after `anchor`:

```ts
  /** Verified CIP-119 givenName, or null. Projection of anchorStateByDRepId. */
  verifiedName: string | null;
```

In `_rehydrateDReps` (`:379-387`) add `verifiedName: entry.verifiedName,` after `anchor: entry.anchor,`.

#### Step 4 — Fix every entry literal the new required field breaks

Run `node_modules/.bin/tsc --noEmit`. It enumerates every literal; add `verifiedName: null` to each. The construction sites, verified by `grep -rn "drepActivity:" source storybook tests --include=*.ts --include=*.tsx`:

Each bullet gives the `anchor:` line of the literal, so the new field goes on the line after it:

- `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx:32` (`baseEntry`, `:31-40`)
- `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx:30`
- `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx:133`
- `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx:30`, `:39`, `:76`
- `source/renderer/app/components/governance/drep-directory/helpers.spec.ts:57`
- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx:36`
- `storybook/stories/governance/CurrentVoteSummary.stories.tsx:49`
- `storybook/stories/governance/DRepDetail.stories.tsx:27` (`withAnchorEntry`, `:26-35`)
- `storybook/stories/governance/DRepDirectory.stories.tsx:42`, `:52`, `:61`
- `storybook/stories/governance/_utils/fixtures.ts:154`, `:164`
- `tests/jest/governance/GovernanceStore.spec.ts:45`, `:68`, `:328`, `:567`
- `tests/jest/governance/logDRepStateSnapshot.spec.ts:41` (inside `DRepListQueryPayload`, `:38-50`)

**Not affected:** `DRepCategoryBadge.spec.tsx` and `DRepCategoryBadge.stories.tsx` build `DRepCategorySource`, a `Pick<AppDRepDirectoryEntry, 'status' | 'drepActivity' | 'anchor'>`, which does not include the new field. `tsconfig.json` has no `include` and excludes only `node_modules`, so `tsc` covers `storybook/` too.

#### Step 5 — Add the anchor-enrichment state to `GovernanceStore`

Extend the type import at `GovernanceStore.ts:9-13` with `AnchorFetchErrorType` (value import — it is an enum) and add `DRepAnchorResult` as a type. Extend the ipc import at `:4-7` with `governanceDRepAnchorChannel`.

Declare, immediately after `AppDRepDirectoryEntry`:

```ts
export type AnchorEnrichEntry =
  | { state: 'loading'; hash: string }
  | { state: 'verified'; hash: string; givenName: string | null; host: string }
  | { state: 'unavailable'; hash: string; reason: AnchorFetchErrorType };
```

Add a module constant beside the cohort constants at `:60-62`:

```ts
/** CIP-119 caps givenName at 80 characters; nothing on the wire enforces it. */
const MAX_VERIFIED_NAME_LENGTH = 80;

function clampVerifiedName(name: string | null): string | null {
  if (name == null) return null;
  return name.length <= MAX_VERIFIED_NAME_LENGTH
    ? name
    : `${name.slice(0, MAX_VERIFIED_NAME_LENGTH - 1)}…`;
}
```

Add the observable beside `favoriteDRepIds` (`:127`):

```ts
  /**
   * Per-DRep anchor verification state, keyed by DRep id. An absent key is
   * idle. Replaced with a fresh Map instance on every change, like drepIndex.
   */
  @observable anchorStateByDRepId: Map<string, AnchorEnrichEntry> = new Map();
```

Add the computed after `showAllList` (`:215-218`):

```ts
  /** DRep ids whose anchor passed Blake2b-256 verification and parsed. */
  @computed get verifiedMetadataIds(): Set<string> {
    const ids = new Set<string>();
    this.anchorStateByDRepId.forEach((entry, drepId) => {
      if (entry.state === 'verified') ids.add(drepId);
    });
    return ids;
  }
```

Add the action after `reshuffleCohort` (`:322-325`):

```ts
  /**
   * Per-DRep, on-demand anchor verification. Nothing is logged on this path:
   * the only payloads available here are a DRep id and an anchor host.
   */
  @action
  async fetchAnchorContent(
    drepId: string,
    anchor: DRepAnchorPresence
  ): Promise<void> {
    const existing = this.anchorStateByDRepId.get(drepId);
    if (existing && existing.hash === anchor.hash) return;

    runInAction(() => {
      this.anchorStateByDRepId = new Map(this.anchorStateByDRepId).set(drepId, {
        state: 'loading',
        hash: anchor.hash,
      });
    });

    let result: DRepAnchorResult;
    try {
      result = await governanceDRepAnchorChannel.request(anchor);
    } catch (_ipcError) {
      result = {
        status: 'unavailable',
        reason: AnchorFetchErrorType.Network,
      };
    }

    runInAction(() => {
      const next: AnchorEnrichEntry =
        result.status === 'verified'
          ? {
              state: 'verified',
              hash: anchor.hash,
              givenName: clampVerifiedName(result.content.givenName),
              host: result.host,
            }
          : { state: 'unavailable', hash: anchor.hash, reason: result.reason };
      this.anchorStateByDRepId = new Map(this.anchorStateByDRepId).set(
        drepId,
        next
      );
      const entries = this._applyVerifiedNames(this.drepList);
      this.drepList = entries;
      this.drepIndex = new Map(entries.map((e) => [e.drepId, e]));
    });
  }
```

Add the private projection beside `_rehydrateDReps` (`:379-387`):

```ts
  /**
   * Re-applies verified names onto a freshly rebuilt list. A name is dropped
   * when the entry's on-chain anchor hash no longer matches the hash that was
   * verified, so a re-registered anchor can never keep showing the old name.
   */
  private _applyVerifiedNames(
    entries: AppDRepDirectoryEntry[]
  ): AppDRepDirectoryEntry[] {
    if (this.anchorStateByDRepId.size === 0) return entries;
    return entries.map((entry) => {
      const state = this.anchorStateByDRepId.get(entry.drepId);
      const verifiedName =
        state != null &&
        state.state === 'verified' &&
        entry.anchor != null &&
        entry.anchor.hash === state.hash
          ? state.givenName
          : null;
      return verifiedName === entry.verifiedName
        ? entry
        : { ...entry, verifiedName };
    });
  }
```

#### Step 6 — Re-apply the projection at both list-rebuild sites

In `fetchDRepList` (`:251-259`) change

```ts
        const entries = this._rehydrateDReps(payload.dreps);
```

to

```ts
        const entries = this._applyVerifiedNames(
          this._rehydrateDReps(payload.dreps)
        );
```

In `_enrichVotingPower` (`:288-297`) change

```ts
        const entries = this.drepList.map((entry) => {
          const stake = payload.stakeByDRepId[entry.drepId];
          return {
            ...entry,
            votingPower: stake ? new BigNumber(stake) : null,
          };
        });
```

to wrap the result:

```ts
        const entries = this._applyVerifiedNames(
          this.drepList.map((entry) => {
            const stake = payload.stakeByDRepId[entry.drepId];
            return {
              ...entry,
              votingPower: stake ? new BigNumber(stake) : null,
            };
          })
        );
```

Leave the following `this.drepList = entries;` / `this.drepIndex = new Map(...)` lines untouched in both places.

#### Step 7 — Extend `DRepSourceLabel` with the three new variants

Replace `source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx:4-35` so the file reads:

```tsx
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
  verified: {
    id: 'governance.drepDetail.sourceLabel.verified',
    defaultMessage: '!!!Verified off-chain content',
    description: 'Source label for hash-verified anchor content',
  },
  verifiedTooltip: {
    id: 'governance.drepDetail.sourceLabel.verified.tooltip',
    defaultMessage:
      '!!!Fetched from {host}, hash-matched the on-chain anchor hash.',
    description: 'Tooltip for the verified off-chain content source label',
  },
  unverified: {
    id: 'governance.drepDetail.sourceLabel.unverified',
    defaultMessage: '!!!Unverified anchor',
    description: 'Source label for fetched but not yet hash-verified content',
  },
  unverifiedTooltip: {
    id: 'governance.drepDetail.sourceLabel.unverified.tooltip',
    defaultMessage:
      '!!!Anchor content fetched but not yet hash-verified. Treat as untrusted.',
    description: 'Tooltip for the unverified anchor source label',
  },
  anchorUnavailable: {
    id: 'governance.drepDetail.sourceLabel.anchorUnavailable',
    defaultMessage: '!!!Anchor unavailable',
    description: 'Source label when the anchor fetch or hash check failed',
  },
  anchorUnavailableTooltip: {
    id: 'governance.drepDetail.sourceLabel.anchorUnavailable.tooltip',
    defaultMessage:
      '!!!The anchor URL could not be retrieved or did not match the on-chain hash. Off-chain profile is not shown.',
    description: 'Tooltip for the anchor unavailable source label',
  },
});

export type DRepSourceLabelVariant =
  | 'on-chain'
  | 'on-chain-anchor-reference'
  | 'verified-off-chain'
  | 'unverified-anchor'
  | 'anchor-unavailable';

type Props = {
  source: DRepSourceLabelVariant;
  host?: string;
  className?: string;
  intl: intlShape.isRequired;
};

function DRepSourceLabel({ source, host, className, intl }: Props) {
  const messageBySource: Record<DRepSourceLabelVariant, MessageDescriptor> = {
    'on-chain': messages.onChain,
    'on-chain-anchor-reference': messages.anchorReference,
    'verified-off-chain': messages.verified,
    'unverified-anchor': messages.unverified,
    'anchor-unavailable': messages.anchorUnavailable,
  };
  const tooltipBySource: Partial<
    Record<DRepSourceLabelVariant, MessageDescriptor>
  > = {
    'verified-off-chain': messages.verifiedTooltip,
    'unverified-anchor': messages.unverifiedTooltip,
    'anchor-unavailable': messages.anchorUnavailableTooltip,
  };
  const message = messageBySource[source];
  if (!message) return null;

  const label = intl.formatMessage(message);
  const tooltipMessage = tooltipBySource[source];
  if (!tooltipMessage) {
    return <span className={className}>{label}</span>;
  }

  const tooltip = intl.formatMessage(tooltipMessage, { host: host ?? '' });
  return (
    <span
      className={className}
      title={tooltip}
      aria-label={`${label}. ${tooltip}`}
    >
      {label}
    </span>
  );
}
```

Extend the react-intl import at `:2` to `import { defineMessages, injectIntl, intlShape } from 'react-intl';` plus `import type { MessageDescriptor } from 'react-intl';` — the same type import precedent as `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts:1`. The `tooltipBySource` annotation is required: without it TypeScript rejects indexing a three-key object literal with the five-member union.

#### Step 8 — Create `DRepDetailAnchorContent`

New file `source/renderer/app/components/governance/drep-detail/DRepDetailAnchorContent.tsx`. It imports `./DRepDetail.scss` — **do not add a new `.scss` file**; every class used below already exists there (`fieldList`, `fieldRow`, `fieldLabel`, `fieldValue`, `mutedValue`, `sectionTitle`, `sourceLabel`).

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import DRepSourceLabel from '../_shared/DRepSourceLabel';
import type { AnchorEnrichEntry } from '../../../stores/GovernanceStore';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDetail.anchorContent.title',
    defaultMessage: '!!!Off-chain profile',
    description: 'Heading of the verified anchor content block',
  },
  givenName: {
    id: 'governance.drepDetail.anchorContent.givenName',
    defaultMessage: '!!!Name',
    description: 'Label for the verified CIP-119 givenName field',
  },
  loading: {
    id: 'governance.drepDetail.anchorContent.loading',
    defaultMessage: '!!!Checking the anchor…',
    description: 'Shown while the anchor is being fetched and verified',
  },
  unavailable: {
    id: 'governance.drepDetail.anchorContent.unavailable',
    defaultMessage:
      '!!!The off-chain profile could not be verified. Only on-chain data is shown.',
    description: 'Shown when the anchor fetch or hash verification failed',
  },
  caption: {
    id: 'governance.drepDetail.anchorContent.caption',
    defaultMessage:
      "!!!This name is the DRep's own claim, hash-matched to the anchor recorded on-chain. Daedalus does not verify identity.",
    description: 'Caption stating that a verified name is not verified identity',
  },
});

interface Props {
  state: AnchorEnrichEntry | null;
  intl: intlShape.isRequired;
}

function DRepDetailAnchorContent({ state, intl }: Props) {
  if (!state) return null;

  if (state.state === 'loading') {
    return (
      <p className={styles.mutedValue}>{intl.formatMessage(messages.loading)}</p>
    );
  }

  if (state.state === 'unavailable') {
    return (
      <p className={styles.mutedValue}>
        {intl.formatMessage(messages.unavailable)}{' '}
        <DRepSourceLabel
          source="anchor-unavailable"
          className={styles.sourceLabel}
        />
      </p>
    );
  }

  if (state.givenName == null) return null;

  return (
    <>
      <h3 className={styles.sectionTitle}>
        {intl.formatMessage(messages.title)}
      </h3>
      <dl className={styles.fieldList}>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.givenName)}
          </dt>
          <dd className={styles.fieldValue}>
            {state.givenName}{' '}
            <DRepSourceLabel
              source="verified-off-chain"
              host={state.host}
              className={styles.sourceLabel}
            />
          </dd>
        </div>
      </dl>
      <p className={styles.mutedValue}>{intl.formatMessage(messages.caption)}</p>
    </>
  );
}

export default injectIntl(DRepDetailAnchorContent);
```

#### Step 9 — Mount the child and gate the anchor URL as an https link (D-5c)

In `source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection.tsx`:

- add `import DRepDetailAnchorContent from './DRepDetailAnchorContent';` and `import type { AnchorEnrichEntry } from '../../../stores/GovernanceStore';`
- extend the props at `:35-38` to `{ anchor: DRepAnchorPresence | null; anchorState: AnchorEnrichEntry | null; onOpenExternalLink: (url: string) => void; intl: intlShape.isRequired }` and the destructure at `:40`
- add this helper at module scope, directly under the `Props` interface:

```tsx
// The renderer offers the link only for schemes main will actually open, so a
// non-https anchor stays inert text instead of a link that does nothing.
const isHttpsAnchorUrl = (url: string): boolean => {
  try {
    return new URL(url).protocol === 'https:';
  } catch {
    return false;
  }
};
```

- replace the URL row's `<dd>` (currently `:55-57`: the two-line gating comment plus `<dd className={styles.anchorValue}>{anchor.url}</dd>`) with exactly this — the comment is deleted, not reworded:

```tsx
            <dd className={styles.anchorValue}>
              {isHttpsAnchorUrl(anchor.url) ? (
                <a
                  href={anchor.url}
                  target="_blank"
                  rel="noopener noreferrer"
                  onClick={(event: React.MouseEvent<HTMLAnchorElement>) => {
                    event.preventDefault();
                    onOpenExternalLink(anchor.url);
                  }}
                >
                  {anchor.url}
                </a>
              ) : (
                anchor.url
              )}
            </dd>
```

- wrap the true half of the `anchor ? (...)` ternary (`:49` opens it; the `<dl>` runs `:50-76`) in a fragment and append the child after the existing `</dl>`, replacing nothing:

```tsx
      {anchor ? (
        <>
          <dl className={styles.fieldList}>
            {/* the hash and Source rows unchanged; the URL row as rewritten above */}
          </dl>
          <DRepDetailAnchorContent state={anchorState} />
        </>
      ) : (
        <p className={styles.mutedValue}>{intl.formatMessage(messages.none)}</p>
      )}
```

Three things that are deliberate: `preventDefault()` then `onOpenExternalLink(url)` mirrors
`AppStore.openExternalLink`'s own `if (event) event.preventDefault()` (`AppStore.ts:79-82`) without handing it a React
synthetic event, so the prop type stays `(url: string) => void`; `href` is still set so the URL is copyable and
screen-reader-visible even though the click never navigates; and no SCSS changes — the `<a>` inherits
`styles.anchorValue`, so no `*.scss.d.ts` regeneration is needed.

#### Step 10 — Thread `anchorState` through `DRepDetail`

In `source/renderer/app/components/governance/drep-detail/DRepDetail.tsx`:

- add `import type { AnchorEnrichEntry } from '../../../stores/GovernanceStore';` beside the existing `AppDRepDirectoryEntry` type import at `:15`
- add `anchorState: AnchorEnrichEntry | null;` and `onOpenExternalLink: (url: string) => void;` to `Props` (`:41-48`) and both to the destructure (`:50-57`)
- change `:110` from `<DRepDetailAnchorSection anchor={entry.anchor} />` to `<DRepDetailAnchorSection anchor={entry.anchor} anchorState={anchorState} onOpenExternalLink={onOpenExternalLink} />`

Do not touch `:103-104` (`DRepIdDisplay`, `DRepCategoryBadge`) — the DRep id stays the primary identifier and the badge is task-172's.

#### Step 11 — Trigger the fetch from the container

In `source/renderer/app/containers/governance/DRepDetailPage.tsx`:

- add `anchorReactionDisposer: IReactionDisposer | null = null;` beside `:22`
- at the end of `componentDidMount`, after the existing reaction (`:43-50`):

```ts
    // Deep links mount before the list resolves, so fireImmediately plus the
    // reaction covers both arrival orders.
    this.anchorReactionDisposer = reaction(
      () =>
        governanceStore.drepIndex.get(this.props.match.params.drepId)?.anchor ??
        null,
      (anchor) => {
        if (anchor) {
          governanceStore.fetchAnchorContent(
            this.props.match.params.drepId,
            anchor
          );
        }
      },
      { fireImmediately: true }
    );
```

- dispose it in `componentWillUnmount` (`:53-58`), mirroring `syncReactionDisposer`
- in `render` (`:89-96`) add both props:

```tsx
        anchorState={governanceStore.anchorStateByDRepId.get(drepId) ?? null}
        onOpenExternalLink={stores.app.openExternalLink}
```

`AppStore.openExternalLink` (`AppStore.ts:79-82`) reads no `this`, so passing the method reference is safe — the same
shape `StakePoolsSettingsPage.tsx:24,33` and `VotingRegistrationPage.tsx:31,38` already use. Its declared signature is
`(url: string, event?: MouseEvent) => void`, which is assignable to the `(url: string) => void` prop because the second
parameter is optional.

#### Step 12 — Keep the DRep-state snapshot provably name-free

`source/main/utils/setupLogging.ts:183` passes the whole `DRepListQueryPayload` through to `DRep-state-snapshot.json`, deliberately bypassing `filterLogData` (`:178-182`). Step 2 makes `verifiedName` part of that payload. Add one case to `tests/jest/governance/logDRepStateSnapshot.spec.ts` asserting every `dreps[]` entry in the written snapshot has `verifiedName: null`. Do not change `setupLogging.ts`.

#### Step 13 — Mint the i18n keys

Add nothing by hand to the catalogs first — run `yarn i18n:manage` after the components above are in place, then fill the ja-JP values. Final state, **eleven new keys per catalog, all `!!!`-prefixed**:

| Key | en-US | ja-JP |
|---|---|---|
| `governance.drepDetail.sourceLabel.verified` | `!!!Verified off-chain content` | `!!!検証済みオフチェーンコンテンツ` |
| `governance.drepDetail.sourceLabel.verified.tooltip` | `!!!Fetched from {host}, hash-matched the on-chain anchor hash.` | `!!!{host}から取得し、オンチェーンのアンカーハッシュと一致しました。` |
| `governance.drepDetail.sourceLabel.unverified` | `!!!Unverified anchor` | `!!!未検証のアンカー` |
| `governance.drepDetail.sourceLabel.unverified.tooltip` | `!!!Anchor content fetched but not yet hash-verified. Treat as untrusted.` | `!!!アンカーコンテンツは取得されましたが、まだハッシュ検証されていません。信頼できないものとして扱ってください。` |
| `governance.drepDetail.sourceLabel.anchorUnavailable` | `!!!Anchor unavailable` | `!!!アンカーを利用できません` |
| `governance.drepDetail.sourceLabel.anchorUnavailable.tooltip` | `!!!The anchor URL could not be retrieved or did not match the on-chain hash. Off-chain profile is not shown.` | `!!!アンカーURLを取得できなかったか、オンチェーンのハッシュと一致しませんでした。オフチェーンプロフィールは表示されません。` |
| `governance.drepDetail.anchorContent.title` | `!!!Off-chain profile` | `!!!オフチェーンプロフィール` |
| `governance.drepDetail.anchorContent.givenName` | `!!!Name` | `!!!名前` |
| `governance.drepDetail.anchorContent.loading` | `!!!Checking the anchor…` | `!!!アンカーを確認中…` |
| `governance.drepDetail.anchorContent.unavailable` | `!!!The off-chain profile could not be verified. Only on-chain data is shown.` | `!!!オフチェーンプロフィールを検証できませんでした。オンチェーンデータのみを表示しています。` |
| `governance.drepDetail.anchorContent.caption` | `!!!This name is the DRep's own claim, hash-matched to the anchor recorded on-chain. Daedalus does not verify identity.` | `!!!この名前はDRep自身による申告であり、オンチェーンに記録されたアンカーとハッシュが一致しています。Daedalusは本人確認を行いません。` |

`governance.*` goes from **84 → 95** keys in each catalog, sets identical.

**Corpus-vs-repo correction, do not act on it:** `shared-design-tokens.md:204` inventories `governance.drepDetail.sourceLabel.onchain`, but the shipped key is `governance.drepDirectory.source.onChain` (`DRepSourceLabel.tsx:6`). Prefer the repo; do not mint a duplicate.

`yarn i18n:manage` writes **four** tracked files: `source/renderer/app/i18n/locales/en-US.json`, `source/renderer/app/i18n/locales/ja-JP.json`, `source/renderer/app/i18n/locales/defaultMessages.json` and `translations/messages.json`. All four are part of this task's diff and all four are committed together — precedent: commit `927978951`, which committed exactly those four for the currentVote keys. Restoring `defaultMessages.json` would desync it from the catalogs. `git restore` only hunks that touch a catalog key outside `governance.*`.

#### Step 14 — Widen the preliminary-copy guard

In `tests/jest/i18n/preliminaryCopyMarkers.spec.ts`, add a constant beside `CURRENT_VOTE_NAMESPACE` (`:12`):

```ts
const GOVERNANCE_NAMESPACE = 'governance.';
```

and a fifth case, modelled on the existing current-vote case (`:45-52`):

```ts
  it('keeps the preliminary marker on every governance key in both locales', () => {
    const unmarked = Object.keys(en)
      .filter((key) => key.startsWith(GOVERNANCE_NAMESPACE))
      .filter(
        (key) => !en[key].startsWith('!!!') || !ja[key].startsWith('!!!')
      );
    expect(unmarked).toEqual([]);
  });
```

#### Step 15 — Store tests

In `tests/jest/governance/GovernanceStore.spec.ts`, extend the ipc mock at `:18-21` with `governanceDRepAnchorChannel: { request: jest.fn() }` and add `const mockAnchorRequest = governanceDRepAnchorChannel.request as jest.Mock;`. Add a new `describe('GovernanceStore anchor enrichment')` with eight cases (35 → 43 tests):

1. a verified result writes the name into `drepList` and `drepIndex` and adds the id to `verifiedMetadataIds`
2. an unavailable result leaves `verifiedName` null, records `{ state: 'unavailable', reason }`, and keeps the id out of `verifiedMetadataIds`
3. a second call for the same hash makes no second IPC request; a changed on-chain hash re-triggers one
4. a rejected IPC request settles as `unavailable` with `AnchorFetchErrorType.Network` and never throws
5. the name survives a `fetchDRepList` rebuild and an `_enrichVotingPower` rebuild
6. a refresh that changes the entry's on-chain anchor hash drops the projected name
7. a `givenName` of 200 characters is stored clamped to 80 characters ending in `…`
8. no `logger.debug/info/warn/error` call is made anywhere on the anchor path, on either the verified or the unavailable branch

Use the real preprod on-chain pair for the anchor in these cases — `hash: '9e8cb2b0f4c2ddbd9dea316b44680d8a989743868aeb40c1e6959982452f38e1'`, `url: 'https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld'` — copied from `.agent/plans/governance/drep-discovery/research/drep-state-preprod-epoch295-sample.json:2852-2855`. (The seam-contract doc cites `:2852-2856`; the anchor object actually closes at `:2855`. Prefer the live file.)

#### Step 16 — Container tests

In `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx`, first extend `buildGovernanceStore` (`:42-51`) with `anchorStateByDRepId: new Map()` and `fetchAnchorContent: jest.fn()` — without them the new reaction throws and all 12 existing cases fail. The harness must also supply an `app` store stub with `openExternalLink: jest.fn()`, since the container now reads `stores.app.openExternalLink`. Then add eight cases (12 → 20 tests, snapshot count unchanged at 1):

1. requests the anchor content exactly once on mount for an entry that has an anchor, passing the `{ url, hash }` pair
2. requests nothing when the entry has no anchor
3. a `verified` state renders the name, the `!!!Verified off-chain content` label, and a `title` containing the host
4. an `unavailable` state renders `!!!The off-chain profile could not be verified. Only on-chain data is shown.` **and** still renders Status, Expires in, Voting power, Current votes, the anchor URL, the anchor hash and `!!!On-chain anchor reference` — the AC-2 regression
5. a `loading` state renders `!!!Checking the anchor…` and no name
6. the verified block renders in ja-JP through the same `locale: 'ja-JP'` harness (`:275-282`)
7. `'opens an https anchor url through the external-link handler'` — the URL row renders an `<a>` whose `href` is the entry's anchor URL and which carries `target="_blank"` and `rel="noopener noreferrer"`; clicking it calls the `openExternalLink` mock exactly once with that URL and does not navigate
8. `'renders a non-https anchor url as inert text'` — override the entry's `anchor.url` to `'http://anchor.example.org/profile.jsonld'`, assert the URL string still renders, assert the row contains **no** `<a>` element, and assert the `openExternalLink` mock was never called

Cases 7 and 8 are the D-5c gate. They are the only proof that the renderer's offer and main's action agree, because `open-external-url.spec.ts` (task-152) proves only the main half.

Feed states by overriding `anchorStateByDRepId` in `governanceOverrides`, e.g. `new Map([[DREP_ID, { state: 'verified', hash: baseEntry.anchor!.hash, givenName: 'Daedalus Test DRep', host: 'raw.githubusercontent.com' }]])`.

Do not touch `DRepCategoryBadge.spec.tsx` — the classifier is task-172's. Instead fold the tooltip guard into case 4: assert that the `!!!On-chain anchor reference` label still renders with **no** `title` attribute, so the untooltipped variants cannot silently drift and take the `CurrentVoteSummary` snapshot with them.

#### Step 17 — Storybook

- `storybook/stories/governance/_utils/fixtures.ts:154`: replace `anchor: null` on the `drepVerified` entry with the real preprod pair used in Step 15. `:164` (`drepUnverified`) keeps `anchor: null`. This closes cv-2 F-15's hash half.
- `storybook/stories/governance/DRepDetail.stories.tsx`: add an anchor-state knob and pass `anchorState` through `renderDetail` (`:46-60`). Use `select('Anchor state', { Verified: 'verified', Unavailable: 'unavailable', 'Not requested': 'none' }, 'verified')` and map it to an `AnchorEnrichEntry | null`. **Never add a local `IntlProvider` and never author per-locale story variants** — the global `StoryWrapper` English/Japanese toggle is the mechanism, as the binding comment at `DRepDetail.stories.tsx:43-45` states.
- Do **not** edit `storybook/stories/index.ts`. Registering `DRepDetail.stories` is task-172's (it registers all three unregistered governance story files at once). Until then this story is authored but not rendered — record it as an in-slice carry, not as done.

#### Verify

```bash
cd /home/node/.claude/jobs/f104125f/wt-anchor-1

# 1. Typecheck. Exit 0. Every literal from Step 4 must already carry verifiedName.
node_modules/.bin/tsc --noEmit
yarn compile                      # exit 0, ~22s (runs typedef:sass then tsc)

# 2. The suites this task changes.
node_modules/.bin/jest --testPathPattern="containers/governance/DRepDetailPage" --no-coverage --runInBand
#   expect 1 suite, 12 -> 20 tests, 1 snapshot (unchanged)
node_modules/.bin/jest --testPathPattern="tests/jest/governance/GovernanceStore" --no-coverage --runInBand
#   expect 1 suite, 35 -> 43 tests
node_modules/.bin/jest --testPathPattern="tests/jest/i18n" --no-coverage --runInBand
#   expect 1 suite, 4 -> 5 tests
node_modules/.bin/jest --testPathPattern="tests/jest/governance/logDRepStateSnapshot" --no-coverage --runInBand
#   expect 1 suite, 4 -> 5 tests

# 3. The suites this task must NOT move.
node_modules/.bin/jest --testPathPattern="(components/governance|CurrentVoteSummary)" --no-coverage --runInBand
#   expect 5 suites, 101 tests, 8 snapshots - all unchanged, zero written snapshots
node_modules/.bin/jest --testPathPattern="tests/jest/governance/GovernanceQueryService" --no-coverage --runInBand
#   expect 1 suite, 38 tests (unchanged)
git diff --stat source/renderer/app/components/voting/voting-governance/__snapshots__/
#   expect NO output: the CurrentVoteSummary snapshot must be byte-identical

# 4. Sanitization floor, both anchors, per the two-anchor re-proof rule.
node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="containers/voting/VotingGovernancePage.spec" --no-coverage --runInBand
#   26 + 27 = 53 at HEAD; task-149 raises the first number. Both must be green and
#   neither may drop a test.

# 5. i18n. Both catalogs, key-identical, all !!!-marked, 84 -> 95 governance keys.
yarn i18n:manage
python3 -c "
import json
en=json.load(open('source/renderer/app/i18n/locales/en-US.json'))
ja=json.load(open('source/renderer/app/i18n/locales/ja-JP.json'))
eg={k for k in en if k.startswith('governance.')}; jg={k for k in ja if k.startswith('governance.')}
print(len(eg), len(jg), eg==jg,
      [k for k in eg if not en[k].startswith('!!!')],
      [k for k in jg if not ja[k].startswith('!!!')])"
#   expect: 95 95 True [] []
git status --porcelain translations/           # inspect; restore anything unrelated

# 6. Lint. Exit 0. The warning baseline moves by the new source/ files (expected).
yarn lint

# 7. Format. nix fmt is unavailable here and stays a user-owned pre-merge obligation.
node_modules/.bin/prettier --write \
  source/common/types/governance.types.ts \
  source/main/governance/GovernanceQueryService.ts \
  source/renderer/app/stores/GovernanceStore.ts \
  source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx \
  source/renderer/app/components/governance/drep-detail/DRepDetailAnchorContent.tsx \
  source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection.tsx \
  source/renderer/app/components/governance/drep-detail/DRepDetail.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.spec.tsx \
  tests/jest/governance/GovernanceStore.spec.ts \
  tests/jest/governance/logDRepStateSnapshot.spec.ts \
  tests/jest/i18n/preliminaryCopyMarkers.spec.ts \
  storybook/stories/governance/DRepDetail.stories.tsx \
  storybook/stories/governance/_utils/fixtures.ts
#   plus every file Step 4 touched.
```

Commit, one subject line, no body, no trailer:

```
feat(gov): task-151 render the verified givenName and expose metadata completeness
```

#### Acceptance

| AC | Verbatim criterion | Discharge |
|---|---|---|
| AC-1 | "Verified givenName renders in the DRep detail view with a verified off-chain content source label." | **Green.** Steps 7–11. `DRepDetailAnchorContent` renders `state.givenName` only on `state === 'verified'`, each field carrying `<DRepSourceLabel source="verified-off-chain" host={state.host} />` per `drep-discovery-design.md:216` and `shared-design-tokens.md:51`. Pinned by Step 16 case 3 (en-US) and case 6 (ja-JP). |
| AC-2 | "Chain-native view remains fully functional when anchor is unavailable or fails verification." | **Green.** Step 9 appends the child and replaces nothing; the four on-chain `fieldRow`s (`DRepDetailOnchainSection.tsx:94,102,114,137`) and the three anchor-reference rows are untouched. Step 5's IPC catch settles every failure as `unavailable` and never throws. Pinned by Step 16 case 4, which asserts all seven chain-native values still render on the unavailable path. |
| AC-3 | "Verified metadata-completeness state is exposed to GovernanceStore for the slice-5 cohort rule." | **Green.** Step 5 adds `@observable anchorStateByDRepId` and `@computed get verifiedMetadataIds(): Set<string>` derived from `state === 'verified'` — never from `anchor != null`. task-172 consumes it as `DRepCohortContext.verifiedMetadataIds`. `defaultCohort` (`GovernanceStore.ts:174-188`) is deliberately unchanged, so invariant #7 and invariant #8 both hold. Pinned by Step 15 cases 1 and 2. |
| AC-4 | "In-slice tests use the real SIPO CIP-119 test vector with verified hash." | **Mechanism green, content OWED.** Green: the real preprod on-chain `(url, hash)` pair from `research/drep-state-preprod-epoch295-sample.json:2852-2855` drives the store tests and the Storybook fixture (Steps 15, 17), and task-150's committed `tests/mocks/governance/anchor-cip119-sample.json` + generated `.hash` drive the verify path. **OWED, never report green:** the real SIPO body bytes from `https://sipo.tokyo/drep/SIPO.jsonld` (`README.md:84`) and the assertion that their Blake2b-256 digest equals the on-chain `dataHash`. There is no network in this devcontainer and no CIP-119 JSON-LD body is committed anywhere in the repo. |
| AC-5 | "The `Registered: epoch 502` row at drep-discovery-design.md:92 no longer reads as a build instruction: it is removed from the DRep-detail wireframe, or annotated as having no local source — `DRepDirectoryEntry` (source/common/types/governance.types.ts:51-62) carries no registration field, so restoring the row needs a new on-chain data source rather than a UI change." | **Discharged before start.** `designs/drep-discovery-design.md:92` is now `│ │ Current votes: 2 Yes · 1 No · 0 Abstain (this epoch)     │  │`; `grep -rn "Registered: epoch" designs/` returns only `:106`, the prose annotation. `governance.types.ts:51-62` still carries no registration field. No step is scheduled. AC anchor drift: `:92` now holds the `Current votes` row, not the `Registered` row. |
| AC-6 | "The adjacent `Current votes` row at drep-discovery-design.md:93 is retained and drawn with the shipped unavailable value — DRepDetailOnchainSection renders that labeled field through `governance.drepDetail.votePositions.unavailable` — so the correction removes one row and not the pair." | **Discharged before start.** The row is present at `:92`, and `:106` states "the row itself is not deferred and must stay in this box". `DRepDetailOnchainSection.tsx:137-143` renders it through `governance.drepDetail.votePositions.unavailable`. AC anchor drift: `:93` is now the closing box border. |
| AC-7 | "The wireframe's On-chain box lists exactly the fields DRepDetailOnchainSection renders: Status, Expires in, Voting power, Current votes." | **Discharged before start.** `designs/drep-discovery-design.md:106` opens with that sentence verbatim; the wireframe box at `:87-93` lists exactly those four; live code renders exactly four `fieldRow`s at `DRepDetailOnchainSection.tsx:94,102,114,137`. |

**Beyond the tracker ACs, this task also discharges D-5c** — the https link gate task-152 could not build without failing its own AC-3. Step 9 renders the anchor URL as an external link only when it parses as `https:`, Step 16 cases 7 and 8 pin both branches, and invariant #3's "anchor URLs open only through the HTTPS-only-hardened `open-external-url` path" stops being vacuously true.

**Carried out of this task (record, do not fake green):** `nix fmt` before merge (no `nix` here); the real SIPO vector (AC-4 content half); any live anchor fetch; the Storybook visual and ja-JP overflow pass for the new anchor-state knob (no browser here, and the story is not registered until task-172 edits `storybook/stories/index.ts:16-18`); a real browser click-through of the anchor link — the gate is proven in jsdom against a mocked `openExternalLink`, never against the OS shell.

### task-172 — Ground DRepCategoryBadge in cohort membership and activate the High value category

Tracker row: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`, `id: "task-172"`,
`status: "pending"`, `priority: "medium"`, `estimatedHours: 6`, `dependencies: ["task-151", "task-118", "task-119"]`,
`targetPath: "source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx"`. Interaction mode:
**autonomous**. Build order: last of the five anchor-1 tasks — task-152 → task-149 → task-150 → task-151 → **task-172**.

**Path correction (record it, do not chase the old path).** The orchestration brief writes this file as
`source/renderer/app/components/voting/governance/_shared/DRepCategoryBadge.tsx`. That path does not exist. The live
file — and the tracker's own `targetPath` — is `source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx`.
Likewise the design doc is at `.agent/plans/governance/drep-discovery/designs/shared-design-tokens.md`, not
`designs/shared-design-tokens.md` at repo root; every `shared-design-tokens.md:NN` citation below is against the
`.agent/plans/...` copy.

#### Context

**The classifier as shipped** — `source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx:43-69`,
verbatim:

```ts
export type DRepCategory = 'primary' | 'threshold' | 'nonMetadata';

export type DRepCategorySource = Pick<
  AppDRepDirectoryEntry,
  'status' | 'drepActivity' | 'anchor'
>;

const THRESHOLD_WINDOW_MIN = 7;
const THRESHOLD_WINDOW_MAX = 12;

/**
 * Category rules with binding priority Threshold > Primary > Non-metadata:
 * the 7-12 remaining-epoch window wins outright; otherwise on-chain anchor
 * presence is the interim metadata-completeness proxy until the verified
 * anchor pipeline exists. Informational only - never used to order or
 * filter the cohort.
 */
export function getDRepCategory(entry: DRepCategorySource): DRepCategory {
  if (
    entry.drepActivity != null &&
    entry.drepActivity >= THRESHOLD_WINDOW_MIN &&
    entry.drepActivity <= THRESHOLD_WINDOW_MAX
  ) {
    return 'threshold';
  }
  return entry.anchor != null ? 'primary' : 'nonMetadata';
}
```

`status` is in the `Pick` but is never read. There is no cohort input. `entry.anchor != null` is the metadata proxy.

**The two false tooltips** — same file, `:13-29`, verbatim:

```ts
  primaryTooltip: {
    id: 'governance.drepDirectory.category.primary.tooltip',
    defaultMessage:
      '!!!Inside the default Recommended view with verified metadata.',
    description: 'Tooltip explaining the Primary category',
  },
  threshold: {
    id: 'governance.drepDirectory.category.threshold',
    defaultMessage: '!!!Threshold',
    description: 'Category badge for DReps in the 7-12 epoch expiry window',
  },
  thresholdTooltip: {
    id: 'governance.drepDirectory.category.threshold.tooltip',
    defaultMessage:
      '!!!Inside the default Recommended view but approaching expiry — review before delegating.',
    description: 'Tooltip explaining the Threshold category',
  },
```

Both assert *"Inside the default Recommended view"*. Both render out of cohort today — on favorites, show-all, search
results, any detail deep link, and across the whole directory whenever `isCohortActive` is false. That is exactly what
`shared-design-tokens.md:41` outlaws. `:71-74` is the component's `Props` (`{ entry: DRepCategorySource; intl }`).

**Component props and render** — same file, `:71-74` and `:76-99`: `getDRepCategory(entry)` at `:77`, then
`labelByCategory` (`:78-82`) and `tooltipByCategory` (`:83-87`) as `Record<DRepCategory, string>`, then a `<span>` with
`classNames(styles.badge, styles[category])`, `title={tooltipByCategory[category]}` and
`aria-label={`${label}. ${tooltip}`}`. `styles[category]` means the SCSS must carry a class literally named after each
category value.

**SCSS** — `source/renderer/app/components/governance/_shared/DRepCategoryBadge.scss:24-50` has exactly three
category rules: `.primary` (`:25-32`), `.threshold` (`:34-41`), `.nonMetadata` (`:43-50`). There is no `.highValue`.

**The two call sites**, both of which must thread the new input:
- `source/renderer/app/components/governance/drep-directory/DRepCard.tsx:119-121` —
  `<DRepStatusBadge status={entry.status} />` / `<DRepCategoryBadge entry={entry} />` / `<DRepIdDisplay drepId={entry.drepId} />`;
  `:139` is `<DRepSourceLabel className={styles.sourceLabel} source="on-chain" />`. Props interface at `:57-66`.
- `source/renderer/app/components/governance/drep-detail/DRepDetail.tsx:103-104` —
  `<DRepIdDisplay drepId={entry.drepId} showCopiedConfirmation />` / `<DRepCategoryBadge entry={entry} />`.
  Props interface at `:41-48`.

**Correction to the props chain given in the decisions doc.** D-9 says `DRepDirectory.tsx` "forwards to `DRepCard`".
It does not — there is an intermediate component. Verified live: `DRepCard` is imported and rendered **only** by
`source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx:5` and `:84-93`, and
`DRepDirectoryList` is rendered twice by `DRepDirectory.tsx` — at `:289-298` (favorites view) and `:359-366`
(directory view). The real chain is
`DRepDirectoryPage → DRepDirectory → DRepDirectoryList → DRepCard → DRepCategoryBadge`. Both `DRepDirectoryList`
render sites must pass the new prop or the favorites view silently loses it.

**The store, which owns the membership source** — `source/renderer/app/stores/GovernanceStore.ts`:

```ts
// :60-62
const COHORT_TOP_EXCLUSION = 35;
const COHORT_MAX_SIZE = 200;
const COHORT_MIN_REMAINING_EPOCHS = 6;

// :160-165
  @computed get isCohortActive(): boolean {
    return (
      this.votingPowerState === VotingPowerEnrichState.Loaded &&
      this.drepList.length > 0
    );
  }

// :174-188
  @computed get defaultCohort(): AppDRepDirectoryEntry[] | null {
    if (!this.isCohortActive) return null;
    const ranked = [...this.drepList].sort(compareByVotingPowerDesc);
    const eligible = ranked
      .slice(COHORT_TOP_EXCLUSION)
      .filter(
        (entry) =>
          entry.status === 'active' &&
          entry.drepActivity != null &&
          entry.drepActivity > COHORT_MIN_REMAINING_EPOCHS
      );
    const selected = eligible.slice(0, COHORT_MAX_SIZE);
    const canonical = [...selected].sort(compareDRepIdAsc);
    return seededShuffle(canonical, this.cohortSeed);
  }

// :191-193
  @computed get displayedDRepList(): AppDRepDirectoryEntry[] {
    return this.defaultCohort ?? this.drepList;
  }
```

`:200-208` is `top35DRepIds` — the shipped precedent for threading a store-derived `Set<string>` through
container → `DRepDirectory` props (`DRepDirectory.tsx:83` `top35DRepIds: ReadonlySet<string>`,
`DRepDirectoryPage.tsx:95`). `AppDRepDirectoryEntry` is `:20-31`: `drepId: string`, `votingPower: BigNumber | null`,
`status`, `drepActivity`, `anchor: DRepAnchorPresence | null`.

**Lovelace type** — `source/common/types/governance.types.ts:47` `export type Lovelace = string;`, and `:51-62` is the
wire `DRepDirectoryEntry` whose `votingPower: Lovelace | null` is rehydrated to `BigNumber` in the renderer. Never
`Number(...)`, `parseInt`, unary `+`, or `.toNumber()` on this path.

**The binding design contract** — `.agent/plans/governance/drep-discovery/designs/shared-design-tokens.md:30-41`,
§1a. Quoted, not paraphrased:

> | Category | Rule (informational) | Label (en) | Tooltip copy (en) |
> |---|---|---|---|
> | High value | Inside the default randomized cohort AND completed metadata AND voting power above the cohort median | High value | "Inside the default Recommended view, with verified metadata and voting power above the cohort median." |
> | Primary | Inside the default randomized cohort AND completed metadata | Primary | "Has verified off-chain metadata." |
> | Threshold | Inside the default randomized cohort but expiry within the 7–12 epoch window (still above the 6-epoch floor) | Threshold | "Approaching expiry — review before delegating." |
> | Non-metadata | Eligible for the cohort but anchor metadata is missing or unverified | Non-metadata | "Eligible for delegation but has no verified off-chain metadata yet." |

`:39`, verbatim:

> **Priority rule (binding).** When a DRep satisfies more than one category simultaneously, the highest-priority badge wins. Priority order (highest → lowest): **High Value → Threshold → Primary → Non-metadata**. A DRep with metadata that is also approaching expiry (7–12 epochs) always shows **Threshold**, not Primary.

`:41`, verbatim:

> **Out-of-cohort classification (binding).** The rules in the table are the *in-cohort* rules. The badge also renders on surfaces the default cohort excludes by construction — the detail view reached by deep link or ID search, favorites, show-all, deduplicated search rows, and the whole directory whenever `isCohortActive` is false and the list falls back to the unfiltered registrations (ranking unavailable, §6). Cohort membership is therefore an explicit input to the classifier, taken from `GovernanceStore.defaultCohort` and never re-derived from the top-35 / 200 / 6-epoch rule, so a later cohort exclusion reaches the badge without changing the classifier. Outside the cohort **High value never renders** — "above the cohort median" is a cohort statistic and is undefined for an excluded entry — and the entry classifies into Threshold / Primary / Non-metadata on its own properties under the same priority order. The same DRep can therefore carry different badges in the directory and in favorites; that is intended. No tooltip may assert default-cohort membership for an entry the cohort excludes.

Note the asymmetry this creates and do not "fix" it: the **High value** tooltip *does* say "Inside the default
Recommended view", and that is correct, because High value renders only when `inCohort` is true. Primary and Threshold
render both in and out of cohort, so their tooltips may not make the claim — hence the rewording.

**i18n catalogs** — `source/renderer/app/i18n/locales/en-US.json:309-314` and `ja-JP.json:309-314` carry exactly six
`governance.drepDirectory.category.*` keys, same six ids in both, all `!!!`-prefixed. No `highValue` key exists in
either catalog, in `source/renderer/app/i18n/locales/defaultMessages.json` (category ids at `:2517-2542`), or in
`translations/messages.json`.

**Committed snapshots that will move** (AC-6's "both call sites"):
- Card side — `source/renderer/app/components/governance/drep-directory/__snapshots__/DRepDirectory.spec.tsx.snap`,
  key `DRepDirectory renders exactly one category badge per card (snapshot) 1`, currently the Threshold badge with the
  false tooltip. Produced by `DRepDirectory.spec.tsx:488-498`.
- Detail side — `source/renderer/app/containers/governance/__snapshots__/DRepDetailPage.spec.tsx.snap`, key
  `DRepDetailPage renders the category badge in the detail header (snapshot) 1`, currently the Primary badge with the
  false tooltip. Produced by `DRepDetailPage.spec.tsx:284-291`.

**Storybook** — `storybook/stories/index.ts:16-18` is verbatim:

```ts
import './voting/Governance.stories';
import './governance/DRepDirectory.stories';
import './governance/CurrentVoteSummary.stories';
```

`storybook/stories/governance/DRepCategoryBadge.stories.tsx`, `DRepDetail.stories.tsx` and
`DRepDirectoryBanner.stories.tsx` exist but are imported nowhere, so they **never render** — which is precisely what
AC-6 needs. `jest.config.js:129` sets `roots: ['<rootDir>/tests', '<rootDir>/source']`, so nothing under `storybook/` is
Jest-provable. `tsconfig.json` has **no** `include` key (only `"exclude": ["node_modules"]`), so `tsc --noEmit` *does*
typecheck `storybook/` — a story that omits a newly-required prop is a typecheck failure, not a silent gap.

**What task-151 has already landed and this task consumes** (do not re-implement, do not substitute):

```ts
// source/renderer/app/stores/GovernanceStore.ts, added by task-151 (seam S-5)
export type AnchorEnrichEntry =
  | { state: 'loading';     hash: string }
  | { state: 'verified';    hash: string; givenName: string | null; host: string }
  | { state: 'unavailable'; hash: string; reason: AnchorFetchErrorType };
@observable anchorStateByDRepId: Map<string, AnchorEnrichEntry>;   // absent key == idle
```

If `anchorStateByDRepId` is not present in `GovernanceStore.ts` when you start, task-151 has not landed and this task
is blocked. **Do not fall back to `entry.anchor != null`** — that fallback is the exact thing AC-2 forbids.

#### Locked invariants this change must not break

Inlined in full from `.agent/plans/governance/drep-discovery/prompt.md`; do not read them by reference.

- **#8 Badges are informational only** (`prompt.md:125-127`, verbatim): *"**Badges are informational only.** The
  category badge (slice-5: Primary / Threshold / Non-metadata; High value only after anchor-1) never reorders,
  filters, or overrides the cohort."*
  Operationally: data flows **one way only** — `defaultCohort` → `cohortContext` → `getDRepCategory` → rendered badge.
  Nothing may read the classifier's output back. No sort comparator, no filter predicate, no search index, no
  favorites rule, no cohort computed may import `getDRepCategory`, `DRepCategory` or `DRepCategoryBadge`. The
  repository already states this in code: `source/renderer/app/components/governance/drep-directory/helpers.ts:177-179`
  carries the comment *"The 7-12 remaining-epoch window is restated here on purpose: filter code must never import
  from the badge module (badges are informational only)."* — `filterDReps` (`helpers.ts:189-220`) therefore duplicates
  `EXPIRY_WINDOW_MIN/MAX` (`:181-182`) rather than importing them. Preserve that duplication; do not "DRY" it.
- **#7 Default cohort is binding** (`prompt.md:121-124`, verbatim): *"**Default cohort is binding.** Exclude top 35 by
  voting power; up to the next 200 eligible (active, remaining `drepActivity` > 6 epochs, completed metadata when
  available), randomized. The 6-epoch floor is binding in production — fixtures that violate it must not ship. The
  default cohort IS the "Recommended" sort: no Recommended tab, no per-card Recommended badge."*
  Operationally: the 35 / 200 / 6 rule exists **once**, at `GovernanceStore.ts:174-188`. This task must not restate
  it anywhere — not in the classifier, not in a test helper, not in a Storybook fixture comment.
- **#5 Lovelace losslessness** (`prompt.md:115-117`): voting power crosses IPC as a decimal string
  (`governance.types.ts:47`) and lives in the renderer as `BigNumber`. Every comparison and every median arithmetic
  step in this task uses `BigNumber` methods (`comparedTo`, `isGreaterThan`, `plus`, `dividedBy`). `Number(...)`,
  `parseInt`, unary `+` and `.toNumber()` are forbidden on this path.
- **#11 Preliminary copy** (`prompt.md:132-133`, verbatim): *"**Preliminary copy.** Every new en-US and ja-JP string
  keeps the leading `!!!` marker. Removing `!!!` is a release-end manual review, never a per-slice task."*
- **Convergence rule** (`prompt.md:237-242`): reuse existing seams. No new npm package. `bignumber.js` is already a
  production dependency and is already imported by `GovernanceStore.ts:2`.
- **Comment and commit conventions.** Comment only where the logic is not self-evident, then 1–3 plain sentence-case
  lines stating the invariant or the why — never the what, never change history, never a task id, review label,
  plan name or ALL-CAPS emphasis, in comments **or** in test names. One commit, one Conventional Commits subject
  line, no body, no `Co-Authored-By`.

#### Resolved judgment calls (do not revisit)

1. **The classifier signature is `(entry, cohort)`.** Exactly seam S-8:
   ```ts
   export type DRepCategory = 'highValue' | 'threshold' | 'primary' | 'nonMetadata';
   export function getDRepCategory(
     entry: Pick<AppDRepDirectoryEntry, 'drepId' | 'votingPower' | 'drepActivity'>,
     cohort: DRepCohortContext
   ): DRepCategory;
   ```
   Keep the exported alias `DRepCategorySource` and redefine it as that exact `Pick` — the spec and the story already
   import the alias, and an alias for the same `Pick` is type-identical to S-8's inline form.
2. **`status` is dropped from the source type.** It is never read at HEAD (`:60-69`) and no new rule reads it: cohort
   membership already encodes active-ness (`GovernanceStore.ts:179-184`). A never-read field in a signature is a lie.
   `anchor` is dropped too — AC-2.
3. **Membership, completeness and the median arrive as one store-computed prop object named `cohort`**, of type
   `DRepCohortContext`, declared in `GovernanceStore.ts`. The badge imports it **type-only**, mirroring the existing
   `import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';` at `DRepCategoryBadge.tsx:4`.
   Erased type imports are allowed; a runtime store import into a component is not.
4. **`memberIds` is `Set<string> | null`, and `null` is not the empty set.** `null` means the cohort does not exist
   (`defaultCohort` returns `null` when `!isCohortActive`, `GovernanceStore.ts:175`). Both `null` and "id not in the
   Set" yield `inCohort === false`, so High value never renders and no tooltip claims membership — which is exactly
   what `shared-design-tokens.md:41` requires for the ranking-unavailable fallback. Keeping them distinct is what lets
   a reader tell "no cohort" from "cohort exists, entry excluded".
5. **High value beats Threshold.** `shared-design-tokens.md:39` is labelled binding and orders
   High Value → Threshold → Primary → Non-metadata. The following sentence ("A DRep with metadata that is also
   approaching expiry (7–12 epochs) always shows **Threshold**, not Primary") governs the Threshold-vs-Primary pair
   only; it does not demote High value. This is the one ambiguous reading in the design and the explicitly-binding
   rule governs.
6. **"Above the median" is strictly greater.** `entry.votingPower.isGreaterThan(median)`. An entry equal to the
   median is not above it, so in an odd-sized cohort the single median entry is excluded. Deterministic, no secondary
   key, and the copy stays literally true.
7. **The median is computed over the cohort only, never the full list**, per `shared-design-tokens.md:41`
   ("'above the cohort median' is a cohort statistic and is undefined for an excluded entry"). Entries with
   `votingPower === null` are excluded from the median sample and can never be above it.
8. **AC-2's "in any code path" is scoped to the classifier.** `filterDReps` (`helpers.ts:189-220`) keeps using
   `entry.anchor == null` for its "With metadata / Without metadata" filter: that filter's user-facing meaning is
   *has an on-chain anchor reference*, its copy is shipped (`governance.drepDirectory.filter.metadata.with` /
   `.without`), and re-pointing it at verification state is a filter-semantics change with its own copy and its own
   design decision. Do not touch it. Recorded as a note to the design owner below.
9. **Storybook registration is absorbed here, not re-deferred** (decision D-8). Add all three missing imports —
   `DRepCategoryBadge.stories`, `DRepDetail.stories`, `DRepDirectoryBanner.stories`. Registering two of three would
   leave the same drift with a smaller number.
10. **No local `IntlProvider` in any story, and no per-locale story variants.** The global `StoryWrapper` EN/JA toggle
    at the top of the preview window is the mechanism; the binding comment already lives at
    `storybook/stories/governance/DRepDetail.stories.tsx:43-45` and at `DRepCategoryBadge.stories.tsx:38-40`. (Jest
    spec files are a different matter — `DRepCategoryBadge.spec.tsx:19-26` legitimately wraps in `IntlProvider`
    because there is no StoryWrapper in Jest.)
11. **No new "Connected flow" story.** The detail view is already reachable through the registered
    `Voting / Governance > Connected flow` in `storybook/stories/voting/Governance.stories.tsx`.
12. **High value gets its own SCSS colour pair, not the existing success green.** `--badge-success-fg` is already the
    Active *status* badge (`DRepStatusBadge.scss:26-27`) and both badges sit in the same card row
    (`DRepCard.tsx:119-120`). §1a specifies no colour, so use a new pair in the established
    `var(--token, #fallback)` shape and record it as a design-owner note.

#### Step 1 — Add `DRepCohortContext` and the two computeds to `GovernanceStore.ts`

File: `source/renderer/app/stores/GovernanceStore.ts`.

**1a.** Immediately after the `AppDRepDirectoryEntry` interface (ends at `:31`), add:

```ts
/**
 * Everything the category badge needs, derived once in the store. memberIds
 * is null when no cohort exists - distinct from an empty cohort - so an
 * out-of-cohort entry and a cohort-less directory classify identically.
 */
export interface DRepCohortContext {
  memberIds: Set<string> | null;
  verifiedMetadataIds: Set<string>;
  medianVotingPower: BigNumber | null;
}
```

**1b.** Add both computeds immediately after `defaultCohort` (ends at `:188`), before `displayedDRepList`:

```ts
  /**
   * Median voting power across the cohort only. Entries without a voting
   * power are outside the sample and can never be above the median.
   */
  @computed get cohortMedianVotingPower(): BigNumber | null {
    const cohort = this.defaultCohort;
    if (cohort === null) return null;
    const powers = cohort
      .map((entry) => entry.votingPower)
      .filter((power): power is BigNumber => power != null)
      .sort((a, b) => a.comparedTo(b));
    const size = powers.length;
    if (size === 0) return null;
    if (size % 2 === 1) return powers[(size - 1) / 2];
    return powers[size / 2 - 1].plus(powers[size / 2]).dividedBy(2);
  }

  /** Explicit classifier input; the badge never re-derives cohort membership. */
  @computed get cohortContext(): DRepCohortContext {
    const cohort = this.defaultCohort;
    return {
      medianVotingPower: this.cohortMedianVotingPower,
      memberIds: cohort === null ? null : new Set(cohort.map((e) => e.drepId)),
      verifiedMetadataIds: this.verifiedMetadataIds,
    };
  }
```

`BigNumber` is already imported at `:2`. **`verifiedMetadataIds` is task-151's existing `@computed`** (added beside
`showAllList`), reused verbatim — do **not** re-walk `anchorStateByDRepId` here. Two derivations of the same signal in
one file is exactly what the convergence rule forbids, and a later change to the verified-state predicate would have to
be made twice. It derives from `anchorStateByDRepId` and from nothing else — never from `entry.anchor`.

#### Step 2 — Rewrite the classifier and its copy in `DRepCategoryBadge.tsx`

File: `source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx`.

**2a.** Change the type-only import at `:4` to also bring in the context type:

```ts
import type {
  AppDRepDirectoryEntry,
  DRepCohortContext,
} from '../../../stores/GovernanceStore';
```

**2b.** In the `defineMessages` block, add the two new descriptors (put them first, so the block reads in priority
order) and replace the two tooltip defaults. The block becomes, in order: `highValue`, `highValueTooltip`, `primary`,
`primaryTooltip`, `threshold`, `thresholdTooltip`, `nonMetadata`, `nonMetadataTooltip`.

```ts
  highValue: {
    id: 'governance.drepDirectory.category.highValue',
    defaultMessage: '!!!High value',
    description: 'Category badge for in-cohort DReps above the cohort median',
  },
  highValueTooltip: {
    id: 'governance.drepDirectory.category.highValue.tooltip',
    defaultMessage:
      '!!!Inside the default Recommended view, with verified metadata and voting power above the cohort median.',
    description: 'Tooltip explaining the High value category',
  },
```

Replace `primaryTooltip`'s `defaultMessage` (currently `:15-16`) with:

```ts
    defaultMessage: '!!!Has verified off-chain metadata.',
```

Replace `thresholdTooltip`'s `defaultMessage` (currently `:26-27`) with:

```ts
    defaultMessage: '!!!Approaching expiry — review before delegating.',
```

Leave `primary`, `threshold`, `nonMetadata` and `nonMetadataTooltip` byte-identical — `nonMetadataTooltip` already
matches `shared-design-tokens.md:35`. Keep the em dash `—` (U+2014) in the threshold tooltip exactly as shown.

**2c.** Replace `:43-69` (both types, the two constants and the whole function) with:

```ts
export type DRepCategory =
  | 'highValue'
  | 'threshold'
  | 'primary'
  | 'nonMetadata';

export type DRepCategorySource = Pick<
  AppDRepDirectoryEntry,
  'drepId' | 'votingPower' | 'drepActivity'
>;

const THRESHOLD_WINDOW_MIN = 7;
const THRESHOLD_WINDOW_MAX = 12;

/**
 * Priority is High value > Threshold > Primary > Non-metadata. Cohort
 * membership and verified metadata are explicit inputs, never re-derived
 * here, and High value cannot render outside the cohort where the median
 * is undefined. Informational only - never read back by ordering,
 * filtering or cohort code.
 */
export function getDRepCategory(
  entry: DRepCategorySource,
  cohort: DRepCohortContext
): DRepCategory {
  const inCohort = cohort.memberIds?.has(entry.drepId) ?? false;
  const hasVerifiedMetadata = cohort.verifiedMetadataIds.has(entry.drepId);
  const isAboveMedian =
    inCohort &&
    entry.votingPower != null &&
    cohort.medianVotingPower != null &&
    entry.votingPower.isGreaterThan(cohort.medianVotingPower);

  if (inCohort && hasVerifiedMetadata && isAboveMedian) {
    return 'highValue';
  }
  if (
    entry.drepActivity != null &&
    entry.drepActivity >= THRESHOLD_WINDOW_MIN &&
    entry.drepActivity <= THRESHOLD_WINDOW_MAX
  ) {
    return 'threshold';
  }
  return hasVerifiedMetadata ? 'primary' : 'nonMetadata';
}
```

**2d.** Replace the `Props` interface (`:71-74`) and the two `Record` maps (`:78-87`), and pass `cohort` through:

```ts
interface Props {
  entry: DRepCategorySource;
  cohort: DRepCohortContext;
  intl: intlShape.isRequired;
}

function DRepCategoryBadge({ entry, cohort, intl }: Props) {
  const category = getDRepCategory(entry, cohort);
  const labelByCategory: Record<DRepCategory, string> = {
    highValue: intl.formatMessage(messages.highValue),
    nonMetadata: intl.formatMessage(messages.nonMetadata),
    primary: intl.formatMessage(messages.primary),
    threshold: intl.formatMessage(messages.threshold),
  };
  const tooltipByCategory: Record<DRepCategory, string> = {
    highValue: intl.formatMessage(messages.highValueTooltip),
    nonMetadata: intl.formatMessage(messages.nonMetadataTooltip),
    primary: intl.formatMessage(messages.primaryTooltip),
    threshold: intl.formatMessage(messages.thresholdTooltip),
  };
```

The JSX body (`:89-98`) is unchanged.

#### Step 3 — Add the `.highValue` rule to the SCSS

File: `source/renderer/app/components/governance/_shared/DRepCategoryBadge.scss`. Insert immediately after the
`/* Category colors … */` comment at `:24`, before `.primary`:

```scss
.highValue {
  color: var(--badge-highlight-fg, #7a5af8);
  background: var(--badge-highlight-bg, rgba(122, 90, 248, 0.12));

  .dot {
    background: var(--badge-highlight-fg, #7a5af8);
  }
}
```

Without this rule `styles[category]` resolves to `undefined` for `highValue` and the badge renders unstyled.
`*.scss.d.ts` files are gitignored (`.gitignore:141`) and regenerated by `yarn compile`'s `precompile`
(`yarn typedef:sass`), so make this edit before running `yarn compile`.

#### Step 4 — Thread `cohort` through the detail chain

**4a.** `source/renderer/app/components/governance/drep-detail/DRepDetail.tsx` — add to the `Props` interface
(`:41-48`), after `entry`:

```ts
  cohort: DRepCohortContext;
```

Add `cohort` to the destructured parameter list (`:50-57`), extend the existing type import to include
`DRepCohortContext` from `'../../../stores/GovernanceStore'`, and change `:104` to:

```tsx
        <DRepCategoryBadge entry={entry} cohort={cohort} />
```

**4b.** `source/renderer/app/containers/governance/DRepDetailPage.tsx` — add one prop to the `<DRepDetail …>` element
at `:90-96`, after `entry`:

```tsx
        cohort={governanceStore.cohortContext}
```

#### Step 5 — Thread `cohort` through the directory chain

Four files, in dependency order. The prop is named `cohort` at every hop.

**5a.** `source/renderer/app/components/governance/drep-directory/DRepCard.tsx` — add `cohort: DRepCohortContext;` to
`Props` (`:57-66`) after `entry`, add it to the destructured parameter list (`:81-90`), import the type alongside the
existing `import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';` at `:10`, and change `:120`:

```tsx
        <DRepCategoryBadge entry={entry} cohort={cohort} />
```

**5b.** `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx` — add
`cohort: DRepCohortContext;` to `Props` (`:31-41`) after `entries`, add it to the destructured parameter list
(`:43-53`), extend the type import at `:7`, and pass it in the `<DRepCard …>` element at `:84-93`:

```tsx
            cohort={cohort}
```

**5c.** `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx` — add
`cohort: DRepCohortContext;` to `Props` (`:79-101`) beside `top35DRepIds` (`:83`), add it to the destructured
parameter list (`:103` onwards), extend the type import, and pass `cohort={cohort}` to **both**
`<DRepDirectoryList …>` render sites — `:289-298` (favorites) and `:359-366` (directory). Missing the favorites site
is the failure mode this step exists to prevent.

**5d.** `source/renderer/app/containers/governance/DRepDirectoryPage.tsx` — add one prop to the `<DRepDirectory …>`
element at `:91-115`, next to `top35DRepIds` at `:95`:

```tsx
        cohort={governanceStore.cohortContext}
```

#### Step 6 — Mint and translate the two new keys, reword the two tooltips

```bash
cd /home/node/.claude/jobs/f104125f/wt-anchor-1
yarn i18n:manage
```

That writes four tracked files: `source/renderer/app/i18n/locales/en-US.json`,
`source/renderer/app/i18n/locales/ja-JP.json`, `source/renderer/app/i18n/locales/defaultMessages.json` and
`translations/messages.json`. This task **owns** that diff — commit all four (precedent: commit `927978951`, which
committed exactly those four for the currentVote keys). Do not `git restore` any of them; but do check
`git status` and restore anything else the run happened to touch.

Then set these eight values by hand and verify them byte-for-byte. The tool does not reliably rewrite an already-
translated ja value when only the en `defaultMessage` changed, so the two reworded tooltips must be checked in **both**
catalogs, not just the new keys.

`source/renderer/app/i18n/locales/en-US.json` (keys sort alphabetically, so `highValue` lands just before
`nonMetadata` at what is currently `:309`):

```json
  "governance.drepDirectory.category.highValue": "!!!High value",
  "governance.drepDirectory.category.highValue.tooltip": "!!!Inside the default Recommended view, with verified metadata and voting power above the cohort median.",
  "governance.drepDirectory.category.primary.tooltip": "!!!Has verified off-chain metadata.",
  "governance.drepDirectory.category.threshold.tooltip": "!!!Approaching expiry — review before delegating.",
```

`source/renderer/app/i18n/locales/ja-JP.json`:

```json
  "governance.drepDirectory.category.highValue": "!!!高価値",
  "governance.drepDirectory.category.highValue.tooltip": "!!!デフォルトの推奨ビュー内で、検証済みメタデータがあり、投票権がコホートの中央値を上回っています。",
  "governance.drepDirectory.category.primary.tooltip": "!!!検証済みのオフチェーンメタデータがあります。",
  "governance.drepDirectory.category.threshold.tooltip": "!!!失効が近づいています。委任前にご確認ください。",
```

The ja terminology is taken from the shipped catalog, not invented: 投票権 is the established rendering of "voting
power" (`ja-JP.json:352-353`, `:360`), コホート of "cohort" (`:319`, `:366`), デフォルトの推奨ビュー of "the default
Recommended view" (`:312`, `:314`), and 検証済みのオフチェーンメタデータ of "verified off-chain metadata" (`:310`).
`高価値` is deliberately three characters — §1a at `:37` requires short labels so cards do not wrap in JA.

All eight strings keep the leading `!!!` (invariant #11). Both catalogs must remain key-identical, and the count is
read **in build order, not from HEAD**: the catalogs hold **95** `governance.*` keys per locale when this task starts
(84 at HEAD plus task-151's eleven) and **97** when it finishes, same set in both files.

#### Step 7 — Rewrite `DRepCategoryBadge.spec.tsx`

File: `source/renderer/app/components/governance/_shared/DRepCategoryBadge.spec.tsx`. Baseline: 11 tests.
Target: **22 tests** in three `describe` blocks with exactly the names below. Keep the existing `IntlProvider`
harness at `:19-26`, extended to pass `cohort`.

Fixtures at the top of the file:

```ts
const DREP_ID = 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b';

const baseEntry: DRepCategorySource = {
  drepActivity: 20,
  drepId: DREP_ID,
  votingPower: new BigNumber('900'),
};

const outOfCohort: DRepCohortContext = {
  medianVotingPower: null,
  memberIds: new Set<string>(),
  verifiedMetadataIds: new Set<string>(),
};

const inCohortVerified: DRepCohortContext = {
  medianVotingPower: new BigNumber('500'),
  memberIds: new Set([DREP_ID]),
  verifiedMetadataIds: new Set([DREP_ID]),
};

const noCohort: DRepCohortContext = { ...outOfCohort, memberIds: null };
```

`describe('getDRepCategory', …)` — 15 cases:

1. `classifies an in-cohort verified entry above the median as highValue`
2. `does not classify an entry equal to the median as highValue` — `votingPower` `new BigNumber('500')` against
   `medianVotingPower` `new BigNumber('500')` → `'primary'`
3. `does not classify an entry without voting power as highValue` — `votingPower: null` → `'primary'`
4. `does not classify as highValue when the cohort has no median` — `medianVotingPower: null` → `'primary'`
5. `gives highValue priority over the threshold window` — `drepActivity: 10` with `inCohortVerified` → `'highValue'`
6. `applies threshold across the whole 7-12 epoch window` — `drepActivity` 7 and 12 with `outOfCohort` → `'threshold'`
7. `gives threshold priority over primary for a verified entry in the window` — `drepActivity: 9`,
   `verifiedMetadataIds` holding the id, `memberIds` empty → `'threshold'`
8. `applies threshold to an out-of-cohort entry` — `noCohort`, `drepActivity: 8` → `'threshold'`
9. `classifies a verified in-cohort entry at or below the median as primary`
10. `classifies a verified out-of-cohort entry as primary`
11. `classifies an entry without verified metadata as nonMetadata`
12. `leaves 6 and 13 remaining epochs outside the threshold window`
13. `treats null drepActivity as outside the threshold window`
14. `classifies the same entry as highValue in cohort and primary out of cohort` — one `baseEntry`, both contexts
15. `never returns highValue while the cohort is inactive` — `noCohort` with a non-null `medianVotingPower` and the
    id in `verifiedMetadataIds` → `'primary'`

Add one more case pinning AC-2 structurally — count it inside case 11 by constructing the entry with an on-chain
anchor present and asserting it still classifies as `nonMetadata`:

```ts
    const withOnchainAnchor = {
      ...baseEntry,
      anchor: {
        hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
        url: 'https://governance-preview.example.org/dreps/1.json',
      },
    } as DRepCategorySource;
    expect(getDRepCategory(withOnchainAnchor, outOfCohort)).toBe('nonMetadata');
```

`describe('DRepCategoryBadge', …)` — 5 cases, each asserting the label text and the exact `title` attribute:

16. `renders the highValue label with its explanatory tooltip` — `!!!High value` /
    `!!!Inside the default Recommended view, with verified metadata and voting power above the cohort median.`
17. `renders the primary label with its explanatory tooltip` — `!!!Primary` / `!!!Has verified off-chain metadata.`
18. `renders the threshold label with its tooltip` — `!!!Threshold` /
    `!!!Approaching expiry — review before delegating.`
19. `renders the nonMetadata label with its tooltip` — `!!!Non-metadata` /
    `!!!Eligible for delegation but has no verified off-chain metadata yet.`
20. `renders category labels in ja-JP` — assert `!!!高価値` and `!!!しきい値` are both in the document

`describe('category badge isolation', …)` — 2 cases discharging invariant #8 and AC-5:

21. `keeps ordering, filtering and search helpers free of the badge module`
22. `keeps the governance store free of the badge module`

```ts
import fs from 'fs';
import path from 'path';

const readSource = (relative: string) =>
  fs.readFileSync(path.resolve(__dirname, relative), 'utf8');

const CONSUMERS_THAT_MUST_NOT_READ_THE_CATEGORY = [
  '../drep-directory/helpers.ts',
  '../drep-directory/DRepDirectory.tsx',
  '../drep-directory/DRepDirectoryList.tsx',
  '../drep-directory/DRepDirectoryFilters.tsx',
  '../drep-directory/DRepDirectorySearch.tsx',
];

it('keeps ordering, filtering and search helpers free of the badge module', () => {
  CONSUMERS_THAT_MUST_NOT_READ_THE_CATEGORY.forEach((relative) => {
    const source = readSource(relative);
    expect(source).not.toMatch(/getDRepCategory|DRepCategoryBadge|DRepCategory\b/);
  });
});

it('keeps the governance store free of the badge module', () => {
  expect(readSource('../../../stores/GovernanceStore.ts')).not.toMatch(
    /getDRepCategory|DRepCategoryBadge|DRepCategory\b/
  );
});
```

Do not add `DRepCard.tsx` or `DRepDetail.tsx` to that list — they are the two legitimate render call sites.

#### Step 8 — Update the card-side spec and snapshot

File: `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`. Baseline: 47 tests,
1 snapshot.

**8a.** Add `cohort` to the `renderComponent` options object (`:84-126`) and to `<DRepDirectory …>` (`:136-157`).
Default it to the cohort-less shape, matching the shipped default `isCohortActive = false` at `:98`:

```ts
  cohort = {
    medianVotingPower: null,
    memberIds: null,
    verifiedMetadataIds: new Set<string>(),
  } as DRepCohortContext,
```

**8b.** In the existing snapshot test at `:488-498`, widen the "exactly one badge" regex to include the fourth
category and leave the rest of the case as-is:

```ts
      screen.getAllByText(/^!!!(High value|Primary|Threshold|Non-metadata)$/)
```

The committed snapshot must be regenerated: the Threshold tooltip text changes. `baseEntries[0]` has
`drepActivity: 12` and the default cohort context is cohort-less, so it stays Threshold — only the tooltip and
`aria-label` strings move.

**8c.** Add one case, `renders the high value badge for an in-cohort verified entry above the median`, which passes a
`cohort` naming `baseEntries[0].drepId` in both `memberIds` and `verifiedMetadataIds` with a `medianVotingPower`
strictly below that entry's `votingPower`, plus `drepActivity` outside 7–12, and asserts `!!!High value` is in the
document. 47 → 48 tests.

Regenerate the snapshot with `-u` (Verify block below) and read the resulting
`__snapshots__/DRepDirectory.spec.tsx.snap` diff before committing: the only permitted changes are the two tooltip
strings inside the single existing snapshot key.

#### Step 9 — Update the detail-side specs and snapshot

**9a.** `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx`. HEAD baseline: 12 tests, 1 snapshot —
**20 tests on entry**, because task-151 added eight. `buildGovernanceStore` at `:42-51` is a plain object; add a
`cohortContext` field. `baseEntry` (`:31-40`) has
`drepActivity: 34` and `votingPower: new BigNumber('23137980123456')`, so pick a median **above** that power to keep
the existing snapshot on Primary:

```ts
  cohortContext: {
    medianVotingPower: new BigNumber('99137980123456'),
    memberIds: new Set([DREP_ID]),
    verifiedMetadataIds: new Set([DREP_ID]),
  },
```

The existing snapshot test `renders the category badge in the detail header (snapshot)` (`:284-291`) stays on
`!!!Primary`; its inline comment at `:287` currently reads `// baseEntry: anchor present, drepActivity 34 -> Primary.`
— replace it with `// baseEntry: verified, in cohort, at or below the median -> Primary.` so it does not restate the
retired rule. The snapshot regenerates with the new tooltip.

**9b.** Add one case to the same file, `renders the high value badge when the entry is above the cohort median
(snapshot)`, overriding `cohortContext.medianVotingPower` to `new BigNumber('1000000')` and snapshotting the
`!!!High value` badge. **+1 test, +1 snapshot** (20 → 21 tests, 1 → 2 snapshots in build order).

**9c.** `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx` — add the same `cohortContext` field to
`buildGovernanceStore` (`:37-52`). `drepEntry` (`:29-35`) has `drepActivity: 12`, so it stays Threshold whatever the
context; use the cohort-less shape (`memberIds: null`, empty `verifiedMetadataIds`, `medianVotingPower: null`). Test
count unchanged at 8.

#### Step 10 — Add store tests for the two new computeds

File: `tests/jest/governance/GovernanceStore.spec.ts`. HEAD baseline: 35 tests — **43 on entry**, because task-151
added eight. Add a
`describe('GovernanceStore cohort context', …)` block after the `GovernanceStore default cohort` block (which opens at
`:315`), reusing that block's `buildDrep` / `stakeFor` / `loadStore` helpers by lifting them to module scope or by
duplicating the minimal harness inside the new block. Six cases, **+6 tests** (43 → 49 in build order, because
task-151 already took this suite from 35 to 43):

1. `takes the middle voting power as the median of an odd-sized cohort`
2. `averages the two middle voting powers for an even-sized cohort`
3. `reports no median while the cohort is inactive` — `cohortMedianVotingPower` is `null`
4. `excludes entries without voting power from the median sample`
5. `reports null cohort members while the cohort is inactive` — `cohortContext.memberIds` is `null`, not an empty Set
6. `includes only verified anchor states in the cohort context` — seed `anchorStateByDRepId` with one `verified`, one
   `unavailable` and one `loading` entry; only the verified id appears in `verifiedMetadataIds`

The cohort fixtures must respect invariant #7's 6-epoch floor — `buildDrep` already defaults `drepActivity: 10`.

#### Step 11 — Storybook: register the three orphaned files and add the cohort knob

**11a.** `storybook/stories/index.ts` — append three imports directly after `:18`
(`import './governance/CurrentVoteSummary.stories';`):

```ts
import './governance/DRepCategoryBadge.stories';
import './governance/DRepDetail.stories';
import './governance/DRepDirectoryBanner.stories';
```

**11b.** `storybook/stories/governance/DRepCategoryBadge.stories.tsx` — replace the three `DRepCategorySource`
fixtures (`:15-36`) and the single story (`:41-53`). Keep the file's existing decorators and the locale comment at
`:38-40` verbatim. Import `withKnobs` and `boolean` from `@storybook/addon-knobs` and `BigNumber` from
`bignumber.js`. Four entries with distinct ids and `drepActivity` of 20 / 9 / 20 / 20; one `boolean('In default
cohort', true)` knob selecting between an in-cohort context (all four ids in `memberIds`, the highValue and primary
ids in `verifiedMetadataIds`, a `medianVotingPower` below the highValue entry's power and above the primary entry's)
and an out-of-cohort context (`memberIds: null`, same `verifiedMetadataIds`, `medianVotingPower: null`). Render all
four badges in the existing `ROW_STYLE` row and pass `cohort={cohort}` to each. Story name: `All categories`.
With the knob on, the row shows High value / Threshold / Primary / Non-metadata; with it off, High value degrades to
Primary — which is the visual proof of `shared-design-tokens.md:41`.

**11c.** `storybook/stories/governance/DRepDetail.stories.tsx` — `renderDetail` (`:46-60`) must pass the new required
prop or `tsc --noEmit` fails. Add a module-scope constant and pass it:

```tsx
const storyCohort: DRepCohortContext = {
  medianVotingPower: new BigNumber('1000000'),
  memberIds: new Set([withAnchorEntry.drepId]),
  verifiedMetadataIds: new Set([withAnchorEntry.drepId]),
};
```

with `cohort={storyCohort}` on the `<DRepDetail …>` element at `:52-58`, importing `DRepCohortContext` as a type from
`'../../../source/renderer/app/stores/GovernanceStore'`. Do **not** add an anchor-state knob here — that is task-151's
content, not this task's.

**11d.** `storybook/stories/governance/DRepDirectory.stories.tsx` renders `<DRepDirectory>` at **two** sites — `:156`
and `:435`. Add the same module-scope `storyCohort` constant and pass `cohort={storyCohort}` at **both**, exactly as
Step 5c does for the two `DRepDirectoryList` sites. Check with `tsc --noEmit`, which covers `storybook/` (no
`include` in `tsconfig.json`), so a missed site costs one iteration rather than shipping silently.

#### Step 12 — Format, gate, record, commit

`nix fmt` is unavailable here and stays a user-owned pre-merge obligation.

Tracker row `task-172` in `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`: set
`"status": "complete"`, `"updatedAt"` to today's `YYYY-MM-DD`, and `"evidence"` to the repo-relative paths touched,
source files first then plan docs. Write a `statusReason` that states the measured gate outcomes (exact test counts,
`tsc` exit, lint warning delta) and names the two OWED items below — never report them green.

Commit, exactly one subject line, no body, no `Co-Authored-By`:

```
feat(gov): task-172 ground the DRep category badge in cohort membership
```

#### Verify

All commands run from `/home/node/.claude/jobs/f104125f/wt-anchor-1`. Baselines below were **measured** at
`bf112d9f8` before any anchor-1 task landed; task-149/150/151 will have moved the sweep totals, so treat the
**per-suite deltas** as the contract and the sweep as a no-regression check.

```bash
cd /home/node/.claude/jobs/f104125f/wt-anchor-1

# 1. Formatting (explicit paths only; nix fmt is unavailable here)
node_modules/.bin/prettier --write \
  source/renderer/app/stores/GovernanceStore.ts \
  source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx \
  source/renderer/app/components/governance/_shared/DRepCategoryBadge.spec.tsx \
  source/renderer/app/components/governance/_shared/DRepCategoryBadge.scss \
  source/renderer/app/components/governance/drep-detail/DRepDetail.tsx \
  source/renderer/app/components/governance/drep-directory/DRepCard.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.spec.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx \
  tests/jest/governance/GovernanceStore.spec.ts \
  storybook/stories/index.ts \
  storybook/stories/governance/DRepCategoryBadge.stories.tsx \
  storybook/stories/governance/DRepDetail.stories.tsx \
  storybook/stories/governance/DRepDirectory.stories.tsx
# expect: every path listed once, no error, exit 0

# 2. Typecheck — covers source/, tests/ AND storybook/ (tsconfig has no "include")
node_modules/.bin/tsc --noEmit           # expect exit 0, zero diagnostics
yarn compile                             # expect exit 0, ~22s (regenerates the gitignored *.scss.d.ts first)

# 3. Classifier suite
node_modules/.bin/jest --testPathPattern="governance/_shared/DRepCategoryBadge" --no-coverage --runInBand
# expect: 1 suite, 11 -> 22 tests passed, 0 snapshots

# 4. Card-side render + snapshot (regenerate, then re-run clean)
node_modules/.bin/jest --testPathPattern="governance/drep-directory/DRepDirectory.spec" --no-coverage --runInBand -u
node_modules/.bin/jest --testPathPattern="governance/drep-directory/DRepDirectory.spec" --no-coverage --runInBand
# expect: 1 suite, 47 -> 48 tests passed, 1 snapshot passed (0 written on the second run)
git diff --stat source/renderer/app/components/governance/drep-directory/__snapshots__/DRepDirectory.spec.tsx.snap
# expect: only the two tooltip strings inside the single existing snapshot key changed

# 5. Detail-side render + snapshot
node_modules/.bin/jest --testPathPattern="containers/governance/DRepDetailPage" --no-coverage --runInBand -u
node_modules/.bin/jest --testPathPattern="containers/governance/DRepDetailPage" --no-coverage --runInBand
# expect: 1 suite, +1 test and +1 snapshot relative to whatever this suite reported
#         before this task (20 -> 21 tests, 1 -> 2 snapshots in build order;
#         0 written on the second run). The delta is the contract, not the total.

# 6. Container mock + store computeds
node_modules/.bin/jest --testPathPattern="containers/governance/DRepDirectoryPage" --no-coverage --runInBand
# expect: 1 suite, 8 tests passed (unchanged)
node_modules/.bin/jest --testPathPattern="tests/jest/governance/GovernanceStore" --no-coverage --runInBand
# expect: 1 suite, +6 tests relative to whatever this suite reported before this
#         task (43 -> 49 in build order). The delta is the contract, not the total.

# 7. i18n markers — both new keys and both reworded tooltips stay !!!-prefixed
node_modules/.bin/jest --testPathPattern="tests/jest/i18n/preliminaryCopyMarkers" --no-coverage --runInBand
# expect: 1 suite, 4 tests passed (task-151 may have widened this to 5)

# 8. Catalog parity — both files must carry the same governance key set
node -e "const e=require('./source/renderer/app/i18n/locales/en-US.json'),j=require('./source/renderer/app/i18n/locales/ja-JP.json');\
const g=o=>Object.keys(o).filter(k=>k.startsWith('governance.')).sort();\
const a=g(e),b=g(j);console.log(a.length,b.length,JSON.stringify(a)===JSON.stringify(b));\
console.log(a.filter(k=>!/^!!!/.test(e[k])||!/^!!!/.test(j[k])))"
# expect: "97 97 true" then "[]"  (95/95 on entry, +2 from this task; 84/84 at HEAD plus task-151's eleven)

# 9. Invariant #8, structurally — the category must not be readable by any
#    ordering, filtering, search or cohort path
grep -rn "getDRepCategory\|DRepCategoryBadge\|DRepCategory\b" \
  source/renderer/app/stores/ \
  source/renderer/app/components/governance/drep-directory/helpers.ts \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryFilters.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectorySearch.tsx
# expect: no output, exit 1

# 10. Governance sweep — no regression anywhere else
node_modules/.bin/jest --testPathPattern="governance" --no-coverage --runInBand
# HEAD basis (bf112d9f8): 17 suites (1 skipped), 313 tests (301 passed, 12 skipped), 9 snapshots
# this task adds +19 tests (+11 badge, +1 directory, +1 detail page, +6 store) and +1 snapshot
# the 1 skipped suite / 12 skipped tests are GovernanceCliArgvSmoke self-skipping (cardano-cli off PATH) - expected

# 11. Lint
yarn lint
# expect: exit 0, 0 errors. The warning count moves upward: every file this task
# touches under source/ and storybook/ is linted (tests/ is eslint-ignored).
# Baseline at HEAD is ~5591 warnings; a higher count is expected, errors are not.
```

**Storybook is verified by eye, and only by eye:** `yarn storybook` (dev server) is the real floor and there is no
browser in this environment. See OWED below.

#### Acceptance

| AC | Verbatim criterion | Discharge |
|---|---|---|
| AC-1 | "getDRepCategory takes an explicit cohort-membership input derived from GovernanceStore.defaultCohort — never a second derivation of the top-35 / 200 / 6-epoch rule — so task-153's doNotList exclusion flows through without touching the classifier." | **GREEN.** Step 1b computes `cohortContext.memberIds` as `new Set(this.defaultCohort.map(e => e.drepId))` — the single existing `defaultCohort` (`GovernanceStore.ts:174-188`) is the only source. Step 2c's classifier reads `cohort.memberIds` and contains no reference to 35, 200 or 6. Steps 4–5 thread it store → container → props at both call sites. Proof: Verify §9's grep shows the constants live only in the store; Step 7 cases 1, 14, 15 pin the behaviour. When task-153 adds a `doNotList` exclusion inside `defaultCohort`, the excluded id simply stops appearing in `memberIds` — zero classifier edits. |
| AC-2 | "getDRepCategory consumes the verified metadata-completeness flag from task-151; on-chain `anchor != null` is no longer the metadata input in any code path." | **GREEN for the classifier.** `anchor` is removed from `DRepCategorySource` (Step 2c), so the classifier is structurally incapable of reading it; completeness comes from `cohort.verifiedMetadataIds`, which Step 1b reads straight off task-151's `@computed get verifiedMetadataIds()` — the single derivation over `anchorStateByDRepId` entries with `state === 'verified'`. Step 7 case 11 passes an entry that *does* carry an on-chain anchor and asserts `nonMetadata`. **Scoped, and recorded:** `filterDReps` (`helpers.ts:189-220`) still uses `entry.anchor == null` for the user-facing "With / Without metadata" filter — see resolved judgment call 8 and the note below. |
| AC-3 | "High value renders per shared-design-tokens §1a (inside the default randomized cohort, completed metadata, voting power above the cohort median), and the classifier's result is defined and tested for entries outside the cohort (detail deep-link, favorites, show-all, search, ranking-unavailable fallback)." | **GREEN.** Step 2c implements the §1a rule exactly (`inCohort && hasVerifiedMetadata && isAboveMedian`) with the median from Step 1b. The out-of-cohort result is total, not undefined: `memberIds?.has(...) ?? false` covers both the empty-Set case (favorites, show-all, search, deep link) and the `null` case (ranking-unavailable fallback), and the entry then falls through Threshold → Primary → Non-metadata. Tested by Step 7 cases 8, 10, 14, 15; the ranking-unavailable path is additionally exercised by Step 8a's cohort-less default and Step 9c. |
| AC-4 | "No category tooltip claims default-Recommended membership for an out-of-cohort entry; reworded en-US and ja-JP strings plus governance.drepDirectory.category.highValue and its .tooltip sibling land via `yarn i18n:manage`, all `!!!`-prefixed." | **GREEN.** Step 2b rewords `primary.tooltip` to `!!!Has verified off-chain metadata.` and `threshold.tooltip` to `!!!Approaching expiry — review before delegating.` — the two categories that render out of cohort now make no membership claim. High value keeps the membership sentence and that is correct: it renders only when `inCohort` is true. Step 6 mints `governance.drepDirectory.category.highValue` and `.highValue.tooltip` in both catalogs through `yarn i18n:manage`, all eight strings `!!!`-prefixed. Proof: Verify §7 (marker spec) and §8 (parity + marker script, expect `97 97 true` then `[]` — 95/95 on entry from task-151, +2 here); Step 7 cases 16–20 assert the exact rendered `title` strings. |
| AC-5 | "Unit tests pin the full priority order High Value > Threshold > Primary > Non-metadata including the High-value/Threshold tie-break, cover in-cohort vs out-of-cohort classification of the SAME entry, and re-assert that no ordering, filtering, or cohort code path reads the category." | **GREEN.** Step 7: case 5 is the High-value/Threshold tie-break (`drepActivity: 10` in cohort, verified, above median → `highValue`); cases 6–8 pin Threshold, 9–10 Primary, 11–13 Non-metadata and the window edges; case 14 runs the *same* `baseEntry` through both contexts; cases 21–22 read the five ordering/filtering/search files and `GovernanceStore.ts` from disk and assert none mentions the badge module. Verify §9 is the same assertion as a grep. |
| AC-6 | "Storybook renders all four categories in en-US and ja-JP without overflow, and the DRepCard / DRepDetail snapshots are refreshed at both call sites." | **SPLIT. Snapshots GREEN, visual pass OWED.** *Green:* the card-side snapshot `DRepDirectory renders exactly one category badge per card (snapshot) 1` and the detail-side `DRepDetailPage renders the category badge in the detail header (snapshot) 1` are both regenerated (Verify §4, §5), and Step 9b adds a second detail snapshot pinning the new High value badge. Step 11a finally registers `DRepCategoryBadge.stories`, `DRepDetail.stories` and `DRepDirectoryBanner.stories` — before this task they were imported nowhere (`storybook/stories/index.ts:16-18`) and never rendered, so the criterion was previously unsatisfiable. Step 11b makes all four categories reachable from one knob. *OWED:* "renders … in en-US and ja-JP without overflow" is a visual judgement. `jest.config.js` `roots` exclude `storybook/` entirely and there is no browser in this environment, so it **cannot** be executed here. Record it as OWED. Do not mark it green. |

#### OWED at close (nothing here may be reported green)

1. **Storybook visual + ja-JP overflow pass** for all four category badges at both call sites (AC-6, second half).
   Requires `yarn storybook` (dev server) and a human eye; no browser exists in this devcontainer, and `storybook/`
   is outside the Jest roots. `yarn storybook:build` is red at HEAD for unrelated reasons and is not a substitute.
   The specific risk to look for: `!!!高価値` plus the `!!!` marker inside the fixed-width card top row
   (`DRepCard.tsx:104-122`), where the status badge, category badge and DRep id share one flex row.
2. **`nix fmt` before merge.** `nix` is absent here; `node_modules/.bin/prettier --write <explicit paths>` is the
   recorded substitute. This remains a user-owned pre-merge obligation.

#### Notes to the design owner (record; do not deviate in code)

- **High value suppresses the expiry hint.** With the binding priority at `shared-design-tokens.md:39`, an in-cohort
  verified DRep above the median that is also 7–12 epochs from expiry shows *High value* and the user never sees
  "Approaching expiry — review before delegating." That is what the binding rule says; it may not be what UX wants.
- **The "With metadata" filter and the Primary badge now mean different things.** After this task, the badge's
  metadata input is Blake2b-256-verified anchor content while `filterDReps`'s metadata filter is still on-chain anchor
  *presence* (`helpers.ts:198` and `:201`). A DRep can therefore match "With metadata" and still show *Non-metadata*.
  Re-pointing the filter is a copy and semantics change that needs its own row.
- **§1a specifies no colour for High value.** Step 3 introduces `--badge-highlight-fg` / `--badge-highlight-bg` with a
  violet fallback, chosen to stay distinguishable from the green Active status badge (`DRepStatusBadge.scss:26-27`)
  that sits beside it in the same card row. Theme-token confirmation is outstanding.
