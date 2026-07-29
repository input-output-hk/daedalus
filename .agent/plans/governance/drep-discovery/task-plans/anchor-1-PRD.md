# ANCHOR-1 PRD: Hardened Anchor Fetch + Verify + givenName Render

> **Planning status:** `draft` | **Slice status:** not started (all 5 tasks `pending`) | **Date:** 2026-07-29 | **Parent plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `anchor-1` — "Anchor 1 - Hardened anchor fetch + verify + givenName render" (`riskLevel: high`; tasks JSON `:1586-1693`)
> **Tasks:** 5 — task-149, task-150, task-151, task-152, task-172 (all `pending` at the planning anchor `bf112d9f8`)
> **Preceding slice:** [cv-2-PRD.md](./cv-2-PRD.md) (closed 2026-07-29 at `fb025e44e`)
> **Findings:** `research/anchor-1-findings.md` — optional; if anchor-1 produces no new research, "no new research" is recorded in the Final Outcome instead (`prompt.md:268-269`).
> **Implementation guide:** `anchor-1-implementation-guide.md` (authored after this PRD)
> **Phase metadata:** the `anchor-1` phase object carries **no `auditSummary`** — only `slice-1` has one (verified: `slice-1` keys include `auditSummary`, `cv-2` and `anchor-1` do not). Do not invent one.
> **Anchors:** every `path:line` in this document was opened and verified in the worktree at branch content `feat/drep-discovery`, commit `bf112d9f8`, tree clean. Where the tasks JSON, a design doc, or an inherited findings entry cites an anchor that no longer matches, the drift is called out in **Corpus-vs-Repo Corrections anchor-1 Inherits** and the live repo wins (`prompt.md:39-41`).

---

## Executive Summary

anchor-1 is the only slice in DRep Discovery that opens an outbound socket. It
builds the feature's transport-security floor complete, in one pass, and then
spends exactly one render on it: the CIP-119 `givenName`, in the detail view,
behind a *Verified off-chain content* label.

- **The scheme gate lands first.** `source/main/ipc/open-external-url.ts:1-15`
  hands any string straight to `shell.openExternal` (`:13-15`). task-152 rejects
  every non-`https:` scheme before that call, logs only the scheme, and fixes the
  one real non-https producer the app-wide audit found —
  `getNetworkExplorerUrl` (`source/renderer/app/utils/network.ts:36-43`) emits
  `'http://'` for STAGING and every unlisted network. Landing the gate without
  that fix silently breaks explorer links on every non-mainnet build (D-5).
- **A hardened, transport-agnostic fetch service.** task-149 adds
  `source/main/governance/AnchorFetchService.ts` over Node's builtin `https` and
  `dns`: TLS verification on, redirects off, ≤10 s connect+total timeout, ~1 MB
  hard cap, a JSON content-type allow-list, an SSRF address guard, and
  DNS-rebinding mitigation that binds the validated resolved IP to the actual TCP
  connection. It returns bounded raw bytes and nothing else — no parse, no cache
  write (S-1). It is also the **first main-process governance code to carry an
  explicit sanitization-floor assertion**, and the row that discharges cv-2 F-9's
  deferred `filterLogData` fallback in full (D-6).
- **Verify before anything.** task-150 adds `AnchorVerificationService` and an
  on-disk, hash-keyed, immutable cache under `stateDirectoryPath`
  (`source/main/config.ts:128`). The binding order is cache read → fetch →
  Blake2b-256 over the bounded bytes → *then* cache write and `JSON.parse`
  (S-2, S-3). Staleness is structural: a changed on-chain hash is a different
  filename, so no code path can serve stale content for a new hash (D-3).
- **One verified field reaches the screen.** task-151 adds
  `verifiedName: string | null` to both directory-entry interfaces (D-1 — this
  discharges cv-2 F-2, the highest-severity residual the slice inherits), opens a
  per-DRep on-demand IPC channel `GOVERNANCE_DREP_ANCHOR_CHANNEL` whose handler
  never rejects (D-2, S-4), owns `GovernanceStore.anchorStateByDRepId` as the
  authoritative verification state (S-5), and renders the name through a new
  `DRepDetailAnchorContent` child (`drep-discovery-design.md:216`) with the
  three new source-label variants from `shared-design-tokens.md:51-53`.
- **The badge stops lying.** At HEAD `getDRepCategory`
  (`DRepCategoryBadge.tsx:60-69`) never reads `status`, has no cohort input at
  all, and falls through to `entry.anchor != null ? 'primary' : 'nonMetadata'`
  (`:68`) — while both shipped tooltips (`:13-29`) assert "Inside the default
  Recommended view", a claim that is false on show-all, in search, in favorites,
  on any deep-linked detail page, and across the entire directory whenever
  `isCohortActive` is false. task-172 rewrites the classifier to take an explicit
  `DRepCohortContext` derived from `GovernanceStore.defaultCohort`
  (`GovernanceStore.ts:174-188`), consumes task-151's verified-completeness set
  instead of `anchor != null`, activates **High value**, and rewords the copy
  (D-9, D-10, S-7, S-8).

Everything is offline-provable except four named items, all recorded as OWED and
none reported green: `nix fmt`, the real SIPO vector bytes, the Storybook
visual/ja-JP pass, and a live anchor fetch. **Zero npm dependencies are added** —
`blake2b`, `blakejs`, `bignumber.js` and `bech32` are already production deps
(`package.json:204-208`) and `https`/`dns` are Node builtins (`plan.md:182`).

## Problem Statement — Why Now

- **The anchor reference is deliberately inert and the code says so.**
  `DRepDetailAnchorSection.tsx:55-56` carries the gating comment: "Deliberately
  inert text: no anchor may be fetched, rendered as a link, or opened before the
  hardened anchor pipeline lands." slice-4 shipped the URL and hash as plain text
  under an *On-chain anchor reference* label (`drep-discovery-design.md:214`).
  anchor-1 is the phase chartered to remove that constraint —
  `plan.md:299`: "`anchor-1` depends on the detail view (`slice-4`) as its first
  verified render surface."
- **A DRep is currently a 56-character identifier.** `plan.md:158` fixes the name
  source: "CIP-119 `body.givenName` only. If the anchor has not been verified,
  the name is hidden and only the DRep id is shown with the on-chain source
  label." `givenName` is compulsory in CIP-119
  (`research/external-research.md:57-65`), so anchor verification is the only
  thing standing between the user and a human-readable delegation target.
- **The category badge asserts something false today.** `DRepCategoryBadge.tsx:16`
  — "!!!Inside the default Recommended view with verified metadata." — and
  `:27` — "!!!Inside the default Recommended view but approaching expiry" — both
  render on entries the cohort excludes by construction.
  `shared-design-tokens.md:41` is binding on this: "No tooltip may assert
  default-cohort membership for an entry the cohort excludes." Once task-150
  lands, the same tooltip additionally claims *verified* metadata for anchors that
  failed Blake2b-256 verification, because the metadata input is still
  `entry.anchor != null` (`:68`). The correction therefore belongs **with** the
  anchor pipeline, not after it (task-172 description).
- **The `givenName` orphan is a live, owner-assigned obligation.**
  `research/cv-2-findings.md:2168-2183` (F-2, *Open at close*, "highest residual
  risk") names this planning pass as owner: "anchor-1 planning extends task-151
  to add a name field to both interfaces from the verified CIP-119 payload…
  Without the first, the second has no data source." Re-verified at `bf112d9f8`:
  `DRepDirectoryEntry` (`governance.types.ts:51-62`) and `AppDRepDirectoryEntry`
  (`GovernanceStore.ts:20-31`) still carry exactly five fields each, none of them
  a name. D-1 discharges it.
- **The sanitization fallback's trigger condition fires here.**
  `cv-2-findings.md:2104-2120` deferred F-9's `filterLogData` extension "to the
  first anchor row that adds a logging or fetch sink". task-149 *is* that row, and
  its AC-9 makes it explicit. `filterLogData` has no main-process counterpart —
  `source/main/utils/logging.ts:26-33` forwards `toJS(data)` into electron-log
  untouched — so the main side needs its own written discipline, not a shared
  helper (D-6c).
- **An unhardened `openExternal` becomes reachable the moment a URL is rendered.**
  `plan.md:344`: harden `open-external-url.ts` "before any anchor URL is
  rendered". The anchor URL is attacker-chosen data recorded on-chain by the
  DRep; `open-external-url.ts:13-15` currently passes it verbatim to the shell.

## Canonical Build Order

The tasks JSON listing order for anchor-1 is **`149, 150, 151, 152, 172`**
(objects at `:1592-1614`, `:1615-1632`, `:1633-1654`, `:1655-1669`,
`:1670-1691`). Dependencies verified per row:

| # | task | `dependencies` (json) | in-slice deps satisfied earlier? |
|---|---|---|---|
| 1 | task-152 | *(none)* | n/a — the only zero-dependency row in the phase |
| 2 | task-149 | task-104 ✔ (`complete`) | n/a (cross-slice, landed) |
| 3 | task-150 | **task-149** | yes (#2) |
| 4 | task-151 | **task-150**, task-116 ✔ (`complete`) | yes (#3) |
| 5 | task-172 | **task-151**, task-118 ✔, task-119 ✔ (both `complete`) | yes (#4) |

**One binding amendment: task-152 is hoisted to position 1.** Its dependency list
is empty, so the hoist breaks no edge. Three reasons make it mandatory rather
than cosmetic:

1. **task-152 AC-3 is a gate, not a deliverable** — "Anchor URL rendering remains
   gated on this hardening landing." A gate that lands after the thing it gates
   is not a gate. `plan.md:344` states the same ordering.
2. **task-151's renderer link gate is only safe once main rejects non-https.**
   D-5c makes `DRepDetailAnchorSection` render the anchor URL as a link only when
   it parses as `https:` (today it is inert text, `:55-57`), and **task-151 owns
   that edit** — task-152 touches no governance component at all. Offering the
   link before the shell handler is hardened would ship the offer without the
   action behind it, which is exactly what task-152 AC-3 forbids.
3. **task-152 carries an app-wide change with zero anchor coupling** — the
   `network.ts:36-43` https fix (D-5d). Isolating it in the first commit keeps a
   regression in explorer links attributable to one small diff.

**Canonical anchor-1 build order (binding):**

```
152 → 149 → 150 → 151 → 172
```

Ordering constraints that are *not* free and must not be re-derived:

- **149 before 150** — task-149 AC-8 forbids parsing and cache writes in the
  fetch row; task-150 AC-1 requires verification "on bounded raw bytes" that only
  S-1 produces. Building them together loses the AC-8 boundary.
- **150 before 151** — the renderer may only ever see verified content
  (invariant #3). S-4's `DRepAnchorResult` is main's promise that it already
  verified; 151 has no verification logic of its own.
- **151 before 172** — task-172 AC-2 forbids `anchor != null` as the metadata
  input "in any code path", and the replacement (`cohortContext.verifiedMetadataIds`,
  S-7) is derived from the `anchorStateByDRepId` map task-151 creates (S-5).
- **172 last** — it is the only row that touches both `DRepCard.tsx:120` and
  `DRepDetail.tsx:104` plus their committed snapshots, and its Storybook
  registration work (D-8) needs `DRepDetail.stories.tsx` to already render the
  anchor states 151 introduced.

## Per-Task Contract (interaction mode, scope, non-goals, dependencies)

No anchor-1 task is in the locked non-autonomous set — `prompt.md:191-195` names
exactly **task-125** (`manual_execution`), the **task-166 remainder**
(`manual_execution`), **task-158** (event-driven standing guardrail), and the
**release-end `!!!` copy review** (user-owned), and instructs: "Never relabel
these autonomous. Everything else is `autonomous` unless slice planning surfaces
a genuine blocking decision." Planning resolved O-1 … O-13 from the sources named
in each decision; **no anchor-1 task is escalated**, so all five rows are
`autonomous`. The one pre-named stop condition — the ~1 MB cap versus an inline
`imageObject` (`prompt.md:253-254`) — is already resolved at `plan.md:346` and
does not fire.

| Task | Mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-152** — HTTPS-only `openExternal` allow-list | `autonomous` | Guard `source/main/ipc/open-external-url.ts:13-15` so a non-`https:` scheme is rejected **before** `shell.openExternal`; log `{ scheme }` only (D-5b); new colocated `source/main/ipc/open-external-url.spec.ts` with `@jest-environment node` and `jest.mock('electron')` (D-5e); change `network.ts:37-40` to always emit `'https://'` plus a unit test over MAINNET / STAGING / an unlisted network (D-5d). **Main-process and `utils/network.ts` only** | No allow-list wider than `https:`; no `send` → `request` conversion (17 call sites, zero benefit — D-5a); no governance-local wrapper that bypasses `open-external-url`; no `mailto:` carve-out (grep → zero callers); **no edit to any file under `source/renderer/app/components/governance/` — AC-3 gates anchor-URL *rendering* on this landing, so the https link gate (D-5c) is task-151's** | *(none)* |
| **task-149** — Hardened anchor fetch service | `autonomous` | New `source/main/governance/AnchorFetchService.ts` exporting S-1 (`AnchorTransport`, `httpsAnchorTransport`, `fetchAnchorBytes`) over builtin `https` + `dns`; the seven guards of AC-1…AC-7; `AnchorFetchErrorType` added to `source/common/types/governance.types.ts` after `:71`; **the F-9 sanitization discharge in full** — twelve `sensitiveData` additions at `source/common/utils/logging.ts:24-49`, domain-shaped cases plus the first main-process spy case in `tests/jest/security/governance-sanitization.spec.ts`, and the `:4-5` docblock renarrow (D-6) | No JSON parsing, no cache write (AC-8); no IPFS implementation — the slot is the *interface*, `ipfs:` returns `UnsupportedScheme` (S-1); no `GovernanceQueryError` construction and no routing through `GovernanceQueryService` (D-12); no retro-fix of the three pre-existing whole-error sinks (D-6d); no new npm package | task-104 ✔ |
| **task-150** — Hash-verify, cache, parse | `autonomous` | New `source/main/governance/AnchorVerificationService.ts` (`resolveVerifiedAnchor`, S-2) and `source/main/governance/anchorCache.ts` (S-3); the on-disk immutable cache at `<stateDirectoryPath>/DRep-anchor-cache/<hash>.json` with the `/^[0-9a-f]{64}$/` key guard, `flag: 'wx'` writes, FIFO bound at 500 entries / 32 MB, and an in-flight dedup map (D-3); the IPC seam S-4 (contract in `api.ts` after `:666`, new `source/main/ipc/governanceAnchorChannel.ts`, registration in `source/main/ipc/index.ts` beside `:29`/`:51`, renderer export appended to `source/renderer/app/ipc/governanceChannel.ts:21`); the committed fixtures of D-11 | No electron-store (D-3); no in-memory cache mirror; no redundant staleness branch — staleness is structural (D-3); no rejection from the handler (D-2); no renderer-side verification | task-149 |
| **task-151** — Verified `givenName` render + completeness | `autonomous` | `verifiedName: string \| null` on `DRepDirectoryEntry` (`governance.types.ts:51-62`, set `null` at `GovernanceQueryService.ts:518`) and `AppDRepDirectoryEntry` (`GovernanceStore.ts:20-31`) (D-1); store enrichment S-5 (`anchorStateByDRepId`, `fetchAnchorContent`, `_applyVerifiedNames`); container reaction S-6 in `DRepDetailPage.tsx` beside `:43-50`; new `DRepDetailAnchorContent` child rendered by `DRepDetailAnchorSection` beneath `:49-79`; **the D-5c https link gate — `DRepDetailAnchorSection` renders the anchor URL as an `<a target="_blank" rel="noopener noreferrer">` routed through `AppStore.openExternalLink` only when `new URL(anchor.url).protocol === 'https:'`, otherwise the inert `<dd>` of today (S-9)**; three new `DRepSourceLabelVariant` values plus an optional `host` prop (S-8); eleven new i18n keys; the `preliminaryCopyMarkers` widening; Storybook fixture hash half (D-8) | No name on `DRepAnchorPresence` (`:66-71` stays the on-chain reference pair); no name on `DRepCard` (`drep-discovery-design.md:216`); no name in any search, sort or filter path (`plan.md:165`); no `objectives`/`motivations`/`qualifications`/`references`/`paymentAddress` (anchor-2, task-157); no confirmation-dialog identity change (anchor-2, task-154); no bulk prefetch (`drep-discovery-design.md:247`) | task-150, task-116 ✔ |
| **task-172** — Cohort-grounded classifier + High value | `autonomous` | `DRepCohortContext` + `@computed get cohortContext()` + `@computed get cohortMedianVotingPower()` in `GovernanceStore` (D-9, D-10, S-7); classifier rewrite replacing `DRepCategoryBadge.tsx:43-48` and `:60-69` with the two-argument signature (S-8); `cohort` prop threaded `DRepDetailPage.tsx:89-96` → `DRepDetail.tsx:41-48`/`:104` and `DRepDirectoryPage.tsx:91` → `DRepDirectory.tsx:79-101` → `DRepCard.tsx:120`; two new + two reworded i18n keys (AC-4); the priority-order and in/out-of-cohort unit suite (AC-5); three Storybook index registrations plus the cohort knob (D-8); snapshot refresh at both call sites | The badge never reorders, filters, or overrides anything (invariant #8); no re-derivation of the top-35 / 200 / 6-epoch rule (AC-1); no `anchor != null` metadata input anywhere (AC-2); no `status` field in the classifier signature — it is never read (D-9); no `Number(…)`/`parseInt`/`+`/`.toNumber()` on lovelace (D-10); no runtime store import inside the badge component | task-151, task-118 ✔, task-119 ✔ |

## Acceptance Criteria (verbatim from governance-drep-discovery-plan-tasks.json)

Quoted exactly as the tracker holds them, in JSON listing order. Line ranges are
the task objects; each row's `acceptanceCriteria` array is noted after it.

### task-149 — Add hardened anchor fetch service (json :1592-1614)

*(`acceptanceCriteria` array at `:1603-1613`)*

- "TLS default verification remains on; no `rejectUnauthorized: false`."
- "Redirects are disabled by default; 3xx surfaces as the same graceful empty state as 404."
- "Per-request connect+total timeout is <= 10 seconds."
- "Hard response-size cap is about 1 MB; abort on overflow."
- "Content-type allow-list includes application/json and application/ld+json; reject otherwise."
- "SSRF guard rejects RFC 1918, loopback, link-local, 0.0.0.0/8, ULA, and IPv6 reserved ranges."
- "DNS-rebinding mitigation binds the validated resolved IP to the actual TCP connection through either IP-literal connect with original Host/servername or a custom cached lookup option."
- "Fetch result is bounded raw bytes plus transport metadata only; no JSON parsing and no cache writes occur in this task."
- "INHERITED sanitization floor, main-process side: a task-111-style spy case asserts this service's own logger payloads carry no anchor URL, no DRep id and no raw error object on any failure path. filterLogData has no main-process counterpart — source/main/utils/logging.ts:26-33 forwards `data` to electron-log untouched — and the existing governance sinks log whole error objects (GovernanceQueryService.ts:523-526 logs `{ index, error }`; governanceChannel.ts:64 and :77 log `{ error }` whose details field holds trimmed cardano-cli stderr), so this is the first main-process regression assertion on the seam this service widens."

*Applied reading:* all three anchors in AC-9 resolve exactly at `bf112d9f8` —
`source/main/utils/logging.ts:26-33` is the `logToLevel` body forwarding
`toJS(data)`, `GovernanceQueryService.ts:523-526` logs `{ index, error: err }`,
and `governanceChannel.ts:64` / `:77` each log `{ error }`. A **third** whole-error
sink AC-9 does not name exists at `governanceChannel.ts:58-60`
(`{ error: snapshotError }`); it is recorded, not fixed (D-6d). "3xx surfaces as
the same graceful empty state as 404" (AC-2) is satisfied by D-2's
never-rejecting handler: both map to `{ status: 'unavailable', reason }`.

### task-150 — Hash-verify, cache, and parse DRep anchor bytes (json :1615-1632)

*(`acceptanceCriteria` array at `:1626-1631`)*

- "Blake2b-256 hash verification runs on bounded raw bytes before JSON parsing."
- "Only hash-verified bytes/content are written to immutable cache keyed by anchor hash."
- "Parse failures surface as graceful anchor-unavailable states without rendering partial content."
- "Stale cached content for a changed on-chain anchor hash is not served."

*Applied reading:* AC-4 is discharged **structurally, not by a check** — the cache
key is the filename, so a changed on-chain hash addresses a different file and no
code path can reach the old one (D-3). The guide must state that as the reason
rather than adding a staleness branch that implies the property is conditional.
AC-3's "without rendering partial content" is enforced at the type level:
`DRepAnchorResult`'s `verified` arm is the only one carrying content (S-4).

### task-151 — Render verified givenName with source label + expose metadata-completeness (json :1633-1654)

*(`acceptanceCriteria` array at `:1645-1653`)*

- "Verified givenName renders in the DRep detail view with a verified off-chain content source label."
- "Chain-native view remains fully functional when anchor is unavailable or fails verification."
- "Verified metadata-completeness state is exposed to GovernanceStore for the slice-5 cohort rule."
- "In-slice tests use the real SIPO CIP-119 test vector with verified hash."
- "The `Registered: epoch 502` row at drep-discovery-design.md:92 no longer reads as a build instruction: it is removed from the DRep-detail wireframe, or annotated as having no local source — `DRepDirectoryEntry` (source/common/types/governance.types.ts:51-62) carries no registration field, so restoring the row needs a new on-chain data source rather than a UI change."
- "The adjacent `Current votes` row at drep-discovery-design.md:93 is retained and drawn with the shipped unavailable value — DRepDetailOnchainSection renders that labeled field through `governance.drepDetail.votePositions.unavailable` — so the correction removes one row and not the pair."
- "The wireframe's On-chain box lists exactly the fields DRepDetailOnchainSection renders: Status, Expires in, Voting power, Current votes."

*Applied reading:* **AC-5, AC-6 and AC-7 are already discharged at HEAD** and
anchor-1 schedules no work for them (D-7). AC-4 is split: discharged in
**mechanism** against the committed fixture and the real preprod on-chain
`(url, hash)` pair, **OWED in content** for the real SIPO body bytes (D-11).
AC-3's "exposed to GovernanceStore" is satisfied by S-5's
`anchorStateByDRepId` — the authoritative verification state — projected into
`cohortContext.verifiedMetadataIds` for task-172 (S-7), not by the
`verifiedName` string, which is a display projection (D-1).

### task-152 — Harden openExternal for anchor URLs (HTTPS-only allow-list) (json :1655-1669)

*(`acceptanceCriteria` array at `:1664-1668`)*

- "openExternal rejects any URL whose scheme is not https before calling shell.openExternal."
- "Jest coverage asserts rejection of javascript:, file:, and data: URLs."
- "Anchor URL rendering remains gated on this hardening landing."

*Applied reading:* AC-2's "before calling" is asserted by proving the
`shell.openExternal` mock was **not** called, not merely that the promise
rejected. AC-3 is a **negative** criterion for this row: task-152 renders no
anchor link at all, and its Verify proves no file under
`source/renderer/app/components/governance/` moved. The link itself — D-5c's
renderer-side gate, offered only when the URL parses as `https:` — is built by
task-151, which lands fourth, so the hardening is on disk first. The hardening is
**app-wide**: `AppStore.openExternalLink`
(`AppStore.ts:80-82`) is the single renderer entry point for every external link
in Daedalus, which is why D-5d's `network.ts` fix rides the same commit.

### task-172 — Ground DRepCategoryBadge in cohort membership and activate the High value category (json :1670-1691)

*(`acceptanceCriteria` array at `:1683-1690`)*

- "getDRepCategory takes an explicit cohort-membership input derived from GovernanceStore.defaultCohort — never a second derivation of the top-35 / 200 / 6-epoch rule — so task-153's doNotList exclusion flows through without touching the classifier."
- "getDRepCategory consumes the verified metadata-completeness flag from task-151; on-chain `anchor != null` is no longer the metadata input in any code path."
- "High value renders per shared-design-tokens §1a (inside the default randomized cohort, completed metadata, voting power above the cohort median), and the classifier's result is defined and tested for entries outside the cohort (detail deep-link, favorites, show-all, search, ranking-unavailable fallback)."
- "No category tooltip claims default-Recommended membership for an out-of-cohort entry; reworded en-US and ja-JP strings plus governance.drepDirectory.category.highValue and its .tooltip sibling land via `yarn i18n:manage`, all `!!!`-prefixed."
- "Unit tests pin the full priority order High Value > Threshold > Primary > Non-metadata including the High-value/Threshold tie-break, cover in-cohort vs out-of-cohort classification of the SAME entry, and re-assert that no ordering, filtering, or cohort code path reads the category."
- "Storybook renders all four categories in en-US and ja-JP without overflow, and the DRepCard / DRepDetail snapshots are refreshed at both call sites."

*Applied reading:* AC-5's "High-value/Threshold tie-break" is resolved in D-9:
**High value wins**, because `shared-design-tokens.md:39` is the explicitly
binding priority order and §1a's "always shows Threshold, not Primary" sentence
governs the Threshold-versus-Primary pair only. AC-6's visual and ja-JP overflow
halves **cannot execute in this container** (no browser) and are OWED; the
snapshot refresh half is a runnable gate. AC-4's "all `!!!`-prefixed" covers the
two reworded tooltips as well as the two new keys.

## Planning Decisions (binding, as applied)

D-1 … D-12 close O-1 … O-13 (D-6 closes two). Each is binding on the
implementation guide's six section authors; contradicting one is a guide defect,
not an implementation choice.

### D-1 — Both directory-entry interfaces gain `verifiedName: string | null`; the authoritative verification state lives in a separate store map. (resolves O-1, cv-2 F-2)

`DRepDirectoryEntry` (`governance.types.ts:51-62`) gains a sixth field
`verifiedName: string | null`, set to `null` unconditionally by the bulk
`drep-state` mapping at `GovernanceQueryService.ts:518` — that query never
fetches an anchor. `AppDRepDirectoryEntry` (`GovernanceStore.ts:20-31`) gains the
identical field, populated by the renderer's per-DRep enrichment (S-5). The name
is **not** the completeness signal: `GovernanceStore.anchorStateByDRepId` is the
single source of truth, and `entry.verifiedName` is a projection re-applied after
every list rebuild.

*Rationale.* `cv-2-findings.md:2168-2183` (F-2, Open at close) assigns this
planning pass the action verbatim: "anchor-1 planning extends task-151 to add a
name field to both interfaces from the verified CIP-119 payload… Without the
first, the second has no data source." Adding the field to the wire type as well
preserves the documented equivalence at `GovernanceStore.ts:17-19` (the two
interfaces differ only in `votingPower`'s type) and reserves the slot for the
deferred bulk-prefetch name-search phase (`drep-discovery-design.md:247`) without
a later breaking wire change.

**`verifiedName`, not `givenName`.** The field may only ever hold a
Blake2b-256-verified value. Naming it after the CIP-119 source field
(`body.givenName`, `research/external-research.md:57-65`) invites a future writer
to fill it from unverified parse output; the CIP-119 mapping is documented once,
at the parse boundary. **`| null`, never `?`** — matching
`votingPower: Lovelace | null` (`:55`) and `anchor: DRepAnchorPresence | null`
(`:61`); an optional property lets a construction site omit it silently.

*Implications.* task-151 owns both edits plus `_applyVerifiedNames`. Storybook
fixtures (`storybook/stories/governance/_utils/fixtures.ts:148-166`) and every
`AppDRepDirectoryEntry` literal in specs and snapshots gain the field;
`tsc --noEmit` finds them all.
**Forbids:** no name field on `DRepAnchorPresence` (`:66-71` stays the on-chain
reference pair); no name on `DRepCard` (`drep-discovery-design.md:216`); no name
in any search, sort or filter path (`plan.md:165`).

### D-2 — Verified anchor content crosses IPC on a new per-DRep on-demand channel `GOVERNANCE_DREP_ANCHOR_CHANNEL` whose handler never rejects. (resolves O-2)

The contract is appended to the governance block opened at
`source/common/ipc/api.ts:654-666` (extending the governance-types import at
`:85-88`):

```ts
export const GOVERNANCE_DREP_ANCHOR_CHANNEL = 'GOVERNANCE_DREP_ANCHOR_CHANNEL';
export type GovernanceDRepAnchorRendererRequest = DRepAnchorPresence;
export type GovernanceDRepAnchorMainResponse = DRepAnchorResult;
```

The request is the on-chain anchor pair and **nothing else — no `drepId`**. The
fetch is keyed by `(url, hash)`, so main is structurally incapable of logging a
DRep id on this seam (invariant #2); the renderer correlates the response itself.
`DRepAnchorPresence` is reused verbatim per the convergence rule
(`prompt.md:237-242`). Response types land in `governance.types.ts` after `:71`:

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

**The handler never rejects.** 404, 3xx, blocked address, size overflow, hash
mismatch, parse failure and malformed request all resolve as
`{ status: 'unavailable', reason }`. task-149 AC-2 requires 3xx to surface as
"the same graceful empty state as 404"; a rejection would force the renderer to
separate transport outcomes from IPC-infrastructure failures on every call. One
renderer code path. **If a later row ever adds a rejection here it MUST re-throw
the plain marked object** `{ __governanceError: true, type, message, details }`
per `source/main/ipc/governanceChannel.ts:26-45` — `Error` instances flatten to
`{ name, message }` under structured clone and lose `details` (the comment at
`:26-31` states exactly this).

**`host` is computed in main** from the already-validated URL and shipped in the
response; the renderer must not re-parse the URL for the "Fetched from {host}"
tooltip (`shared-design-tokens.md:51`). A second parser is a second chance to
disagree with the one that made the security decision.

The four-file registration recipe mirrors `governanceChannel` exactly:
(1) the three lines above in `api.ts`; (2) a **new** `source/main/ipc/governanceAnchorChannel.ts`
built per `governanceChannel.ts:16-24` and `:47-82` — a separate file keeps the
anchor service's imports and its sanitization discipline (D-6) in one reviewable
unit; (3) import beside `source/main/ipc/index.ts:29` and call beside `:51`;
(4) a third export appended after `source/renderer/app/ipc/governanceChannel.ts:21`,
with the **reversed type-param order** the renderer file uses (`:13-16`).

**Forbids:** no bulk or prefetch anchor fetching
(`drep-discovery-design.md:247`); no anchor content on `DRepListQueryPayload`; no
anchor fetch from the directory list or `DRepCard`.

### D-3 — The immutable cache is an on-disk directory under `stateDirectoryPath`, one file per verified anchor hash — never electron-store. (resolves O-3)

Path: `<stateDirectoryPath>/DRep-anchor-cache/<hash>.json`
(`source/main/config.ts:128`). Directory precedent:
`source/main/utils/downloadManager.ts:36`, `source/main/utils/chainStorageManager.ts:80`.

- **Key** = `DRepAnchorPresence.hash` (`governance.types.ts:66-71`), lowercased
  and **validated against `/^[0-9a-f]{64}$/` before any `path.join`**. A failing
  hash returns `InvalidRequest` and never touches the filesystem — this is the
  path-traversal guard and needs a named test.
- **Stores the verified raw bytes**, not the parsed object. A cache hit **re-runs
  Blake2b-256 over the file bytes and treats a mismatch as a miss** (delete,
  refetch) — defence in depth against a tampered or truncated file — and lets
  anchor-2 extract more CIP-119 fields with no format change.
- **Immutable:** `fs.writeFileSync(p, bytes, { flag: 'wx' })`; `EEXIST` is
  success (another writer won the race). Never rewritten in place.
- **Staleness (task-150 AC-4) holds structurally, not by a check** — a changed
  on-chain hash is a different filename.
- **Bound:** `ANCHOR_CACHE_MAX_ENTRIES = 500`, `ANCHOR_CACHE_MAX_BYTES = 32 * 1024 * 1024`.
  On write, delete oldest-`mtime`-first until both hold. FIFO by write time, **no
  read-touch** — entries are immutable, so an LRU refresh would add a write per
  read for no correctness gain.
- **Restart:** the cache survives, which is the point. No in-memory mirror (a
  second layer needs its own invalidation). An **in-flight dedup map keyed by
  hash**, the same shape as `GovernanceQueryService.ts:78-79`, collapses
  concurrent requests for one anchor to one fetch.

*Why not electron-store.* `electron-store@8.0.1` (`package.json:223`) holds one
JSON blob in memory and rewrites the whole file on every `set`; an unbounded set
of up-to-1 MB payloads is the wrong shape. Its keys are schema-bound to
`STORAGE_KEYS` (imported at `source/main/ipc/electronStoreConversation.ts:5-8`
from `source/common/config/electron-store.config`) and network-prefixed at `:19`,
which a hash-keyed namespace does not fit. Invariant #2 (`prompt.md:101-105`)
bans DRep ids in electron-store payloads; the cached bodies are public
on-chain-referenced data and the key is a content hash, so nothing here is
sensitive — but staying out of the shared settings store means no reviewer has to
re-litigate that.

### D-4 — The "anchor-display feature flag" is NOT BUILT, and the plan reference is corrected to say so. (resolves O-4)

`plan.md:320` reads "Use the anchor-display feature flag for staged verification
control, not as a permanent production off-switch." No such flag exists:
`grep -rn "featureFlag\|FEATURE_FLAG\|isFeatureEnabled" source/` returns zero
hits, and the project has no feature-flag mechanism at all. The only precedent is
a launch-time env override (`source/main/environment.ts:132-133`,
`votingVisibleOverride: process.env.VOTING_VISIBLE_OVERRIDE === 'true'`).

anchor-1 already has the staging gate structurally:
`DRepDetailAnchorContent` renders only on an `anchorStateByDRepId` entry with
`state: 'verified'`, and the fetch is requested only on `DRepDetailPage` mount
(S-6). There is no staging window in which a flag would be the only off switch.
Minting the project's first flag mechanism for one render is disproportionate and
would itself need a removal task. `plan.md:319` ("keep slice-4 detail
local-on-chain only") is discharged by slice-4 having shipped.

*Implication:* anchor-1 adds a one-line correction to `plan.md:320` so the
reference stops reading as a build instruction — the same shape as the
`Registered: epoch N` correction already carried at
`drep-discovery-design.md:106`.

### D-5 — task-152 keeps the fire-and-forget contract, logs the scheme only, gates the link in the renderer, and fixes the one real non-https caller the app-wide audit found. (resolves O-5)

**(a) Rejection stays silent to the renderer.** `onReceive` keeps returning a
rejected promise (`open-external-url.ts:13-15`) and `AppStore.openExternalLink`
keeps `send()` (`AppStore.ts:80-82`). Converting `send` → `request` is an
app-wide change across 17 call sites for zero benefit — every caller already
ignores the result.

**(b) Main logs the scheme, never the URL:**
`logger.warn('Open external URL: rejected non-https scheme', { scheme })`, with
`scheme` = `new URL(url).protocol`, or the literal `'unparseable'` when `new URL`
throws. `filterLogData` is renderer-only (D-6c), so this is hand-enforced.

**(c) The renderer gates the offer, main gates the action — and task-151 owns the
renderer half.** `DRepDetailAnchorSection` renders the anchor URL as a link only
when it parses as `https:`; otherwise it stays the inert text it is today
(`:55-57`). A link that silently does nothing is worse than no link. The two
guards stay independent.

*Ownership corrected during planning review.* D-5 originally assigned this
renderer edit to task-152. It is **task-151's**, for one reason that overrides the
grouping convenience: task-152 AC-3 reads "Anchor URL rendering remains gated on
this hardening landing", so a task-152 commit that renders the link fails its own
criterion. task-152 therefore touches no file under
`source/renderer/app/components/governance/`, and its Verify proves that. The
decision's substance — the gate exists, it is renderer-side, it is independent of
main's guard — is unchanged; only the owning row moved. The gate is still
sequenced after the hardening because task-152 lands first.

**(d) The audit found one real non-https producer, and task-152 fixes it in the
same commit.** `getNetworkExplorerUrl` (`source/renderer/app/utils/network.ts:36-43`)
returns `'http://'` for every network that is not MAINNET / TESTNET /
DEVELOPMENT — i.e. STAGING and every unlisted network (preprod, preview,
shelley_qa, vasil_dev, selfnode). That URL reaches `openExternalLink` at
`containers/staking/StakingRewardsPage.tsx:39`,
`components/wallet/paper-wallet-certificate/InstructionsDialog.tsx:145`,
`components/wallet/paper-wallet-certificate/CompletionDialog.tsx:123`, and via
`getNetworkExplorerUrlByType` (`network.ts:70`) at
`containers/wallet/WalletSummaryPage.tsx:134` and
`containers/wallet/WalletTransactionsPage.tsx:46`. **Landing AC-1 without
touching `network.ts` silently breaks explorer links on every
non-mainnet/testnet build.** task-152 therefore changes `network.ts:37-40` to
always emit `'https://'`: the targets are all public explorer hostnames
(`source/renderer/app/config/urlsConfig.ts:3-5`), never loopback, and the http
branch already returns the *mainnet* host for unlisted networks (`network.ts:34`),
so `http://explorer.cardano.org` was already wrong. A unit test pins https for
MAINNET, STAGING and an unlisted network. No `mailto:` caller exists (grep → zero
hits).

**(e) Spec location:** `source/main/ipc/open-external-url.spec.ts`, colocated,
`@jest-environment node`, `jest.mock('electron')` for `shell`. Precedent:
`source/main/ipc/{chainStorageChannel,mithrilBootstrapChannel,mithrilPartialSyncChannel}.spec.ts`.
Cases: `https:` passes; `javascript:`, `file:`, `data:` rejected **before**
`shell.openExternal` (assert the mock was not called, per AC-2); plus `http:` and
unparseable input. A new spec under `source/` moves the `yarn lint` warning
baseline (cv-2 F-29) — expected, not a regression.

**Forbids:** no allow-list wider than `https:`; no governance-local wrapper that
bypasses `open-external-url`.

### D-6 — task-149 discharges F-9's sanitization fallback in full: twelve `sensitiveData` additions, domain-shaped floor-suite cases, an explicit main-process rule, and the docblock renarrow. (resolves O-6, O-9)

**(a) Extend `sensitiveData` (`source/common/utils/logging.ts:24-49`) by twelve
names.** F-9's seven — `drepIdentity`, `currentVote`, `votingTarget`,
`chosenOption`, `raw`, `cip105`, `credentialHex` — **plus `cip129`**, which F-9
omits although `DRepIdentity` carries it (`governance.types.ts:20-31`) and
shipping the sibling without it leaves the CIP-129 form unguarded — **plus the
four anchor-shaped names anchor-1 introduces**: `anchorUrl`, `anchorContent`,
`givenName`, `verifiedName`. A verified name is a DRep identity; logging it while
the user is on that DRep's detail page leaks the delegation target exactly as a
bech32 id would. Accepted tradeoff, stated rather than hidden: `raw` is generic
and a hit deletes the whole subtree (`:59-61`), so unrelated `{ raw: … }`
diagnostics elsewhere lose detail — verbosity loss, not a break, and F-9 named it.

**(b) Domain-shaped cases in `tests/jest/security/governance-sanitization.spec.ts`.**
F-31 (`cv-2-findings.md:2104-2112`) forbids a two-key patch. Added to the
`filterLogData` describe at `:71`: (1) nested
`{ drepIdentity: { raw, cip129, cip105, credentialHex } }` removed entirely;
(2) `{ currentVote: { voteKind: 'abstain', drepId } }` removed; (3)
`{ votingTarget }` and `{ chosenOption: 'no_confidence' }` removed; (4)
`{ anchorUrl }`, `{ givenName }`, `{ verifiedName }`,
`{ anchorContent: { givenName } }` removed; (5) **the first main-process case in
this file** (task-149 AC-9) — `jest.spyOn` over `source/main/utils/logging.ts`'s
`logger` `error`/`warn`/`info`, driving every `AnchorFetchService` failure path
and asserting no anchor URL, no host, no DRep id and no raw error object, using
the file's `jsonStrWithErrors` helper (`:65-69`) and never bare `JSON.stringify`;
(6) a negative case documenting the limit — a sensitive-looking *value* under a
non-sensitive *key* is NOT removed, because `sensitiveData.includes(key)` is
exact string equality (`:59`).

**(c) Main-process discipline is its own rule.** `filterLogData` has no main
counterpart: there is **no call site anywhere in `source/main/`**, and the single
textual occurrence is the comment at `source/main/utils/setupLogging.ts:180`
documenting the DRep-state snapshot's deliberate bypass, while
`source/main/utils/logging.ts:26-40` forwards `toJS(data)` untouched. Binding for
every new main-process governance log line: log only enum values —
`{ errorType: AnchorFetchErrorType.X }`, **never `{ error }`, never
`err.message`, never `err.stack`** (mirroring the shipped renderer discipline at
`GovernanceStore.ts:262-265` and `:302-304`); never a URL, host, hostname,
resolved IP or DRep id (the host the renderer needs travels in the IPC response,
not a log); scalars that identify nothing are allowed — `{ bytes }`,
`{ statusCode }`, `{ elapsedMs }`.

**(d) Do not retro-fix the three pre-existing whole-error sinks:**
`GovernanceQueryService.ts:523-526` (`{ index, error: err }`),
`governanceChannel.ts:58-60` (`{ error: snapshotError }` — a third sink AC-9 does
not name), `:64` and `:77` (`{ error }`, whose `details` holds trimmed
cardano-cli stderr). Pre-existing, outside anchor-1's diff, already assessed by
cv-2. Recorded under OWED for a later hardening row.

**(e) Renarrow the contradictory docblock in the same edit pass** (cv-2 F-31's
owner rule, `cv-2-findings.md:2093-2102`).
`tests/jest/security/governance-sanitization.spec.ts:4-5` claims no sentinel
reaches "any logger call or analytics payload", which `:500-504` contradicts by
*requiring* `sendEvent(…, 'abstain')`. Replace with:

> `* Asserts that no DRep ID, CIP-129/CIP-105 bech32 string, anchor URL or verified`
> `* anchor content reaches any logger call, and that no vote target reaches an`
> `* analytics payload. The derived vote kind is a sanctioned analytics dimension;`
> `* the vote target never is.`

**(f) The two-anchor re-proof rule is binding** (cv-2 F-31,
`cv-2-findings.md:2069-2080`): "the two anchors must be run and cited
**together**, and any future statement of the sanitization floor that names only
the security suite is incomplete." Every sanitization claim in this slice runs
and cites both commands in the Definition of Done.

### D-7 — task-151 AC-5, AC-6 and AC-7 are discharged before start; anchor-1 schedules no work for them. (resolves O-7)

Recorded as discharged-before-start with evidence, mirroring cv-2's F-30 shape.
**Do not edit the wireframe again.** `drep-discovery-design.md:92` is now
`│ │ Current votes: 2 Yes · 1 No · 0 Abstain (this epoch)     │  │`, and
`grep -rn "Registered: epoch" designs/` returns **exactly one** hit — the prose
correction at `:106` stating the row is deliberately absent. No wireframe under
`designs/` carries the row AC-5 names. `:106` carries that correction, retains
`Current votes` with "the row itself is not deferred and must stay in this box"
(AC-6), and opens with "The On-chain box lists exactly the fields
`DRepDetailOnchainSection` renders: Status, Expires in, Voting power, Current
votes" (AC-7). Live code agrees: exactly four `fieldRow`s at
`DRepDetailOnchainSection.tsx:94`, `:102`, `:114`, `:137`, the last rendering
`governance.drepDetail.votePositions.unavailable` (`:142`).

**Corpus-vs-repo correction to record:** AC-5's `:92` and AC-6's `:93` anchors
have drifted by one line — `:93` is now the closing box border. Prefer the live
repo; record the drift; restore nothing.

### D-8 — anchor-1 absorbs the Storybook registration gap for all three unregistered governance story files. (resolves O-8)

task-172 adds three imports to `storybook/stories/index.ts` after `:18`:
`./governance/DRepCategoryBadge.stories`, `./governance/DRepDetail.stories`,
`./governance/DRepDirectoryBanner.stories`. Verified: `:16-18` currently imports
only `./voting/Governance.stories`, `./governance/DRepDirectory.stories`,
`./governance/CurrentVoteSummary.stories`, so `DRepCategoryBadge.stories.tsx` and
`DRepDetail.stories.tsx` never render — which makes task-172 AC-6 unsatisfiable
without this. Registering two of the three named by cv-2 F-8
(`cv-2-findings.md:2192-2197`) would leave the same drift with a smaller number.

**No new connected-flow story.** Extend `DRepDetail.stories.tsx` with an
anchor-state knob (`verified` / `unavailable` / `no anchor`) and
`DRepCategoryBadge.stories.tsx` with a cohort-membership knob covering all four
categories in and out of cohort. The detail view is already reachable via the
registered `Voting / Governance > Connected flow`
(`storybook/stories/voting/Governance.stories.tsx`, registered at `index.ts:16`).
**Never a local `IntlProvider`, never per-locale story variants** — the global
`StoryWrapper` EN/JA toggle is the mechanism, per the binding comment at
`storybook/stories/governance/DRepDetail.stories.tsx:43-45`.

**Closes cv-2 F-15's hash half** (`cv-2-findings.md:2184-2191`):
`storybook/stories/governance/_utils/fixtures.ts:154` and `:164` both ship
`anchor: null`. task-151 gives `drepVerified` the real on-chain pair from
`research/drep-state-preprod-epoch295-sample.json:2852-2856` (`dataHash`
`9e8cb2b0f4c2ddbd9dea316b44680d8a989743868aeb40c1e6959982452f38e1`, url the
Cardano Academy `.jsonld`); `drepUnverified` keeps `anchor: null`.

**OWED:** `storybook/` is outside jest roots (`jest.config.js:129`
`roots: ['<rootDir>/tests', '<rootDir>/source']`) and there is no browser here —
task-172 AC-6's visual and ja-JP overflow pass cannot run. `yarn storybook:build`
is red at HEAD for unrelated reasons; `yarn storybook` (dev server) is the real
floor.

### D-9 — Cohort membership, verified completeness and the cohort median reach the classifier as ONE store-owned `DRepCohortContext` prop, threaded container → component. (resolves O-10)

The store computes it, beside `AppDRepDirectoryEntry` (`GovernanceStore.ts:20-31`):

```ts
export interface DRepCohortContext {
  memberIds: Set<string> | null;          // null when defaultCohort is null — never an empty Set for that case
  verifiedMetadataIds: Set<string>;       // ids whose anchor passed Blake2b-256 verification and parsed
  medianVotingPower: BigNumber | null;
}
@computed get cohortContext(): DRepCohortContext;
```

`memberIds` = `new Set(defaultCohort.map(e => e.drepId))`, or `null` when
`defaultCohort` is `null` (`GovernanceStore.ts:175` returns `null` when
`!isCohortActive`). `verifiedMetadataIds` derives from `anchorStateByDRepId`
entries with `state === 'verified'` — **never from `anchor != null`**
(task-172 AC-2).

Classifier signature, replacing `DRepCategoryBadge.tsx:43-48` and `:60-69`:

```ts
export type DRepCategory = 'highValue' | 'threshold' | 'primary' | 'nonMetadata';
export function getDRepCategory(
  entry: Pick<AppDRepDirectoryEntry, 'drepId' | 'votingPower' | 'drepActivity'>,
  cohort: DRepCohortContext
): DRepCategory;
```

`status` is deliberately dropped: it is never read at HEAD (`:60-69`) and the new
rules do not read it — cohort membership already encodes active-ness
(`GovernanceStore.ts:181-183`). A never-read field in a signature is a lie.
Body, in the binding priority order at `shared-design-tokens.md:39`:

```
inCohort    = cohort.memberIds?.has(entry.drepId) ?? false
verified    = cohort.verifiedMetadataIds.has(entry.drepId)
aboveMedian = inCohort && entry.votingPower != null && cohort.medianVotingPower != null
              && entry.votingPower.isGreaterThan(cohort.medianVotingPower)

if (inCohort && verified && aboveMedian)  -> 'highValue'
if (drepActivity in [7,12])               -> 'threshold'
if (verified)                             -> 'primary'
                                          -> 'nonMetadata'
```

**High value vs Threshold tie-break (AC-5's named case): High value wins.**
`shared-design-tokens.md:39` is the binding priority order; §1a's sentence "A DRep
with metadata that is also approaching expiry (7–12 epochs) always shows
**Threshold**, not Primary" governs the Threshold-versus-Primary pair only and
does not demote High value. This is the one ambiguous reading in the design and
the priority rule is explicitly labelled binding, so it governs. The UX concern
(a High-value badge suppresses the expiry hint) is recorded as a note to the
design owner in Risks, not as a code deviation.

**`null` memberIds behaves as out-of-cohort** — `shared-design-tokens.md:41`
already binds this ("the whole directory whenever `isCohortActive` is false and
the list falls back to the unfiltered registrations"). Both `null` and "not in the
Set" give `inCohort === false`: High value never renders and no tooltip may claim
membership.

**Props chain — store → container → component, one new prop named `cohort`, no
store import in a component.** `DRepDetailPage.tsx:89-96` adds
`cohort={governanceStore.cohortContext}`; `DRepDetail.tsx:41-48` gains
`cohort: DRepCohortContext` and forwards it at `:104`.
`DRepDirectoryPage.tsx:91` adds the same; `DRepDirectory.tsx:79-101` gains it
beside the existing `top35DRepIds: ReadonlySet<string>` (`:83`) — the exact
precedent for threading a store-derived Set through this chain — and forwards to
`DRepCard`, which passes it at `DRepCard.tsx:120`. `DRepCohortContext` is
declared in the **store** module and imported by the badge as a **type-only**
import, mirroring the shipped
`import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore'` at
`DRepCategoryBadge.tsx:4`; cv-2 D-6 forbids runtime store access, not erased type
imports.

**Invariant #8 restated (`prompt.md:125-127`):** the badge is informational only.
No ordering, filtering, cohort, search or favorites path may read
`getDRepCategory` or `DRepCategory`. Flow is one-way:
`defaultCohort` → `cohortContext` → classifier. AC-5 requires an explicit
regression test; keeping `GovernanceStore` free of any import from
`DRepCategoryBadge` is structurally checkable.

### D-10 — The cohort median is a BigNumber computed over the cohort only, and "above" means strictly greater. (resolves O-11)

`@computed get cohortMedianVotingPower(): BigNumber | null` in `GovernanceStore`,
feeding `cohortContext.medianVotingPower`: (1)
`const cohort = this.defaultCohort; if (cohort === null) return null;` (2) sample
= cohort entries with `votingPower != null`, sorted with `a.comparedTo(b)`
(returns -1/0/1 and never coerces); (3) empty sample → `null`; (4) odd `n` →
`powers[(n - 1) / 2]`, even `n` → `powers[n/2 - 1].plus(powers[n/2]).dividedBy(2)`.
Entries with `votingPower === null` are excluded from the sample and can never be
above the median.

**Tie-break: strictly above** — `entry.votingPower.isGreaterThan(median)`. An
entry *equal* to the median is not above it, so with an odd cohort the single
median entry is excluded. Deterministic, no secondary key, and "above the cohort
median" stays literally true.
**Scope: the cohort, never the full list** — `shared-design-tokens.md:41`:
"'above the cohort median' is a cohort statistic and is undefined for an excluded
entry."
**Invariant #5 restated (`prompt.md:115-117`):** `Lovelace = string`
(`governance.types.ts:47`), rehydrated to `BigNumber` at
`GovernanceStore.ts:288-295`. **Never `Number(…)`, `parseInt`, unary `+`, or
`.toNumber()`** on this path.

### D-11 — anchor-1 commits a clearly-labelled synthetic CIP-119 fixture whose digest is generated from the committed bytes; the real SIPO vector is OWED. (resolves O-12)

**Real SIPO bytes cannot be produced offline — say so, do not fake it.** There is
no network here and no CIP-119 JSON-LD body is committed anywhere in the repo
(`tests/mocks/governance/` holds `drep-state.json`,
`drep-stake-distribution.json`, `gov-state.json` only). The canonical
`drep.jsonld` digest is known from the corpus
(`a14a5ad4f36bddc00f92ddb39fd9ac633c0fd43f8bfa57758f9163d10ef916de`,
`README.md:83`, `plan.md:382`) but its bytes are not, so it cannot anchor a test.

Files to create:

1. `tests/mocks/governance/anchor-cip119-sample.json` — a synthetic CIP-119 body:
   `@context` abbreviated and labelled as such, `hashAlgorithm: "blake2b-256"`,
   `body.givenName` a short ASCII name (e.g. `"Daedalus Test DRep"`), plus one
   each of `objectives`/`motivations`/`qualifications` so anchor-2 reuses the file
   unchanged. Exact whitespace is irrelevant **because the digest is generated
   from whatever bytes are committed**.
2. `tests/mocks/governance/anchor-cip119-sample.hash` — one lowercase 64-hex line
   plus `\n`, **generated, never typed**:
   ```bash
   node -e "const fs=require('fs');const blake2b=require('blake2b');\
   const d=fs.readFileSync('tests/mocks/governance/anchor-cip119-sample.json');\
   process.stdout.write(blake2b(32).update(d).digest('hex')+'\n')" \
     > tests/mocks/governance/anchor-cip119-sample.hash
   ```
   Committing a literal instead of recomputing inside the test pins the algorithm
   — blake2b-256, unkeyed, no personalization — against a constant rather than
   against itself.
3. `tests/mocks/governance/anchor-malformed.txt` — bytes that fetch cleanly but do
   not parse (e.g. `{"body":`), for task-150 AC-3. `.txt`, not `.json`, so editor
   and lint JSON validation do not trip on it.
4. `tests/mocks/governance/README.md` — provenance: the fixture is synthetic; the
   real vectors are `https://sipo.tokyo/drep/SIPO.jsonld` (mainnet,
   `README.md:84`) and the Cardano Academy preprod `.jsonld` (`README.md:85`); the
   real on-chain pair used for cache-key and mismatch tests is
   `research/drep-state-preprod-epoch295-sample.json:2852-2856`; no offline copy
   of either body exists in this repo.

**Free offline cross-check:** one test asserts
`blake2b(32).update(bytes).digest('hex')` and
`blakejs.blake2bHex(bytes, null, 32)` agree on the committed fixture. Both are
production deps (`package.json:207-208`); agreement rules out a keying or
personalization mistake with no network. `blake2b` is the main-process precedent
(`source/main/utils/restoreKeystore.ts:68`, `blake2b(20).update(xpub).digest('hex')`)
and its streaming API suits a size-capped byte stream.

**task-151 AC-4 disposition:** discharged in **mechanism** (the real preprod
on-chain `(url, hash)` pair drives cache-key derivation, the hash-mismatch path
and the fixtures); **OWED in content** (the real SIPO body bytes and the assertion
that their digest equals the on-chain `dataHash`).

### D-12 — Every anchor error is an enum value; the word "latest" cannot reach `_shouldRetryWithConway`. (resolves O-13)

The anchor path transports `AnchorFetchErrorType` values only (D-2) — no free
text crosses IPC, so no anchor string can contain "latest". Additionally the
anchor path **must never construct a `GovernanceQueryError`**
(`GovernanceQueryService.ts:23-32`) and must never route through
`GovernanceQueryService`, so `_shouldRetryWithConway`'s text match
(`research/slice-1-final-pass-findings.md:80-82`: "Any new
`GovernanceQueryError(QueryFailed)` message that happens to contain the word
'latest' would trigger a spurious conway retry") cannot see it at all. Enum-only
transport makes the property structural rather than a convention someone must
remember.

Harmless but noticeable: the shipped renderer copy
`governance.drepDetail.notFound` = "!!!This DRep was not found in the latest
on-chain data." contains "latest". It is catalog copy in the renderer, never near
the CLI retry matcher. Leave it; noted so nobody "fixes" it in a panic.

## Cross-Task Seam Contracts

Six authors write the implementation guide in parallel from this PRD. The
semantics below are **binding and not re-derivable**; contradicting them is a
guide defect, not an implementation choice.

### S-1 — Bounded raw-bytes fetch (task-149 → task-150)

`source/main/governance/AnchorFetchService.ts`:

```ts
export interface AnchorFetchOk   { ok: true;  bytes: Buffer; host: string; contentType: string; byteLength: number }
export interface AnchorFetchFail { ok: false; reason: AnchorFetchErrorType }
export type AnchorFetchResult = AnchorFetchOk | AnchorFetchFail;
export interface AnchorTransport { readonly scheme: string; fetch(url: string): Promise<AnchorFetchResult> }
export const httpsAnchorTransport: AnchorTransport;
export function fetchAnchorBytes(url: string): Promise<AnchorFetchResult>;
```

`fetchAnchorBytes` selects a transport by scheme and returns `UnsupportedScheme`
for anything unregistered, `ipfs:` included — the reserved slot is the
*interface*, never a stub implementation. **Never throws, never parses JSON,
never writes cache** (task-149 AC-8). Built on Node builtins `https` and `dns`;
`source/main/mithril/mithrilNetworkConfig.ts:49-55` is an **API-shape reference
only, not a security model** (it has no timeout, no size cap and no content-type
check).

### S-2 — Verify + cache + parse (task-150 → task-151, via IPC)

`source/main/governance/AnchorVerificationService.ts`:

```ts
export function resolveVerifiedAnchor(anchor: DRepAnchorPresence): Promise<DRepAnchorResult>;
```

Binding order: cache read (re-verify digest) → on miss `fetchAnchorBytes` →
Blake2b-256 over the bounded bytes compared to `anchor.hash` → **only then**
cache write and `JSON.parse` (task-150 AC-1, AC-2). Never throws.

### S-3 — Cache primitives (task-150, and across restarts)

`source/main/governance/anchorCache.ts`:

```ts
export function readVerifiedAnchorBytes(hash: string): Buffer | null;
export function writeVerifiedAnchorBytes(hash: string, bytes: Buffer): void;
```

Both reject any `hash` failing `/^[0-9a-f]{64}$/` **before touching the
filesystem** (D-3).

### S-4 — The IPC seam (task-150 → task-151)

`source/common/ipc/api.ts` (appended to the governance block opened at `:654`):
`GOVERNANCE_DREP_ANCHOR_CHANNEL`,
`GovernanceDRepAnchorRendererRequest = DRepAnchorPresence`,
`GovernanceDRepAnchorMainResponse = DRepAnchorResult`.
`source/renderer/app/ipc/governanceChannel.ts` (appended after `:21`):
`export const governanceDRepAnchorChannel: RendererIpcChannel<GovernanceDRepAnchorMainResponse, GovernanceDRepAnchorRendererRequest>;`
— note the reversed type-param order versus main (`:13-16`).

### S-5 — Store enrichment (task-151 → task-172), the verified-completeness producer

`source/renderer/app/stores/GovernanceStore.ts`:

```ts
export type AnchorEnrichEntry =
  | { state: 'loading';     hash: string }
  | { state: 'verified';    hash: string; givenName: string | null; host: string }
  | { state: 'unavailable'; hash: string; reason: AnchorFetchErrorType };
@observable anchorStateByDRepId: Map<string, AnchorEnrichEntry>;   // absent key == idle
@action fetchAnchorContent(drepId: string, anchor: DRepAnchorPresence): Promise<void>;
private _applyVerifiedNames(entries: AppDRepDirectoryEntry[]): AppDRepDirectoryEntry[];
```

`fetchAnchorContent` no-ops when the map already holds `loading` or a terminal
state **for the same hash**; a changed on-chain hash re-triggers. On `verified` it
writes `verifiedName` into both `drepIndex` and `drepList` (rebuilt together at
`GovernanceStore.ts:288-295`). `_applyVerifiedNames` runs at the end of
`fetchDRepList` and `_enrichVotingPower` — both rebuild the list from the IPC
payload and would otherwise drop the projection; the map stays authoritative.

### S-6 — Container trigger (task-151)

`source/renderer/app/containers/governance/DRepDetailPage.tsx` adds a second
`reaction` beside the `isNodeInSync` one at `:43-50`:

```ts
reaction(
  () => governanceStore.drepIndex.get(this.props.match.params.drepId)?.anchor ?? null,
  (anchor) => { if (anchor) governanceStore.fetchAnchorContent(drepId, anchor); },
  { fireImmediately: true }
);
```

Deep links mount before the list resolves, so `fireImmediately` plus the reaction
covers both orders. The `drepId` read mirrors the shipped lookup at `:87`/`:91`.

### S-7 — Cohort + completeness input (GovernanceStore → task-172)

`DRepCohortContext`, `@computed get cohortContext()`, and
`@computed get cohortMedianVotingPower(): BigNumber | null` exactly as specified
in D-9 and D-10. Prop name at every call site: `cohort`.

### S-8 — Classifier and source label (task-172, task-151)

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

`host` is passed only for `'verified-off-chain'`, taken from the IPC response
(D-2), and interpolates into the tooltip (`shared-design-tokens.md:51`). The new
child per `drep-discovery-design.md:216` is
`source/renderer/app/components/governance/drep-detail/DRepDetailAnchorContent.tsx`,
props `{ state: AnchorEnrichEntry | null; intl }`, rendered by
`DRepDetailAnchorSection` beneath the existing URL/hash/Source rows (`:49-79`),
replacing nothing.

**`'unverified-anchor'` has no production emitter in anchor-1:** a hash mismatch
maps to *Anchor unavailable* per `shared-design-tokens.md:53` ("Fetch or hash
check failed" / "…could not be retrieved **or did not match** the on-chain
hash"), and main verifies before responding, so the renderer never sees
fetched-but-unverified content. The variant and its copy are minted anyway
because §2's five-label set is the design contract
(`shared-design-tokens.md:47-53`) and Storybook renders it — stated here so a
reviewer does not hunt for the missing code path.

### S-9 — The https link gate (task-151, D-5c)

```ts
// DRepDetailAnchorSection.tsx, extending the props at :35-38
type Props = {
  anchor: DRepAnchorPresence | null;
  anchorState: AnchorEnrichEntry | null;
  onOpenExternalLink: (url: string) => void;
  intl;
};
```

`onOpenExternalLink` is `stores.app.openExternalLink` (`AppStore.ts:79-82`),
threaded `DRepDetailPage` → `DRepDetail` → `DRepDetailAnchorSection` beside
`anchorState`. The URL row renders
`<a href={anchor.url} target="_blank" rel="noopener noreferrer">` only when
`new URL(anchor.url).protocol === 'https:'`; anything else — including an
unparseable URL — stays the inert `<dd>` text of today
(`DRepDetailAnchorSection.tsx:57`). **task-152 builds none of this.** Its AC-3
gates anchor-URL *rendering* on the hardening landing, so it touches no file
under `source/renderer/app/components/governance/`; the gate is task-151 Step 9.

## User Stories

- **US-A1.1 — See who I am delegating to.** As a wallet owner opening a DRep's
  detail page, I see the DRep's name when — and only when — its registered anchor
  content hash-matched the on-chain hash, labelled *Verified off-chain content*,
  so I know exactly how much the name is worth.
- **US-A1.2 — Never be misled by a broken or hostile anchor.** As a wallet owner,
  when an anchor is unreachable, oversized, wrongly typed, redirected, or fails
  the hash check, I see an *Anchor unavailable* state and the full on-chain view
  keeps working — I am never shown partial or unverified profile content.
- **US-A1.3 — My machine is not a probe.** As a user on a home or corporate
  network, a DRep cannot make Daedalus fetch `http://192.168.1.1/`, a
  link-local metadata endpoint, or an IPv6 reserved address by putting one in its
  anchor URL, and cannot smuggle one in through a DNS answer that changes between
  validation and connection.
- **US-A1.4 — Links open in a browser, or not at all.** As a wallet owner
  clicking a DRep's anchor link, the OS opens an `https:` page or nothing
  happens — a `javascript:`, `file:` or `data:` anchor URL can never reach the
  shell handler.
- **US-A1.5 — Badges tell the truth about where I am.** As a wallet owner viewing
  a DRep through search, favorites, show-all, or a direct link, the category
  badge never claims the DRep is "inside the default Recommended view" when it is
  not, and *High value* appears only for a cohort member with verified metadata
  and above-median voting power.
- **US-A1.6 — Repeat visits are instant and still safe.** As a wallet owner
  revisiting a DRep, the profile comes from the local hash-keyed cache without a
  second network call, and if the DRep re-registers with a new anchor hash I never
  see the old content.

## Non-Functional Requirements

- **Security floor is the deliverable (invariant #3, inlined).** TLS verification
  on with no `rejectUnauthorized: false` anywhere; redirects disabled by default;
  connect+total timeout ≤ 10 s per request — **one budget armed before DNS
  resolution, not after**, because `dns.promises.lookup` carries no timeout of its
  own and a resolver that never answers would otherwise sit outside the guard
  entirely; ~1 MB hard response cap with abort on
  overflow; content-type allow-list of `application/json` and
  `application/ld+json`; SSRF rejection of RFC 1918, loopback, link-local,
  `0.0.0.0/8`, ULA and IPv6 reserved ranges; DNS-rebinding mitigation binding the
  validated resolved IP to the actual TCP connection; Blake2b-256 verification
  before parse, cache or render; an immutable hash-keyed cache. **Lands complete
  in this slice and is never thinned** (`plan.md:293`).
- **Local-first (invariant #1, inlined).** The anchor fetch is the **only**
  outbound network call DRep Discovery makes, and it goes only to the DRep's own
  registered anchor URL. No explorer, indexer, GovTool, Koios, Blockfrost or
  aggregator (`prompt.md:98-100`).
- **Sanitization floor (invariant #2, inlined), widened by this slice.** No DRep
  id, no `abstain`/`no_confidence` literal, no CIP-129/CIP-105 bech32 string —
  and now no anchor URL, no host, no resolved IP and no verified name — in any
  logger, analytics or electron-store payload. Main-process log payloads carry
  enum error types and identity-free scalars only (D-6c). `logDRepStateSnapshot`
  (`source/main/utils/setupLogging.ts:178-183`) remains the one documented
  exception. Fixtures, specs and docs MAY contain DRep ids and anchor URLs — the
  floor binds runtime logging, analytics and store paths only.
- **Lovelace losslessness (invariant #5, inlined).** `Lovelace = string`
  (`governance.types.ts:47`) → `BigNumber` rehydration
  (`GovernanceStore.ts:288-295`). The new median computation and the above-median
  comparison use `BigNumber` arithmetic and `comparedTo`/`isGreaterThan` only —
  never `Number(…)`, `parseInt`, unary `+`, or `.toNumber()` (D-10).
- **Badges informational only (invariant #8, inlined).** No ordering, filtering,
  cohort, search or favorites code path may read `getDRepCategory` or
  `DRepCategory`. Data flows one way: `defaultCohort` → `cohortContext` →
  classifier.
- **i18n (invariant #11, inlined).** Every new en-US and ja-JP string keeps the
  leading `!!!`. Removing `!!!` is a release-end manual review, never a per-slice
  task (`prompt.md:132-133`, `README.md:18`). Both catalogs stay key-identical —
  measured today at 84 `governance.*` keys per locale, sets identical, all
  `!!!`-marked.
- **Zero new dependencies.** `blake2b@2.1.3` and `blakejs@1.1.0` are production
  deps (`package.json:207-208`); `bignumber.js@9.0.1` (`:205`) and `bech32@2.0.0`
  (`:204`) are already in use; `https` and `dns` are Node builtins. `axios@1.7.7`
  is a **devDependency** (`package.json:112`) and is not shippable in main.
  `plan.md:182` confirms nothing new is required.
- **Performance.** One anchor fetch per DRep detail view at most, deduped
  in-flight by hash and served from disk on repeat (D-3). No bulk or prefetch
  fetching (`drep-discovery-design.md:247`). The classifier stays two `Set`
  lookups plus one `BigNumber` comparison per render; the median is a `@computed`
  over at most 200 cohort entries (`GovernanceStore.ts:61` `COHORT_MAX_SIZE = 200`).
- **Accessibility.** Every source label carries text, never colour alone
  (`shared-design-tokens.md:18` contrast rule); the anchor link keeps
  `target="_blank" rel="noopener noreferrer"` (`plan.md:160`); the four category
  labels stay short to avoid wrapping in JA (`shared-design-tokens.md:37`).
- **Offline provability.** Every guard is proven against mocked `https`/`dns`
  following the main-process spec precedent at
  `tests/jest/governance/GovernanceQueryService.spec.ts:1-32`. What cannot be
  proven offline is enumerated under OWED and never asserted green.

## Architecture: Data Flow (anchor-1 delta)

```
MAIN PROCESS                                          RENDERER
────────────────────────────────────────────          ──────────────────────────────────────────
open-external-url.ts  (task-152)                      AppStore.openExternalLink :80-82
  scheme !== 'https:' -> reject, log { scheme }   <──  send(url)   (fire-and-forget, unchanged)
                                                      DRepDetailAnchorSection renders a LINK
                                                        only when url parses as https:
                                                        (D-5c, built by task-151)

AnchorFetchService.ts          (task-149, S-1)
  scheme select -> httpsAnchorTransport
  SSRF address guard  ─┐
  dns.lookup           ├─ validated IP bound to the TCP connect   (AC-7)
  https.request        ─┘  TLS on · redirects off · <=10s · ~1MB cap · JSON content-type
        │ AnchorFetchResult = { ok, bytes, host, contentType, byteLength } | { ok:false, reason }
        ▼
AnchorVerificationService.ts   (task-150, S-2)
  anchorCache.read(hash) ──> re-digest ──> miss? fetchAnchorBytes
  blake2b(32).update(bytes).digest('hex') === anchor.hash ?
        │ no  -> { status:'unavailable', reason: HashMismatch }
        │ yes -> anchorCache.write(hash, bytes)  [wx, immutable]  -> JSON.parse -> givenName
        ▼
governanceAnchorChannel.ts     (task-150, S-4)   GOVERNANCE_DREP_ANCHOR_CHANNEL
  request  = DRepAnchorPresence   { url, hash }   ── no drepId ever crosses this seam
  response = DRepAnchorResult                     ── never rejects (D-2)
        │
        ▼ ─────────────────────────── IPC ───────────────────────────►
                                                 GovernanceStore              (task-151, S-5)
                                                   @observable anchorStateByDRepId: Map<id, AnchorEnrichEntry>
                                                   fetchAnchorContent(drepId, anchor)   ← S-6 reaction
                                                   _applyVerifiedNames() -> entry.verifiedName  (D-1)
                                                        │                    │
                                    cohortContext  ◄────┤                    ▼
                                    (memberIds, verifiedMetadataIds,   DRepDetailPage :89-96
                                     medianVotingPower)  (D-9, D-10)         │
                                            │                                ▼
                                            │                          DRepDetail :104 :110
                                            │                            DRepDetailAnchorSection
                                            │                              └ DRepDetailAnchorContent  (NEW)
                                            │                                  givenName + DRepSourceLabel
                                            ▼                                  source="verified-off-chain" host={…}
                                    getDRepCategory(entry, cohort)   (task-172, S-8)
                                      highValue > threshold > primary > nonMetadata
                                      rendered at DRepCard :120 and DRepDetail :104
                                      read by NOTHING that orders, filters or selects  (invariant #8)
```

## What anchor-1 Deliberately Does NOT Include

- **The six anchor-2 tasks.** `task-153` honor CIP-119 `doNotList` in the default
  cohort (store-side exclusion; the DRep stays reachable via search / show-all /
  direct id); `task-174` the dual CIP-129 / CIP-105 `DRepIdDisplay` both-forms
  mode in detail and deduped search rows; `task-154` confirmation-dialog identity
  migration to the verified name (byte-equality preserved, HW on-device
  `vote.chosenOption` check); `task-155` the source-labeling sweep across DRep
  Discovery surfaces; `task-156` the `Abstain` / `No Confidence` treatment in
  directory surfaces; `task-157` the remaining verified CIP-119 profile fields
  (objectives, motivations, qualifications, references, paymentAddress). All six
  are `pending` in phase `anchor-2` and five of the six depend on task-151.
- **IPFS transport.** The interface slot is reserved, not implemented (task-149
  description). `ipfs:` returns `UnsupportedScheme` from `fetchAnchorBytes`
  (S-1); a stub implementation is forbidden.
- **`image` / `imageObject`.** "deferred out of this render set — dropped, not
  staged" (`drep-discovery-design.md:218`); the risk-table resolution at
  `plan.md:346` places the deferral in anchor-2/task-157. **The ~1 MB cap is
  unchanged and is not raised for the image case in this release.**
- **Directory-wide names and name search.** "Directory cards and search are
  **DRep-ID-only** in v1. Verified `givenName` (CIP-119) appears only in the
  detail view (anchor-1) and confirmation" (`plan.md:165`). `DRepCard` "does
  **not** render verified anchor content even after anchor-1/anchor-2"
  (`drep-discovery-design.md:216`).
- **Bulk or prefetch anchor fetching.** Per-DRep lazy fetch only:
  "per-DRep lazy anchor fetch (anchor-1) does not make names searchable across
  unvisited DReps" (`drep-discovery-design.md:247`).
- **Cucumber / e2e.** A deliberate v1 non-goal (`plan.md:166`, `README.md:45`) —
  anchor-1 ships Jest and Storybook coverage only.
- **No feature flag.** The "anchor-display feature flag" named at `plan.md:320`
  is not built; the reference is corrected instead (D-4).
- **No retro-fix of the three pre-existing main-process whole-error sinks**
  (`GovernanceQueryService.ts:523-526`, `governanceChannel.ts:58-60`, `:64`,
  `:77`) — recorded under OWED for a later hardening row (D-6d).
- **No `Retired` status and no "Expiring soon" / "Excluded from default cohort"
  badges.** Invariant #14 defers `retired`; slice-6 F-6
  (`research/slice-6-findings.md:90`) records the two designed badges as unowned
  drift, and anchor-1 does not adopt them.
- **No aggregate trust score, star rating, inline social embed, or
  auto-verified identity claim** — `research/external-research.md:69-71` rejects
  all of these outright, and anchor-1 renders exactly one anchor-derived field.

## i18n Key Inventory

All new keys are `!!!`-prefixed in **both** catalogs and land via
`yarn i18n:manage` (`package.json:52-54`), which runs `i18n:extract` then
`i18n:check`. Baseline measured at `bf112d9f8`: 84 `governance.*` keys in
`en-US.json` and 84 in `ja-JP.json`, key sets identical, **every one already
`!!!`-marked**.

**task-151 mints eleven** — six source-label keys and five anchor-content keys:

| key | en-US source copy |
|---|---|
| `governance.drepDetail.sourceLabel.verified` | `!!!Verified off-chain content` |
| `governance.drepDetail.sourceLabel.verified.tooltip` | `!!!Fetched from {host}, hash-matched the on-chain anchor hash.` |
| `governance.drepDetail.sourceLabel.unverified` | `!!!Unverified anchor` |
| `governance.drepDetail.sourceLabel.unverified.tooltip` | `!!!Anchor content fetched but not yet hash-verified. Treat as untrusted.` |
| `governance.drepDetail.sourceLabel.anchorUnavailable` | `!!!Anchor unavailable` |
| `governance.drepDetail.sourceLabel.anchorUnavailable.tooltip` | `!!!The anchor URL could not be retrieved or did not match the on-chain hash. Off-chain profile is not shown.` |
| `governance.drepDetail.anchorContent.title` | verified-content section heading |
| `governance.drepDetail.anchorContent.givenName` | the `givenName` field label |
| `governance.drepDetail.anchorContent.loading` | transitional caption while the fetch is in flight |
| `governance.drepDetail.anchorContent.unavailable` | the graceful anchor-unavailable caption |
| `governance.drepDetail.anchorContent.caption` | `!!!This name is the DRep's own claim, hash-matched to the anchor recorded on-chain. Daedalus does not verify identity.` |

The three source-label strings are the design's §2 copy verbatim
(`shared-design-tokens.md:51-53`), inventoried at `shared-design-tokens.md:205-207`.
**None of the eleven exists today** (verified absent from both catalogs).

`anchorContent.caption` is the one key beyond the S-8 seam-contract inventory. It
is additive, not a substitution, and it is what discharges the impersonation risk
at `plan.md:335` and `research/external-research.md:71` in the UI rather than only
in prose. Its ja-JP copy is
`!!!この名前はDRep自身による申告であり、オンチェーンに記録されたアンカーとハッシュが一致しています。Daedalusは本人確認を行いません。`
All eleven ja-JP strings are tabled verbatim beside their en-US pairs in the
implementation guide's task-151 Step 13.

**task-172 mints two and rewords two:**

| key | disposition | en-US source copy |
|---|---|---|
| `governance.drepDirectory.category.highValue` | **new** (`shared-design-tokens.md:190`) | `!!!High value` |
| `governance.drepDirectory.category.highValue.tooltip` | **new** (`:194`) | `!!!Inside the default Recommended view, with verified metadata and voting power above the cohort median.` |
| `governance.drepDirectory.category.primary.tooltip` | **reworded** — ships as "!!!Inside the default Recommended view with verified metadata." (`DRepCategoryBadge.tsx:16`), which is the AC-4 defect | `!!!Has verified off-chain metadata.` (`shared-design-tokens.md:33`) |
| `governance.drepDirectory.category.threshold.tooltip` | **reworded** — ships as "!!!Inside the default Recommended view but approaching expiry — review before delegating." (`:27`) | `!!!Approaching expiry — review before delegating.` (`:34`) |

`governance.drepDirectory.category.nonMetadata.tooltip` already matches the design
and is **not** touched.

**Arithmetic, in build order.** Baseline `84 / 84` at `bf112d9f8`. task-151's
eleven take both catalogs to **95**; task-172's two take them to **97**. Thirteen
new keys plus two rewordings, fifteen strings under the `!!!` rule. The gate in
the guide's task-172 Verify §8 therefore expects `97 97 true`, not `86`.

**Reused unchanged, no new key:** `governance.drepDirectory.source.onChain`
(`DRepSourceLabel.tsx:6`), `governance.drepDetail.sourceLabel.anchorReference`,
and `governance.drepDetail.votePositions.unavailable`
(`DRepDetailOnchainSection.tsx:142`).

**Guard widening (task-151, cv-2 D-14 precedent).**
`tests/jest/i18n/preliminaryCopyMarkers.spec.ts` is extended in place with a
`GOVERNANCE_NAMESPACE = 'governance.'` case asserting every such key is
`!!!`-marked in both catalogs, alongside the existing
`CURRENT_VOTE_NAMESPACE` case (`:12`) and the single whitelisted asymmetry at
`:8-10`.

## Docs / Designs / Research / Workflows / Skills Consulted

- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
  — the `anchor-1` phase `:1586-1693` (authoritative task contracts, quoted
  verbatim above); the `anchor-2` phase from `:1694` for the non-goals list;
  dependency-row statuses for task-104, task-116, task-118, task-119 (all
  `complete`) and task-109/110/111 (`verified`).
- `.agent/plans/governance/drep-discovery/prompt.md` — PRD minimum contents
  `:68-73`; guide bar `:75-89`; locked invariants `:98-138`; slice order
  `:146-149`; non-autonomous set `:191-195`; build loop and commit rule
  `:217-220`; status rule `:225-233`; convergence rule `:237-242`; stop
  conditions `:253-254`; Definition of Done `:261-270`; live-repo-wins rule
  `:39-41`; file-naming rule `:61-62`.
- `.agent/plans/governance/drep-discovery/README.md` — status vocabulary `:12`;
  slice docs and the append-only rule `:14`; one commit per task `:15`; the `!!!`
  rule `:18`; anchor-1 scope `:39`; High-value activation `:43`; no e2e in v1
  `:45`; test vectors `:82-85`.
- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md` —
  Anchor Metadata Pipeline `:244-252`; Key Decisions `:146-148`, `:158`, `:160`,
  `:165`, `:166`; no-new-package `:182`; IPC contract note `:219`; sequencing
  `:293`, `:299`, `:319-320`; risk table `:334`, `:335`, `:344`, `:346`; CIP-119
  canonical vector `:382`.
- `designs/shared-design-tokens.md` — §1a category table `:24-41` (High value
  `:32`, Primary `:33`, Threshold `:34`, Non-metadata `:35`, binding priority
  `:39`, binding out-of-cohort clause `:41`); §2 source labels `:43-57`
  (`:51-53` are anchor-1's three new variants); §9 key inventory `:190-195`,
  `:204-208`.
- `designs/drep-discovery-design.md` — the DRep-detail wireframe `:88-93` and its
  prose correction `:106`; the anchor render treatment `:208-226` (the
  `DRepDetailAnchorContent` child and the `DRepCard` exclusion at `:216`); the
  `image`/`imageObject` deferral `:218`; the no-prefetch rule `:247`.
- `research/external-research.md` — anchor structure `:50`; the CIP-119 field
  treatment table `:57-65`; patterns to reject outright `:67-71`.
- `research/cv-2-findings.md` — F-2 the `givenName` orphan `:2168-2183`
  (**Open at close, owner = anchor-1 planning**); F-31 the two-anchor re-proof
  rule `:2069-2080`, the docblock contradiction `:2093-2102`, and F-9's carried
  fallback `:2104-2120`; F-15 `:2184-2191`; F-8 `:2192-2197`.
- `research/slice-5-findings.md` — F-3 `:35-52`, the interim `anchor != null`
  proxy and its explicit statement that the current classifier signature "does not
  survive that upgrade".
- `research/slice-1-final-pass-findings.md` — `_shouldRetryWithConway` string
  fragility `:80-86`.
- `research/slice-6-findings.md` — F-6 `:90`, the two unowned designed badges.
- `research/drep-state-preprod-epoch295-sample.json:2852-2856` — the committed
  real on-chain `(url, hash)` pair used for cache-key and mismatch tests.
- `task-plans/cv-2-PRD.md` — section skeleton, depth and OWED convention
  precedent; `task-plans/cv-2-implementation-guide.md:211-237` — comment, commit
  and tracker conventions; `task-plans/cv-2-code-review.md` — the append-only
  review-log shape this slice reuses.
- Live code verified during planning — every anchor cited in this PRD was opened
  in the worktree at `bf112d9f8`.
- **Skills flagged for implementation, not invoked at planning:**
  `storybook-creation` (task-172 / D-8), `i18n-messaging` (task-151, task-172),
  `evidence-rules` (binding on every doc in this slice), `git-commit-formatter`
  (one subject-only commit per task). `e2e-test-creation` is **not applicable**
  (`README.md:45`). `cardano-cli-*` skills are not applicable — anchor-1 issues no
  CLI query.

## Locked Invariants Touched (inlined)

- **(1) Local-first** (`prompt.md:98-100`) — "Discovery data comes only from the
  local node via the main-process `GovernanceQueryService`. No hosted explorers,
  indexers, GovTool, Koios, Blockfrost, or public governance APIs." anchor-1 adds
  the feature's **only** outbound call, and it is to the DRep's own registered
  anchor URL — never an aggregator. Directory data still comes exclusively from
  the local node.
- **(2) Sanitization floor** (`prompt.md:101-105`) — "No DRep id, no `abstain` /
  `no_confidence` literal, no CIP-129/CIP-105 bech32 string in any logger,
  analytics, or electron-store payload — re-asserted via the task-111 spy suite in
  every slice. The task-168 DRep-state snapshot is the one documented exception."
  anchor-1 **widens** the floor to anchor URLs, hosts, resolved IPs and verified
  names (D-6), adds the first main-process assertion to the suite (task-149
  AC-9), and re-proves it across both anchors.
- **(3) Anchor transport-security floor** (`prompt.md:106-111`) — "The full
  anchor-1 guard set (TLS on, redirects off, ≤10s timeouts, ~1 MB cap, JSON
  content-type allow-list, SSRF + DNS-rebinding mitigation, Blake2b-256
  hash-verify before parse/cache/render, immutable hash-keyed cache) lands
  complete in `anchor-1` and is never thinned. No anchor-derived content renders
  without verification + a verified off-chain source label. Anchor URLs open only
  through the HTTPS-only-hardened `open-external-url` path (task-152)." This is
  the slice's charter; every clause maps to an acceptance criterion.
- **(5) Lovelace losslessness** (`prompt.md:115-117`) — "`json-bigint` lossless
  parse → decimal-string IPC → renderer `BigNumber` rehydration. Never route raw
  `JSONbig` objects across IPC or into observables." Touched by D-10's median.
- **(7) Default cohort is binding** (`prompt.md:120-124`) — "Exclude top 35 by
  voting power; up to the next 200 eligible (active, remaining `drepActivity` > 6
  epochs, completed metadata when available), randomized… The default cohort IS
  the 'Recommended' sort." task-172 **reads** membership from
  `GovernanceStore.defaultCohort` and never re-derives the rule (AC-1). The
  "completed metadata when available" clause is the signal task-151 AC-3 exposes;
  applying it to cohort *eligibility* is task-153's (anchor-2), not anchor-1's.
- **(8) Badges are informational only** (`prompt.md:125-127`) — "The category
  badge (slice-5: Primary / Threshold / Non-metadata; High value only after
  anchor-1) never reorders, filters, or overrides the cohort." Holds structurally
  at HEAD (`GovernanceStore.ts:174-188` filters on `status`/`drepActivity` only
  and the store imports nothing from `DRepCategoryBadge`) and must still hold
  after D-9 introduces a store → badge data path, which is one-way.
- **(10) Byte-equality** (`prompt.md:130-131`) — untouched but must not regress:
  anchor-1 adds a display-only name and never re-encodes an identifier. The
  confirmation-dialog identity change is anchor-2/task-154.
- **(11) Preliminary copy** (`prompt.md:132-133`) — "Every new en-US and ja-JP
  string keeps the leading `!!!` marker. Removing `!!!` is a release-end manual
  review, never a per-slice task." Binds all fifteen strings anchor-1 writes —
  thirteen new keys (eleven from task-151, two from task-172) and two rewordings.
- **(14) DRep status grounding** (`prompt.md:136-138`) — untouched:
  `DRepStatus` stays the closed two-value union at `governance.types.ts:35`, and
  D-9 removes `status` from the classifier signature rather than widening it.
- **Convergence rule** (`prompt.md:237-242`) — "Prefer the smallest truthful
  change… Reuse existing seams: the `RendererIpcChannel` pattern,
  `GovernanceQueryService` + `governanceChannel`, the `_shared` governance
  components (`DRepIdDisplay`, `DRepSourceLabel`, `DRepStatusBadge`)… and the
  existing bech32 helpers (no new bech32 dependency)." anchor-1 reuses
  `MainIpcChannel`/`RendererIpcChannel`, `DRepAnchorPresence`, `DRepSourceLabel`,
  and adds **zero** packages.
- **Comment and commit conventions** — comment only where the logic is not
  self-evident, then 1–3 plain sentence-case lines stating the invariant or the
  why; never task ids, review labels, ALL-CAPS or change history, in comments
  **or** test names. Exactly one commit per task, one Conventional Commits
  subject line, no body, no `Co-Authored-By`:
  `<type>(gov): task-NNN <short imperative summary>` (`prompt.md:217-220`,
  `README.md:15`).

## Dependencies

- **In-slice chain (built order):** 152 (no deps) → 149 → 150 → 151 → 172.
- **Cross-slice prerequisites, all landed:** task-104 governance IPC channels
  (`complete`), task-116 DRep detail view (`complete`), task-118 default cohort +
  randomization seed (`complete`), task-119 slice-5 `DRepCategoryBadge`
  (`complete`). The sanitization floor anchor-1 extends was earned by task-109 /
  task-110 / task-111 (all `verified`).
- **What anchor-1 hands forward:** task-153, task-154, task-155, task-156 and
  task-157 all depend on task-151; task-153 additionally depends on task-172.
  D-1's `verifiedName` field and S-5's `anchorStateByDRepId` are the data source
  anchor-2 needs; cv-2 F-2's second action (the `CurrentVoteSummary` verified-name
  render and its unverified→verified story) remains anchor-2 planning's to own.
- **Runtime and tooling:** node v24.16.0, jest 27.5.1, prettier 2.1.2, TypeScript
  4.9.5, Electron 41.3.0, React 16.14.0, MobX 5.15.7.
- **Environment.** `nix` is **absent** in this devcontainer, so `nix fmt`
  (`prompt.md:217`) cannot run and stays a pre-merge obligation the user owns;
  `node_modules/.bin/prettier --write <explicit paths>` is the substitute. `gh`
  and push credentials are absent, so all work stays local on the working branch.
  There is **no browser**, so task-172 AC-6's visual and ja-JP overflow pass
  cannot execute here. There is **no network**, so every transport guard is proven
  against mocked `https`/`dns` and no live anchor fetch happens in this container.

## Corpus-vs-Repo Corrections anchor-1 Inherits

Recorded here so no guide author re-derives them. **Live repo wins**
(`prompt.md:39-41`).

| # | corpus claim | live repo at `bf112d9f8` | disposition |
|---|---|---|---|
| C-1 | task-151 AC-5 requires removing/annotating a `Registered: epoch 502` row at `drep-discovery-design.md:92` | **Already done.** `:92` is `│ │ Current votes: 2 Yes · 1 No · 0 Abstain (this epoch)     │  │`; the phrase survives under `designs/` only inside `:106`'s prose correction, never as a wireframe row | **AC-5 discharged before start** (D-7). Record with evidence; schedule no work; do not edit the wireframe again |
| C-2 | task-151 AC-6 anchors the retained `Current votes` row at `drep-discovery-design.md:93` | `:93` is now the closing box border; the `Current votes` row is at `:92` and its retention is stated at `:106` ("the row itself is not deferred and must stay in this box") | **AC-6 discharged before start**; the AC's `:93` anchor has drifted by one line (D-7) |
| C-3 | task-151 AC-7 requires the wireframe's On-chain box to list exactly what `DRepDetailOnchainSection` renders | `:106` opens with that exact sentence; live code renders exactly four `fieldRow`s — Status `:94`, Expires in `:102`, Voting power `:114`, Current votes `:137` (value `governance.drepDetail.votePositions.unavailable`, `:142`) | **AC-7 discharged before start** (D-7) |
| C-4 | the ~1 MB anchor cap versus an inline base64 `imageObject` is an open tradeoff, pre-named at `prompt.md:253-254` as a possible stop condition | `plan.md:346` **resolves it**: "Resolved to the second branch: anchor-2 (task-157) defers the CIP-119 `image` field… The ~1 MB cap is unchanged and is not raised for the image case in this release", recorded at `drep-discovery-design.md:218` | **The stop condition does not fire.** anchor-1 escalates nothing on this and must not re-open it |
| C-5 | `plan.md:320` instructs "Use the anchor-display feature flag for staged verification control" | No flag exists — `grep -rn "featureFlag\|FEATURE_FLAG\|isFeatureEnabled" source/` returns zero hits; the only precedent is `source/main/environment.ts:132-133`'s env override | **Not built** (D-4). anchor-1 adds a one-line correction to `plan.md:320` so it stops reading as a build instruction |
| C-6 | `shared-design-tokens.md:204` inventories `governance.drepDetail.sourceLabel.onchain` | The shipped key is `governance.drepDirectory.source.onChain` (`DRepSourceLabel.tsx:6`) | Prefer the repo; **do not mint a duplicate**. Only `:205-207` correspond to keys anchor-1 actually creates |
| C-7 | task-149 AC-9 names two whole-error sinks: `GovernanceQueryService.ts:523-526` and `governanceChannel.ts:64` / `:77` | All three anchors resolve exactly — **and a fourth sink the AC does not name exists** at `governanceChannel.ts:58-60` (`{ error: snapshotError }`) | Record it; do **not** retro-fix any of them (D-6d). Carried to OWED for a later hardening row |
| C-8 | cv-2 F-15 cites the committed anchor pair at `drep-state-preprod-epoch295-sample.json:2852-2855` | The `anchor` key opens at `:2852` and the object spans `:2852-2856`; the content is correct | One-line-off citation, record-only. Cite `:2852-2856` |
| C-9 | design §1a's Primary tooltip is "Has verified off-chain metadata." (`shared-design-tokens.md:33`) and Threshold's is "Approaching expiry — review before delegating." (`:34`) | The **shipped** strings at `DRepCategoryBadge.tsx:16` and `:27` both assert "Inside the default Recommended view" | This *is* the task-172 AC-4 defect, not a doc error. The design copy is the target; both keys are reworded |
| C-10 | slice-5 F-3 describes `getDRepCategory`'s entry snapshot as extensible | F-3 itself says the signature "does not survive that upgrade"; live `DRepCategorySource` (`:45-48`) still includes `status`, which `:60-69` never reads | D-9 changes the signature and drops `status` rather than extending the rules within it |
| C-11 | `plan.md:219` anticipates governance IPC contracts for "anchor metadata payloads when available" without specifying the shape | No channel, type or store field for anchor content exists at HEAD | D-2 defines the shape: a per-DRep on-demand channel keyed by `(url, hash)`, never an extension of `DRepListQueryPayload` |
| C-12 | the anchor-1 decisions note cites `network.ts:33` as the mainnet-default return | The `return MAINNET_EXPLORER_URL;` default is at `network.ts:34` (inside `getNetworkExplorerUri`, `:20-35`); `getNetworkExplorerUrl` is `:36-43`, its closing `};` on `:43` | One-line-off citation, record-only. The substance of D-5d is unchanged |
| C-13 | design §2 lists five source labels including *Unverified anchor* (`shared-design-tokens.md:52`) | anchor-1's pipeline verifies in main before responding, so the renderer never receives fetched-but-unverified content — the variant has **no production emitter** | Mint the variant and its copy anyway (§2 is the design contract and Storybook renders it), and say so explicitly so a reviewer does not hunt for the missing path (S-8) |
| C-14 | the inherited planning notes state `rg filterLogData source/main` returns "zero hits" and `grep -rn "Registered: epoch" designs/` "returns nothing" | Each returns **one** hit, and both are benign: `source/main/utils/setupLogging.ts:180` is a *comment* documenting the snapshot's deliberate bypass (no call site exists in `source/main/`), and `drep-discovery-design.md:106` is the *prose correction* stating the row is deliberately absent (no wireframe row exists) | Substance unchanged; the claims are restated precisely in D-6c and D-7 so a guide author re-running either grep is not misled by a non-zero count |

## Risks and Open Questions

- **R-1 (high) — hash verification proves authorship, not identity.**
  `plan.md:335` is explicit: "Hash verification proves only that the registrant
  authored the blob — **which an impersonator satisfies exactly** — so it is not
  on its own a mitigation for a claimed identity." A DRep can register an anchor
  whose `givenName` is any string it likes, and anchor-1 will render it under a
  *Verified off-chain content* label. *Mitigation:* the label's copy and tooltip
  say what was actually proven — "Fetched from {host}, hash-matched the on-chain
  anchor hash" (`shared-design-tokens.md:51`) — and never "verified identity";
  `research/external-research.md:69-71` forbids trust scores and
  identity-verification-as-fact; the CIP-119 `references[@type=Identity]`
  "verify by visiting the URL" treatment is anchor-2/task-157's. **No copy in this
  slice may imply Daedalus endorses or has verified the DRep's identity.**
- **R-2 (high) — the guard set is only as good as its offline proof.** There is no
  network in this container, so every SSRF, redirect, TLS, timeout, size-cap and
  content-type assertion runs against mocked `https`/`dns`. A mock that is wrong
  in the same direction as the code proves nothing. *Mitigation:* follow the
  main-process spec precedent at
  `tests/jest/governance/GovernanceQueryService.spec.ts:1-32` (hoisted
  `jest.mock`, `EventEmitter`-based fake responses); assert the *arguments* passed
  to `https.request` and `dns.lookup`, not only the outcome; and record **a live
  anchor fetch as OWED** rather than implying the guards were exercised against a
  real server.
- **R-3 (medium) — task-152 is an app-wide behaviour change wearing a governance
  label.** `AppStore.openExternalLink` (`:80-82`) is the single external-link
  entry point for all of Daedalus. *Mitigation:* D-5d fixes the one real non-https
  producer (`network.ts:36-43`) in the same commit and pins it with a unit test
  over MAINNET, STAGING and an unlisted network. **Residual:** whether
  `explorer.staging.cardano.org` actually serves https cannot be verified offline
  — OWED.
- **R-4 (medium) — the cache is a new filesystem writer in main.** A malformed
  hash reaching `path.join` would be a path-traversal primitive. *Mitigation:*
  D-3's `/^[0-9a-f]{64}$/` guard runs **before** any path construction in both
  `readVerifiedAnchorBytes` and `writeVerifiedAnchorBytes` (S-3) and needs its own
  named test; the FIFO bound caps growth at 500 entries / 32 MB; `flag: 'wx'`
  makes entries immutable and `EEXIST` a success.
- **R-5 (medium) — task-172 touches two call sites with committed snapshots and a
  props chain through four files.** *Mitigation:* AC-6 makes the snapshot refresh
  part of the task's scope, not a review surprise; D-9 fixes the prop name
  (`cohort`) at every site and points at the shipped `top35DRepIds` thread
  (`DRepDirectory.tsx:83`) as the exact precedent; `tsc --noEmit` finds every
  literal that must gain `verifiedName` (D-1).
- **R-6 (medium) — the High-value/Threshold tie-break suppresses an expiry
  warning.** Under D-9 a cohort member with verified metadata, above-median power
  and 7–12 remaining epochs shows *High value*, not *Threshold* — so the "review
  before delegating" hint disappears for exactly the high-power DReps where it
  matters most. `shared-design-tokens.md:39` is labelled binding and governs, so
  the code follows it. *Recorded as a note to the design owner, not a code
  deviation:* if the design intends the expiry hint to survive, §1a needs an
  explicit High-value/Threshold sentence and a follow-up task. anchor-1 implements
  the binding rule as written.
- **R-7 (medium) — the sanitization widening has a blast radius.** `raw` is a
  generic key name and a `filterLogData` hit deletes the whole subtree
  (`source/common/utils/logging.ts:59-61`), so unrelated `{ raw: … }` diagnostics
  elsewhere in the renderer lose detail. *Mitigation:* accepted and stated
  (D-6a) — verbosity loss, not a break, and cv-2 F-9 named it. The negative test
  case in D-6b documents the complementary limit: a sensitive *value* under a
  non-sensitive *key* is not removed, because the match is exact string equality.
- **R-8 (medium) — main-process logging has no automatic net.**
  `source/main/utils/logging.ts:26-40` forwards `toJS(data)` untouched; the
  discipline in D-6c is hand-enforced. *Mitigation:* task-149 AC-9's spy case is
  the executable guard, and it is the first of its kind in this repo — every later
  main-process governance row extends it rather than reinventing it.
- **R-9 (low) — the `unverified-anchor` label ships with no emitter.** A reviewer
  may read it as dead code. *Mitigation:* S-8 states the reason inline (design §2
  is the contract; Storybook renders it) so the answer is in the doc, not in a
  reviewer's head.
- **R-10 (low) — `_shouldRetryWithConway` string fragility.**
  `research/slice-1-final-pass-findings.md:80-82` warns that any
  `GovernanceQueryError(QueryFailed)` message containing "latest" triggers a
  spurious conway retry. *Mitigation:* D-12 makes this structural — the anchor
  path never constructs a `GovernanceQueryError` and never routes through
  `GovernanceQueryService`, and transports enum values only.
- **Resolved, not open:** O-1 … O-13 are closed by D-1 … D-12. No anchor-1 task
  requires an `interactive_decision`, `interactive_validation`, or
  `manual_execution` classification, and no question in this slice is
  user-blocking.

### OWED at slice close — nothing here may be reported green

1. **`nix fmt` before merge.** `nix` is absent in this devcontainer, so
   `nix fmt` (`prompt.md:217`) cannot run. The substitute is
   `node_modules/.bin/prettier --write <explicit paths>`; **never** `yarn prettier`
   (its script carries a repo-wide `"**/*.*"` glob and rewrites ~238 files) and
   **never** pass `source/renderer/app/containers/voting/Governance.tsx` to
   prettier (2.1.2 cannot parse its line-4 inline type import and exits 2, failing
   the whole invocation). Running `nix fmt` before merge is an outstanding
   **user-owned** obligation and must be reported as an environment deviation, not
   hidden.
2. **The real SIPO CIP-119 vector** — task-151 AC-4's content half. No network,
   and no CIP-119 body is committed anywhere in the repo (D-11). Discharged in
   mechanism against the synthetic fixture and the real preprod on-chain pair;
   the real SIPO bytes and the assertion that their digest equals the on-chain
   `dataHash` stay owed.
3. **A live anchor fetch** against any real anchor URL. Every transport guard is
   proven against mocked `https`/`dns` only.
4. **The Storybook visual + ja-JP overflow pass** for all four categories at both
   call sites — task-172 AC-6. No browser here; `storybook/` is outside jest roots
   (`jest.config.js:129`); `yarn storybook:build` is red at HEAD for unrelated
   reasons, so `yarn storybook` (dev server) is the real floor.
5. **Residual, not anchor-1's diff:** three main-process whole-error sinks stay
   unhardened — `GovernanceQueryService.ts:523-526`,
   `governanceChannel.ts:58-60`, `:64` and `:77` (D-6d).
6. **Residual:** whether `explorer.staging.cardano.org` serves https cannot be
   verified offline (D-5d, R-3).
7. **Residual, carried forward:** cv-2 F-15's provenance half
   (`cv-2-findings.md:2184-2191`); slice-6 F-6's unowned "Expiring soon" and
   "Excluded from default cohort" badges (`research/slice-6-findings.md:90`); and
   cv-2 F-2's second action — the `CurrentVoteSummary` verified-name render and
   its unverified→verified story — which anchor-2 planning owns now that D-1
   supplies the data source.

## Definition of Done

**Per task** (`prompt.md:263-265`): acceptance criteria met · verification
executed and reported · code review clean · tasks JSON synchronized (`status`,
`statusReason`, `evidence` as an array of repo-relative paths with source files
first, `updatedAt` as `YYYY-MM-DD`) · exactly one commit, subject-only
Conventional Commits, `<type>(gov): task-NNN <short imperative summary>`.

**Per slice:**

- All **29** verbatim acceptance criteria above pass, except the scoped
  dispositions below. Each carries a truthful `statusReason`; none may be
  reported green.

  | criterion | disposition | reason |
  |---|---|---|
  | task-151 AC-4 | satisfied in part | mechanism half met against the committed fixture and the real preprod on-chain pair; the real SIPO body bytes are **OWED** (D-11) |
  | task-151 AC-5 | discharged before start | `drep-discovery-design.md:92`, `:106`; the AC's `:92` anchor drifted one line (D-7) |
  | task-151 AC-6 | discharged before start | `drep-discovery-design.md:92`, `:106`; the AC's `:93` anchor is now the closing box border (D-7) |
  | task-151 AC-7 | discharged before start | `drep-discovery-design.md:106` plus `DRepDetailOnchainSection.tsx:94`, `:102`, `:114`, `:137` (D-7) |
  | task-172 AC-6 | satisfied in part | registration and snapshot halves are runnable gates; "renders … in en-US and ja-JP without overflow" needs a browser — **OWED** (D-8) |
  | task-149 AC-1…AC-7 | satisfied against mocks | proven with mocked `https`/`dns`; a live fetch is **OWED** (R-2) |

- **Gates, run from the worktree root:**
  - `node_modules/.bin/tsc --noEmit` — exit 0 (baseline: exit 0 at HEAD,
    TypeScript 4.9.5). Use `yarn compile` instead whenever a new `.scss` class
    lands, because its `precompile` hook regenerates the gitignored
    `*.scss.d.ts` files (baseline: exit 0, ~22 s, working tree clean afterwards).
  - `yarn lint` — exit 0 (baseline: exit 0 with ~5591 warnings). The warning count
    moves **iff** a changed spec lives under `source/` or `storybook/`; task-152's
    new `source/main/ipc/open-external-url.spec.ts` does exactly that (cv-2 F-29)
    — expected, not a regression.
  - `node_modules/.bin/jest --testPathPattern='(governance|Governance|DRep|anchor|Anchor|open-external)' --no-coverage --runInBand`
    — green. `--no-coverage` is load-bearing (`jest.config.js:20` sets
    `collectCoverage: true`). `tests/jest/governance/GovernanceCliArgvSmoke.spec.ts`
    self-skips because `cardano-cli` is off PATH — environment-gated, not broken.
  - **The sanitization floor is re-proved across BOTH anchors and cited together**
    (cv-2 F-31, binding — `cv-2-findings.md:2069-2080`):
    ```bash
    node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" --no-coverage --runInBand
    node_modules/.bin/jest --testPathPattern="containers/voting/VotingGovernancePage.spec" --no-coverage --runInBand
    ```
    Baseline for the first at HEAD: **1 suite / 26 tests, green (measured)**. A
    sanitization claim citing only the security suite is incomplete and must be
    rejected at review.
  - `yarn i18n:manage` runs clean and idempotent after the copy rows — the second
    run adds zero keys and deletes zero keys. It rewrites both catalogs **and**
    `translations/messages.json`; the owning task keeps its own diff and every
    **other** file that was clean at HEAD is restored with
    `git restore` / `git checkout -- <paths>`.
  - `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` green with the new
    `governance.` namespace case; both catalogs key-identical and every
    `governance.*` value `!!!`-marked.
- **Formatting rule (binding).** `node_modules/.bin/prettier --write` on
  **explicitly listed files this slice creates only** — the new main-process
  services, the anchor cache, the anchor IPC channel and specs, the new fixtures,
  `DRepDetailAnchorContent.tsx`, and `open-external-url.spec.ts`. Never
  `yarn prettier`. Never a pre-existing file (~238 are prettier-2.1.2-dirty at
  HEAD). Never a tool-managed JSON (the tasks tracker, the locale catalogs,
  `translations/messages.json`). **Never include
  `source/renderer/app/containers/voting/Governance.tsx` in any prettier
  invocation.** `nix fmt` is unavailable here and stays a pre-merge obligation the
  user runs.
- **Never read `yarn check:all` or `yarn storybook:build` as an anchor-1
  regression** — both are red at HEAD for unrelated reasons. `yarn storybook`
  (dev server) is the real Storybook floor.
- **Never `git stash`** — the stash stack is shared across worktrees and
  concurrent sessions. Discard with `git restore` / `git checkout -- <paths>`.
- `grep -rn "rejectUnauthorized" source/main/governance/` returns nothing
  (task-149 AC-1).
- `grep -rn "anchor != null\|anchor !== null" source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx`
  returns nothing (task-172 AC-2).
- `grep -n "DRepCategoryBadge\|getDRepCategory" source/renderer/app/stores/GovernanceStore.ts`
  returns nothing (invariant #8).
- `grep -rn 'task-1[0-9][0-9]' source/ tests/ storybook/` returns nothing (no
  process artifacts in code comments or test names).
- D-4's one-line correction exists at `plan.md:320`.
- `research/anchor-1-findings.md` written, **or** "no new research" recorded in
  the Final Outcome; `anchor-1-code-review.md` preserved with `Planner:`
  open/close entries, a `Critiquer:` entry and per-task `Code Review:` entries;
  this PRD's Final Outcome filled and its `Planning status:` advanced from
  `draft`.
- The phase object still carries **no `auditSummary`** — only `slice-1` has one;
  anchor-1 does not invent one.

## Final Outcome

**Planning-time stub.** _Filled at slice close._

### What shipped, task by task

_Filled at slice close._

### Gates at close — measured, not asserted

_Filled at slice close._

### Final status of every task

_Filled at slice close._

### Definition of Done exception table, as adjudicated at close

_Filled at slice close._

### Deviations from this PRD and its guide

_Filled at slice close._

### OWED at close — nothing here is faked green

_Filled at slice close._

### Residual gaps a later phase inherits

_Filled at slice close._

## References

- Tasks tracker: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json:1586-1693`
- Orchestration prompt: `.agent/plans/governance/drep-discovery/prompt.md`
- Working conventions: `.agent/plans/governance/drep-discovery/README.md`
- Parent plan: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md`
- Designs: `designs/shared-design-tokens.md`, `designs/drep-discovery-design.md`
- Research: `research/external-research.md`, `research/cv-2-findings.md`, `research/slice-5-findings.md`, `research/slice-6-findings.md`, `research/slice-1-final-pass-findings.md`, `research/drep-state-preprod-epoch295-sample.json`
- Preceding slice: `task-plans/cv-2-PRD.md`, `task-plans/cv-2-implementation-guide.md`, `task-plans/cv-2-code-review.md`
- Companion docs for this slice: `task-plans/anchor-1-implementation-guide.md`, `task-plans/anchor-1-code-review.md`
