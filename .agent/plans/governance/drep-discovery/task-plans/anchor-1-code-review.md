# anchor-1 Code Review Log

> Append-only transcript: `Planner:` entries (planning open/close), one `Critiquer:` entry
> (required review pass over the PRD + guide), and per-task `Code Review:` entries.
> Companion docs: [anchor-1-PRD.md](./anchor-1-PRD.md) ·
> [anchor-1-implementation-guide.md](./anchor-1-implementation-guide.md)

---

## Planner: 2026-07-29 — anchor-1 planning pass

**Scope.** Five tasks — task-152, task-149, task-150, task-151, task-172 — all `pending` at
`bf112d9f8`, covering the hardened anchor path end to end: an HTTPS-only `openExternal` gate,
a bounded raw-bytes fetch service, Blake2b-256 verify + immutable on-disk cache + parse behind a
never-rejecting IPC channel, the verified `givenName` render with source labels, and a
cohort-grounded `DRepCategoryBadge` with the High value category
(`anchor-1-implementation-guide.md:28-32`).

**Build order `152 → 149 → 150 → 151 → 172` is dependency-forced by the tracker, not preference**
(`anchor-1-implementation-guide.md:34-56`): task-152 is the only `"dependencies": []` row
(`governance-drep-discovery-plan-tasks.json:1662`) and also gates the render surface via locked
invariant #3; then `task-149 → task-150` (`:1622-1623`), `task-150 + task-116 → task-151`
(`:1640-1642`), `task-151 + task-118 + task-119 → task-172` (`:1677-1680`), with task-104,
task-116, task-118 and task-119 all `complete`.

**Interaction mode.** All five classified `autonomous`
(`anchor-1-PRD.md:182-186`). None falls in the locked non-autonomous set — task-125, the task-166
remainder, task-158, and the release-end `!!!` copy review — and no blocking decision survived
planning: D-1 … D-12 close O-1 … O-13 (`anchor-1-PRD.md:290`).

**Decisions.**

- **D-1** — both directory-entry interfaces gain `verifiedName: string | null`; authoritative
  verification state lives in a separate store map (`anchor-1-PRD.md:294`).
- **D-2** — verified content crosses a new per-DRep on-demand `GOVERNANCE_DREP_ANCHOR_CHANNEL`
  whose handler never rejects (`anchor-1-PRD.md:330`).
- **D-3** — the immutable cache is an on-disk directory under `stateDirectoryPath`, one file per
  verified anchor hash, never electron-store (`anchor-1-PRD.md:390`).
- **D-4** — the "anchor-display feature flag" is NOT BUILT and the plan reference is corrected to
  say so (`anchor-1-PRD.md:428`).
- **D-5** — task-152 keeps the fire-and-forget contract, logs `{ scheme }` only, gates the link in
  the renderer, and fixes the one real non-https caller found by the app-wide audit
  (`anchor-1-PRD.md:450`).
- **D-6** — task-149 discharges F-9's sanitization fallback in full: twelve `sensitiveData`
  additions, domain-shaped floor-suite cases, an explicit main-process rule, and the docblock
  renarrow (`anchor-1-PRD.md:499`).
- **D-7** — task-151 AC-5, AC-6, AC-7 are discharged before start; anchor-1 schedules no work for
  them (`anchor-1-PRD.md:566`).
- **D-8** — anchor-1 absorbs the Storybook registration gap for all three unregistered governance
  story files (`anchor-1-PRD.md:585`).
- **D-9** — cohort membership, verified completeness and the cohort median reach the classifier as
  one store-owned `DRepCohortContext` prop, threaded container → component
  (`anchor-1-PRD.md:619`).
- **D-10** — the cohort median is a BigNumber over the cohort only, and "above" means strictly
  greater (`anchor-1-PRD.md:701`).
- **D-11** — a clearly-labelled synthetic CIP-119 fixture is committed with its digest generated
  from the committed bytes; the real SIPO vector is OWED (`anchor-1-PRD.md:724`).
- **D-12** — every anchor error is an enum value, so the word "latest" cannot reach
  `_shouldRetryWithConway` (`anchor-1-PRD.md:776`).

**Seams** (signatures in `anchor-1-implementation-guide.md:226-368`).

- **S-1** `fetchAnchorBytes(url: string): Promise<AnchorFetchResult>` plus `AnchorTransport` /
  `httpsAnchorTransport` in `source/main/governance/AnchorFetchService.ts` — never throws, never
  parses, never caches (task-149 → task-150).
- **S-2** `resolveVerifiedAnchor(anchor: DRepAnchorPresence): Promise<DRepAnchorResult>` in
  `source/main/governance/AnchorVerificationService.ts` — cache read → fetch → verify → cache write
  → parse, in that order, never throws (task-150 → task-151).
- **S-3** `readVerifiedAnchorBytes(hash): Buffer | null` / `writeVerifiedAnchorBytes(hash, bytes): void`
  in `source/main/governance/anchorCache.ts`, both rejecting any hash failing `/^[0-9a-f]{64}$/`
  before touching the filesystem.
- **S-4** `GOVERNANCE_DREP_ANCHOR_CHANNEL` with request `= DRepAnchorPresence` (no `drepId`) and
  response `= DRepAnchorResult`, plus `AnchorFetchErrorType`, `VerifiedDRepAnchorContent` and
  `DRepAnchorResult` in `source/common/types/governance.types.ts`.
- **S-5** `anchorStateByDRepId: Map<string, AnchorEnrichEntry>`, `fetchAnchorContent(drepId, anchor)`
  and `_applyVerifiedNames(entries)` in `GovernanceStore` — absent key means idle; a changed hash
  re-triggers.
- **S-6** a second `reaction` in `DRepDetailPage.tsx` beside the `isNodeInSync` one at `:43-50`,
  `fireImmediately: true`, covering deep-link and list-first mount orders.
- **S-7** `DRepCohortContext { memberIds, verifiedMetadataIds, medianVotingPower }` behind
  `@computed get cohortContext()` — `verifiedMetadataIds` derives from verified anchor state, never
  from `anchor != null`; the prop is named `cohort` at every call site.
- **S-8** `getDRepCategory(entry, cohort): DRepCategory` and the five-value
  `DRepSourceLabelVariant`, with `host` passed only for `'verified-off-chain'`.

**Already discharged before start.** task-151 AC-5, AC-6 and AC-7 (D-7,
`anchor-1-PRD.md:566-580`): the `Registered: epoch 502` row is gone from every wireframe under
`designs/`, the `Current votes` row is retained at `drep-discovery-design.md:92`, and
`DRepDetailOnchainSection.tsx:94,102,114,137` renders exactly the four fields the On-chain box
lists. No step is scheduled for them.

**OWED — this environment cannot discharge these and they may never be reported green**
(`anchor-1-PRD.md:1400-1421`): (1) `nix fmt` before merge — `nix` is absent, the substitute is
`node_modules/.bin/prettier --write <explicit paths>`, and the real run stays a user-owned
obligation; (2) the real SIPO CIP-119 vector, task-151 AC-4's content half (D-11); (3) a live
anchor fetch — every transport guard is proven against mocked `https`/`dns` only; (4) the Storybook
visual and ja-JP overflow pass, task-172 AC-6 — no browser, `storybook/` is outside jest roots
(`jest.config.js:129`).

**Docs produced.** `task-plans/anchor-1-PRD.md` (1560 lines) and
`task-plans/anchor-1-implementation-guide.md` (4785 lines, five `### task-NNN` sections at lines
370, 926, 2068, 2925, 3799), plus this review log.

---

## Critiquer: 2026-07-29 — required planning review over anchor-1 PRD + implementation guide

**Verdict at entry:** four blockers, nine minor items. The reviewer's own summary: "The docs are unusually well
grounded: all 29 acceptance criteria are quoted verbatim from the tracker (machine-checked), and every measured
baseline I re-ran in the worktree matched exactly — 136 tests across the 6 named suites (badge 11 / detail page 12 /
directory 47 / store 35 / i18n 4 / VotingGovernancePage 27), 313 tests / 17 suites / 9 snapshots on the governance
sweep, 158 / 11 on tests/jest, 26 on the security floor, 84/84 key-identical `!!!`-marked governance i18n keys,
101 / 5 / 8 on the component-governance sweep. Line anchors resolve … The `@types/node` 14.18.1
`LookupFunction`/`lookup` typings and the `import * as blake2b` precedent both check out, and zero npm dependencies
are added. Four blockers remain: an orphaned deliverable (the https-gated anchor link that task-152 and task-151 each
assign to the other), one incompletely-total transport guard (the ≤10 s budget starts after DNS), one provably-wrong
i18n gate number in task-172, and a same-guide contradiction that derives `verifiedMetadataIds` twice. All four are
small, surgical fixes; nothing here requires re-planning the slice."

### Blockers (ranked)

**B-1 — The renderer-side https link gate is owned by nobody.** *(PRD:182 task-152 scope cell, PRD:136-140,
PRD D-5c :463-466, PRD architecture diagram :1021-1022, PRD US-A1.4 :944-947 vs guide:383-386, guide:507-509,
guide Step 6 :814-824, guide task-151 judgment call 16 :3114, guide Step 9 :3578.)* Decision D-5c, which the PRD
declares binding, assigns to task-152's scope, diagrams as task-152's work, and uses to justify the whole build-order
hoist ("task-152 and task-151 edit the same component"), is implemented by no step in the guide. Guide task-152 lists
"every file under `source/renderer/app/components/governance/`" under "Files deliberately NOT touched", states
"Rendering the anchor URL as a link … belongs to the anchor-1 render task, not here", and adds Step 6 to *prove* no
governance file changed. Guide task-151 judgment call 16 sends it straight back: "the https link gate belongs to
task-152. task-152 … owns rendering the anchor URL as a link only when it parses as `https:`." task-151 Step 9 only
deletes the two-line inert-text comment at `DRepDetailAnchorSection.tsx:55-56`; it adds no anchor element. Net effect:
the anchor URL ships as inert text, PRD user story US-A1.4 describes a link that will not exist, and `plan.md:160`'s
"Show as an external link with `target="_blank" rel="noopener noreferrer"`" is neither built nor recorded as deferred.
*Why blocking:* a binding PRD decision with a named owner is executed by nobody, and the two guide sections point at
each other, so a small model working from either section alone will correctly conclude the work is not its own. It
also silently changes what anchor-1 ships versus what the PRD promises, and it makes locked invariant #3's "Anchor
URLs open only through the HTTPS-only-hardened open-external-url path" true only vacuously.
**Decision:** ACCEPTED — fixed by option (a), giving the gate to task-151.

**B-2 — The ≤10 s budget starts after DNS resolution.** *(guide:1417-1420, guide:1526, guide:1539, guide:1924,
AC-3 discharge row at guide:2030.)* The wall-clock budget is armed inside `requestAnchorBytes`, which `fetchOverHttps`
only calls after `await dns.promises.lookup(host, { all: true })`. `dns.promises.lookup` carries no timeout of its
own, so resolution time is entirely outside the budget. The guide records the consequence without addressing it, at
Step 11 case 28: "The microtask flush is required because `dns.promises.lookup` is awaited before the timer is armed."
AC-3 is "Per-request connect+total timeout is <= 10 seconds", and locked invariant #3 lists "≤10s timeouts" as a floor
clause that "lands complete in anchor-1 and is never thinned".
*Why blocking:* a hostile or merely slow resolver leaves the fetch bounded only by the OS resolver's default
behaviour, and the 38-case spec cannot detect it because no case drives a hanging lookup. This is exactly the "guard
specified weakly" case the floor forbids.
**Decision:** ACCEPTED — fixed as specified.

**B-3 — task-172's catalog-parity gate pins a provably wrong number.** *(guide:4397-4398, Verify §8 at
guide:4715-4720, AC-4 discharge at guide:4760 — contradicted by guide task-151 Step 13 :3645 and task-151 Verify §5
:3751.)* The gate expects `86 86 true`, derived from the HEAD baseline of 84 as if no earlier anchor-1 task had
landed. HEAD measured 84/84, key-identical, all `!!!`-marked — so 84 is right for HEAD. But the build order is
152 → 149 → 150 → 151 → 172, and task-151 Step 13 mints eleven keys (84 → 95). task-172 then mints two, so the true
value when this gate runs is 97. AC-4's discharge row cites this gate as its proof.
*Why blocking:* a small model sees `97 97 true`, reads the gate as failed, and either stops or "fixes" the catalogs.
Unlike task-172's other counts this one is not covered by the section preamble's "treat the per-suite deltas as the
contract" caveat, because it is an absolute script output, not a delta.
**Decision:** ACCEPTED — fixed, and resolved together with M-1.

**B-4 — `verifiedMetadataIds` is derived twice.** *(guide task-151 Step 5 :3220-3227 and judgment call 15 :3113,
versus guide task-172 Step 1b :4140-4151.)* task-151 adds a `@computed get verifiedMetadataIds()` and its judgment
call 15 states the handoff: "task-172 wires `DRepCohortContext.verifiedMetadataIds = this.verifiedMetadataIds`."
task-172 Step 1b does not wire it — it re-walks `anchorStateByDRepId` inline inside `cohortContext` with its own
`new Set<string>()` and `forEach`, producing a second derivation of the same signal in the same file and leaving
task-151's computed with no production reader.
*Why blocking:* the two guide sections contradict each other on a seam handoff, a small model executing task-172 in
isolation ships the duplicate, and it violates the convergence rule in the one place the slice is most sensitive to
duplicated derivations.
**Decision:** ACCEPTED — fixed as specified.

### Minor

- **M-1** — the PRD's i18n inventory is stale: PRD:1112-1129 lists ten task-151 keys; the guide mints eleven, adding
  `governance.drepDetail.anchorContent.caption`. PRD:1257 says "binds all twelve new/reworded keys" where the totals
  are 13 new plus 2 reworded. **Decision:** ACCEPTED — fixed.
- **M-2** — task-172 Verify §5 and §6 pin absolute post-task counts (`12 -> 13` / `1 -> 2` snapshots; `35 -> 41`) that
  are only true if task-151 never landed. **Decision:** ACCEPTED — restated as explicit deltas.
- **M-3** — nothing verifies that task-150 Step 6's registration happened, and the IPC handler has no test; D-2's
  "the handler never rejects" is untested. **Decision:** ACCEPTED — grep gate plus a two-case handler spec added.
- **M-4** — task-151 Step 13 names only `translations/messages.json` as the extra file `yarn i18n:manage` rewrites,
  so a small model would `git restore` `defaultMessages.json` and desync it. **Decision:** ACCEPTED — fixed.
- **M-5** — task-152 adds a main-process logger sink but its Verify runs neither sanitization anchor, against cv-2
  F-31's two-anchor rule. **Decision:** ACCEPTED — both commands appended.
- **M-6** — residual one-line anchor drift in six citations. **Decision:** PARTIALLY ACCEPTED — four fixed, two
  rejected with proof (see the fix-pass entry).
- **M-7** — `DRepDirectory.stories.tsx` renders `<DRepDirectory>` at two sites but Step 11d names neither.
  **Decision:** ACCEPTED — both line numbers named.
- **M-8** — repeated environment boilerplate buries the steps across eleven recurrences. **Decision:** ACCEPTED —
  collapsed to one line per recurrence.
- **M-9** — `enforceCacheBound` runs a full `readdirSync` plus up to 500 synchronous `statSync` calls on the
  main-process event loop on every cache write. **Decision:** ACCEPTED — guarded, with the derivation stated.

### Dropped findings

None. The reviewer raised no finding it withdrew before reporting.

## Critiquer: 2026-07-29 — fix-pass verification over B-1 … M-9

One fix pass, both documents. Every anchor below was re-opened in the worktree at `bf112d9f8`.

**B-1 — FIXED, option (a): the gate is task-151's.** Resolved toward the locked invariant rather than toward the
decisions contract's original owner, because the tracker's own text settles it: task-152 AC-3 reads "Anchor URL
rendering remains gated on this hardening landing"
(`governance-drep-discovery-plan-tasks.json:1667`), so a task-152 commit that rendered the link would fail its own
criterion. D-5c's *substance* is unchanged — the gate exists, it is renderer-side, it is independent of main's guard —
only the owning row moved, and that change is recorded in the PRD at D-5c itself. Deferring the link (option b) was
rejected: it would leave invariant #3's "anchor URLs open only through the HTTPS-only-hardened `open-external-url`
path" vacuously true, and the floor is "never thinned".
Changed in `anchor-1-PRD.md`: `:136-141` (build-order reason 2 rewritten — the hoist now rests on the gate having to
follow the hardening), `:183` (task-152 scope narrowed to main-process + `utils/network.ts`, with the
governance-components exclusion moved into its non-goals), `:186` (task-151 scope gains the gate, cross-referenced
S-9), `:264-268` (task-152's AC-3 applied reading rewritten as a negative criterion), `:473-481` (the D-5c ownership
correction recorded explicitly, inside D-5c itself), `:944-965` (new **S-9** seam contract with the exact prop shape),
`:1063` (architecture diagram retagged "D-5c, built by task-151"). US-A1.4 was left as written — the link it describes
now exists.
Changed in `anchor-1-implementation-guide.md`: `:31` (ToC subject), `:228` (`S-1 … S-9`), `:365-370` (S-9),
`:513-516` (task-152 bullet names task-151 Step 9), `:821-831` (Step 6 names task-151 Step 9), `:919` (AC-3 discharge
row), `:3206` (judgment call 16 inverted), `:3649-3710` (Step 9 rewritten with the exact JSX: module-scope
`isHttpsAnchorUrl` at `:3658-3665`, the `<a>` with `target="_blank" rel="noopener noreferrer"` at `:3672-3686`,
`event.preventDefault()` then `onOpenExternalLink(anchor.url)`, and the inert `<dd>` fallback), `:3716-3718` (Step 10
threads the prop), `:3750-3757` (Step 11 passes `stores.app.openExternalLink`, with the assignability note),
`:3827` and `:3835-3838` (Step 16 gains cases 7 and 8, one per branch; 12 → 20 tests), `:3861` (Verify count),
`:3936` (the D-5c discharge note plus the owed browser click-through). No SCSS changes, so no `*.scss.d.ts`
regeneration.

**B-2 — FIXED.** `requestAnchorBytes` now takes a fourth parameter `budgetMs` and arms both `totalTimer` and
`options.timeout` from it, returning `Timeout` immediately if the remaining budget is already spent
(guide:1396-1400 signature, `:1427-1437` budget arming, `:1452` `timeout: budgetMs`). `fetchOverHttps` takes
`const deadline = Date.now() + ANCHOR_TIMEOUT_MS` as its first statement (`:1544`) and races the resolver through a
new `lookupWithinBudget` helper (`:1525-1541`) against a `TIMEOUT_SENTINEL`, clearing the timer on settle
(`:1563-1567`), then passes `deadline - Date.now()` down (`:1580-1585`). Step 11 grew a tenth describe,
`'Anchor fetch service — DNS budget'`, holding case 39 `'aborts when DNS resolution never settles'`
(`mockLookup.mockReturnValue(new Promise(() => {}))`, fake timers, expect `Timeout` and `mockRequest` not called) —
appended rather than inserted so cases 29-38 and the AC-4/AC-5/AC-8 discharge rows that cite them keep their numbers.
Case 26 was retargeted from an equality assertion on `options.timeout` to `> 0 && <= ANCHOR_TIMEOUT_MS`, since the
value is now the remainder (`:1962-1965`); case 28's trailing note was corrected (`:1972-1973`). Header count
38 → 39 (guide:1913), Verify step 2 38 → 39 (`:2039`), Verify step 5's `tests/jest` totals 205/193 → 206/194
(`:2054`), and the AC-3 discharge row (`:2080`) now states the before-DNS ordering. New case 39 sits at `:1999-2005`.
`anchor-1-PRD.md:999-1002` restates the floor as one budget armed before resolution.
`.finally` is safe here: `tsconfig.json` targets `es2019` and `source/main/mithril/MithrilStartupGate.ts:501` already
uses it.

**B-3 — FIXED, and reconciled with M-1.** guide:4535-4538 now reads "the catalogs hold **95** `governance.*` keys per
locale when this task starts (84 at HEAD plus task-151's eleven) and **97** when it finishes"; Verify §8 expects
`"97 97 true"` then `"[]"` (guide:4865); the AC-4 discharge row (guide:4901) quotes the same number. PRD gained an
explicit arithmetic paragraph after the task-172 key table (`:1193-1197`: 84 → 95 → 97; thirteen new keys plus two
rewordings, fifteen strings under the `!!!` rule), and PRD:1312-1313's "binds all twelve new/reworded keys" was
corrected to fifteen.

**B-4 — FIXED.** guide:4281-4294: `cohortContext` now returns `verifiedMetadataIds: this.verifiedMetadataIds,`
(`:4286`) and the inline `new Set<string>()` / `forEach` block is deleted, with one sentence at `:4291-4294` stating
why a second derivation is forbidden. task-151 judgment call 15 stands unchanged, as instructed. The AC-2 discharge
row (guide:4899) was reworded
to cite the computed rather than a local rebuild. Step 10 case 6 still exercises the computed through `cohortContext`
and needed no change.

**M-1 — FIXED.** PRD gained the `governance.drepDetail.anchorContent.caption` row with its en-US copy, its ja-JP copy
quoted verbatim below the table, and a sentence recording that it is the one key beyond the S-8 seam inventory and
what it discharges (`plan.md:335`, `research/external-research.md:71`) — PRD:1153 (the "mints eleven" line), `:1167`
(the new caption row), `:1171` ("None of the eleven exists today"), `:1173-1179` (the ja-JP copy and the rationale).
"task-151 mints ten" → "eleven"; "None of the ten exists today" → "eleven". The full ja-JP table stays in the guide's
Step 13 (`:3760-3773`) rather than being duplicated.

**M-2 — FIXED.** guide:4692-4694 and `:4713` restate the detail-page figures as "+1 test, +1 snapshot (20 → 21,
1 → 2 in build order)" and name the 20-on-entry baseline; `:4722-4726` restates the store figures as "+6 tests
(43 → 49 in build order)". Verify §5 (`:4845-4847`) and §6 (`:4852-4854`) now say "the delta is the contract, not the
total".

**M-3 — FIXED.** guide:2699-2701 states plainly that skipping the registration is invisible to `tsc`, `yarn lint` and
every spec in the slice. A new **Step 6b** (guide:2703-2720) specifies `source/main/ipc/governanceAnchorChannel.spec.ts` — the
`MainIpcChannel` mock shape task-152's spec already establishes, plus a mocked `resolveVerifiedAnchor` — with two
cases: the result passes through unchanged, and a throwing service still **resolves**
`{ status: 'unavailable', reason: InvalidRequest }` with no `'boom'` in any logger payload. Verify gained
`grep -n "handleGovernanceAnchorRequests" source/main/ipc/index.ts` expecting two hits (guide:2942-2946), and the new
spec was added to the prettier path list (`:2969`).

**M-4 — FIXED.** guide:3787 now names all four written files, states that all four are committed together, cites
commit `927978951` as precedent, warns that restoring `defaultMessages.json` desyncs it from the catalogs, and scopes
`git restore` to hunks outside `governance.*` — matching task-172 Step 6 word for word in substance.

**M-5 — FIXED.** task-152's Verify gained both floor anchors as step 7 (guide:900-905 — security suite: 1 suite /
26 tests unchanged; `VotingGovernancePage.spec`: 1 suite / 27 tests unchanged), with the old step 7 renumbered to 8.

**M-6 — PARTIALLY ACCEPTED. Four fixed, two REJECTED with proof.**
Fixed: the `filterLogData` describe in `tests/jest/security/governance-sanitization.spec.ts` closes at `:225`, not
`:226` — line 225 is `});` and line 226 is blank (corrected at guide:1029 and `:1672`); the `sendEvent(…, 'abstain')`
assertion spans `:500-504`, not `:500-505` — line 504 is `);` and line 505 is
`expect(analytics.sendEvent.mock.calls[0]).toHaveLength(3);` (corrected at guide:1039, `:1666`, PRD:567);
`getNetworkExplorerUrl` spans `network.ts:36-43`, not `:36-42` — its closing `};` is on line 43 (corrected at PRD:25,
`:143`, `:484`, `:1373`, `:1404`); invariant #11 is `prompt.md:132-133`, not `:132-134` (corrected at guide:3186 and
`:4177`, matching the guide's own header correction at `:136`).
**REJECTED — the anchor `<dl>` in `DRepDetailAnchorSection.tsx` closes at `:76`, not `:77`.** Proof, from the file in
this worktree: `:76` is `        </dl>` and `:77` is `      ) : (`. The guide's `:49-76` for the `anchor ? (…)` true
branch is correct as written; only the ternary's own `:49` opener sits outside the `<dl>`. Step 9's wording was made
explicit about that split rather than renumbered.
**REJECTED — task-149's `acceptanceCriteria` array is `:1603-1613`, exactly as PRD:195 states.** Proof, from
`governance-drep-discovery-plan-tasks.json`: `:1592` is the object's opening `{`, `:1603` is
`"acceptanceCriteria": [`, `:1604` is the first criterion ("TLS default verification remains on…"), `:1612` is the
ninth, `:1613` is the closing `]`. The critique's `:1604` / `:1605-1613` is off by one in the opposite direction.

**M-7 — FIXED.** guide:4776-4779 names both `<DRepDirectory>` render sites in
`storybook/stories/governance/DRepDirectory.stories.tsx` — `:156` and `:435`, verified — and points at Step 5c's
two-site pattern as the model.

**M-8 — FIXED.** Eight recurrences collapsed to the single prettier command plus "`nix fmt` is unavailable here and
stays a user-owned pre-merge obligation": guide:835 (task-152 Step 7), `:2009` (task-149 Step 12), `:2964` and
`:3899` (the `# 7. Format` comment blocks in task-150's and task-151's Verify), `:4783` (task-172 Step 12), plus
three "Never run …" restatements deleted from the task-152, task-149 and task-172 Verify blocks and the duplicated
`nix fmt` paragraph deleted from task-151's Verify. The header's own tables at `:158-191` and `:210-212` are unchanged
and remain the single statement of the rule; exactly one OWED entry per task survives.

**M-9 — FIXED, with the arithmetic corrected.** The critique's suggested guard
(`if (fileNames.length <= ANCHOR_CACHE_MAX_ENTRIES) return;`) is unsound: at ~1 MB per entry the 32 MB byte bound can
bite at 33 entries, far below 500, so that guard would let the byte bound be violated. The guide instead derives
`ANCHOR_CACHE_SWEEP_FLOOR = Math.floor(ANCHOR_CACHE_MAX_BYTES / ANCHOR_MAX_BYTES)` (32) and returns below it —
neither bound can be exceeded there, because no entry exceeds the fetch layer's cap. `anchorCache.ts` gains one import
of `ANCHOR_MAX_BYTES` from `AnchorFetchService`; there is no cycle, since task-149 AC-8 forbids the fetch layer from
touching the cache. Judgment call 7 (guide:2313-2316) records the reason; the constant is at guide:2386-2390, the
import at `:2377`, the early return at `:2449`. Step 9 case 8 pre-creates `ANCHOR_CACHE_MAX_ENTRIES + 1`
files, so it still crosses the floor and needs no change.

**Cross-document consistency re-checked after the pass.** Every seam, name and key that moved landed in both
documents: S-9 exists in the PRD (`:944-965`) and the guide (`:365-370`); the guide's ToC and seam header now say
`S-1 … S-9`; the task-151 i18n count is eleven in both; the 84 → 95 → 97 arithmetic appears in the PRD's inventory,
guide task-151 Step 13 and Verify §5, and guide task-172 Step 6, Verify §8 and AC-4. No committed snapshot moves from
the B-1 change: `source/renderer/app/containers/governance/__snapshots__/DRepDetailPage.spec.tsx.snap` contains no
match for `anchor`, so task-151's Verify §3 "must NOT move" list is unaffected.

**Final sizes.** `anchor-1-PRD.md` 1616 lines (was 1560); `anchor-1-implementation-guide.md` 4926 lines (was 4785),
five `### task-NNN` sections at `:376`, `:935`, `:2118`, `:3017`, `:3940`.

**OWED after the fix pass — unchanged in kind, one item added.**
1. `nix fmt` before merge — `nix` is absent here; `node_modules/.bin/prettier --write <explicit paths>` is the
   substitute (these `.md` files are outside `.prettierignore`'s allow-list and are not prettier-formatted at all).
2. The real SIPO CIP-119 vector — task-151 AC-4's content half.
3. A live anchor fetch — every transport guard, now including the DNS budget, is proven against mocked `https`/`dns`.
4. The Storybook visual and ja-JP overflow pass — no browser.
5. **New:** a real browser click-through of the https-gated anchor link. Step 16 cases 7 and 8 prove the gate in jsdom
   against a mocked `openExternalLink`; nothing here exercises the OS shell.
6. Residual, unchanged: the three main-process whole-error sinks (D-6d); `explorer.staging.cardano.org` serving https;
   cv-2 F-15's provenance half and slice-6 F-6's unowned badges.
