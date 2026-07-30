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

---

## Implementer: 2026-07-29 — task-152 close-out record and audit-scope correction

Entry written by the review-fix pass for task-152, in response to round 2's B-1 and B-2. It carries the
measured results, the corrected statement of what the Step 1 audit actually covers, the recorded
consequences, and the OWED items. New role prefix: the log header at `anchor-1-code-review.md:3-4` names
`Planner:`, `Critiquer:` and `Code Review:` entries only; relabel if the slice prefers one of those.

**Code state — unchanged by this pass, and green.** Round 2 found the code half faithful to the guide, so
nothing in `source/` or `tests/` was edited here. Re-measured in the worktree after the review:

| Gate | Result |
|---|---|
| `jest --testPathPattern="source/main/ipc/open-external-url"` | 1 suite / 13 tests, green (guide:877-878 expects 13) |
| `jest --testPathPattern="tests/common/unit/networks"` | 1 suite / 12 tests, green (4 at HEAD → 12, guide:880-881) |
| `tsc --noEmit` | exit 0 |
| `prettier --check` over the four changed paths | "All matched files use Prettier code style!" |
| `yarn lint` | exit 0, **5599** warnings (0 errors) — up 8 from the ~5591 baseline (`guide:153`), the pre-declared cost of a new spec under `source/` (`guide:529`) |
| `jest --testPathPattern="tests/jest/security/governance-sanitization"` | 1 suite / 26 tests, unchanged from baseline (`guide:902-903`) |
| `jest --testPathPattern="containers/voting/VotingGovernancePage.spec"` | 1 suite / 27 tests, unchanged from baseline (`guide:904-905`) |

`git status --porcelain` lists four source files, so Step 6 holds: no governance component is touched and no
anchor URL is rendered as a link by this change.

**Audit-scope correction (round 2 B-2).** Step 1's three commands (`guide:536-539`) are literal greps over
`source/`, so their reach is source-literal URL producers, not URLs that arrive as data at runtime. Within
that reach the audit's conclusion holds: `getNetworkExplorerUrl` (`source/renderer/app/utils/network.ts:36-39`
after the fix) was the one real producer and is fixed in the same change. **Two runtime-sourced classes lie
outside what those greps can see, and an `http:` value in either now fails silently under the guard:**

1. **Stake-pool homepage.** `source/renderer/app/components/staking/widgets/TooltipPool.tsx:512` —
   `onClick={() => onOpenExternalLink(homepage)}`, wired from
   `source/renderer/app/containers/staking/StakePoolsListPage.tsx:90` and
   `source/renderer/app/containers/staking/DelegationCenterPage.tsx:111`
   (`onOpenExternalLink={app.openExternalLink}`). `homepage` is operator-registered pool metadata typed only
   as `string` (`source/renderer/app/domains/StakePool.ts:17`,
   `source/renderer/app/api/staking/types.ts:43`) — no scheme constraint anywhere on the path.
2. **Newsfeed action URLs.** `source/renderer/app/stores/NewsFeedStore.ts:220-224` destructures
   `newsItem.action` and calls `this.stores.app.openExternalLink(url, e)` on the remote feed's `url` verbatim;
   the overlays that trigger it are `source/renderer/app/containers/news/NewsOverlayContainer.tsx:40` →
   `source/renderer/app/components/news/IncidentOverlay.tsx:43` and
   `source/renderer/app/components/news/AlertsOverlay.tsx:72`.

How often either carries `http://` in production is **unmeasured** — there is no network in this environment
to sample registered pool metadata or the live feed.

**Widening the allow-list is not the remedy.** "The allow-list is exactly `https:` — nothing wider"
(`guide:506`) and locked invariant #3 (`guide:76-81`) forbids thinning the floor. The pool-homepage case is a
product decision — accept the silent failure, surface an error in the UI, or normalise the value at the
producer — and is raised to the user rather than resolved inside task-152.

**Recorded consequence (`guide:929-933`).** With the rejection kept fire-and-forget,
`AppStore.openExternalLink` (`source/renderer/app/stores/AppStore.ts:80-83`) ignores the promise
`send()` returns, so a blocked URL now produces an unhandled promise rejection in the renderer console
instead of a visible error. Console noise, not a crash — and after the Step 2 fix no *source-literal* caller
produces a non-https URL — but it is the observable cost of the silent-rejection decision.

**Risk carried into task-151 (round 2 M-1).** `source/main/index.ts:276-286` is a second, unguarded path into
the OS shell: `contents.setWindowOpenHandler` calls `shell.openExternal(url)` at `:283` with no scheme check
and logs the full URL at `:279-281` (`logger.info('Prevented creation of new browser window', { url })`).
It is correctly outside task-152's five-file scope, but task-151 Step 9 renders the anchor as
`<a href target="_blank" rel="noopener noreferrer">`, so a modifier or middle click — or any `window.open` —
bypasses the hardened IPC path and writes the anchor URL into a main-process log. Task-151 must either route
that click through `openExternalLink` or harden the handler; against invariant #3 and the sanitization floor
this is a task-151 blocker, not a nicety.

**Noted, no change requested.** `source/main/ipc/open-external-url.spec.ts:5` is
`import type {} from './open-external-url';`, a no-op — verbatim from the guide and harmless, since the module
is really loaded through `jest.isolateModules` + `require`.

**OWED — not provable here, never to be reported green.**
1. `nix fmt` before merge. `nix` is absent in this devcontainer;
   `node_modules/.bin/prettier --write` over the four explicit paths is the substitute and is clean. The
   obligation stays open and user-owned.
2. That `explorer.staging.cardano.org` actually serves https. No network here, so the Step 2 change is verified
   as a code property (the scheme emitted) and not as a reachable endpoint. A runtime check on a staging build
   is owed before release.

**Step 8 is NOT discharged by this pass.** The tracker row is still `"status": "pending"`
(`governance-drep-discovery-plan-tasks.json:1659`) and no commit exists; this fix pass was instructed not to
commit and not to edit the tracker JSON, so both remain open for whoever closes the task. The `statusReason`
must state the corrected audit scope above rather than an unqualified "the audit found exactly one real
non-https producer"; `evidence` and `updatedAt` follow `guide:848-861`, and the commit subject is
`fix(gov): task-152 restrict open-external-url to the https scheme` (`guide:869`).

---

## Code Review: task-152 — round 3 (2026-07-29)

**Verdict: approved.** Three rounds over the uncommitted task-152 diff — four source/test files
(`source/main/ipc/open-external-url.ts`, `source/main/ipc/open-external-url.spec.ts`,
`source/renderer/app/utils/network.ts`, `tests/common/unit/networks.spec.ts`) plus this log — against
`anchor-1-implementation-guide.md:1-934`. Round 3 was a single broad pass; the reviewer re-measured every
gate itself rather than accepting the implementer's table.

### Blockers

**None in round 3.** No blocker survived to this round and none was raised in it.

Earlier rounds, recorded for the transcript:

- **Round 1.** Its findings are **not transcribed in this log** and are not reconstructed here. The only
  entries preceding this one are `Planner:` (`:10`), two `Critiquer:` planning passes over the PRD + guide
  (`:106`, `:200`) and the implementer close-out (`:348`); none of them is a round-1 code-review record.
  Recorded as not-transcribed rather than invented.
- **Round 2, B-1 — DISCHARGED.** Required a close-out record carrying the measured results, the recorded
  consequences and the OWED list; the implementer wrote it at `:348-430`.
- **Round 2, B-2 — DISCHARGED.** Required correcting the audit-scope claim. The Step 1 audit
  (`guide:536-539`) is three greps over `source/`, so "exactly one real non-https producer" is true only of
  source-literal producers. The correction is at `:371-391` and is now also carried in the task-152
  `statusReason` and in `research/anchor-1-findings.md` F-1.
- **Round 2, M-1 — CARRIED, not fixed.** `source/main/index.ts:276-286` is a second, unguarded
  `shell.openExternal` path that logs the full URL at `:279-281`. Correctly outside task-152's scope; it
  becomes a **task-151 blocker** once the anchor renders as `<a target="_blank">`. Recorded as F-4.

### Minor

- **Noted, no change requested.** `source/main/ipc/open-external-url.spec.ts:5` is
  `import type {} from './open-external-url';`, a no-op — verbatim from the guide and harmless, since the
  module is really loaded through `jest.isolateModules` + `require` in `loadModule` (`:33-40`).
- **No new npm dependencies, no new abstractions, no dead code.** The two module-local exports
  (`isAllowedExternalUrl`, `handleOpenExternalUrl`) exist so the spec can drive the handler directly, per
  `guide:683-684`.
- **No task ids, review labels, ALL-CAPS words or change history** in any comment or test name across the
  four changed paths (verified by grep).

### Independent re-checks

Every number below was measured in the worktree, not asserted. The verifier and the reviewer measured
independently and agreed; the first two rows were additionally re-run by this scribe pass.

| Gate | Guide expectation | Measured |
|---|---|---|
| `jest --testPathPattern="source/main/ipc/open-external-url"` | 13 tests (`guide:877-878`) | 1 suite / **13 tests**, green |
| `jest --testPathPattern="tests/common/unit/networks"` | 4 at HEAD → 12 (`guide:880-881`) | 1 suite / **12 tests**, green — Step 3 saved |
| `tsc --noEmit` | exit 0 (`guide:883-884`) | exit 0 |
| `yarn compile` | exit 0, ~22 s (`guide:886-887`) | exit 0, 18.9 s |
| `yarn lint` | exit 0, 0 errors (`guide:889-891`) | exit 0, **5599 warnings / 0 errors** — up 8 from the ~5591 baseline (`guide:153`), the pre-declared cost of a new spec under `source/` (`guide:529`) |
| `prettier --check` over the four changed paths | clean (`guide:893-898`) | "All matched files use Prettier code style!" |
| `jest --testPathPattern="tests/jest/security/governance-sanitization"` | 26, unchanged (`guide:902-903`) | 1 suite / **26 tests**, unchanged |
| `jest --testPathPattern="containers/voting/VotingGovernancePage.spec"` | 27, unchanged (`guide:904-905`) | 1 suite / **27 tests**, unchanged |
| `git status --porcelain` | nothing outside the intended files (`guide:907-908`) | 3 modified + 1 untracked source/test files, plus plan docs |

**Invariant #3 (`guide:75-81`) satisfied and not thinned.** The guard at
`source/main/ipc/open-external-url.ts:27-32` runs before `shell.openExternal` is referenced at all (`:33`);
the allow-list is the single constant `'https:'` (`:10`) with no `http:` / `mailto:` / `ipfs:` widening;
unparseable input maps to `'unparseable'` (`:17`) and is therefore rejected. `it.each` at `spec:61-74` pins
`javascript:`, `file:`, `data:`, `http:`, mixed-case `JavaScript:` and non-URL input, each with
`expect(shell.openExternal).not.toHaveBeenCalled()`. The `registers the hardened handler on the channel`
case (`spec:110-116`) proves the guarded function is the one wired to `onReceive`, so the guard is on the
wire and not an unused export.

**Sanitization floor holds on the new main-process sink.** The only logger call (`:28-30`) ships
`{ scheme }` — a bare protocol token — never the URL, host, userinfo or error object; `spec:100-108` pins
that `internal.example` and `secret` never appear in the warn payload. The reviewer re-ran the
`filterLogData` check against `source/main` itself: the single grep hit is a comment at
`source/main/utils/setupLogging.ts:178-182`, not a call site, so the "renderer-only" premise holds and
hand-enforcement was the right discipline (recorded as F-3).

**AC-3 holds negatively.** `git status --short` lists no file under
`source/renderer/app/components/governance/`, and
`.../governance/drep-detail/DRepDetailAnchorSection.tsx:55-57` still emits the inert
`<dd>{anchor.url}</dd>`. No anchor link is rendered by this change (recorded as F-6).

**Audit spot-check (informational).** The reviewer independently grepped `source/**/*.ts{,x}` for `http://`
literals to test the guide's producer claim. It holds within grep reach: `main/config.ts:185` and
`CardanoSelfnodeLauncher.ts:35` are local token-metadata server URLs, `main/windows/main.ts:84` is the
dev-server `loadURL`, `MatomoClient.ts:65` is a synthetic analytics URL,
`WalletTokenPicker.stories.tsx:27` is story fixture data, and `About.tsx:136` is a cosmetic
`label="http://daedaluswallet.io"` whose `onClick` passes the `https://` form. Newsfeed URLs
(`urlsConfig.ts`) are fetch targets, not `openExternal` targets.

**Out of this diff's reach:** invariants #5, #7, #8 and #11 — no lovelace arithmetic, no cohort code, no
badge code, no new i18n strings. `yarn i18n:manage` was correctly **not** run: no message catalog appears
in the diff because this task changes no copy.

### Merged and dropped

- **Merged.** Round 2's B-2 and the reviewer's own round-3 audit spot-check reach the same conclusion from
  opposite directions (one narrowing the claim, one testing it), and are recorded together as F-1 rather
  than as two findings.
- **Dropped.** The suggestion to widen the allow-list so the pool-homepage and newsfeed cases stop failing
  silently. `guide:506` fixes it at exactly `https:` and invariant #3 is "never thinned"; the silent
  failure is a product decision raised to the user (F-1), not a code change inside task-152.
- **Dropped.** Converting `send` → `request` in `AppStore.openExternalLink` so the rejection becomes
  visible. Explicitly resolved against at `guide:507-509` ("Do **not** touch `AppStore.ts`"); the
  consequence is recorded instead (F-5).

**Decision: approve.** No blockers; the code half is byte-faithful to guide Steps 2-6.

### OWED — never reported green

1. `nix fmt` before merge. `nix` is absent in this devcontainer, so it **cannot** run;
   `node_modules/.bin/prettier --check` over the four explicit paths passed and is the substitute. The
   obligation stays open and user-owned.
2. That `explorer.staging.cardano.org` actually serves https — and likewise `explorer.cardano.org` and
   `explorer.cardano-testnet.iohkdev.io`. There is no network here, so
   `source/renderer/app/utils/network.ts:36-39` is verified only as a code property (the scheme string it
   emits). A runtime reachability check on a staging build is owed before release.
3. How often stake-pool homepage metadata and newsfeed action URLs carry `http://` in production —
   unmeasurable without a network (F-1).
4. No browser click-through and no ja-JP visual pass were possible. Neither is required by task-152, which
   changes no UI and no copy, but both are recorded as not-run rather than claimed.
5. The single close-out commit `fix(gov): task-152 restrict open-external-url to the https scheme`
   (`guide:869`) was still unmade when this entry was written; `git log -1` was `33c02840a docs(gov): add
   anchor-1 slice planning docs`. The tracker half of Step 8 is discharged by this pass — the task-152 row
   at `governance-drep-discovery-plan-tasks.json:1656-1679` is now `"status": "complete"` with
   `statusReason`, `evidence` and `updatedAt: 2026-07-29`.

---

## Code Review: task-152 — round 1 (2026-07-29)

**Verdict: approved. No blockers.** Scope note, so the numbering is not misread: this is the
**post-commit** review pass and its own orchestration numbered it round 1, but it is the *fourth*
code review of task-152 recorded in this log — the three pre-commit rounds are at `:434-554`, over
the then-uncommitted diff. This round reviewed commit `3a9b36daa` `fix(gov): task-152 restrict
open-external-url to the https scheme` (single subject line, no body, no trailer) with
`git status --short` empty. Seven paths in the diff: the four source/test files, the tracker row,
and the two plan docs `research/anchor-1-findings.md` and `task-plans/anchor-1-code-review.md`.

### Blockers

**None raised in this round, and none survived from the earlier three.** Round 2's B-1 (close-out
record) and B-2 (audit-scope correction) were discharged at `:348-430` and re-confirmed here; round
3 raised none. Nothing was reopened.

### Minor

- **The `git status` file count deviates from the guide, and this is not a defect.** Guide Verify
  run 8 reads `# 8. Nothing outside the five files changed.`
  (`anchor-1-implementation-guide.md:907`); the commit touches seven. The extra two are this
  slice's own plan docs, which Step 8 (`:845-861`) requires in prose but which run 8's count never
  absorbed. No source file and no governance component outside the four named paths is in the diff.
  Recorded as F-8.
- **Tracker row conforms to the completed sibling.** Key order on the task-152 object is `id,
  title, description, status, statusReason, evidence, updatedAt, priority, estimatedHours,
  dependencies, targetPath, acceptanceCriteria` — identical to task-148 — and the file parses.
- **Carried unchanged from round 3, no action requested.** `source/main/ipc/open-external-url.spec.ts:5`
  is the no-op `import type {} from './open-external-url';`, verbatim from the guide; the module is
  really loaded through `jest.isolateModules` + `require`.
- **No new npm dependencies, no new abstractions, no dead code, and no task ids, review labels,
  ALL-CAPS words or change history in any comment or test name** across the four changed paths.

### Independent re-checks

Every number measured in this worktree by the verifier; the first two rows were additionally re-run
by this scribe pass and agreed exactly.

| Gate | Guide expectation | Measured |
|---|---|---|
| `jest --testPathPattern="source/main/ipc/open-external-url"` | 13 tests (`guide:877-878`) | 1 suite / **13 tests**, green |
| `jest --testPathPattern="tests/common/unit/networks"` | 4 at HEAD → 12 (`guide:880-881`) | 1 suite / **12 tests**, green |
| `tsc --noEmit` | exit 0 (`guide:883-884`) | exit 0 |
| `yarn compile` | exit 0, ~22 s (`guide:886-887`) | exit 0, 22.09 s |
| `yarn lint` | exit 0, 0 errors (`guide:889-891`) | exit 0, **0 errors** |
| `prettier --check` over the four changed paths | clean (`guide:893-898`) | clean on all four |
| `jest --testPathPattern="tests/jest/security/governance-sanitization"` | 26, unchanged (`guide:902-903`) | 1 suite / **26 tests**, unchanged |
| `jest --testPathPattern="containers/voting/VotingGovernancePage.spec"` | 27, unchanged (`guide:904-905`) | 1 suite / **27 tests**, unchanged |
| `git status --porcelain` | nothing outside the intended files (`guide:907-908`) | **empty** — the work is committed |

**Guide fidelity, step by step.** Step 2: `source/renderer/app/utils/network.ts:36-38` is the exact
three-line body the guide prescribes, and `MAINNET`/`STAGING`/`TESTNET`/`DEVELOPMENT` all stay
referenced elsewhere in the file, so the collapse left no dead import. Step 3:
`tests/common/unit/networks.spec.ts:37-56` appended verbatim with the four pre-existing cases
untouched. Steps 4 and 5: `open-external-url.ts` and its spec match the guide, including the
deliberate `shell.openExternal(url) ? Promise.resolve() : Promise.reject()` on the allowed branch;
prettier reflowed two `{ scheme }` object literals onto three lines, which is cosmetic.

**Invariant #3 (`guide:75-81`) satisfied and not thinned.** The guard at
`source/main/ipc/open-external-url.ts:27-32` runs before `shell.openExternal` is referenced at all
(`:33`); the allow-list is the single constant `'https:'` (`:10`) with no `http:` / `mailto:` /
`ipfs:` widening; unparseable input maps to `'unparseable'` (`:17`) and is therefore rejected.
`spec:61-74` pins `javascript:`, `file:`, `data:`, `http:`, mixed-case `JavaScript:` and non-URL
input, each asserting `shell.openExternal` was not called, and `spec:110-116` proves the guarded
function is the one wired to `onReceive` — the guard is on the wire, not an unused export.

**Sanitization floor holds on the new main-process sink.** `open-external-url.ts:28-30` logs
`{ scheme }` — a bare protocol token — never the URL, host, userinfo or error object;
`spec:100-108` pins that host and path never appear in the warn payload.

**AC-3 holds negatively.** No file under `source/renderer/app/components/governance/` is in the
diff, and `.../governance/drep-detail/DRepDetailAnchorSection.tsx:55-57` still emits the inert
`<dd className={styles.anchorValue}>{anchor.url}</dd>` with no `<a>` or `href` anywhere in the file;
its last touching commit is `34296ec16` (task-116). This commit renders no link.

**Out of reach of this diff:** no i18n catalog appears in it, so `yarn i18n:manage` was correctly
not required and was not run.

### Merged and dropped

- **Merged.** This round's guide-fidelity walk and the round-3 audit spot-check reach the same
  conclusion about the source-literal producer claim and stay recorded together as F-1, not as a
  second finding.
- **Dropped.** Widening the allow-list so the pool-homepage and newsfeed cases stop failing
  silently — `guide:506` fixes it at exactly `https:` and invariant #3 is never thinned; the silent
  failure is a product decision raised to the user (F-1).
- **Dropped.** Converting `send` → `request` in `AppStore.openExternalLink` to make the rejection
  visible — resolved against at `guide:507-509` ("Do **not** touch `AppStore.ts`"); the consequence
  is recorded instead (F-5).

**Decision: approve.** Round 1 of the post-commit pass, no blockers, nothing carried forward into a
further round.

### OWED — never reported green

1. `nix fmt` before merge. `nix` is absent in this devcontainer, so it **cannot** run;
   `node_modules/.bin/prettier --write` over the four explicit paths is the substitute and
   `--check` confirms it clean. The obligation stays open and user-owned.
2. That `explorer.staging.cardano.org` — and likewise the preprod, selfnode and development
   explorer hosts now forced to https by `source/renderer/app/utils/network.ts:36-38` — actually
   serves https. No network here, so Step 2 is verified only as a code property (the scheme string
   emitted), never as a reachable endpoint. A runtime check on a staging build is owed before release.
3. How often stake-pool homepage metadata (`TooltipPool.tsx:512`) and newsfeed action URLs
   (`NewsFeedStore.ts:220-224`) carry `http://` in production. Both arrive as runtime data outside
   the audit's grep reach, an `http:` value in either is now rejected silently, and no test here
   covers it. Raised as an open product decision (F-1), not resolved.
4. The renderer-console behaviour of the fire-and-forget rejection. `AppStore.openExternalLink`
   (`AppStore.ts:80-83`) ignores the promise `send()` returns, so a blocked URL produces an
   unhandled promise rejection instead of a visible error. Recorded as F-5 and at `:398-402`, but
   there is no browser here to observe the console, so the user-visible impact is unverified.
5. No browser click-through and no ja-JP visual pass were possible — no network, no browser.
   Neither is required by a task that changes no UI and no copy, and the exposure is low, but both
   are recorded as unproven rather than green.

---

## Code Review: task-149 — round 1 (2026-07-29)

**Verdict: approved. No blockers, one round.** The review ran over the uncommitted working tree at
HEAD `6d38d2bfb` — three modified files (`source/common/types/governance.types.ts`,
`source/common/utils/logging.ts`, `tests/jest/security/governance-sanitization.spec.ts`) and two
untracked (`source/main/governance/AnchorFetchService.ts`,
`tests/jest/governance/AnchorFetchService.spec.ts`). The diff implements the approved guide
(Steps 1–11 at `anchor-1-implementation-guide.md:1171-2005`) faithfully and completely; not one
line changed in review.

### Blockers

**None raised.**

### Minor

**None raised.** The review's convention sweep found nothing to file: no `.skip` / `.only`, no task
ids, review labels, ALL-CAPS words or change history in any comment or test name, comments limited
to the guide-specified plain invariant/why lines, no new npm dependencies, and no dead code.

### Independent re-checks

The reviewer re-ran every verification command rather than inheriting the verifier's numbers, and
the two passes agreed exactly. Guide expectations are the Verify block at
`anchor-1-implementation-guide.md:2027-2069`.

| Gate | Guide expectation | Measured (verifier and reviewer, agreeing) |
|---|---|---|
| `tsc --noEmit` | exit 0 | exit 0 |
| `yarn compile` | exit 0, ~22s (`:2034`) | exit 0 |
| `jest --testPathPattern="tests/jest/governance/AnchorFetchService"` | 1 suite / 39 tests (`:2039`) | 1 suite / **39 tests**, green |
| `jest --testPathPattern="tests/jest/security/governance-sanitization"` | 26 → 35 tests (`:2044`) | 1 suite / **35 tests** (26 baseline + 9), green |
| `jest --testPathPattern="containers/voting/VotingGovernancePage.spec"` — floor anchor 2 of 2, mandatory | 27 unchanged (`:2049`) | 1 suite / **27 tests**, unchanged |
| `jest --testPathPattern="tests/jest"` full sweep | 12 suites (1 skipped), 206 total, 194 passed, 12 skipped (`:2054`) | **12 suites / 206 total / 194 passed / 12 skipped** — exact match |
| `jest --testPathPattern="source/renderer/app/api"` | 6 unchanged (`:2056`) | 1 suite / **6 tests**, unchanged |
| `yarn lint` | exit 0, ~5591 baseline warnings (`:2058`) | exit 0, **0 errors**, 5614 warnings — the +23 is the new source file, as the guide predicts |
| structural grep (`JSON.parse\|fs\|blake\|axios\|rejectUnauthorized\|latest`) | no output (`:2066`) | no output |
| builtin-import grep | exactly the two builtin imports (`:2068`) | exactly `https` and `dns` |

Both sanitization floor anchors were run; the reviewer and verifier each note that citing the
security suite alone would have been a false green. The 1 skipped suite / 12 skipped tests is
`tests/jest/governance/GovernanceCliArgvSmoke.spec.ts` self-skipping with `cardano-cli` off PATH,
per the guide's own note below the Verify block.

**Guide fidelity, spot-checked against the live tree.** Step 1: the full 13-member
`AnchorFetchErrorType` enum at `source/common/types/governance.types.ts:75-89` and nothing else —
no IPC channel, no task-150 types (the enum-only split). Steps 2–6: never-throwing
discriminated-union result; single `fail()` logging point emitting only `{ errorType }`
(`AnchorFetchService.ts:160`) and a success line emitting only `{ byteLength }` (`:197-198`),
satisfying invariant #12 / AC-9; SSRF prefix tables covering all AC-6 categories plus tunneling
ranges with unparseable input default-blocked; pinned custom `lookup` with original
host/servername (`:217-229`, AC-7); one shared 10s budget across DNS and transfer (`:9`,
`:297-308`, AC-3); dual size guards (`:258`, `:267`, AC-4); content-type allow-list (`:245-249`,
AC-5); explicit 3xx rejection that never reads `location` (`:233-238`, AC-2); no
`rejectUnauthorized` token anywhere (AC-1); no `fs` / `JSON.parse` / hash import (AC-8); zero new
npm dependencies. Step 7: `filterLogData`'s `sensitiveData` grows by exactly the twelve mandated
names (`source/common/utils/logging.ts:52-63`). Step 8: the floor-suite docblock renarrowed
(`tests/jest/security/governance-sanitization.spec.ts:1-11`). Step 9: eight domain-shaped
`filterLogData` cases (`:235`, `:253`, `:264`, `:271`, `:278`, `:284`, `:290`) including the
exact-key-match negative case (`:300`). Step 10: the first main-process spy case (`:641-654`)
with its positive `ANCHOR_TLS_FAILED` assertion (`:744`).

### Merged and dropped

**Nothing to merge and nothing dropped** — no lens filed a finding at any severity, so no
adjudication was needed.

**Decision: approve.** One round, nothing carried forward.

### OWED — never reported green

1. **The Step 12 commit is unmade.** `feat(gov): task-149 add the hardened anchor fetch service`
   (`anchor-1-implementation-guide.md:2024`) — all five work files sit uncommitted at HEAD
   `6d38d2bfb`. Recorded as F-11.
2. **The Step 12 `prettier --write` needs a scope decision before that commit.**
   `source/common/utils/logging.ts` and `tests/jest/security/governance-sanitization.spec.ts` fail
   `prettier --check` on proven pre-existing HEAD drift — `git show HEAD:<file>` piped through
   `prettier --stdin-filepath` reproduces the identical hunks (the `Object.keys(value).reduce(`
   reflow and the `(MatomoTracker as unknown) as jest.Mock` cast, the known 2.1.2 oscillation) —
   so `--write` would commit unrelated churn. No line this task added is prettier-dirty; the other
   three paths check clean. Recorded as F-9.
3. `nix fmt` before merge — `nix` is absent here and **cannot** run; explicit-path prettier is the
   substitute. User-owned obligation.
4. **No live anchor fetch, ever.** Every guard (SSRF, DNS pinning, TLS, redirect, timeout, size
   cap, content-type) is proven only against mocked `https.request` and `dns.promises.lookup`;
   TLS default verification has never met a real certificate chain, and no SIPO or Cardano Academy
   anchor vector has been fetched end to end. No network in this environment. Recorded as F-10.
5. No browser click-through and no ja-JP visual pass — no browser and no network; not applicable
   to a diff with no copy and no component, recorded so it is never implied green. No i18n catalog
   is in the diff, so `yarn i18n:manage` was correctly not required and not run.
6. **Residual, outside this task's diff** and carried from the guide's own OWED list, not
   re-verified in this round: the three pre-existing main-process whole-error sinks stay
   unhardened at `GovernanceQueryService.ts:523-526` and `governanceChannel.ts:58-60`, `:64` /
   `:77`. F-3's rule stands — every new main-process sink needs hand-enforcement plus its own
   containment assertion, which this task's service is the first to carry.

## Code Review: task-150 — round 1 (2026-07-29)

**Verdict: approved. No blockers, one round.** The review ran over the uncommitted working tree at
HEAD `71ad2b4a1` — four modified files (`source/common/ipc/api.ts`,
`source/common/types/governance.types.ts`, `source/main/ipc/index.ts`,
`source/renderer/app/ipc/governanceChannel.ts`) and ten untracked
(`source/main/governance/AnchorVerificationService.ts`, `source/main/governance/anchorCache.ts`,
`source/main/ipc/governanceAnchorChannel.ts` and its spec, the two Jest suites under
`tests/jest/governance/`, and the four fixture files under `tests/mocks/governance/`). The diff
lands the guide's verify-cache-parse pipeline faithfully: the Blake2b-256 digest gates
`JSON.parse` and every cache write on both the fetch and cache-hit paths
(`AnchorVerificationService.ts:66-95`); the hash-keyed cache validates `/^[0-9a-f]{64}$/` before
any `path.join` and writes immutably with `wx`/`EEXIST` tolerance (`anchorCache.ts:19-62`); the
S-4 IPC seam carries `DRepAnchorPresence` with no `drepId`, and the handler never rejects and logs
enum values only; all 24 prescribed test cases (9 cache + 13 verification + 2 handler) exist with
the load-bearing assertions intact (the `JSON.parse` spy, the `fs.writeFileSync` spy, tamper
recovery, in-flight dedup, and the four-level logger sanitization sweep). Not one line changed in
review.

### Blockers

**None raised.**

### Minor

**None raised.** The convention sweep found nothing to file: zero new dependencies, zero i18n
strings, no comment or test-name convention violations, and no sensitive value reaching any
main-process logger.

### Independent re-checks

The verifier ran all eight Verify steps of the guide (`anchor-1-implementation-guide.md:2920-2979`)
and the reviewer independently re-ran the load-bearing gates rather than inheriting the numbers;
the two passes agreed on every count.

| Gate | Guide expectation | Measured (verifier; reviewer agreeing where re-run) |
|---|---|---|
| Fixture digest, step 1 (`:2925-2930`) | prints nothing | prints nothing — digest reproduces; reviewer additionally confirmed the fixture is prettier-stable, so the later format pass cannot drift it |
| `yarn compile` (`:2933`) | exit 0 | exit 0; reviewer also ran `tsc --noEmit`, exit 0 |
| `jest --testPathPattern="tests/jest/governance/anchorCache"` (`:2937`) | 1 suite / 9 tests | 1 suite / **9 tests**, green |
| `jest --testPathPattern="tests/jest/governance/AnchorVerificationService"` (`:2939`) | 1 suite / 13 tests | 1 suite / **13 tests**, green |
| `jest --testPathPattern="source/main/ipc/governanceAnchorChannel"` (`:2941`) | 1 suite / 2 tests | 1 suite / **2 tests**, green |
| handler-registration grep (`:2945-2946`) | exactly two hits | exactly two — `source/main/ipc/index.ts:30` import, `:53` call |
| `jest --testPathPattern="tests/jest/governance"` (`:2949-2952`) | >= 7 passed suites, >= 120 passing tests | **8 passed + 1 skipped suites; 159 passed + 12 skipped tests** |
| `jest --testPathPattern="tests/jest/security/governance-sanitization"` — floor anchor 1 of 2 (`:2955-2956`) | 26 at HEAD, more once task-149's cases land | 1 suite / **35 tests**, green |
| `jest --testPathPattern="containers/voting/VotingGovernancePage.spec"` — floor anchor 2 of 2 (`:2957-2958`) | 1 suite, all green | 1 suite / **27 tests**, green |
| `yarn lint` (`:2960-2962`) | exit 0; ~5591 baseline moves | exit 0, **0 errors**, 5615 warnings — movement expected per the guide's own step-6 note |
| step-7 format pass (`:2965-2976`) | `prettier --write` over the eleven paths | `prettier --check` clean on all eleven — run read-only as the substitute; `--write` would be a no-op, so step 8's hash regeneration (`:2978`) is moot |

`yarn i18n:manage` was deliberately not run, per the guide's own instruction (`:2981`) — task-150
mints no copy and the diff touches no catalog. The 1 skipped suite / 12 skipped tests is the
environment-gated `GovernanceCliArgvSmoke.spec.ts` self-skipping with `cardano-cli` off PATH, as
in every prior round.

### Merged and dropped

**Nothing to merge and nothing dropped** — no lens filed a finding at any severity, so no
adjudication was needed.

**Decision: approve.** One round, nothing carried forward.

### OWED — never reported green

1. **No live anchor fetch, ever** (guide OWED 1, `anchor-1-implementation-guide.md:3000-3001`) —
   no network in this devcontainer; every path is proven only against a mocked `fetchAnchorBytes`.
   F-10's caveat carries to this task's offline proofs exactly as predicted.
2. **The real SIPO / Cardano Academy CIP-119 body bytes** (guide OWED 2, `:3002-3004`) — no
   CIP-119 body is committed and the digest at `drep-state-preprod-epoch295-sample.json:2853`
   cannot be reproduced offline; carried forward to task-151 AC-4.
3. `nix fmt` before merge (guide OWED 3, `:3005-3006`) — `nix` is absent here and **cannot** run;
   explicit-path prettier (verified clean via `--check`) is the substitute and the real `nix fmt`
   run stays a user-owned pre-merge obligation.
4. **The close-out commit is unmade.** `feat(gov): task-150 verify, cache and parse DRep anchor
   bytes` (`:2986`) — four modified and ten untracked files sit uncommitted at HEAD `71ad2b4a1`.
   Recorded as F-12; unlike task-149, no prettier scope decision blocks it.

## Code Review: task-151 — round 1 (2026-07-29)

**Verdict: approved. No blockers, one round.** The review ran over the uncommitted working tree at
HEAD `aa77b475c` — 24 modified files and one untracked
(`source/renderer/app/components/governance/drep-detail/DRepDetailAnchorContent.tsx`). The diff
implements every in-scope guide step (1–17) faithfully: the wire and app types gain a required
`verifiedName: string | null`, written unconditionally `null` by main
(`GovernanceQueryService.ts:518-526` — the bulk query never fetches an anchor); the store adds
`AnchorEnrichEntry`, `@observable anchorStateByDRepId`, the `verifiedMetadataIds` computed derived
from `state === 'verified'` only, `clampVerifiedName` (80-char cap), `fetchAnchorContent` with
hash dedup and IPC-rejection fallback to `AnchorFetchErrorType.Network`, and `_applyVerifiedNames`
with the hash-mismatch name drop, re-applied at both list-rebuild sites; `DRepSourceLabel` gains
exactly the three new variants with tooltips only on those three, so the four untooltipped
On-chain spans stay byte-identical; `DRepDetailAnchorContent` renders `givenName` only from a
verified state with the verified-off-chain label, host tooltip and identity-claim caption; the
D-5c https link gate renders an `<a target="_blank" rel="noopener noreferrer">` routed through
`stores.app.openExternalLink` only for https URLs, with both branches pinned (cases 7 and 8) and
the old inert-text gating comment deleted. No logger/analytics/electron-store call site exists on
the anchor path (grepped the diff; pinned by the new no-logging store test); no renderer
fetch/parse/hash; `verifiedName` has zero readers in cards, search or sort; zero new dependencies;
the story uses the global StoryWrapper toggle with no local IntlProvider and
`storybook/stories/index.ts` is untouched. Not one line changed in review.

### Blockers

**None raised.**

### Minor

**None raised.** The convention sweep found nothing to file: comments and test names carry no task
ids, review labels, ALL-CAPS or change history; catalogs are 95/95 governance keys, key-identical,
all `!!!`-prefixed, with `defaultMessages.json` and `translations/messages.json` updated in the
same diff.

### Independent re-checks

The verifier ran the guide's Verify block (`anchor-1-implementation-guide.md:3850-3919`) and the
reviewer independently re-ran the load-bearing gates; the two passes agreed on every count.

| Gate | Guide expectation | Measured |
|---|---|---|
| `tsc --noEmit` and `yarn compile` (`:3855-3856`) | exit 0 | both exit 0 |
| `jest --testPathPattern="containers/governance/DRepDetailPage"` (`:3861`) | 12 → 20 tests, 1 snapshot unchanged | 1 suite / **20 tests**, 1 snapshot, green |
| `jest --testPathPattern="tests/jest/governance/GovernanceStore"` (`:3863`) | 35 → 43 tests | 1 suite / **43 tests**, green |
| `jest --testPathPattern="tests/jest/i18n"` (`:3865`) | 4 → 5 tests | 1 suite / **5 tests**, green |
| `jest --testPathPattern="tests/jest/governance/logDRepStateSnapshot"` (`:3867`) | 4 → 5 tests | 1 suite / **5 tests**, green |
| must-not-move `(components/governance\|CurrentVoteSummary)` (`:3871`) | 5 suites / 101 tests / 8 snapshots, zero written | **5 / 101 / 8**, zero written |
| must-not-move `GovernanceQueryService` (`:3873`) | 38 tests unchanged | 1 suite / **38 tests**, green |
| `git diff --stat` on the CurrentVoteSummary snapshots dir (`:3875`) | no output | no output — byte-identical |
| sanitization floor, both anchors (`:3880`) | 26 + 27 = 53 at HEAD; first number raised by task-149/150; neither drops a test | **35 + 27 = 62**, both green |
| `yarn i18n:manage` + parity probe (`:3893`) | `95 95 True [] []` | exit 0, a no-op against the tree (identical diff stats before/after), probe prints exactly **`95 95 True [] []`**; all 11 new keys per catalog are `governance.drepDetail.*` |
| `yarn lint` | exit 0, warnings only | exit 0, **0 errors** |
| step-7 format pass (`:3877` ff.) | `prettier --write` over the touched paths | `prettier --check` run read-only as the substitute (the `--write` is the committer's step): 19/21 clean; the 2 flagged files (`GovernanceQueryService.ts` :65-66/:396-397, `DRepDirectory.stories.tsx` :358-363) reproduce identical drift at HEAD, entirely outside this task's hunks — pre-existing 2.1.2 oscillation, recorded as F-15 |

### Merged and dropped

**Nothing to merge and nothing dropped** — no lens filed a finding at any severity, so no
adjudication was needed.

**Decision: approve.** One round, nothing carried forward into a fix pass.

### OWED — never reported green

1. **`nix fmt` before merge** — `nix` is absent in this devcontainer; explicit-path prettier
   (verified via `--check`) is the substitute, and the real run stays a user-owned pre-merge
   obligation that also settles the two HEAD-drifted files above.
2. **AC-4 content half** (`anchor-1-implementation-guide.md:3931`) — the real SIPO body bytes from
   `https://sipo.tokyo/drep/SIPO.jsonld` were never fetched and their Blake2b-256 digest never
   compared to the on-chain `dataHash`; only the committed mock vector and the real on-chain
   `(url, hash)` pair are proven. Recorded as F-13.
3. **Any live anchor fetch** — no network here; every path is proven against mocks (F-10's caveat
   carries).
4. **Storybook visual and ja-JP overflow pass** for the new anchor-state knob — no browser here,
   and the story is unregistered until task-172 edits `storybook/stories/index.ts:16-18`. Recorded
   as F-14.
5. **A real browser click-through of the anchor link** — the https gate is proven only in jsdom
   against a mocked `openExternalLink`, never against the OS shell (`:3938`).
6. **The close-out commit is unmade.** `feat(gov): task-151 render the verified givenName and
   expose metadata completeness` (`:3921`) — 24 modified files plus the new untracked component
   sit uncommitted at HEAD `aa77b475c`. Recorded as F-15.

## Code Review: task-172 — round 1 (2026-07-29)

**Verdict: approved. No blockers, one round.** The review ran over the uncommitted working tree on
top of HEAD `351467833` — 25 modified files, +640/−102, nothing untracked. The diff implements
every in-scope guide step faithfully: `GovernanceStore` gains `DRepCohortContext` plus the two
computeds exactly as specified (`GovernanceStore.ts:38-48`, `:227-252`), reusing task-151's
`verifiedMetadataIds` computed (`:285`) with no second derivation; the classifier matches seam S-8
(`DRepCategoryBadge.tsx:75-98`) — explicit cohort input, `anchor` and `status` dropped from the
source type, priority High value > Threshold > Primary > Non-metadata, High value structurally
impossible out of cohort; the cohort prop is threaded through all six hops including both
`DRepDirectoryList` render sites (`DRepDirectory.tsx:293`, `:364`). i18n lands via
`yarn i18n:manage` across all four tracked files, catalogs key-identical at 97/97 governance keys
with every string `!!!`-prefixed, and the two out-of-cohort tooltips no longer claim
Recommended-view membership. Invariant #8 is pinned twice (file-reading tests plus the grep),
invariant #7 is never restated, the #5 median math is pure BigNumber, and the sanitization surface
is untouched — zero main/common changes, no logger lines added. Storybook registers the three
orphaned story files and adds the cohort knob with no local IntlProvider and no per-locale story
variants. Snapshot diffs are exactly the permitted tooltip strings plus the one new High value
key. Not one line changed in review.

### Blockers

**None raised.**

### Minor

**None raised.** The convention sweep found nothing to file: comments and test names carry no task
ids, review labels, ALL-CAPS or change history; the one new comment block states the classifier's
invariant in plain sentence case.

### Independent re-checks

The verifier ran all 11 steps of the guide's Verify block
(`anchor-1-implementation-guide.md:4796-4889`) and every count matched the guide's expectation;
the reviewer independently confirmed the load-bearing gates and the two passes agreed throughout.

| Gate | Guide expectation | Measured |
|---|---|---|
| step 1 format pass (`:4801-4820`) | 18 explicit paths, exit 0 | `prettier --check` run read-only as the strictly-equivalent substitute (the tree already carries formatted files): "All matched files use Prettier code style!", exit 0 |
| `tsc --noEmit` and `yarn compile` (`:4822-4824`) | exit 0 | both exit 0 |
| classifier suite (`:4826-4828`) | 11 → 22 tests, 0 snapshots | 1 suite / **22 tests**, green |
| `DRepDirectory.spec` (`:4830-4836`) | 47 → 48 tests, 1 snapshot, snap diff confined to the two tooltip strings | **48 tests / 1 snapshot**, diff confined to the two reworded tooltip strings in the single existing key |
| `DRepDetailPage` (`:4838-4844`) | 20 → 21 tests, 1 → 2 snapshots | **21 tests / 2 snapshots**, green |
| `DRepDirectoryPage` (`:4846-4848`) | 8 tests unchanged | **8 tests**, green |
| `GovernanceStore.spec` (`:4849-4851`) | 43 → 49 tests | **49 tests**, green |
| copy markers (`:4853-4855`) | 4 (or 5 after task-151) | **5 tests**, green |
| catalog parity (`:4857-4863`) | `97 97 true` then `[]` | exactly **`97 97 true`** then **`[]`** |
| invariant #8 grep (`:4865-4874`) | no output, exit 1 | no output, **exit 1** |
| governance sweep (`:4876-4880`) | no regression; +19 tests, +1 snapshot over the pre-task basis | **20 passed suites + 1 self-skipping**, 409 passed + 12 skipped tests, **10 snapshots** (9 → 10); per-suite deltas reconcile to the promised +19 |
| `yarn lint` (`:4882-4886`) | exit 0, 0 errors, upward warning drift | exit 0, **0 errors**, 5628 warnings (baseline ~5591) |
| sanitization floor, both anchors | neither drops a test | `security/governance-sanitization` **35** passed, `containers/voting/VotingGovernancePage` **27** passed |
| `yarn i18n:manage` | exit 0, no-op | exit 0, provably a no-op (i18n diff stats unchanged at 14/6/6/14) |

Snapshot regeneration was verified with clean non-`-u` runs (0 snapshots written) since the tree
already carries the regenerated snapshots — strictly equivalent to the guide's
regenerate-then-rerun sequence.

### Merged and dropped

**Nothing to merge and nothing dropped** — no lens filed a finding at any severity, so no
adjudication was needed.

**Decision: approve.** One round, nothing carried forward into a fix pass.

### OWED — never reported green

1. **Storybook visual + ja-JP overflow pass** for all four category badges at both call sites
   (AC-6 second half, `anchor-1-implementation-guide.md:4907-4911`) — needs `yarn storybook` and a
   human eye; no browser here, `storybook/` is outside the Jest roots, and `yarn storybook:build`
   is red at HEAD for unrelated reasons. Specific risk: `!!!高価値` plus the `!!!` marker inside
   the fixed-width card top row (`DRepCard.tsx:109-125`). Recorded as F-16.
2. **`nix fmt` before merge** — `nix` is absent in this devcontainer; explicit-path prettier
   (verified via `--check`, all 18 paths clean) is the substitute, and the real run stays a
   user-owned pre-merge obligation.
3. **Theme-token confirmation** for the new `--badge-highlight-fg` / `--badge-highlight-bg` violet
   fallback (`DRepCategoryBadge.scss:26-27`; guide note at
   `anchor-1-implementation-guide.md:4924-4926`). Recorded as F-19.
4. **The close-out commit is unmade.** `feat(gov): task-172 ground the DRep category badge in
   cohort membership` (`:4793`) — 25 modified files sit uncommitted on top of `351467833`.
   Recorded as F-20.
