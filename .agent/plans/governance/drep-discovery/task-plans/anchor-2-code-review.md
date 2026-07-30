# anchor-2 Code Review Log

> Append-only transcript: `Planner:` entries (planning open/close), one `Critiquer:` entry
> (required review pass over the PRD + guide), and per-task `Code Review:` entries.
> Companion docs: [anchor-2-PRD.md](./anchor-2-PRD.md) ·
> [anchor-2-implementation-guide.md](./anchor-2-implementation-guide.md)

---

## Planner: 2026-07-30 — anchor-2 planning pass

**Scope.** Six tasks — task-157, task-153, task-174, task-154, task-155, task-156 — all `pending`
at `55e8985bf`, completing the anchor enrichment the anchor-1 slice opened: the five remaining
CIP-119 profile fields on the detail view, the `doNotList` cohort exclusion, the dual
CIP-129 / CIP-105 identity display, the confirmation dialog's verified name, the provenance-label
sweep, and the `Abstain` / `No Confidence` directory guard
(`anchor-2-implementation-guide.md:166`, `:2272`, `:3404`, `:4924`, `:5752`, `:6185`).

**Interaction mode.** All six classified `autonomous` (`anchor-2-PRD.md:163-179`). None falls in
the locked non-autonomous set — task-125, the task-166 remainder, task-158, and the release-end
`!!!` copy review — and no blocking decision survived planning.

**R-1 — build order `157 → 153 → 174 → 154 → 155 → 156`, deviating from the JSON listing order**
(`anchor-2-PRD.md:111-160`, `:284-296`; guide preamble `anchor-2-implementation-guide.md:28-55`).
The tasks-JSON listing order is `153, 174, 154, 155, 156, 157`
(`governance-drep-discovery-plan-tasks.json:1798-1826`, `:1827-1849`, `:1850-1867`, `:1868-1885`,
`:1886-1902`, `:1903-1925`). **anchor-2 has zero intra-slice dependency edges** — all twelve
dependency entries across the six rows are cross-slice and already `complete` — so the JSON order
encodes no ordering information and is not a contract. The canonical order is forced by file-level
coupling instead: 157 before 153 because both extend `VerifiedDRepAnchorContent`
(`source/common/types/governance.types.ts:93-96`) and `parseVerifiedContent`
(`source/main/governance/AnchorVerificationService.ts:43-62`), and task-153 AC-1 is unsatisfiable
for a `doNotList: true` document that omits `givenName` until 157 relaxes the `null` return at
`:60`; 174 and 154 before 155 because a labelling sweep that runs before the surfaces it audits
guarantees a second sweep; 155 after 157 because 157 labels every field it adds; 156 last because
it verifies the end state of three surfaces that five earlier commits change. This also supersedes
the grounding brief's recommended `157 → 153 → 155 → 174 → 154 → 156`, which left task-154's
confirmation-dialog name line outside the sweep's audit window (`anchor-2-PRD.md:154-160`).

**R-2 — seam contract: each shared seam is widened exactly once, by a named owner**
(`anchor-2-PRD.md:297-317`, S-1 … S-7 at `:543-629`). Every other task consumes the seam as already
present and must not re-declare it.

- **S-1** — `VerifiedDRepAnchorContent` (`governance.types.ts:93-96`) and `AnchorEnrichEntry.verified`
  (`GovernanceStore.ts:49-52`) are widened once, by **task-157**, to carry `givenName`, `objectives`,
  `motivations`, `qualifications`, `references`, `paymentAddress` **and `doNotList`**, every field
  `| null` rather than optional. task-153 adds nothing here (`anchor-2-PRD.md:543`).
- **S-2** — `parseVerifiedContent` is relaxed once, by **task-157**: it returns content whenever the
  body parses as a JSON object, and the "no name, no block" guard moves to the renderer, which
  already holds it at `DRepDetailAnchorContent.tsx:66`. The digest gate is untouched — unverified
  bytes still never reach `JSON.parse` (`anchor-2-PRD.md:562`).
- **S-3** — `filterLogData`'s key list (`source/common/utils/logging.ts:44-63`) gains all six new
  names in one edit, by **task-157**, with matching cases in both blocks of the task-111 spy suite
  (`anchor-2-PRD.md:575`).
- **S-4** — CIP-105 for display is always `normalizeDRepIdentity`; **task-174** owns the opt-in
  both-forms mode on `DRepIdDisplay`. `helpers.ts:77`'s `Cardano.DRepID.toCip105DRepID` stays the
  search index's derivation and is out of scope (`anchor-2-PRD.md:587`).
- **S-5** — the confirmation dialog's verified name comes from `entry.verifiedName` via
  `governance.drepIndex`, resolved in `VotingGovernancePage.tsx:84-87`; **task-154** owns the render
  and it is scoped to the `{verified givenName}` line plus the extended source label
  (`anchor-2-PRD.md:598`).
- **S-6** — `DRepSourceLabel` gains no new variant; the §7 composite is two instances joined by a
  literal separator, so each tooltip stays correct (`anchor-2-PRD.md:609`).
- **S-7** — label ownership: **task-157** labels what it renders, **task-155** sweeps what remains,
  so no field is labelled twice (`anchor-2-PRD.md:621`).

**Decisions R-3 … R-9.** The orchestration labels map 1:1 onto the PRD's `D-n` (the alias is already
used in the PRD at `:177`). Each closes one of the grounding brief's open questions G-1 … G-7.

- **R-3 / D-3** — `CurrentVoteSummary` is out of scope for task-155: it renders no anchor-derived
  content and already carries `source="on-chain"` at `:90`; its enrichment is cv-track work
  (`anchor-2-PRD.md:318`).
- **R-4 / D-4** — `DRepDetailOnchainSection` gains one section-level `DRepSourceLabel`, beside the
  `<h2>`, not one per field row (`anchor-2-PRD.md:331`).
- **R-5 / D-5** — `doNotList` is session-scoped and lazily known, because it rides the same
  per-detail-visit anchor fetch as `verifiedName`; the limitation is documented in the PRD's
  "does NOT include" section, not engineered around with a bulk prefetch (`anchor-2-PRD.md:341`).
- **R-6 / D-6** — five acceptance criteria are already satisfied on disk and are re-scoped from
  "make the edit" to "verify and record" — adding a second paragraph beside existing text is a
  defect, not a completion (`anchor-2-PRD.md:369`).
- **R-7 / D-7** — task-153's `Retired` / `Excluded from default cohort` status-badge claim is
  unimplementable: `DRepStatusBadge.tsx:26-29` is an exhaustive `Record<DRepStatus, string>` over
  `active | inactive` and AC-5 plus invariant 14 forbid a new member. The favorite keeps its real
  badge plus the inline caption and the design-doc claim is struck (`anchor-2-PRD.md:387`).
- **R-8 / D-8** — task-174's "search rows can pass through the CIP-105 form the search index already
  derives" is false — `searchDRepsByIdPrefix` discards it at `helpers.ts:129` — so `DRepIdDisplay`
  derives its own second form and the search pipeline's carrier type is not widened
  (`anchor-2-PRD.md:405`).
- **R-9 / D-9** — one verified-name source per surface: the detail view reads
  `AnchorEnrichEntry.givenName`, the confirmation dialog reads the hash-guarded, already
  sanitization-listed `entry.verifiedName` (`anchor-2-PRD.md:421`).

D-10 … D-16 (`anchor-2-PRD.md:441-540`) record the downstream applications: per-field CIP-119 length
policy replacing the blanket 80-char clamp and never truncating a `paymentAddress`; `doNotList`
riding the renamed hash-guarded projection pass; the search-result variant selected by a boolean
threaded through the existing row path; the renderer https gate lifted into a shared predicate
rather than copied; task-157 AC-2 jointly discharged with task-154 and recorded in both tracker
rows; task-156's IA rationale going to the design doc and never to in-app copy; and tracker edits
staying value-only so the tasks JSON is never reformatted.

**Doc reconciliations anchor-2 owns** (`anchor-2-PRD.md:630-667`). **Two real edits, both to
`.agent/plans/governance/drep-discovery/designs/drep-discovery-design.md`:** Edit 1 (task-153, R-7)
strikes the impossible `Retired` / `Excluded from default cohort` badge claim at `:112` and replaces
it with "its current status badge" plus an explicit note that `DRepStatus` is the closed union
`active | inactive`; Edit 2 (task-156, D-15) adds the paragraph recording that `Abstain` and
`No Confidence` are form-only sentinels with no registration, anchor, voting power or detail view,
and that the empty-state copy must therefore never point at the directory for them. **Everything
else is verify-and-record, no edit** (`anchor-2-PRD.md:658-666`): task-153 AC-9/AC-10 at `:239`,
AC-11 at `:110`, AC-12 at `:239` + `:241-245`, task-157 AC-1 at `:218`, and task-174 AC-6 at
`:251-259` — all already present on disk after task-165's inserted "Directory Identity: ID-Only in
v1" section shifted the anchors the tasks JSON still cites. Each is recorded in its task's
`statusReason`.

**Planning status.** PRD and implementation guide complete; no task escalated; no open question
carried into implementation. Next gate is the required `Critiquer:` pass over both documents, then
task-157.

**OWED at slice close — nothing here may be reported green** (`anchor-2-PRD.md:979-992`):
(1) `nix fmt` — `nix` is absent from this devcontainer, the substitute is
`node_modules/.bin/prettier --write <explicit paths>`, and the real run stays a user-owned pre-merge
obligation; (2) the Storybook visual pass and the ja-JP overflow check for the dual-ID search row
and the five new detail fields — no browser here; (3) a live anchor fetch against a real host
(inherits anchor-1 F-10); (4) the real SIPO / CIP-119 canonical vector bytes and their digest check
(inherits anchor-1 F-13); (5) `yarn check:all` — red at HEAD for a pre-existing Storybook
manager-webpack reason unrelated to any anchor-2 change; (6) the release-end `!!!` copy review, out
of scope for every slice by invariant 11.

**Docs produced.** `task-plans/anchor-2-PRD.md` (1049 lines) and
`task-plans/anchor-2-implementation-guide.md` (6937 lines — a 164-line shared preamble followed by
six task sections at `:166` task-157, `:2272` task-153, `:3404` task-174, `:4924` task-154,
`:5752` task-155, `:6185` task-156), plus this review log.

---

## Critiquer: 2026-07-30 — required review pass over the PRD + implementation guide

**Decision: `requires_changes`.** One fix pass followed; every blocker below is resolved in this
same pass. Nothing was deferred and no blocker was rejected.

**What the review found sound and left alone.** Coverage of all 30 acceptance criteria is complete,
including the five R-6 verify-and-record items and the R-7 / R-8 rewrites. The R-2 seam contract
holds — no seam is widened twice. Invariants are inlined per task section rather than referenced.
Spot-checked anchors were accurate: `DRepDetail.tsx:113`, `DRepCard.tsx:126`,
`DRepDirectoryList.tsx:88-100`, `logging.ts:62-63`, the i18n baseline 1631 / 97,
`drep-discovery-design.md:206`, `:218`, `:239`, `:247`, `:251-259`, `shared-design-tokens.md:250`,
and both sets of bech32 test vectors (re-derived with the repo's own `bech32` package; all six
literals correct). Manual checkpoints are correctly declared OWED, not green. Out-of-scope items —
pending tracker rows, absent commits, unimplemented source, the stub Final Outcome — were not
counted against the documents.

**Nine blockers, all cross-section coherence defects. Resolution of each:**

- **B1 (high, task-153, guide).** task-153's section was written against a flat
  `AnchorEnrichEntry.verified` shape (`verified.givenName` / `verified.doNotList`) while the same
  guide's task-157 Step 4b lands the nested
  `{ state: 'verified'; hash; host; content: VerifiedDRepAnchorContent }`, so Step 3's replacement
  would not compile. **Resolved:** the Step 0b two-shape table and the Step 0c flat fallback snippet
  are deleted and replaced with a single fixed-shape statement plus a STOP condition
  (guide `:2474-2500`); Step 3's projection now reads `verified.content.givenName` /
  `verified.content.doNotList`, and its notes explain the narrowing and drop the `=== true`
  coercion because task-157 types `doNotList: boolean`. Step 0's precondition greps are unchanged;
  the Step 7 note clarifying that the mock `content` is the IPC response shape is rewritten.
- **B2 (high, task-157, guide).** Step 12b told the implementer to render `DRepDetailAnchorContent`
  inside `tests/jest/security/governance-sanitization.spec.ts` with no code, while task-174 Step 12
  states that no spec under `tests/jest/` renders React. **Resolved:** Step 12b is now an explicit
  "do not add a render case here" with the reason (the file imports no React and has no
  ThemeProvider/IntlProvider harness) and a pointer to the new home (guide `:2078-2095`). The spy
  moved into `DRepDetailPage.spec.tsx` Step 11c as the fully-coded case
  `reaches no logger on either payment-address copy path` (guide `:1935`), which spies the same
  `source/renderer/app/utils/logging` sink the floor suite uses at `:40` and covers both the success
  and clipboard-unavailable branches; Step 11a gains the `rendererLogger` import. Counts corrected:
  sanitization `+3 → +2`, `DRepDetailPage.spec.tsx` `+9 → +10` (21 → 31), in both the Verify block
  and the files-touched table.
- **B3 (high, task-174 + task-154, guide).** Cumulative i18n totals ignored the build order.
  **Resolved:** every key-adding task now states its delta as the contract and its cumulative
  absolute only as a build-order expectation — task-157 `+13` (110 / 1644), task-174 `+5`
  (115 / 1649), task-154 `+2` (115 / 1651), task-155 `+1` (116 / 1652). The same correction was
  applied to the jest counts that shift for the same reason: task-174's Step 1 baseline table gains
  an "expected when you start" column (109 → 125), its Verify block switches to per-suite deltas,
  and the stale `48 → 51` / `21 → 23` / `35 → 37` step footers now carry both the alone-measurement
  and the build-order number. task-153, task-154, task-155 and task-156 sanitization-suite
  expectations were re-anchored the same way.
- **B4 (medium, task-156, guide).** Step 4b passed `verifiedName` as a bare string while task-154
  types it `VerifiedDRepNameSource | null` = `{ host, name }`, making the sentinel assertion vacuous.
  **Resolved:** the fixture is now `{ host: 'example.org', name: 'Verified Sentinel Name' }`, and the
  "if task-154 isn't on the branch the key is inert" escape clause is replaced by a STOP condition —
  task-156 runs strictly last, so a missing prop means task-154 did not land.
- **B5 (medium, task-155, PRD).** PRD D-4 put the label "beside the section `<h2>`" and the i18n
  inventory recorded task-155 as adding zero keys, while the guide adds a `Source` row plus the key
  `governance.drepDetail.onchain.source`. **Resolved:** D-4 (PRD `:332`) now records the
  Source-row placement, explicitly supersedes its own first draft, and carries the duplicate-wording
  rationale — `governance.drepDetail.onchain.title` and `governance.drepDirectory.source.onChain` are
  both `"!!!On-chain"` in en-US and both `"!!!オンチェーン"` in ja-JP, re-measured at `55e8985bf` — plus
  the duplicate-id reason for a new key rather than reusing `governance.drepDetail.anchor.source`.
  The i18n inventory row for task-155 changes 0 → 1, and the executive summary, the per-task contract
  row and seam S-7 were brought in line. **This supersedes the `beside the <h2>` wording in the
  Planner entry above, which stands unedited as the record of what was decided at planning time.**
- **B6 (medium, task-156 + task-174, guide).** The cv-2 F-31 two-anchor rule was applied with three
  different second anchors. **Resolved:** `tests/jest/governance/logDRepStateSnapshot.spec.ts` is now
  the mandatory second anchor in all six Verify blocks. task-174 keeps `_shared/DRepIdDisplay.spec`
  as an explicitly-labelled *third* run, not a substitute; task-156's duplicate
  `containers/voting/VotingGovernancePage.spec` run is replaced and annotated as not being an anchor
  for this rule.
- **B7 (medium, task-157, guide).** PRD R-7 delegated the "does a nameless parsed anchor count as
  completed metadata" decision to the guide, which never stated it. **Resolved:** new decision
  **D-H** in task-157 §5 (guide `:335`) states that a hash-matched parse counts as complete
  regardless of `givenName`, since `verifiedMetadataIds` (`GovernanceStore.ts:283-291`) keys only on
  `entry.state === 'verified'`, with the consequences spelled out for `cohortContext` eligibility and
  the task-172 High value badge, and the rejected alternative recorded. The Step 10c assertion
  `expect(store.verifiedMetadataIds.has(ANCHOR_DREP_ID)).toBe(true)` is labelled as that decision's
  proof (guide `:1710`). PRD R-7 is updated from "the guide must pin this" to a pointer at D-H.
- **B8 (low, task-153, guide).** `git restore source/renderer/app/i18n/defaultMessages.json` — the
  real path is `.../i18n/locales/defaultMessages.json`, and `git restore` aborts the whole invocation
  on an unmatched pathspec, so the three sibling paths were not being restored either.
  **Resolved:** path corrected and the abort-on-unmatched-pathspec reason recorded inline.
- **B9 (low, task-153, guide).** The preamble claimed the spec files are untouched by task-157 while
  the Verify block said the opposite. **Resolved:** the claim is narrowed to `helpers.ts`,
  `DRepDirectory.tsx`, `DRepCard.tsx` and `DRepDirectoryList.tsx`, followed by an explicit list of
  the three files task-157 *does* move (`DRepDetailPage.spec.tsx`,
  `tests/jest/governance/GovernanceStore.spec.ts`, `DRepDetail.stories.tsx`). The Step 6 table marks
  `DRepDetailPage.spec.tsx:37` and `DRepDetail.stories.tsx:66` as re-locate-by-quoted-text, the
  wire-type "do not touch" list flags the shifting `GovernanceStore.spec.ts` sites, and the Verify
  preamble now names the three suites whose counts move and by how much.

**Documents after the fix pass.** `anchor-2-PRD.md` 1080 lines (was 1049);
`anchor-2-implementation-guide.md` 7105 lines (was 6937) — six task sections at `:166` task-157,
`:2359` task-153, `:3511` task-174, `:5058` task-154, `:5898` task-155, `:6340` task-156. No source
file was touched, no commit was made, and the tasks JSON is unchanged.

**Gate.** Both documents are cleared for implementation. Next step is task-157.

---

## Post-fix verification: 2026-07-30 — independent re-check of the fix pass

A dedicated verifier re-read both documents against the live worktree at `55e8985bf`, independently
of the fix agent's report, and confirmed all nine critique blockers (B1–B9) genuinely resolved. Two
low-severity residuals survived, both of the same class the critique had flagged — build-order-blind
absolutes and a stale re-location key — and both were applied by the orchestrator:

- **B10** — task-174's sanitization-floor Verify run carried the absolute `35 -> 37 tests`, computed
  as if task-174 ran alone. In build order task-157 has already taken the suite to 37, so the run
  prints 39. Rewritten to the delta form used everywhere else in the guide, with the Step 1
  measurement named as the authority.
- **B11** — task-153's seam E quotes `? state.givenName` under a section preamble instructing the
  implementer to re-locate anchors by quoted code rather than by line number. task-157 Step 4d
  rewrites exactly that line to `? state.content.givenName` first, so for this one seam the
  re-location key itself is stale. A note now directs the implementer to locate
  `_applyVerifiedNames` by name and replace it whole, and records that seams A–D are genuinely
  untouched and do match verbatim.

The verifier also confirmed the small-model bar is met: a scan for hedging language ("in the style
of", "as appropriate", "left to the implementer", "TODO") returns nothing, every task section
carries full code deltas, quoted seams, measured test baselines and explicit STOP conditions on
cross-task preconditions. One record-only trivium: guide `:338` cites `verifiedMetadataIds` at
`GovernanceStore.ts:283-291` where the computed begins at `:285`; the symbol is named in the same
sentence and the PRD has it right, so no fix was made.

**Gate.** Planning is approved. Build proceeds in the locked order
task-157 → task-153 → task-174 → task-154 → task-155 → task-156.

---

## Code Review: task-157 — round 2 (2026-07-30)

**Verdict: approved, zero surviving blockers.** Two rounds over the uncommitted task-157 diff — 16
modified source/test/i18n files plus the new `source/renderer/app/utils/governance/isHttpsUrl.ts` —
against `anchor-2-implementation-guide.md:166-2271` (Steps 1–15b). The task was built in three
implementer shards (Steps 1–5, 6–8, 9–14); each reported all steps landed, no STOP condition fired,
and no shard formatted, staged, committed or wrote to the tracker.

### Blockers

**None survived to round 2.** Round 1's findings are **not transcribed in this log** and are not
reconstructed here — this entry records only what is provable from the worktree. Two round-1 defects
are provable, because both are items the verifier proved RED and both are closed in the tree:

- **The three `no-continue` errors — DISCHARGED.** `git show HEAD:source/main/governance/AnchorVerificationService.ts | grep -n continue`
  exits 1 (zero matches) while the verifier measured three in the working tree at `:82:25`, `:96:7`
  and `:100:64`, the only errors in the entire `yarn lint` run. The guide's Step 15b expects exit 0
  with 0 errors. The loops were rewritten; `eslint` on that file now exits 0 with one
  `prefer-destructuring` warning, and `AnchorVerificationService.spec.ts` re-runs at 21/21 after the
  rewrite.
- **The un-regenerated message catalogs — DISCHARGED.** The verifier found
  `source/renderer/app/i18n/locales/defaultMessages.json` and `translations/messages.json` clean at
  the point it ran `yarn i18n:manage`, i.e. never regenerated, although the guide's file table lists
  `defaultMessages.json` as regenerated by that gate and both `351467833` (task-151) and `74bf92cdd`
  (task-172) carry the pair. The verifier restored both, correctly, to return the tree to the state
  the implementer left it in. They are now in the diff at **+65 lines each** — exactly the 13 new
  keys, no unrelated churn, no whitelist JSON touched.

### Minor

- **Guide-internal contradiction, resolved toward the asserted behaviour.** Step 6's JSX defines the
  `paymentAddressCaption` descriptor and Step 8 requires its catalog key, but the JSX never renders
  it — while the guide's own Step 11 spec (`guide:1913-1918`) asserts that exact string is in the
  document. Following Step 6 literally would ship a dead key and a guaranteed red test. The caption
  renders as `<p className={styles.mutedValue}>` between the payment-address heading and the value
  row, mirroring the identity-references caption-before-content ordering the guide mandates.
  Independently corroborated by `drep-discovery-design.md:226`. Recorded as F-1 in
  `research/anchor-2-findings.md`.
- **Deliberate asymmetry with the `DRepIdDisplay` precedent, accepted.** The payment-address copy
  handler carries no `logger.warn` on either the missing-clipboard or the rejected-write path,
  unlike `DRepIdDisplay.tsx:52` and `:62`. That is the sanitization floor, not an omission, and the
  guide's `reaches no logger on either payment-address copy path` test depends on it. Recorded as
  F-2.
- **Formatting-only deviations from the guide's verbatim blocks, accepted.** Three one-line
  collapses that are exactly what prettier produces (`qualifications: readCip119String(...)`, the
  `Buffer.from(JSON.stringify({ body: { doNotList: true } }))` fixture, and the two migrated
  detail-page fixtures folding to `governanceOverrides: { anchorStateByDRepId: verifiedState() }`).
  Semantics identical.
- **No task ids, review labels, ALL-CAPS emphasis or change history** in any comment or test name
  across the changed paths. No local `IntlProvider` and no per-locale story variant was added to
  `DRepDetail.stories.tsx`; the new `'Verified — prose only'` option rides the existing global
  English/Japanese toggle.

### Verifier's verdict

**RED at the time it ran** — one this-task failure and one this-task contract gap, both since
discharged above, plus one inherited failure it proved was not this task's:

- **Inherited, not this task's.** `prettier --check` warns on `source/common/utils/logging.ts`,
  `tests/jest/governance/GovernanceStore.spec.ts` and `tests/jest/security/governance-sanitization.spec.ts`.
  The verifier proved this pre-existing by writing each HEAD version to a **sibling path inside the
  same directory** so `.prettierrc` resolves identically — it explicitly discarded a first result
  taken under `/tmp`, which gives a false clean — and the HEAD copies reproduce the *same three
  hunks*: the `.reduce(…)` hug, the long-URL `url:` break, and the `((x as unknown) as jest.Mock)`
  double-paren. `git diff` confirms this task touched none of those regions; its `logging.ts` change
  is six array entries. The sibling files were deleted.
- **Green, every count matching the guide exactly.** `yarn compile` exit 0. Nine jest gates:
  AnchorVerificationService 13 → 21, GovernanceStore 49 → 51, DRepDetailPage 21 → 31 with snapshots
  unchanged at 2, governance-sanitization 35 → 37, logDRepStateSnapshot 5 → 5,
  governanceAnchorChannel 2 (file untouched), i18n copy markers 5, and the unfiltered
  `node_modules/.bin/jest --runInBand` at 91 passed + 1 skipped of 92 suites / 1272 passed + 12
  skipped of 1284 tests / 10 snapshots — the one skip being the environment-gated
  `GovernanceCliArgvSmoke`. i18n: `governance.*` 97 → 110 and whole-catalog 1631 → 1644 in both
  catalogs, 0 unmarked governance keys. Every baseline was re-derived from `git show HEAD:<path>`
  rather than taken on trust.
- **Never run.** `yarn storybook` — an interactive visual check, outside a headless gate, and
  `yarn storybook:build` is red at HEAD for reasons unrelated to this branch. The Storybook visual
  and ja-JP overflow pass over the new prose, reference and payment-address rows stays owed.

### Owed at close

The close-out commit `feat(gov): task-157 render the remaining verified CIP-119 profile fields` is
unmade. `nix fmt` cannot run in this devcontainer and stays a user-owned pre-merge obligation that
will also settle the three HEAD-drifted files. AC-2's delegation-confirmation half is not discharged
here at all and rides with task-154. AC-1's image deferral was verified present at
`drep-discovery-design.md:218` with no design-doc edit required; the AC's own `:215` citation is
stale, the render list sits at `:216`. No live anchor fetch has ever run, so every widened parse
path is proven only against fixture bytes and a mocked transport.

**Gate.** task-157 is closed. Next in the locked build order is task-153.

---

## Code Review: task-153 — round 1 (2026-07-30)

**Verdict: approved, zero blockers raised.** One round over the uncommitted task-153 diff — 14
modified files, no new files — against `anchor-2-implementation-guide.md` Steps 0–13. The task was
built in two implementer shards (Steps 0–6, Steps 7–12); both reported every step landed, no STOP
condition fired, and neither formatted, staged, committed or wrote to the tracker.

### Blockers

**None.** The diff matches the guide's locked shape: `doNotList: boolean` projected onto
`AppDRepDirectoryEntry`, `_applyVerifiedNames` renamed to `_applyVerifiedMetadata` carrying both
fields with the identity short-circuit intact, a single `!entry.doNotList &&` clause added to
`defaultCohort` after the top-exclusion slice, and `isStaleFavorite` returning
`entry.doNotList || STALE_FAVORITE_STATUSES.has(entry.status)`. Every negative invariant holds and
was re-checked by grep rather than taken on trust: `showAllList`, `drepIndex`, `top35DRepIds`,
`displayedDRepList`, the search index and the comparators are untouched; `DRepStatus` is still the
closed `'active' | 'inactive'` union; no favorite is purged on any path; `doNotList` reaches no
logger, no analytics call and no electron-store write; and no wire field, parser rule or IPC change
was added — the flag is consumed from the `VerifiedDRepAnchorContent` shape task-157 already
widened.

### Minor

- **Design-doc conflict at `:112`, resolved by striking rather than by shipping.** The line promised
  `Retired` and `Excluded from default cohort` status badges for stale favorites. Both are
  unimplementable against the closed `DRepStatus` union, and AC-5 of the acceptance list bans adding
  a member to it. `:112` was rewritten to say the caption alone carries the signal and that
  `Retired` stays deferred until a distinct unregistration signal exists. The design-doc diff is
  exactly one insertion and one deletion. Recorded as F-8 in `research/anchor-2-findings.md`.
- **Guide-internal count error, record-only.** Step 6's heading says "14 construction sites" while
  its own table enumerates 15. The implementer followed the table, which is the authority; the
  typecheck is the real gate and it exits 0.
- **A `grep -c "doNotList: false"` over the swept files returns 18, not 15.** Three of those
  occurrences are pre-existing task-157 fixtures — `DRepDetailPage.spec.tsx:56` and
  `DRepDetail.stories.tsx:75, :92` — not this task's. A future reader auditing the sweep by count
  alone will mis-read it.
- **The AC's own design-doc citations had drifted before this task started.** AC-9/AC-10 cite `:228`
  and AC-11 cites `:109`; the live anchors are `:239` and `:110`. All three substantive requirements
  were already satisfied, so they were verified by grep and left unedited rather than re-written.
- **No task ids, review labels, ALL-CAPS emphasis or change history** in any comment or test name
  across the changed paths. The rewritten `Favorites view — stale favorite` story drops the injected
  `isStaleFavoriteEntry` and rides the real flag plus the existing global English/Japanese toggle; no
  local `IntlProvider` and no per-locale variant was added.

### Verifier's verdict

**GREEN on the test, lint, typecheck, format and i18n gate — zero this-task failures and zero
inherited failures surfaced — with one unmet guide requirement at the time it ran.**

- **Every count matched the guide's predicted delta**, with each baseline re-measured after task-157
  rather than read off the `55e8985bf` table: `helpers.spec` 25 → 26, `DRepDirectory.spec` 48 → 52
  with the snapshot unchanged at 1, `DRepDirectoryPage.spec` 8 → 9, `GovernanceStore.spec` 51 → 56,
  `governance-sanitization` 37 unchanged, `logDRepStateSnapshot` 5 unchanged, i18n copy markers 5
  unchanged. `typed-scss-modules` then `tsc --noEmit` exits 0, which is the authority for the Step 6
  fixture sweep. The unfiltered `node_modules/.bin/jest --runInBand` is 91 passed + 1 skipped of 92
  suites and 1283 passed + 12 skipped of 1295 tests with 10 snapshots, the one skip being the
  environment-gated `GovernanceCliArgvSmoke`. `yarn lint` exits 0 with 0 errors. `yarn i18n:manage`
  exits 0 and wrote nothing — all four catalogs byte-identical by md5 — so no `git restore` was
  needed. `yarn storybook` builds the preview clean.
- **Inherited, not this task's.** `prettier --check` flags exactly
  `tests/jest/governance/GovernanceStore.spec.ts`,
  `storybook/stories/governance/DRepDirectory.stories.tsx` and
  `storybook/stories/governance/_utils/fixtures.ts` — precisely the trio the guide names as carrying
  pre-existing HEAD drift, and no others. The ten guide-listed paths are clean.
- **The one unmet requirement, now discharged.** The verifier found
  `governance-drep-discovery-plan-tasks.json` unmodified: neither the Step 13b task-122
  re-verification row nor the Step 13c task-153 completion row had been written. The evidence Step
  13a asks for was already green — the combined
  `helpers.spec`/`DRepDirectory.spec`/`DRepDirectoryPage.spec` run at 3 suites / 87 tests / 1
  snapshot, and `isStaleFavoriteEntry` absent from `DRepDirectoryPage.tsx` — so the gap was only that
  the rows were unwritten. Both rows are written in this pass.

### Owed at close

The close-out commit `feat(gov): task-153 exclude doNotList DReps from the default cohort` is unmade.
The Storybook visual pass over the rewritten stale-favorite story — exactly one of two cards
captioned, then the ja-JP toggle — never ran: this devcontainer has no browser, and
`yarn storybook:build` is red at HEAD for reasons unrelated to this branch. `nix fmt` cannot run here
and stays a user-owned pre-merge obligation that will also settle the three drifted files. AC-1's
stated limitation is carried, not closed: the anchor fetch is lazy and per-detail-visit, so an
unvisited `doNotList: true` DRep stays in the default cohort and an unvisited `doNotList: true`
favorite shows no caption — best-effort courtesy, not a privacy control (F-7).

**Gate.** task-153 is closed. Next in the locked build order is task-174.

---
