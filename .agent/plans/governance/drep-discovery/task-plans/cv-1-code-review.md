# CV-1 Code Review Log

> Append-only transcript: `Planner:` entries (planning open/close), one `Critiquer:` entry
> (required review pass over the PRD + guide), and per-task `Code Review:` entries.
> Companion docs: [cv-1-PRD.md](./cv-1-PRD.md) ·
> [cv-1-implementation-guide.md](./cv-1-implementation-guide.md)

---

## Planner: 2026-07-27 — cv-1 planning complete (status: in_review)

**Scope planned.** Ten tasks, forced order 126 → 127 → 128 → 129 → 130 → 131 →
132 → {133, 135} with 134 after {130, 131}; all classified `autonomous` (none is
in the locked non-autonomous set, and no blocking decision survived planning).
cv-1 opens Track V: task-126 commits four authored cardano-wallet fixtures
(voting-DRep / delegating_and_voting / abstain / no_confidence, wire-shaped,
`next` as array); task-127 fixes the latent `'voting_and_delegating'` →
`'delegating_and_voting'` wire-literal bug (constant export name preserved,
pin spec added); task-128 widens `WalletDelegation`/`WalletNextDelegation` with
`voting?: WalletVotingTarget` (importing the existing `DRepIdentity`); task-129
adds the pure HRP-preserving `normalizeDRepIdentity` helper + spec; task-130
maps `delegation.active.voting` through `_createWalletFromServerData`
(`parseVoting` + 4-way status switch, voting-only ⇒ `delegatedStakePoolId =
null`, sanitized HRP-only warning, mapper exported for specs); task-131 adds
`Wallet.votingTarget` (@observable) + `currentVote`/`isVoting` computeds AND
the `update()` pick-list entry; task-132 renders the four `CurrentVoteSummary`
CORE states only (no live badge, no anchor display, component-local intl
labels); task-133 adds the 4-knob Storybook entry (global EN/JA toggle);
task-134 lands mapper/computeds/snapshot Jest coverage and re-asserts the
sanitization floor; task-135 lands the 12 `voting.governance.currentVote.*`
core keys in both catalogs with `!!!` markers and ja-JP drafts. Everything is
renderer-only: no new IPC channel, no new cardano-wallet endpoint, no
signing-path change, no WalletsStore polling change.

**Numbered decisions applied (binding; full text in the PRD).**
- **D-1** — Wire-literal fix owned by task-127; plan :152 reconciled from
  task-128 to task-127 (tasks JSON authoritative). Recorded as F-1.
- **D-2** — Task-count drift (plan "58 tasks / 13 phases" vs JSON 69/14)
  record-only. Recorded as F-2.
- **D-3** — Live conventions over stale doc names: `.spec.ts(x)` (never
  `.test.ts`), floor suite is `tests/jest/security/governance-sanitization.spec.ts`,
  Storybook uses the global English/Japanese toggle (no local `IntlProvider`,
  no per-locale variants). Recorded as F-3.
- **D-4** — `DRepSourceLabelVariant` cannot express the new status labels;
  `CurrentVoteSummary` renders them via a component-local react-intl message
  set, reusing `DRepSourceLabel` unchanged only for the on-chain source label.
  Never English-literal fallback.
- **D-5** — Fixtures are AUTHORED from the pinned cardano-wallet v2026-05-11
  swagger (commit c642e0779676d2567e3d5fa1e2db9f029b6398e1) + live `ApiWallet`
  consumption — no running wallet, no network in this devcontainer.
- **D-6** — `DRepIdentity` already exists at
  `source/common/types/governance.types.ts:20-31`; cv-1 imports it, never
  redefines it.
- **D-7** — `update()` pick list is in-scope explicitly: task-131 extends BOTH
  `WalletProps` (done by task-130's pass-through) and the pick list, or polls
  silently drop `votingTarget`.
- **D-8** — No hand-invented bech32: every vector is repo-sourced or
  synthesized with the `bech32` library and checksum-verified before commit.
- **D-9** — `delegation.next` modeled as an ARRAY (consumer `last(next)` wins
  over the singular `AdaWallet` type); fixing the type itself is out of scope.
- **D-10** — task-134's `Wallet.pendingVote` mention is description drift: no
  new computed; the "pending" case is the `wallet-delegating-and-voting.json`
  fixture's non-empty `next` entry.

**Risks carried into the build phase (PRD R-1…R-5).** Literal-fix blast radius
(mitigated by constant-name preservation + pin spec + zero-stale-literal grep);
silent stale `votingTarget` on poll (pick-list entry + update() propagation
specs); bech32 fixture validity (D-8 decode checks); Node v24 gate flakiness
(`node_modules/.bin/tsc --noEmit` fallback, env failure never read as code
failure); ja-JP overflow (task-135 Storybook review). Guide-authoring pass
additionally verified empirically that importing `api.ts` under jest needs
`jest.mock` of BOTH `utils/logging` AND `api/utils/request` (`request.ts:20-21`
constructs `global.https.Agent` at module scope; jest defines
`globals.environment` but not `https`) — the task-134 section pins the working
recipe. Record-only observation for a later slice: `DRepCategoryBadge`,
`DRepDetail`, and `DRepDirectoryBanner` stories exist under
`storybook/stories/governance/` but are not imported by
`storybook/stories/index.ts`, so they never render; cv-1 registers its own new
story explicitly and does not silently fix the siblings.

**Verification contract.** After every task: `yarn compile` (fallback
`node_modules/.bin/tsc --noEmit`) zero errors; focused Jest via
`yarn test:jest <spec> --runInBand`; sanitization floor suite re-asserted green
after task-130 and task-134; `yarn i18n:manage` clean after task-135;
`node_modules/.bin/typed-scss-modules source/renderer/app` for the task-132
scss module; prettier only on files cv-1 creates; one subject-only
Conventional Commits line per task with explicit paths staged.

Planning status set to `in_review` — awaiting the required Critiquer pass.

---

## Critiquer: 2026-07-27 — required planning review of cv-1 PRD + implementation guide

**Scope reviewed.** `cv-1-PRD.md` and `cv-1-implementation-guide.md` in full,
against the cv-1 phase of `governance-drep-discovery-plan-tasks.json`
(:804-995), the parent plan, design/ux/research docs, and live code at
`b900b99b3`.

**Checks performed and results.**

- **Coverage / verbatim ACs.** All ten tasks (task-126 … task-135) have PRD
  contracts and full guide sections. All 42 acceptance criteria are quoted
  verbatim from the JSON (3+4+4+7+5+4+6+2+4+3 = 42; spot-compared per task —
  no paraphrase drift found).
- **Consistency with tasks JSON + locked invariants.** D-1…D-10 all verified:
  plan :152 now reads task-127 (F-1 reconciled); JSON metadata is 69 tasks /
  14 phases (:16, :1729-1730); `DRepIdentity` exists once repo-wide at
  `governance.types.ts:20-31` with `DRepId = string` (no branding issue);
  whitelist files are empty arrays at
  `source/renderer/app/i18n/locales/whitelist_*.json`; findings F-1…F-3
  present in `research/cv-1-findings.md`.
- **Bech32 vectors (all six decoded with bech32@2.0.0).** KEY_CIP129: drep,
  29 bytes, header 0x22, credential == KEY_CREDENTIAL_HEX == KEY_CIP105
  payload. SCRIPT_CIP129: drep, 29 bytes, header 0x23, credential ==
  SCRIPT_CREDENTIAL_HEX == SCRIPT_CIP105 payload. Deprecated vector: drep
  HRP, 28 bytes (valid rejection vector). POOL: pool, 28 bytes of 0x03.
  Fixture bodies: `delegation.next` is an array in all four; wallet ids are
  synthetic repeated-digit 40-char hex; key-vector provenance
  `research/slice-3-findings.md:116-119` confirmed.
- **Live-code anchors.** Far more than five spot-verified — all exact:
  `Wallet.ts:33-43/:42/:161-164/:172-174/:177-201/:239-247`;
  `types.ts:80-84/:105-114` (+ `:45` singular `next`, `:53` `isLegacy`);
  `api.ts:3010/:3051-3056/:3057-3063/:3089-3110/:3013-3027/:6-9/:91/:99/:918`;
  `DRepIdDisplay.tsx:28-32/:35-37/:74 (aria-label carries full id)/:98`;
  `DRepSourceLabel.tsx:18-24` (accepts `className`; on-chain label is
  `!!!On-chain` in `en-US.json:354`); `jest.config.js:63-66/:129/:156/:203`
  (scss handled by `jest-css-modules-transform`); `package.json:43/:45/
  :52-55/:73/:204`; `tsconfig.json:14/:79-81/:103` (+ `resolveJsonModule`
  on); `storybook/main.ts:8`; `stories/index.ts:17`; `preview.tsx:8`
  (global `StoryWrapper` decorator — the EN/JA toggle claim holds);
  `request.ts:20-21` (`global.https.Agent` at module scope — the two-mock
  jest recipe is necessary and sufficient per static analysis:
  `cardano.ipc` channel construction touches no globals);
  `declaration.d.ts:11`; `.gitignore:141` (`*.scss.d.ts`);
  `governance-sanitization.spec.ts:64`; ja-JP anchors :354/:944/:953-954/
  :958/:962. The record-only observation (three unregistered governance
  story files) reproduced exactly.
- **cv-1/cv-2 boundary.** No badge (negative greps + spec assertions), no
  VotingPowerDelegation edits (byte-identical check pinned), no
  `sameVoteHint`/`status.*` keys, no anchor-derived display. The
  `drep.viewDetails`/`drep.anchorMetadata` messages are defined-not-rendered,
  which task-135 AC-1 ("DRep link labels") itself demands — not a leak.
  Reserved `previousVote`/`newVote` keys confirmed absent from catalogs and
  code, so the task-135 Step 4 grep behaves as written.
- **i18n inventory.** All 12 ids and en-US copy match
  `current-vote-display-ux.md:154-186` verbatim; every string keeps the
  leading `!!!`; ja-JP drafts reuse reviewed catalog vocabulary.
- **Tests.** Pin spec, normalizer spec (three prefixes + sentinel/deprecated/
  checksum rejections), mapper spec (five cases + sanitized-warning spys +
  byte-identity case), computeds spec (all kinds + null + never-set +
  `update()` both directions — the executable R-2 mitigation), four
  snapshots with negative badge/anchor assertions, floor suite re-asserted.
  No gap found.
- **Small-model bar.** Met: every new file has exact contents; every edit
  pins a verified quoted seam; commands are exact; judgment calls are
  resolved inline as numbered decisions or "do not revisit" blocks.

**Non-blocking notes (no action required before build).**

1. task-129's JSON *description* says the helper classifies the
   `abstain`/`no_confidence` sentinels; the guide has the normalizer reject
   them and the mapper handle them first — matching the design pseudocode
   (design :122-146) and the ACs (which require only the three prefixes +
   invalid input). Same species of description drift as D-10; the guide pins
   it in the task-129 notes. Record-only.
2. The Storybook visual pass (task-133) and ja-JP overflow review (task-135)
   remain human-eye checks; the guide provides automated floors
   (`storybook:build`, wrap-only scss, text/snapshot assertions) and an
   explicit fallback to record a main-checkout follow-up here rather than
   skip. Correctly surfaced, not hidden autonomy.
3. Cosmetic: PRD cites the `update()` pick list as `Wallet.ts:177-200`, the
   guide as `:177-201`; immaterial under the guide's re-anchor-by-content
   rule.

**Blockers.** None.

Decision: approved

---

## Planner: 2026-07-27 — critique clean, planning approved

Critiquer pass returned zero blockers; `cv-1-PRD.md` and
`cv-1-implementation-guide.md` verified present and coherent (all ten task
sections, matching AC counts and anchors) — PRD planning status flipped to
`approved`; build phase may start.

---

## Code Review: task-126 — iteration 1 (2026-07-27)

**Scope reviewed.** Working tree vs guide section "task-126: Commit
cardano-wallet voting/delegating fixtures"
(cv-1-implementation-guide.md:121-429) + Cross-Cutting notes (:52-119) +
task-126 acceptance criteria in
governance-drep-discovery-plan-tasks.json. Working-tree state at review time:
`git status --porcelain -uall` shows exactly four untracked files under
`tests/mocks/wallets/` and zero tracked modifications — no runtime, logging,
analytics, or store code was touched anywhere in the tree.

**Checks performed (all commands run from the worktree root).**

1. **Guide conformance / doc drift.** Extracted the four ```json blocks from
   the task-126 section and compared programmatically against the four files:
   all four are BYTE-IDENTICAL to the guide (wallet-voting-drep.json,
   wallet-delegating-and-voting.json, wallet-voting-abstain.json,
   wallet-voting-no-confidence.json). No drift; nothing extra created
   (directory contains exactly the four specified files).
2. **ApiWallet-shape correctness.** Every field present is one the mapper
   consumes: `id` (api.ts:3014), `address_pool_gap` (:3015), `name` (:3017),
   `balance.available/total/reward` as `{ quantity, unit: "lovelace" }`
   (:3028-3049), `assets.available/total` as arrays (:3065-3088),
   `delegation.active.{status,target,voting}` (:3051-3056; `voting` consumed
   once task-130 lands), `delegation.next` consumed via `last(next)`
   (:3057-3059), `state` (:3021), `discovery` (:3023). Wire keys are
   snake_case only; no Daedalus-injected `isLegacy`/`isHardwareWallet` (both
   default to `false` in the destructure, api.ts:3022/:3024); `passphrase`
   omitted per guide (read via `get(..., null)` at :3027). `delegation.next`
   is an ARRAY in all four fixtures (D-9; the singular
   `AdaWallet['delegation']['next']` at types.ts:44-45 is the documented
   pre-existing mismatch, out of cv-1 scope). The pending-next entry carries
   only `status`/`target`/`changes_at` per the wire shape — nothing
   decorative.
3. **Guide Step 6 verification loop** (id regex `^([0-9a-f])\1{39}$`, `next`
   is array, required top-level keys, balance triple well-formed): `ok` for
   all four files.
4. **Bech32 checksum validity (D-8).** Decoded with the repo's `bech32`
   package: `drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy` →
   prefix `drep`, 29-byte payload, header byte `0x22`;
   `drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l` → prefix
   `drep_vkh`, 28 bytes, and its payload is byte-equal to the CIP-129
   vector's payload minus the header (shared credential
   `a1b2c3d4…293a4b5c`, matching the guide's credential-hex row);
   `pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2` → prefix
   `pool`, 28 bytes, all `0x03`. All checksums valid.
5. **Synthetic wallet ids.** `1…1` / `2…2` / `3…3` / `4…4` × 40 hex — no real
   ids.
6. **Formatting.** `node_modules/.bin/prettier --check
   "tests/mocks/wallets/*.json"` → "All matched files use Prettier code
   style!". No pre-existing file was reformatted (tracked diff is empty).
7. **Invariant sweep.** (2) Sanitization floor: fixtures contain DRep ids,
   which is explicitly allowed; no logger/analytics/electron-store path was
   touched (tracked tree clean), and the floor suite at
   tests/jest/security/governance-sanitization.spec.ts is unmodified. (13)
   `abstain`/`no_confidence` appear only as `delegation.active.voting` wire
   sentinels, never as directory entries. (14) No component code exists yet,
   so no badge could be rendered. (4)/(9)/(10)/(11): no code, signing-path,
   or i18n surface touched — not applicable to this task.

**Acceptance criteria.** AC-1 met (voting-DRep / delegating_and_voting /
abstain / no_confidence all covered, with the CIP-129 form on the voting
fixture and the CIP-105 `drep_vkh` form plus pool target and non-empty
pending `next` on the dual fixture, per D-10). AC-2 met (shape conformance as
in check 2). AC-3 met (minimal field set, synthetic ids). All five guide
acceptance boxes verified.

**Findings.** None. No blockers, no advisories.

Decision: approved

---

## Code Review: task-127 — iteration 1 (2026-07-27)

**Scope reviewed.** Working tree vs guide section "task-127: Fix latent
delegating_and_voting literal mismatch"
(cv-1-implementation-guide.md:443-559) + task-127 acceptance criteria in
governance-drep-discovery-plan-tasks.json:846-849. Working-tree state at
review time: four tracked modifications plus one new untracked spec —
`git diff --stat` = 4 insertions / 4 deletions across four files, and
`tests/jest/api/walletDelegationStatuses.spec.ts` (new, 17 lines).

**The diff.**

- `source/renderer/app/api/wallets/types.ts:84` — union member
  `'voting_and_delegating'` → `'delegating_and_voting'`. Only the last member
  changed; `:81-83` untouched.
- `source/renderer/app/domains/Wallet.ts:42` —
  `VOTING_AND_DELEGATING: 'delegating_and_voting',`. Key name, the type
  annotation block (`:33-38`) and every sibling entry untouched.
- `tests/jest/api/walletDelegationStatuses.spec.ts` — new pin spec, matching
  the guide's prescribed content (cv-1-implementation-guide.md:519-534).
- `.agent/system/api-endpoints.md:50` — authorized doc fix; single-line value
  swap under the heading "Delegation status values in
  `delegation.active.status` include:" (`:46`), which is correctly scoped to
  wallet delegation, not DRep status. It is the only occurrence of the status
  enum in that file; `:47-49` untouched.
- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md:335`
  — authorized attribution fix, `task-128` → `task-127` and nothing else on
  the row; now agrees with the already-reconciled `:152` (F-1).

**Invariant sweep.** (10) Byte-equality: the whole diff is two string VALUES;
no bech32 helper, no signing path, no `vote.id` construction is in it. The
literal is inbound-only — `source/renderer/app/api/api.ts:3054-3056` reads
`get(active, 'status', null)` and passes it through verbatim to
`delegationStakePoolStatus`; the only comparisons go through the constant
(`Wallet.ts:244-245`, `WalletRow.tsx:191`). No outbound cardano-wallet request
payload embeds a `DelegationStatus` value, so no wire bytes change.
`grep -rn "delegating_and_voting" source/` returns exactly the two edited
lines. (2) Sanitization floor: zero runtime code and zero
logger/analytics/electron-store calls added; no `delegationStakePoolStatus`
reference exists in `source/main` or `source/common`;
tests/jest/security/governance-sanitization.spec.ts is unmodified and green
23/23. (14) `DelegationStatus` (`types.ts:80-84`) remains the
wallet-delegation union; nothing introduces or conflates DRep
`active|inactive` status. Guide-inlined rule: the constant NAME
`VOTING_AND_DELEGATING` is byte-unchanged at all three sites (`Wallet.ts:37`
annotation, `:42` key, `:245` consumer), and `isDelegating` (`:244-245`) was
correctly NOT edited — it consumes the constant and inherits the fix. No IPC
or contract drift: `DelegationStatus` never crosses an IPC boundary, and
`tests/delegation/e2e/steps/delegation-pending.ts:20` maps only
delegated/undelegated. `storybook/stories/staking/DelegationCenter.stories.tsx`
and `WalletRow.tsx` consume the constant and inherit the fix with no edit.

**Wire-literal correctness.** The new value is byte-identical to the
task-126 fixture at `tests/mocks/wallets/wallet-delegating-and-voting.json:25`
(`"status": "delegating_and_voting"`), which is the only in-repo artifact
claiming wire truth.

**Verification commands run (results as observed).**

1. `yarn compile` → exit 0 (precompile `typedef:sass`, then `tsc --noEmit`,
   no diagnostics). The gitignored `.scss.d.ts` regeneration did not dirty the
   tracked tree.
2. `yarn lint` → exit 0, 5591 warnings, 0 errors — the documented pre-existing
   baseline; the task introduces none. (`package.json:43` scopes lint to
   source/storybook/utils, so the new spec under `tests/` is out of its
   scope — same as the existing tests/jest/security spec.)
3. `yarn test:jest tests/jest/api/walletDelegationStatuses.spec.ts
   --runInBand` → exit 0, 1 suite / 2 tests passed. AC-3 satisfied.
4. `yarn test:jest tests/jest/security/governance-sanitization.spec.ts
   --runInBand` → exit 0, 23/23 passed — unchanged from baseline.
5. `yarn test:jest --runInBand` (full tree) → exit 0; 80 suites passed, 1
   skipped; 1030 tests passed, 12 skipped; 2 snapshots passed; 0 failures.
6. `node_modules/.bin/prettier --check` on all five touched files → exit 0,
   "All matched files use Prettier code style!".
7. `grep -rn "voting_and_delegating" source tests storybook` → zero hits
   (AC-4 satisfied). Repo-wide, the surviving hits are all `.agent/`
   planning prose narrating the bug, including task-127's own tracker
   description and acceptance criteria
   (governance-drep-discovery-plan-tasks.json:837,846,849) — removing those
   would destroy the task record.
8. `yarn i18n:manage` → not run, deliberately: the task changes no copy and
   adds no message ids.

**Acceptance criteria.** AC-1 met (`types.ts:84`). AC-2 met (`Wallet.ts:42`,
constant name preserved). AC-3 met (new pin spec, 2/2 green). AC-4 met — AC-4
is scoped to "the renderer codebase", which is clean.

**Findings (five nits, zero blockers).**

1. The pin spec is self-referential with respect to the wire contract: it
   hardcodes `'delegating_and_voting'` on both sides, so it proves
   regression-resistance but not wire-truth. **Resolution:** accepted as-is;
   the guide prescribed this exact content
   (cv-1-implementation-guide.md:519-534). Anchoring the assertion to
   `tests/mocks/wallets/wallet-delegating-and-voting.json` instead
   (`tsconfig.json:38` sets `resolveJsonModule: true`) is recorded as an
   optional strengthening for task-134.
2. No test covers the behavioral consequence — `Wallet.isDelegating`
   (`Wallet.ts:239-248`) returning true for a pool-and-DRep wallet.
   **Resolution:** deferred by design; computeds are task-134's scope
   (governance-drep-discovery-plan-tasks.json:970). This is the reason the
   tracker status is `complete`, not `verified`.
3. The wire value cannot be independently verified in this devcontainer — no
   cardano-wallet swagger is vendored and there is no live wallet, and the
   task-126 fixture was itself authored from the swagger shape rather than
   captured live (cv-1-PRD.md:58). Fixture and code therefore agree by
   construction. **Resolution:** flagged pre-merge — confirm against the
   pinned cardano-wallet v2026-05-11 `ApiWalletDelegationStatus` enum in an
   environment that has it. Not a blocker: the whole approved planning chain
   (design :78/:136, PRD :79-80, guide :443-478, tracker :846-847) is
   internally consistent on the value.
4. `nix fmt` could not be run (no nix in this devcontainer);
   `node_modules/.bin/prettier` 2.1.2 was substituted and is clean.
   **Resolution:** flagged for a pre-merge `nix fmt` pass; recorded as F-5.
5. Scribe steps were outstanding at review time (expected — review precedes
   scribe). **Resolution:** closed by this entry plus the tracker update to
   `status: complete`. Note the task prompt's anchor `:826-841` for the
   task-127 object was imprecise; pre-scribe the object spanned
   `:834-851` (id at `:835`, ACs at `:846-849`).

**Comment convention.** The single comment
(`tests/jest/api/walletDelegationStatuses.spec.ts:3-4`) is two plain lines
stating the why; no task id, no review label, no ALL-CAPS, no change history.

**No unnecessary complexity.** No new abstraction, helper, or rename — the
smallest truthful change. No file was reformatted; no stray artifacts.

Decision: approved

---

## Code Review: task-128 — iteration 1 (2026-07-27)

**Scope reviewed.** Working tree vs guide section "task-128: Widen
`WalletDelegation`/`WalletNextDelegation` with `voting` field"
(cv-1-implementation-guide.md:565-694) + task-128 acceptance criteria in
governance-drep-discovery-plan-tasks.json (pre-scribe `:872-877`).
Working-tree state at review time: one tracked modification and nothing else —
`git status --porcelain -uall` = ` M source/renderer/app/api/wallets/types.ts`,
`git --no-pager diff --stat` = 1 file changed / 11 insertions / 0 deletions,
HEAD unchanged at f948845a5. One review round; no iteration 2 was needed.

**The diff.**

- `source/renderer/app/api/wallets/types.ts:6` — new
  `import type { DRepIdentity } from '../../../../common/types/governance.types';`,
  placed directly after the `ApiTokens` import at `:5` (guide Step 1). Path
  resolution was checked rather than assumed: from
  `source/renderer/app/api/wallets`, `../../../../` walks
  wallets → api → app → renderer → source, so the specifier targets
  `source/common/types/governance.types.ts`. Same-depth precedent at
  `source/renderer/app/containers/voting/VotingGovernancePage.tsx:12`.
- `source/renderer/app/api/wallets/types.ts:86-93` — new `WalletVotingTarget`
  union, placed immediately after the `DelegationStatus` union (`:81-85`) and
  before `WalletSyncStateProgress` (`:94`) per guide Step 2. Body is verbatim
  to designs/current-vote-display-design.md:80-91, modulo prettier's wrapping
  of the multi-property `drep` member.
- `source/renderer/app/api/wallets/types.ts:117` — `voting?: WalletVotingTarget;`
  added to `WalletDelegation` (`:114-118`).
- `source/renderer/app/api/wallets/types.ts:123` — the same field added to
  `WalletNextDelegation` (`:120-125`), positioned *before* `changes_at`
  exactly as the guide's Step 3 block shows.
- Nothing else. Every one of the 11 inserted lines sits inside an
  `import type` / `export type` declaration — no runtime value, no constant,
  no comment. `DelegationStakePool` (`:106-109`) was correctly left unedited
  and inherits the widening structurally.

**Review lenses (four, one round).**

1. *Guide-conformance / acceptance-criteria lens.* Confirmed all four ordered
   guide steps applied in order with zero skips and zero reordering (Step 4 is
   verification-only), and all four ACs met. Re-ran the authoritative gate
   itself: `node_modules/.bin/tsc --noEmit` → exit 0, no output. Noted as
   cosmetic-only that the design sketch (`current-vote-display-design.md:80-86`)
   writes `DRepIdentity` as a local alias with `raw: string` while live code
   imports the canonical `interface` where `raw: DRepId` and
   `DRepId = string` — structurally identical, and D-6 is the correct
   resolution.
2. *Type-system lens.* Proved the discrimination empirically rather than by
   reading: a scratch file outside the repo (`/tmp/narrowcheck`, since removed)
   compiled under this repo's flag set and confirmed that
   `if (v.kind === 'drep')` narrows so `v.drep.raw`, `v.drep.credentialType`
   and `v.source` all resolve, and that a three-case `switch (v.kind)` with
   `const never_: never = v` in the default branch compiles — i.e. the union is
   exhaustively discriminated. Also confirmed no circular-import hazard
   (`source/common/types/governance.types.ts` has zero imports of any kind) and
   traced consumers: `WalletDelegation` has no external type-annotated
   consumer; `WalletNextDelegation`'s only one is
   `source/renderer/app/components/staking/delegation-center/WalletRow.tsx:9,182`,
   which reads every field through lodash `get` and is structurally immune.
   Independent gate re-run: `node_modules/.bin/tsc --noEmit` → exit 0,
   `grep -c "error TS"` = 0.
3. *Invariant / security lens.* Swept invariants 2, 5, 10 and 13 (below) and
   re-executed the one gate this lens owns:
   `yarn test:jest tests/jest/security/governance-sanitization.spec.ts
   --runInBand` → exit 0, 1 suite / 23 tests passed, 1.664 s. Confirmed the
   added lines contain zero logger/analytics/electron-store/IPC/persistence
   tokens and that `grep -rn "WalletDelegation\|WalletNextDelegation"
   source/common/ source/main/` returns zero hits, so the widened types are
   named in no IPC contract and no main-process module (design:112-115).
4. *Scope, style and simplicity lens.* Confirmed the tree holds exactly one
   modified file with no stray or untracked artifacts, that all four hunks are
   pure additions touching no pre-existing line's whitespace or quoting (so no
   file carrying the repo's known prettier drift was reformatted), and that
   declaration style matches the surrounding file — two-space indent, single
   quotes, trailing semicolon, no blank line between adjacent top-level
   `export type` declarations. `node_modules/.bin/prettier --check
   source/renderer/app/api/wallets/types.ts` → exit 0, "All matched files use
   Prettier code style!" (read-only check; no prettier write was run anywhere).

**Blockers raised and adjudicated.** Zero raw blockers were raised by any of
the four lenses, so there was nothing to refute adversarially and no confirmed
blocker required a fix. The pre-adjudicated non-issues were correctly not
re-litigated: the `source: 'verified' | 'unverified' | 'onchain'` member is
guide- and design-mandated (guide:613-621, design:88-91) despite being named in
no acceptance criterion and is in scope; the deliberate type-vs-wire looseness
(guide:583-589) is task-130's problem, not cv-1's; and the singular
`WalletNextDelegation` vs array-shaped fixtures (D-9) stays out of scope.

**Invariant sweep.** (10) Byte-equality: nothing in the diff touches
`source/common/types/governance.types.ts` — `raw` stays required at `:22` and
`credentialType` required at `:30`, so the same-vote comparator cannot be
handed an identity lacking the key/script discriminator, and the `drep`
variant carries the full `DRepIdentity` rather than a bare id string. (13)
Abstain / No Confidence remain form-only sentinels: `types.ts:92-93` are
literally `| { kind: 'abstain' }` and `| { kind: 'no_confidence' };` —
payload-free, one property each, so `voting.drep` is a compile error in those
branches and the type system itself now prevents a sentinel from carrying a
DRepIdentity into the directory. (5) Lovelace losslessness: no numeric
lovelace field was added — the ten `WalletUnits.LOVELACE` hits in the file are
all at pre-existing lines (20, 29, 73, 104, 129, 234, 238, 254, 258, 262), none
of which this diff touches. (2) Sanitization floor: zero runtime code and zero
logger/analytics/electron-store lines added; `git --no-pager diff --name-only
-- tests/` is empty, so the floor suite is unmodified, and it was re-run green
at 23/23. design:99 also holds — `grep -n "givenName\|anchorUrl\|lovelace"` on
the changed file returns zero hits. Invariants 1, 3, 4, 6, 7, 8, 9, 11, 12 and
14 are untouched: the task adds no data source, no IPC, no copy, no UI and no
ordering.

**Verification commands run (results as observed).**

1. `git --no-pager diff --stat` → 1 file changed, 11 insertions, 0 deletions;
   `git --no-pager diff --name-only` confirms
   `source/renderer/app/api/wallets/types.ts` as the sole entry; HEAD still
   f948845a5.
2. `node_modules/.bin/tsc --noEmit` → exit 0; output file 0 lines,
   `grep -c "error TS"` = 0. This is the authoritative TypeScript gate.
3. `yarn compile` → exit 0, "Done in 16.51s." It did *not* hit the known Node
   v24 flakiness this run, so no substitute was required, but the direct `tsc`
   gate above was still run. The preceding `[GENERATED TYPES]` lines are the
   benign `typed-scss-modules` pretask.
4. `yarn lint` → exit 0; 5591 warnings, 0 errors (count of eslint `✖` error
   markers = 0 across 7147 output lines) — the documented pre-existing
   baseline. No new warning is attributable to the diff:
   `grep -ac "api/wallets/types.ts"` and `grep -ac "wallets/types"` over the
   captured output both return 0, i.e. the changed file never appears in lint
   output at all. Sampled warnings all live in untouched files
   (storybook/stories/nodes/errors/Errors.stories.tsx:14:3,
   storybook/stories/nodes/status/Diagnostics.stories.tsx:93:6,
   storybook/stories/wallets/transactions/Utxo.stories.tsx:58:34), none of
   which appear in `git diff --name-only`.
5. `yarn test:jest tests/jest --runInBand` → exit 0; 1 suite skipped, 5 passed,
   5 of 6 total; 12 tests skipped, 102 passed, 114 total; 0 snapshots; 2.091 s.
   The single skipped suite is
   `tests/jest/governance/GovernanceCliArgvSmoke.spec.ts`, which self-skips by
   design when cardano-cli is absent from PATH (spec `:28`,
   `const describeWithCli = isCliOnPath ? describe : describe.skip;`) —
   environmental and pre-existing.
6. `yarn test:jest tests/jest/security/governance-sanitization.spec.ts
   --runInBand` → exit 0; 1 suite / 23 tests passed, 1.392 s. Sanitization
   floor unchanged from the slice-7 close baseline of 23/23.
7. `yarn test:jest tests/jest/api/walletDelegationStatuses.spec.ts
   --runInBand` → exit 0; 1 suite / 2 tests passed, 0.547 s. The task-127
   wire-literal pin still holds after the widening.
8. `yarn test:jest --runInBand` (bare, full repo scope — run because the guide
   names no specific renderer suites) → exit 0; 1 suite skipped, 80 passed, 80
   of 81 total; 12 tests skipped, 1030 passed, 1042 total; zero failures
   repo-wide. This covers the delegation-type consumers, all passing:
   `source/renderer/app/stores/VotingStore.spec.ts`,
   `containers/voting/VotingGovernancePage.spec.tsx`,
   `containers/governance/DRepDetailPage.spec.tsx`,
   `components/governance/drep-directory/DRepDirectory.spec.tsx`,
   `.../DRepDirectoryBanner.spec.tsx`,
   `components/governance/_shared/DRepCategoryBadge.spec.tsx`. Same single
   environmental skip as (5).
9. `grep -rn "interface DRepIdentity\|type DRepIdentity" source/ storybook/
   tests/` → exactly one hit,
   `source/common/types/governance.types.ts:20:export interface DRepIdentity {`.
   Decision D-6 (reuse, never redefine) upheld; widening the search to the
   `type` alias form and to storybook/tests surfaces no shadow definition.
10. `grep -rn "WalletVotingTarget" source/ storybook/ tests/` → 3 hits, all
    inside the single changed file (`:86` definition, `:117`, `:123`) — the
    type is newly introduced here and defined exactly once.
11. Direct content read of `source/common/types/governance.types.ts:18-33` and
    `source/renderer/app/api/wallets/types.ts:80-126` — the primary evidence
    for all four AC verdicts below.
12. `grep -n "givenName\|anchorUrl\|lovelace"
    source/renderer/app/api/wallets/types.ts` → zero hits.
13. `node_modules/.bin/prettier --check
    source/renderer/app/api/wallets/types.ts` → exit 0, plus the same check on
    the HEAD version via `--stdin-filepath` as a baseline control → exit 0. The
    file was prettier-clean both before and after, so the additions introduced
    no drift. Per F-5, `nix fmt` still cannot run in this devcontainer and
    remains owed pre-merge; the prettier check does not discharge it.

**Acceptance criteria.** AC-1 met — `voting?: WalletVotingTarget;` on both
types, at `source/renderer/app/api/wallets/types.ts:117` (`WalletDelegation`)
and `:123` (`WalletNextDelegation`), both optional, so no existing construction
site can break (consistent with the zero-error `tsc`). AC-2 met —
`types.ts:86-93` is a three-member union whose only common property is `kind`,
with exactly the literals `'drep'`, `'abstain'`, `'no_confidence'` and no
fourth member; exhaustive discrimination proven by the `never`-assignment
compile in lens 2. AC-3 met by import, not redefinition — `types.ts:6` resolves
to `source/common/types/governance.types.ts:20-31`, where `raw: DRepId;` (`:22`)
is required and `cip129?` (`:24`), `cip105?` (`:26`), `credentialHex?` (`:28`)
are optional. AC-4 met — `governance.types.ts:30` declares
`credentialType: 'key' | 'script';` as required, reachable from the union
because the `drep` variant carries the whole `DRepIdentity`.

**Findings (three forward-looking notes, zero blockers).**

1. `import type` at `types.ts:6` is load-bearing, not stylistic:
   `source/common/types/governance.types.ts:105` exports a runtime
   `export enum GovernanceQueryErrorType`, so the module is not purely
   erasable and a plain value import would have created a real runtime module
   edge from renderer wallet-types into common — on top of the pre-existing
   `types.ts` ↔ `domains/Wallet.ts` cycle. **Resolution:** correct as written;
   recorded so a future edit does not silently drop the `type` keyword.
2. Because `strictNullChecks` is commented out at `tsconfig.json:81`, the `?`
   on `voting` is not enforced at use sites — a consumer writing
   `wallet.delegation.active.voting.kind` will compile cleanly and then throw
   at runtime for the overwhelmingly common `not_delegating` wallet.
   **Resolution:** nothing to change in a types-only diff; the guard must be
   explicit runtime code. Flagged for the task-130/131 reviewer to look for.
3. The widened types are transitively visible from a common logging type —
   `AdaWallet.delegation.active: WalletDelegation` (`types.ts:44-47`) and
   `BodyData.wallets?: AdaWallet | null | undefined`
   (`source/common/types/logging.types.ts:52`, import at `:10-14`). This is
   type-level only and changes nothing about what is actually logged, and the
   runtime redactor already covers this exact path
   (tests/jest/security/governance-sanitization.spec.ts:64, :96, :118, :124
   cover `delegation.active.voting`, the `voting` key, and the `abstain` /
   `no_confidence` sentinels). **Resolution:** no action here; carry into
   task-130+ review that whatever `parseVoting` attaches must stay under a key
   the redactor already covers.

**Comment convention.** Zero comments were added anywhere in the diff, so the
convention (no task ids, no review labels, no ALL-CAPS, no change history) is
trivially satisfied.

**No unnecessary complexity.** Three tiny additions — one import, one union,
one optional field on each of two object types. No helper, no type guard, no
constant, no re-export barrel; type-guard and parsing work correctly stays with
task-129/130. No file was reformatted; no stray artifacts.

Decision: approved
