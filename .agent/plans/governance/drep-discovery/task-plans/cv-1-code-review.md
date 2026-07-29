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

---

## Code Review: task-129 — iteration 1 (2026-07-27)

**Scope reviewed.** Two new files against the guide section "task-129:
`normalizeDRepIdentity` helper" (cv-1-implementation-guide.md:697-807 for the
module, :818-923 for the spec) and task-129's seven acceptance criteria in
governance-drep-discovery-plan-tasks.json (pre-scribe `:898-906`). This is a
pure-create task: neither `source/renderer/app/utils/governance/` nor
`tests/jest/governance/normalizeDRepIdentity.spec.ts` existed at HEAD.
Working-tree state at review time: `git status --porcelain -uall` = exactly two
untracked entries and nothing else — `?? source/renderer/app/utils/governance/
normalizeDRepIdentity.ts` and `?? tests/jest/governance/
normalizeDRepIdentity.spec.ts` — with `git diff --stat` empty, i.e. no
pre-existing tracked file was touched. HEAD unchanged at 83edc15fa. One review
round; no iteration 2 was needed.

**What landed.**

- `source/renderer/app/utils/governance/normalizeDRepIdentity.ts:1` — the
  existing renderer `bech32` 2.0.0 dependency (package.json:204), imported in
  exactly the form already used at `source/renderer/app/utils/crypto.ts:4`. No
  `@cardano-sdk/core`, no main-process import, no new package.
- `:2` — `import type { DRepIdentity } from '../../../../common/types/
  governance.types';`. Type-only, per F-7.
- `:4-9` — three module constants (`0x22` key header, `0x23` script header,
  28-byte credential length) and a local `toHex` over `number[]`.
- `:17-62` — the exported function. Single `try` around
  `bech32.decode` + `bech32.fromWords` with a bare `catch { return null; }`
  (`:24-26`); the HRP `drep` branch (`:27-45`) requires a 29-byte payload and a
  known header byte, then derives the CIP-105 form; the
  `drep_vkh` / `drep_script` branch (`:46-60`) requires 28 bytes and derives the
  CIP-129 form by prepending the header; unknown HRP falls through to
  `return null` at `:61`.
- `tests/jest/governance/normalizeDRepIdentity.spec.ts:1-105` — eight `it()`
  blocks over a checksum-verified vector set (`:7-21`) plus two in-test
  synthesized vectors.
- Nothing else. `git diff package.json yarn.lock` is 0 bytes.

**Review method (three parallel lenses, one round).** Correctness /
bech32 semantics; locked invariants and the sanitization floor; tests and
guide-conformance. Every blocker a lens raised was then handed to two
independent skeptics for refutation, and a blocker was only allowed to stand if
it survived both.

1. *Correctness lens.* Decoded all six literal vectors independently with the
   repo's own bech32 2.0.0 rather than trusting the guide: `KEY_CIP129`
   (spec `:7`) → prefix `drep`, 29 bytes, header `0x22`, credential matching
   `KEY_CREDENTIAL_HEX`; `SCRIPT_CIP129` (`:12-13`) → header `0x23`;
   both CIP-105 forms → 28 bytes matching the same credentials. Confirmed the
   two branches are exact mirror images and that the derived-form encoding
   cannot throw: the longest output is `drep_script` (11) + `1` + 45 words +
   6 checksum = 63 chars, comfortably inside bech32's default 90-char limit, so
   `bech32.encode` at `:41` and `:55` is total for any input that decoded.
   Also confirmed the function is total for non-string input (`null`,
   `undefined`, `42`, `{}`, `[]` all return `null`, because `bech32.decode`
   throws inside the `try`) — relevant because DRep ids cross IPC even though
   the signature is typed `string`.
2. *Invariant / sanitization lens.* Invariant 1 (sanitization floor): `grep -n
   "logger\.\|console\."` over the module returns nothing — there is no logging
   on any path, including the failure paths, so no DRep id, sentinel literal or
   bech32 string can reach a logger, analytics or electron-store payload from
   here. Re-ran the floor suite this lens owns: `yarn test:jest
   tests/jest/security/governance-sanitization.spec.ts --runInBand` → 1 suite /
   23 tests passed. Invariant 2 (byte-equality): `raw` is returned by reference
   at `:39` and `:54` with no `trim`, no `toLowerCase`, no normalization
   anywhere in the file. Invariant 3 (form-only sentinels): `'abstain'` and
   `'no_confidence'` are not bech32 and are rejected by the `catch` at `:24`,
   asserted at spec `:86-87`; no sentinel branch was added here. Invariant 4
   (reuse existing seams): only two imports, `bech32` and a type; `git diff
   package.json` empty. Invariant 5 (key/script non-conflation):
   `credentialType` is set on every success path (`:35`, `:43`, `:50`, `:58`)
   and the derived `cip129` differs in the header byte, asserted at
   spec `:73-81`.
3. *Tests / guide-conformance lens.* `diff -u` of both new files against the
   guide's code blocks (cv-1-implementation-guide.md:746-807 and :819-923) is
   empty — byte-identical transcription, zero drift. Confirmed the spec's
   rejection vector set is real rather than decorative: `''`, `'abstain'`,
   `'no_confidence'`, `'not-a-bech32-string'`, a pool id (`:21`), the
   deprecated 28-byte `drep1` form (`:19-20`) and a single-character checksum
   corruption (`:91`) each hit a different rejection path. Also verified the
   `import type` is genuinely elided: transpiling the module with `tsc` emits
   JS whose only `require` is `'bech32'`, with zero reference to
   `governance.types`.

**Blockers raised and adjudicated.** Zero blockers survived refutation, and
therefore zero changes were made during review — the files as reviewed are the
files as written. What was raised and refuted:

1. *"Line 48 is uncovered — the CIP-105 length-mismatch `return null` is never
   executed."* Refuted on behaviour, not on argument: the branch was exercised
   directly against the transpiled module —
   `bech32.encode('drep_vkh', toWords(29 bytes))` → `null` and
   `bech32.encode('drep_script', toWords(27 bytes))` → `null`. The mirror-image
   CIP-129 length check at `:28-30` *is* covered, via
   `DEPRECATED_DREP_28_BYTE`. AC-4 ("all three prefixes plus invalid input") is
   satisfied without it, and adding a vector would break byte-exactness with
   the approved guide. Recorded as F-10 rather than changed.
2. *"Uppercase bech32 input breaks the round-trip invariant."* Real behaviour,
   not a defect here. BIP-173 permits all-uppercase encodings and bech32 2.0.0
   lower-cases the HRP internally, so
   `normalizeDRepIdentity('DREP1Y2SM9S75UHM…')` succeeds and returns `raw` /
   `cip129` uppercase while the derived `cip105` is lowercase. Refuted because
   invariant 2 *forbids* case normalization — "fixing" it would violate a
   locked invariant to satisfy a weaker one. Mixed case is correctly rejected
   by the library. Carried forward as F-9 for task-130's comparator.
3. *"The JSDoc at `:11-16` runs to four content lines, over the repo's 1-3 line
   comment guidance."* Refuted: it is an exported-API contract doc stating the
   invariant, not an inline logic comment; it carries no task ids, no review
   labels, no ALL-CAPS markers and no change history; and it is the approved
   guide's exact text.
4. *"`credentialHex` is identical for a key and a script DRep sharing 28 bytes
   — invariant 5 is violated."* Refuted by reading the assertion it points at:
   spec `:77` asserts that equality *deliberately*, and non-conflation is
   carried by `credentialType` (`:78-79`) and `cip129` (`:80`), which differ.
   The real consequence is a downstream constraint, not a defect: any same-vote
   comparator must key on `cip129` or on the (`credentialHex`,
   `credentialType`) pair, never on `credentialHex` alone.
5. *"The tracker says this function classifies `abstain` / `no_confidence`, but
   it returns null for both."* Refuted as a code defect and re-classified as a
   doc conflict: locked invariant 3 and the guide's resolved-judgment-calls
   block (cv-1-implementation-guide.md:721-733, :926-928) both put the sentinel
   branch in task-130's `parseVoting`. The tracker prose is the stale side.
   Recorded as F-8 under the source-of-truth rule; no code change.
6. *"AC-1 (purity, no side effects) is not actually asserted by any test."*
   Partially conceded but refuted as a task-129 blocker: the property holds
   structurally today (grep is clean), the guide deliberately places the logger
   spy at task-130 (cv-1-implementation-guide.md:2041-2062, :2165), and the
   floor suite `tests/jest/security/governance-sanitization.spec.ts` is
   boundary-based by design (its imports at `:21-28` do not include this
   module). Carried as F-10; a floor assertion covering this module should land
   with task-130.

**Verification commands run (results as observed).**

1. `yarn compile` → exit 0, "Done in 18.86s.", zero TypeScript errors across
   the whole tree — which matters here because tsconfig has no `include` and
   excludes only `node_modules` (`tsconfig.json:103`), so the new spec under
   `tests/` is typechecked too. The `typed-scss-modules` pretask ran cleanly
   under Node v24.16.0, so the anticipated Node-v24 breakage did not occur and
   the direct `node_modules/.bin/tsc --noEmit` fallback was never needed.
2. `yarn test:jest tests/jest/governance/normalizeDRepIdentity.spec.ts
   --runInBand` → 1 suite passed, 8 of 8 tests passed, 0 snapshots. Module
   coverage 96.66% statements / 94.11% branch / 100% functions / 96.55% lines;
   the sole uncovered line is `:48`.
3. `yarn test:jest tests/jest/security/governance-sanitization.spec.ts
   --runInBand` → 1 suite / 23 tests passed. The inherited sanitization floor
   is intact under this change.
4. `yarn lint` → exit 0; 5591 warnings, 0 errors — the documented pre-existing
   baseline. A targeted re-run grepping the captured output for
   `normalizeDRepIdentity|utils/governance|error` returned no matches (grep
   exit 1), so the new module contributes zero diagnostics at any severity;
   sampled warnings all live in untouched files
   (source/renderer/app/themes/daedalus/index.ts:28,
   source/renderer/app/utils/validations.ts:100, numerous
   storybook/stories/*). The spec is outside lint's reach by convention
   (`package.json:43` scopes lint to source/storybook/utils; `.eslintignore`
   covers `tests/`), so it is gated by `tsc` alone.
5. `node_modules/.bin/prettier --check` on the two new files → exit 0, "All
   matched files use Prettier code style!" under prettier 2.1.2. No `--write`
   was needed, so no file was rewritten and no pre-existing file carrying the
   repo's known prettier drift was reformatted. Per F-5, `nix fmt` still cannot
   run in this devcontainer and remains owed pre-merge; this check does not
   discharge it.
6. `git diff package.json` and `git diff yarn.lock` → both empty; the combined
   `git diff package.json yarn.lock` is exactly 0 bytes. AC-7 met.
7. `git status --porcelain --untracked-files=all` → exactly the two new
   untracked paths, no modifications, no deletions; re-checked *after* all
   suites ran to rule out coverage or test artifacts, with the same result.
   `git diff --stat` empty. Zero scope violations.
8. `diff -u` of both new files against the guide code blocks
   (cv-1-implementation-guide.md:746-807, :819-923) → empty.
9. Independent bech32 2.0.0 decode of all six literal vectors plus direct
   execution of the `:48` branch and of the non-string input set, against the
   transpiled module.

**Acceptance criteria.** AC-1 met — no assignment outside function scope, no
I/O, no logging, no mutation of the input (`grep` for `logger.`/`console.`
returns nothing). AC-2 met — spec `:64-71` asserts
`normalizeDRepIdentity(normalizeDRepIdentity(KEY_CIP129).cip105).cip129 ===
KEY_CIP129` for both key and script. AC-3 met — `null` is returned on every
rejection path and the function never throws, including for non-string input.
AC-4 met — all three prefixes are covered (spec `:24-62`) plus a seven-entry
invalid-input set (`:83-96`) and an unknown-header case (`:98-104`). AC-5 met —
`bech32.decode` preserves the HRP and the code branches on
`decoded.prefix` at `:27` and `:46`, so `drep`, `drep_vkh` and `drep_script`
are distinguished even with identical 28-byte payloads. AC-6 met —
`credentialType` is populated on all four success returns and the spec pins
key ≠ script for shared credential bytes at `:73-81`. AC-7 met — bech32 2.0.0
is reused from `dependencies` (package.json:204, not devDependencies, so it
resolves in the renderer bundle at runtime and not only under jest), and both
dependency-manifest diffs are empty.

**Comment convention.** Two comments exist: the JSDoc contract block at
module `:11-16` and the vector-provenance note at spec `:4-6`, plus the
one-line deprecation note at spec `:18`. All state the invariant or the why;
none carries a task id, review label, ALL-CAPS marker or change history.
Adjudicated at blocker 3 above.

**No unnecessary complexity.** One exported function, one 2-line local helper,
three named constants, two symmetric branches and a single `try`. No class, no
factory, no error type, no barrel re-export, no caching. The sentinel handling
and the caller-side sanitized warning correctly stay with task-130.

**Out-of-scope observations carried forward.** Pre-existing ad-hoc
`DRepIdentity` construction survives at
`source/renderer/app/containers/voting/VotingGovernancePage.tsx:75-83`, which
infers `credentialType` from `chosenOption.startsWith('drep_script')` — a
heuristic that labels a CIP-129 `drep1…` script DRep as `'key'`, exactly the
conflation invariant 5 targets. A similar shortcut exists at
`storybook/stories/voting/Governance.stories.tsx:58-61`. Both are untouched
here (git status shows only the two new paths) and are the natural call sites
to replace in task-130/131. Separately,
`source/common/types/governance.types.ts:18` carries a task id inside a code
comment, contrary to convention; `git log -S` confirms it was introduced in
0f47402b6 (slice-1), not by this task, and it was correctly left alone.

Decision: approved

---

## Code Review: task-130 — iteration 1 (2026-07-27)

**Scope reviewed.** The working diff against the guide section "task-130:
Mapper in `_createWalletFromServerData` + collision rules"
(cv-1-implementation-guide.md:961-1204 — seven ordered steps with verbatim
code blocks, the resolved-judgment-calls block at `:990-1007`, the
verification commands at `:1179-1184` and the seven-item acceptance checklist
at `:1186-1202`), the design contract
(designs/current-vote-display-design.md:101-110 HRP→kind table, `:114`
in-memory-only storage, `:118-146` mapper pseudocode, `:164` the HRP-only
warning allowance) and task-130's five acceptance criteria in
governance-drep-discovery-plan-tasks.json (pre-scribe `:927-931`). This is a
modify-only task: two tracked files, `+49/-2`. Working-tree state at review
time: `git status --porcelain --untracked-files=all` = exactly
` M source/renderer/app/api/api.ts` and
` M source/renderer/app/domains/Wallet.ts`, zero untracked paths, empty
`git diff package.json yarn.lock`. HEAD unchanged at 40bcd990a. One review
round; no iteration 2 was needed.

**What landed.**

- `source/renderer/app/api/api.ts:100` — `import { normalizeDRepIdentity }
  from '../utils/governance/normalizeDRepIdentity';`, placed directly after
  the `filterLogData` import. `:162` — `WalletVotingTarget,` inserted
  immediately after `AdaWallets,` in the existing wallets-types import block.
  `logger` (`:91`), lodash `get`/`last` (`:1`) and `WalletDelegationStatuses`
  (`:6-9`) were already imported; nothing redundant was added.
- `:3012` — `const LOGGABLE_HRP_PATTERN = /^[a-z_]{1,16}$/;`, module-private.
- `:3014-3031` — the three-line contract comment plus
  `const parseVoting = (voting: unknown): WalletVotingTarget | null`. Guards
  `null`/non-string first (`:3018`), then the two sentinel branches
  (`:3019-3020`), then `normalizeDRepIdentity(voting)` (`:3021`), then the
  unknown-HRP degradation with a single sanitized `logger.warn` (`:3025-3027`),
  then `{ kind: 'drep', drep, source: 'onchain' }` (`:3030`).
- `:3033` — `export const _createWalletFromServerData = action(`. The `export`
  keyword is the guide's own resolved judgment call (`:990-993`) so task-134
  can import the mapper.
- `:3078-3100` — `delegationStakePoolStatus` (unchanged expression) moved one
  line up, a two-line collision comment (`:3079-3080`), the two
  `let … = null` initializers
  and the four-way `switch (status)` inside the pre-existing `if (!isLegacy)`
  guard. `:3153` — `votingTarget,` in the `new Wallet({ … })` literal, between
  `pendingDelegations: next,` and `discovery,`.
- `source/renderer/app/domains/Wallet.ts:11` — `WalletVotingTarget,` appended
  to the existing type-only import; `:130` —
  `votingTarget?: WalletVotingTarget | null;` directly after
  `pendingDelegations?`. Exactly two added lines; nothing else in the file
  changed.
- Nothing else. One deleted line only:
  `const delegatedStakePoolId = isLegacy ? null : target;`.

**Review method (three parallel lenses, one round).** Guide fidelity and
acceptance-criteria adjudication; locked invariants and sanitization-floor
regression; runtime correctness, edge cases and unnecessary complexity. As in
the previous cv-1 tasks, any blocker a lens raised would have been handed to
three independent skeptics for refutation and allowed to stand only if it
survived — no lens raised one on this diff, so the panel had nothing to
adjudicate.

1. *Guide-fidelity lens.* Extracted all ten fenced `ts` blocks from the
   guide's task-130 section and substring-matched each against the live files
   rather than eyeballing them: every "after" block is present byte-for-byte
   (blocks 0,1,2,4,6,7 in api.ts; blocks 8,9 in Wallet.ts), including comment
   wording and the em dash, and block 5 — the Step 4 "before" snippet
   containing `const delegatedStakePoolId = isLegacy ? null : target;` — is
   correctly absent, being the line replaced. All seven steps implemented,
   zero skipped, zero divergent. The one apparent departure from the design
   pseudocode (`:137`/`:143` write `delegation.active.target ?? null` where
   the code reuses the pre-existing `target`) is the guide's own deliberate
   byte-identity formulation (`:1124-1127`) and is semantically equivalent,
   lodash `get` substituting `null` for `undefined`. Greped `source/`,
   `tests/` and `storybook/` for `_createWalletFromServerData`: the only hits
   are the declaration and the 15 internal call sites (api.ts:404…:2080), none
   changed, no re-export, no name collision.
2. *Invariant / sanitization lens.* Invariant 2 (sanitization floor): the diff
   introduces exactly one new sink, and a diff-scoped grep for
   `logger\.|analytics|electron-store|JSON.stringify` returns exactly one line,
   `logger.warn('AdaApi::parseVoting unrecognized voting target', {`. Its only
   payload member is `hrp`. Bounding was reasoned adversarially rather than
   assumed: bech32's data charset excludes `1`, so `lastIndexOf('1')` is the
   true separator for well-formed input; `separatorIndex > 0` (not `>= 0`)
   forces `hrp = ''` for a missing or leading separator, and `''` fails
   `{1,16}`; any slice that reaches into a data part necessarily carries
   digits, and the allowlist admits none — so every such case collapses to the
   fixed literal `'invalid'`. All-uppercase bech32 (BIP-173 legal, F-9)
   likewise collapses to `'invalid'`. Neither the raw id nor the full input is
   reachable from the payload, matching design `:164`. The sentinels cannot
   reach the logger at all (they return two lines earlier and contain no `1`).
   The warn deliberately does not route through `filterLogData`, correctly:
   `hrp` is not a sensitive key (source/common/utils/logging.ts:32-48 lists
   `votingKey`/`drepId`/`vote`/`voting`) and the value is already bounded.
   Downstream reachability was walked, not assumed — a repo-wide grep for
   `votingTarget` returns exactly six hits (api.ts:3082/3087/3091/3097/3153,
   Wallet.ts:130), and the two sinks that take a `Wallet`
   (analytics/utils/getEventNameFromWallet.ts:3-4, which derives a constant
   string from `isHardwareWallet` alone, and stores/WalletsLocalStore.ts:57-67,
   which accepts `walletId` plus an explicit object) never serialize the
   instance, so design `:114` (in-memory only, no electron-store, no IPC,
   never user input) holds. Invariant 10 (byte-equality): a diff-scoped grep
   for `toLowerCase|toUpperCase|\.trim\(|\.normalize\(|\.replace\(|substring`
   returns no matches; the single `slice(` in the diff builds the log token
   only, and the value handed to the normalizer is the untouched `voting`
   binding, which `normalizeDRepIdentity.ts:39`/`:54` return by reference.
   Invariant 13 (form-only sentinels): `:3019` and `:3020` both precede the
   normalizer call at `:3021`. Invariants 5 and 4: no balance-parsing line and
   no submission path appears in the diff; api.ts:3060-3072, the
   BigNumber balance/reward block, is byte-identical and sits above the first
   changed line.
3. *Correctness / complexity lens.* Traced the edge cases individually.
   `active === null` (or `delegation` absent): the `get` chain yields
   `target = null`, `status = null`, the switch falls to `default`, and both
   locals end `null` — identical to the old `isLegacy ? null : target`.
   `isLegacy === true`: the `if (!isLegacy)` guard is never entered and both
   locals keep their `null` initializers, matching today exactly, with
   `delegationStakePoolStatus = isLegacy ? null : status` untouched.
   Non-string `voting` (object/number/boolean): short-circuits to `null` with
   no log, so no object can be spread into the logger. A pool id supplied as
   `voting` decodes with prefix `pool`, falls through
   `normalizeDRepIdentity.ts:61` to `null`, and warns with `hrp: 'pool'` —
   exactly the last row of the design's HRP table. `votingTarget` is never
   `undefined` on the constructor literal (initialized `null`, every arm
   assigns `null` or an object), and `WalletProps.votingTarget?:
   WalletVotingTarget | null` accepts both, landing on the instance through
   the `Object.assign` at Wallet.ts:173-175.

**Blockers raised and adjudicated.** Zero blockers were raised by any of the
three lenses, so no change was made during review — the diff as reviewed is
the diff as written. Recorded as non-blocking:

1. *Type-vs-wire divergence carried forward.*
   `source/renderer/app/api/wallets/types.ts:117` and `:123` declare
   `voting?: WalletVotingTarget` (an object) while the wire value is a raw
   string, as the task-126 fixtures show. `parseVoting` absorbs this correctly
   by taking `unknown` and guarding on `typeof voting !== 'string'`, and the
   guide's resolved-judgment-calls block (`:997-1000`) acknowledges it, so
   there is no runtime hazard today. Worth tightening to an `ApiDRep` string
   alias in cv-2; not a task-130 change.
2. *Log cadence.* `stores/WalletsStore.ts:69,264` polls every 5000 ms and the
   mapper runs per wallet per poll, so a persistently unrecognized `voting`
   value emits one bounded `logger.warn` roughly every 5 s per affected
   wallet. The payload is floor-compliant and the warn is guide-mandated
   verbatim; adding rate-limiting here would itself be an unrequested
   abstraction and a guide deviation. Flagged so the choice is conscious.
3. *Asymmetry in `parseVoting`.* A non-string `voting` degrades to `null`
   silently while an unknown-HRP string warns. Guide-verbatim and the safer
   choice (never spread an unknown object into the logger), but it means a
   cardano-wallet wire-shape change would degrade without a signal. Better
   answered by a task-134 spec assertion than by a code change.
4. *Import-block placement.* `WalletVotingTarget` joins the value import block
   at api.ts:159-190 rather than an `import type` block. This matches the
   guide's Step 1 instruction and every other type already in that block;
   `isolatedModules` is commented out (tsconfig.json:72) and `tsc` is clean,
   so there is no emit hazard. Observation only.
5. *Pre-existing sanitization exposure outside the fence.* api.ts:379-383 logs
   the raw `wallets` array, which carries `delegation.active.voting`. Verified
   byte-identical at HEAD via `git show`, therefore neither introduced nor
   worsened here, and outside task-130's scope fence. Recorded as F-11 for a
   follow-up floor task rather than fixed.

**Verification commands run (results as observed).**

1. `node_modules/.bin/typed-scss-modules source/renderer/app` → exit 0;
   regenerated the gitignored `*.scss.d.ts` files in the fresh worktree, no
   errors.
2. `node_modules/.bin/tsc --noEmit` → exit 0, zero diagnostics. Per PRD R-4
   this was the planned Node-v24 fallback; it ran clean under Node v24.16.0
   (see F-13).
3. `node_modules/.bin/eslint source/renderer/app/api/api.ts
   source/renderer/app/domains/Wallet.ts --ext .ts` → exit 0; 72 problems,
   0 errors, 72 warnings — exactly the pre-existing per-file baseline
   (`consistent-return`, `@ts-ignore` bans, unused mobx/type imports in
   Wallet.ts). No warning points at any new line.
4. `node_modules/.bin/jest tests/jest --runInBand` → exit 0; 6 passed +
   1 skipped = 7 of 7 suites, 110 passed + 12 skipped = 122 tests. Recorded
   with the caveat that this path filter matches only 7 of the repo's 82 test
   files and is *not* the tree-wide baseline quoted in earlier cv-1 entries
   (F-13).
5. `node_modules/.bin/jest --runInBand --coverage=false` (unfiltered, the run
   the tree-wide baseline actually describes) → exit 0; 81 passed + 1 skipped
   = 82 suites, 1038 passed + 12 skipped = 1050 tests, 2 snapshots passed,
   zero failures. AC-3's byte-identity claim is corroborated by this run.
6. `node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts
   --runInBand` → 1 suite passed, 23 of 23 tests passed. The inherited
   sanitization floor is intact with the new `logger.warn` in place.
7. `node_modules/.bin/jest tests/jest/governance/normalizeDRepIdentity.spec.ts
   --runInBand` → 1 suite passed, 8 of 8 tests passed.
8. `node_modules/.bin/prettier --check` on the two changed files → exit 1.
   Wallet.ts is clean; api.ts is flagged and the failure is proven
   pre-existing: extracting the HEAD blob of api.ts to a scratch file and
   running the identical check fails the same way, and formatting each yields
   the *same nine* drift hunks at offsets differing by exactly `+2` (the two
   added import lines) — `@@788/790`, `1035/1037`, `1637/1639`, `1655/1657`,
   `2162/2164`, `2187/2189`, `2211/2213`, `2400/2402`, `2605/2607`. Every hunk
   is the known prettier-2.1.2 assignment-break drift and the furthest sits
   ~400 lines above the new code at `:3012+`, so no new line is implicated.
   Correctly left unformatted per F-5; the scratch files were deleted and the
   tree returned to exactly two modified paths.
9. `git diff HEAD -- <the two files> | grep -nE
   "logger\.|analytics|electron-store|JSON.stringify"` → exactly one match,
   the sanctioned warn at diff line 39. No analytics call, no electron-store
   write, no `JSON.stringify` anywhere in the diff.
10. `git diff HEAD | grep -nE "toLowerCase|toUpperCase|\.trim\(|normalize\("`
    → exit 1, "no matches". Invariant 10 holds.
11. `grep -n "givenName\|anchorUrl" source/renderer/app/api/api.ts` → no
    matches. `grep -n "votingTarget"` → the five api.ts insertions and the one
    Wallet.ts line, i.e. exactly the guide's edits.
12. `git status --porcelain --untracked-files=all` → the two ` M` entries and
    nothing else; `git diff package.json yarn.lock` = 0 bytes.
13. Independent bech32 2.0.0 decode of both task-126 DRep vectors:
    `drep1y2sm9s75uhm…` → prefix `drep`, 29 payload bytes, header `0x22`;
    `drep_vkh15xev84897…` → prefix `drep_vkh`, 28 payload bytes. Both clear
    `normalizeDRepIdentity`'s length and header gates, so AC-1 and AC-2 are
    non-vacuous rather than silently degrading to a warning plus `null`.

**Acceptance criteria.** AC-1 met — `wallet-voting-drep.json` (`status:
'voting'`) enters the `VOTING` case (`api.ts:3084-3088`), which pins
`delegatedStakePoolId = null` and sets `votingTarget = parseVoting(get(active,
'voting', null))`, yielding `{ kind: 'drep', drep: { raw: 'drep1y2sm9s75…'
byte-equal, cip129, derived cip105, credentialHex, credentialType: 'key' },
source: 'onchain' }`; no `logger.warn` fires on this path. AC-2 met —
`wallet-delegating-and-voting.json` matches
`WalletDelegationStatuses.VOTING_AND_DELEGATING` (Wallet.ts:42, the value
task-127 corrected) and enters `api.ts:3089-3092`, giving
`delegatedStakePoolId = 'pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2'`
*and* a populated `votingTarget` built from the CIP-105 form; the fixture's
`next[]` entry still flows through the untouched `// Last` block. AC-3 met —
the only deleted line is `const delegatedStakePoolId = isLegacy ? null :
target;`; the legacy path short-circuits on the `null` initializers
(`:3081-3083`), and `delegating`, `not_delegating` and every unrecognized or
`null` status share the `default` arm (`:3093-3098`), assigning the same
`target` read from the same unmoved `get(active, 'target', null)`. The
`const`→`let` change and the one-line statement reorder are semantically
inert (both operands are pure `get` reads), `delegationStakePoolStatus` is
byte-identical, and the full unfiltered jest run is green. AC-4 met —
`parseVoting` is called at exactly two sites and both pass
`get(active, 'voting', null)`; `target` is never passed to it anywhere in the
file, so an absent `active.voting` returns `null` on the first guard without
reaching the normalizer or the logger, and the `VOTING` arm independently
forces `delegatedStakePoolId = null` so a DRep-era `active.target` cannot
surface as a pool id either. AC-5 met — `DRepIdentity`
(source/common/types/governance.types.ts:20-31) declares only `raw`,
`cip129?`, `cip105?`, `credentialHex?` and `credentialType`, and grep confirms
neither `givenName` nor `anchorUrl` appears anywhere in api.ts; the mapper
hydrates no anchor-derived display metadata, so invariant 3 is enforced
negatively as specified. Separately, `source: 'onchain'` is present at
`:3030` — the required member of `WalletVotingTarget.drep`
(api/wallets/types.ts:86-93) that no acceptance criterion names (F-6).

**Comment convention.** Two comments were added: the three-line `parseVoting`
contract note at api.ts:3014-3016, stating the wire values and the
sanitization floor, and the two-line collision note at api.ts:3079-3080,
stating that a voting-only status never carries a stake-pool target. Both are
plain prose stating the invariant or the why; neither carries a task id, a
review label, an ALL-CAPS marker or change history. Both are the approved
guide's exact text.

**No unnecessary complexity.** One module-private arrow function, one module
constant, one `switch`, two `let` locals and one added object property. No new
file, no class, no error type, no barrel, no caching, no helper module, no
rate-limiter. The only new export is the one the guide explicitly resolved
(`:990-993`) so task-134 can import the mapper, and it is a no-op at runtime.
The seemingly redundant `delegatedStakePoolId = null` in the `VOTING` arm and
`votingTarget = null` in the `default` arm are guide-verbatim and are the
point of the task ("explicit collision rules") — removing them would be the
deviation. Scope fence clean in both directions: Wallet.ts has no `@observable
votingTarget`, no `currentVote`, no `isVoting`, and its `update()` pick list
still holds exactly its 19 pre-existing entries with `votingTarget` absent
(task-131's boundary), and no spec file was created (task-134's boundary).

**Out-of-scope observations carried forward.** The pre-existing ad-hoc
`DRepIdentity` construction at
`source/renderer/app/containers/voting/VotingGovernancePage.tsx:75-83` — which
infers `credentialType` from `chosenOption.startsWith('drep_script')` and so
mislabels a CIP-129 `drep1…` script DRep as `'key'` — and the matching
shortcut at `storybook/stories/voting/Governance.stories.tsx:58-61` both
survive untouched. Carried over from the task-129 review; the approved guide
does not touch either file and no task-130 acceptance criterion covers them,
so their correct home is cv-2 / task-140. Newly recorded as F-11: api.ts:379-383
logs the raw `wallets` array in `AdaApi::getWallets success` with only
`hwLocalData` sanitized, so `delegation.active.voting` — a CIP-129/CIP-105 id
or a sentinel literal — reaches the log file verbatim; the line is
byte-identical at HEAD, `filterLogData` is call-site-only
(utils/logging.ts:26-43 forwards `data` untouched and there is no global
sanitizing transport), and the floor suite has no `getWallets` case. Also
still open: `source/common/types/governance.types.ts:18` carries a task id
inside a code comment from slice-1 (0f47402b6), deliberately left alone; and
the api.ts prettier drift under F-5, which `nix fmt` still owes pre-merge.

Decision: approved

---

## Planner: 2026-07-27 — findings audit outcome (six new rows, nine amendments)

**What was audited.** Every findings, code-review and research file in this
plan directory was swept against the tracker and the outcome approved. Result:
six new task rows (task-170 … task-175), nine acceptance-criterion amendments
across seven pending host tasks, one status demotion with one status caveat,
and one recorded non-goal. Two of the new rows and three of the amendments are
cv-1 work; the cv-1 PRD and implementation guide are not rewritten by this
entry — the guide lines that change are named inside the amendments' own
acceptance criteria.

**New rows.**

- **task-170** (cv-1) — closes F-11 as recorded at `:1119-1125`: wrap the
  `AdaApi::getWallets` and `AdaApi::getWallet` payloads so
  `delegation.active.voting` cannot reach the log file, and add the missing
  `getWallets` case to `tests/jest/security/governance-sanitization.spec.ts`.
- **task-171** (cv-1) — restore the leading `!!!` on the nineteen
  feature-introduced ja-JP keys and add a Jest guard, so the governance
  strings still to be minted (task-135, task-146, anchor-2 copy) cannot
  silently reopen the gap. The guard restores markers only; stripping them
  remains the user-owned release-end copy review.
- **task-172** (anchor-1) — ground `getDRepCategory` in a store-owned
  cohort-membership input and activate the fourth (High value) category.
- **task-173** (cv-2) — build the confirmation-dialog identity with
  `normalizeDRepIdentity`; must be sequenced before task-141.
- **task-174** (anchor-2) — additive dual CIP-129 / CIP-105 mode on
  `DRepIdDisplay`.
- **task-175** (cv-2) — render the pre-anchor shared-design-tokens §7
  confirmation identity block (CIP-129, CIP-105 when derivable, the
  signed-payload line, the on-chain source label); depends on task-173.

**Amendments (nine across eight hosts).** task-131 takes two — F-13's
unfiltered `yarn test:jest --runInBand` gate in place of the `tests/jest` path
filter (the filter matches 7 of 82 suites, as recorded at `:1014-1018`), and
the governance.types.ts:18 comment fix. task-134 takes one (F-10's
length-guard and success-path no-logging pins). F-9's case-stable comparison
key spans two hosts in one amendment: task-140 defines the comparator and
task-147 carries its letter-case regression vector, because task-140's own
criterion delegates that vector rather than writing it, and task-147 already
depends on task-140 so the split adds no edge. task-138, task-151 and
task-157 take one each; task-153 takes two. task-154 takes none — its §7 work
became task-175 instead.

**One below-cutoff absorption was applied rather than dropped.** The merged
governance-IPC-error-contract item stayed below the cutoff as a row, but its
load-bearing half landed as a single acceptance criterion on task-149: a
task-111-style spy case over the new fetch service's failure paths. This one
could not be discharged the way the other cut riders were — by recording the
drop in the item's own findings file — because its source is the slice-1
final-pass record, which is closed precedent. The gap it closes is live:
`source/main/utils/logging.ts:26-33` forwards `data` to electron-log with no
`filterLogData` equivalent, and the existing governance sinks
(`GovernanceQueryService.ts:523-526`, `governanceChannel.ts:64` and `:77`)
log whole error objects, so invariant 2 has had no main-process assertion at
all. task-149 estimatedHours 6 → 7.5. The discardable remainder — the
`GovernanceWireError` type and the structured-clone round-trip test — is not
tasked; if it is ever added it must not re-introduce a task id into
`governance.types.ts`.

**Ordering is carried by `dependencies`, not by listing order.** The two
sequencing constraints the new rows assert in prose are now in the
authoritative field: task-135 and task-146 gain `task-171` (the `!!!` guard
must precede every remaining governance copy mint), and task-141 gains
`task-173` (the container identity fix must land before the task that edits
the same container). `summary.criticalPath` gains `task-173` between
`task-140` and `task-141` — the only critical-path change this sweep earns.

**task-109 / task-111.** task-109 moves `verified` → `complete`, carrying a
statusReason that names the api.ts:379-383 gap its AC-2 never covered and
points at task-170; it returns to `verified` when task-170 lands. task-111
stays `verified` and gains a statusReason caveat recording the same residual:
the floor suite has no `getWallets` case, exactly as this log noted at
`:1125`.

**Non-goal recorded.** Cucumber/e2e coverage of DRep Discovery is a deliberate
non-goal for v1 — recorded in the plan's Key Decisions and the README binding
scope, with no task row.

**Ownership of the observations this log carried forward.** The ad-hoc
`DRepIdentity` construction at `VotingGovernancePage.tsx:75-83` and its
Storybook twin at `Governance.stories.tsx:58-61`, recorded at `:825-833` and
again at `:1111-1119`, are now owned by task-173. Both homes this log
predicted for them are superseded: task-130 and task-131 leave the call sites
untouched (`:831-833`), and the "cv-2 / task-140" reading at `:1119` is not
the owner either. The task id inside the
`source/common/types/governance.types.ts:18` comment — flagged twice and
deliberately left, at `:833-836` and `:1126-1128` — is owned by the task-131
amendment above.

**Correction owed on the comparator note at `:737-739`.** That note offers
`cip129` or the (`credentialHex`, `credentialType`) pair as the same-vote key.
Read as a plain string comparison the `cip129` half is case-unstable — BIP-173
permits an all-uppercase encoding and the live form gate accepts it (F-9) — so
a case-stable comparator must use the pair, or a *case-insensitive* `cip129`
comparison, never a case-sensitive canonical-string one. task-140's amendment
owns this correction here and on designs/current-vote-display-design.md:95;
the substance is recorded now so the note is not read at face value in the
meantime. Per the README's slice-level-docs convention this log is
append-only, so that amendment must discharge it by appending too, never by
editing `:737-739` in place.

---

## Code Review: task-131 — iteration 1 (2026-07-28)

**Scope reviewed.** The working diff against the guide section "task-131:
Wallet domain `votingTarget`/`currentVote`/`isVoting` incl. `update()` pick
list" (cv-1-implementation-guide.md:1225-1415 — the two-file "Files touched"
list at `:1227-1232`, the inline locked invariants at `:1244-1256`, the
resolved-judgment-calls block at `:1258-1279`, Steps 1-4 with verbatim code
blocks at `:1283-1365`, the Step 5 verify block at `:1370-1391` and the
seven-item acceptance checklist at `:1393-1414`), the design contract
(designs/current-vote-display-design.md:112 the "anything else" wire row,
`:114` in-memory-only storage) and task-131's nine acceptance criteria in
governance-drep-discovery-plan-tasks.json (pre-scribe `:957-967`). This is a
modify-only task: two tracked files, `+14/-1`, no untracked path, empty
`git diff package.json`, HEAD unchanged at 2ee5f74cf. One review round.

**What landed.**

- `source/renderer/app/domains/Wallet.ts:165-166` — the declared observable
  `votingTarget: WalletVotingTarget | null | undefined;`, inserted between
  `pendingDelegations` and `discovery` exactly as Step 1 prescribes. The
  tri-state type is the guide's own resolved judgment call (`:1259-1266`),
  matching the sibling optional-observable style at `:153-154`.
- `:200` — `'votingTarget',` in the `update()` `pick(other, [...])` list,
  directly after `'pendingDelegations',` and before `'discovery',`. This is
  the D-7/R-2 trap the task exists to close: the list is explicit, so an
  omission compiles clean and every 5-second poll refresh would silently keep
  the stale vote target.
- `:254-262` — the two computeds, between `isDelegating` and `isSequential`:
  ```ts
  @computed
  get currentVote(): WalletVotingTarget | null {
    return this.votingTarget ?? null;
  }

  @computed
  get isVoting(): boolean {
    return this.currentVote !== null;
  }
  ```
- `source/common/types/governance.types.ts:18` — one line, Step 4:
  ` * Populated by normalizeDRepIdentity (cv-1, task-129).` →
  ` * Populated by normalizeDRepIdentity.` The sentence, the surrounding
  block comment, the interface and every member doc comment are byte-identical.
- Nothing else. No deleted line other than that comment text; `WalletProps`
  (`:130`) and the type-only import (`:11`) were correctly NOT re-touched —
  both already carried task-130's entries.

**Review method (four concern areas in one round, adversarial refutation).**
The round returned eight raw findings, all fresh. Every candidate blocker was
handed to three independent skeptics and allowed to stand only if it survived;
seven were refuted 3-0 and one was confirmed. The concern areas represented
were guide fidelity and tracker bookkeeping; sanitization-floor and log-sink
reachability of the new field name; MobX runtime semantics of `@observable`
plus the pick list; and the temporal/edge-case semantics of the two computeds
against the four delegation statuses.

**Blockers raised and adjudicated.**

1. *Confirmed — the tracker row.* The task-131 row in
   governance-drep-discovery-plan-tasks.json was still `"status": "pending"`
   with no `statusReason`, while AC-5 requires the unfiltered run's suite and
   test counts to be reported there, and every sibling row (task-127 …
   task-130) carries both fields. Discharged by this task's scribe pass rather
   than by a code change: the row is now `complete` and its statusReason
   carries the verbatim counts. No source file changed as a result.
2. *Refuted 3-0 (raised twice, as two separate findings) — "AC-9's single
   commit does not exist yet".* Circular with the review's own premise: the
   subject is by definition the uncommitted diff, and this repo's per-task
   commits bundle this very log (`git show --stat 1d33baa2c` and `40bcd990a`
   each carry cv-1-code-review.md), so the commit is structurally downstream
   of the review. AC-9's review-time-verifiable half is independently met —
   the diff adds zero code comments, and the one comment it touches *removes*
   a task id. The index is empty and exactly two paths are modified, so the
   tree makes the atomic commit the default outcome, not the hard one.
3. *Refuted 3-0 — unfiltered wallet payloads at `api.ts:379-383`/`:458-460`.*
   Technically accurate but zero-delta: api.ts is byte-identical to HEAD, the
   payload shape blames to 0b74fb818 (2021-04-15), and the fix is already
   specified as task-170 (guide `:2714-2942`, including the missing
   `getWallets` floor case). Already on record as F-11.
4. *Refuted 3-0 as a task-131 defect — `votingTarget` is not in
   `filterLogData`'s key list.* The exposure is pre-existing at HEAD: the
   constructor is `Object.assign(this, data)` (`Wallet.ts:175-177`),
   `WalletProps.votingTarget` landed at `:130` in task-130 and `api.ts:3153`
   already passed the value in, so every `Wallet` instance already carried the
   key before this diff; the two computeds are prototype accessors and never
   appear in `Object.keys`, so the diff adds no redactor surface at all. The
   underlying key-name gap is real and unrecorded, so it was kept — see F-15
   below — rather than fixed here, where it would mean editing a third file
   and the frozen floor suite.
5. *Refuted 3-0 — `@observable` deep-converts, so `currentVote` returns an
   observable wrapper rather than the identical object `parseVoting` built.*
   Reproduced, but identical for the pre-existing sibling observables
   (`pendingDelegations`, `syncState`) and for the instance itself — a
   `Wallet` has never been structured-cloneable — so nothing here shifts.
   Plain `@observable` is guide-verbatim inside a "do not revisit" block, and
   `observable.ref` has zero precedent in the repo. Invariant 10 is unaffected
   because the encodings are primitive strings: `cip129`, `cip105` and `raw`
   compare byte-equal through the computed. The one useful residue — task-134
   must assert with `toJS(...)`/`toEqual`, never `toBe` — is already written
   into the approved guide at `:2080-2083`, so it needs no new record.
6. *Refuted 3-0 — `isVoting` is false when `delegationStakePoolStatus` is
   `'voting'` but the wire id fails to parse.* This is the specified contract,
   not a defect: AC-2 defines `isVoting` as `currentVote !== null`, Step 3 is
   implemented verbatim, and the design's §6.2 table
   (current-vote-display-design.md:112) already rules that an unparseable
   target is treated "as if `voting === undefined`". The `currentVote == null`
   render is a designed first-class state (guide `:1445-1447`: the
   reward-withdrawal warning plus CTA, never a hidden panel), and task-134's
   approved spec asserts `isVoting === false` for exactly this case.
7. *Refuted 3-0 — `isDelegating` counts pending delegations while `isVoting`
   counts only the active target.* The asymmetry is real but pre-existing and
   untouched: `isDelegating`'s `lastDelegationStakePoolStatus || …` precedence
   at `Wallet.ts:245-247` is unmodified context. D-10 (guide `:1252-1255`)
   explicitly forbids a `pendingVote` computed in v1, and the pending data
   stays reachable on `pendingDelegations`, which sits directly above
   `'votingTarget'` in the same pick list. No consumer pairs the two computeds
   — none exists yet at all.

**Acceptance criteria.** AC-1 met — `currentVote` (`:254-257`) returns the
stored target or `null`; behavioural pin deferred to task-134. AC-2 met —
`isVoting` (`:259-262`) is literally `this.currentVote !== null`; same
deferral. AC-3 **not discharged by this diff, by design** — the guide
(`:1273-1279`, restated at `:1400-1402`) formally assigns "all four delegation
statuses plus pending" to the task-134 specs
(`tests/jest/api/createWalletFromServerData.spec.ts` for the statuses and the
`wallet-delegating-and-voting.json` pending fixture,
`tests/jest/api/walletVotingComputeds.spec.ts` for the computeds and `update()`
propagation), and both files were confirmed still absent. Writing them here
would collide head-on with task-134 Step 2. AC-4 met — `'votingTarget'` at
`:200`. AC-5 met — the gate was the unfiltered `node_modules/.bin/jest
--runInBand`, and its counts are recorded in the statusReason. AC-6 met — all
four excluded Wallet-importing specs plus the sanitization floor appear as
`PASS` by name in that log. AC-7 confirmed **already met at HEAD** by commit
2ee5f74cf and deliberately not re-edited: guide `:1376` (this task) and
`:1800` (task-132) both read `node_modules/.bin/jest --runInBand   # whole
tree: all 82 suites stay green`. AC-8 met — the parenthetical is gone, the
sentence intact, and `grep -rn "task-1[0-9][0-9]" source tests storybook`
returns zero hits. AC-9 met in substance — both files sit in one change set
with an empty index, and the diff introduces no comment at all.

**Comment convention.** Zero comments were added. The diff's only comment
change is a deletion of a task id from an existing block comment; the
remaining sentence states what populates the type, which is the invariant and
not change history. Nothing carries a review label, an ALL-CAPS marker or a
task reference.

**No unnecessary complexity.** Thirteen added lines: one field, one string in
an existing list, two computeds. No new file, no helper, no new export, no
error type, no caching, no `observable.ref` one-off, no historical-vote field
and no `pendingVote` computed (D-10). Scope fence clean in both directions —
`api.ts` is byte-identical, and no spec file was created (task-134's boundary).

**Verification commands run (results as observed).**

1. `node_modules/.bin/tsc --noEmit` → exit 0, zero diagnostics. Used as the
   authoritative gate over `yarn compile`, which prepends `typedef:sass` and
   this task touches no scss.
2. `node_modules/.bin/eslint` on the two changed files → exit 0; 20 problems,
   0 errors, 20 warnings. Proven to be exactly the HEAD baseline by re-linting
   the HEAD blobs through `--stdin --stdin-filename` (11 + 9 = 20, same rule
   and line set). Every warning sits outside the edited hunks.
3. `node_modules/.bin/jest --runInBand` — **unfiltered, no `tests/jest`
   argument** (AC-5) → exit 0 in 46.841 s. Test Suites: 1 skipped, 81 passed,
   81 of 82 total. Tests: 12 skipped, 1038 passed, 1050 total. Snapshots: 2
   passed. `grep -n "^FAIL"` over the log → no matches. The single skipped
   suite is the pre-existing `tests/jest/governance/GovernanceCliArgvSmoke.
   spec.ts`; this diff modifies no test file.
4. The five AC-6 specs confirmed `PASS` by name in that log —
   `WalletSendForm.spec.tsx` (12.179 s),
   `tests/jest/security/governance-sanitization.spec.ts`,
   `VotingGovernancePage.spec.tsx`,
   `VotingPowerDelegationConfirmationDialog.spec.tsx`, and
   `tests/wallets/unit/wallet-utils.spec.ts`.
5. `node_modules/.bin/jest --runInBand --coverage=false
   tests/jest/security/governance-sanitization.spec.ts` → 1 suite passed,
   23 of 23 tests. The inherited floor holds exactly; the diff adds no logger,
   analytics or electron-store sink.
6. `grep -n "votingTarget" source/renderer/app/domains/Wallet.ts` → exactly 4
   hits (`:130` WalletProps, `:166` observable, `:200` pick list, `:256`
   computed body), matching the guide's prediction at `:1372-1374`;
   `grep -rn "task-1[0-9][0-9]" source tests storybook` → zero hits (AC-8);
   `grep -rn "interface DRepIdentity" source | wc -l` → 1.
7. `git diff --name-only` → the two files only, so invariant 4 holds:
   `VotingPowerDelegation.tsx`, `VotingStore.ts` and `routes-config.ts` are
   byte-identical. `git diff package.json` → empty.
8. `node_modules/.bin/prettier --check` on both files → "All matched files use
   Prettier code style!", exit 0, and the same check against the HEAD blobs is
   equally clean — neither pre-existing nor introduced drift. `--write` was
   never run.

**Out-of-scope observations carried forward.** Newly recorded as F-15: the
renderer-side key names `votingTarget` / `currentVote` are absent from
`filterLogData`'s `sensitiveData` list (source/common/utils/logging.ts:24-48,
which is keyed to the wire shape — `drepId`, `vote`, `voting`), and the floor
suite's redaction cases are all wire-keyed too, so the domain shape is
unguarded the moment cv-2 gives it a consumer. F-11 / task-170 remains open and
does not close it: task-170 wraps wire payloads whose key is `voting`. Still
carried from earlier entries: F-5 — `nix fmt` cannot run in this devcontainer,
prettier is the substitute and the format pass is owed pre-merge. Finally, the
`tests/jest` path filter that AC-7 removed from `:1376` and `:1800` survives at
guide `:566`, `:693` and `:1191`; all three sit inside the already-complete
task-127 / task-128 / task-130 sections, so no pending task can be misled by
them and they were left alone (F-13 records the underlying 7-of-82 fact).

Decision: approved

---

## Code Review: task-132 — iteration 1 (2026-07-28)

**Scope reviewed.** The working tree against the guide section "task-132:
`CurrentVoteSummary` CORE states, no live badge"
(cv-1-implementation-guide.md:1416-1829 — the three-file "Files created (all
new)" list at `:1418-1422`, the context block at `:1424-1435`, the seven
inline locked invariants at `:1437-1463`, the six resolved judgment calls at
`:1465-1491`, Steps 1-3 with verbatim file bodies at `:1500-1574`,
`:1590-1667` and `:1676-1779`, the Step 4 verify block at `:1789-1807` and
the eight-item acceptance checklist at `:1814-1829`), the design contract
(designs/current-vote-display-design.md:177-179 the pinned prop type,
`:184-187` the four render rules, `:189` the no-cli / no-fallback-IPC rule)
and task-132's six acceptance criteria in
governance-drep-discovery-plan-tasks.json. This is a create-only task: three
new untracked files, 251 lines, `git diff --name-only` empty, HEAD unchanged
at 2baed760c on `wt/cv-1-task-132`. One review round.

**What landed.** All three files are byte-identical to the guide's verbatim
Step 1-3 blocks — established by extracting `:1501-1573`, `:1591-1666` and
`:1677-1778` and comparing the exact bytes, not by eye.

- `CurrentVoteSummary.messages.ts` (73 lines) — the full 12-key core
  inventory under `voting.governance.currentVote.*`: 12 keys, 12 ids, 12
  `!!!` prefixes (invariant 11), and zero id collisions anywhere else in
  `source`, `storybook` or `tests`. Includes `drep.viewDetails` and
  `drep.anchorMetadata`, defined but deliberately not rendered in cv-1.
- `CurrentVoteSummary.scss` (76 lines) — 12 classes, every colour routed
  through a `var(--theme-*)` / `var(--badge-*)` token. No fixed height and no
  `text-overflow: ellipsis`, which is what keeps task-135's ja-JP overflow
  review meaningful.
- `CurrentVoteSummary.tsx` (102 lines) — function component with `injectIntl`
  + `intlShape` typing per the `DRepIdDisplay` precedent; props exactly
  `{ currentVote: WalletVotingTarget | null }` plus the injected `intl`.
  Three returns: `currentVote == null` → title + `role="alert"`
  reward-withdrawal warning + subline + CTA `Button`; `kind === 'drep'` →
  header + "Delegated to DRep" badge + `<DRepSourceLabel source="on-chain" />`
  + `<DRepIdDisplay drepId={currentVote.drep.raw} />`; sentinels → header +
  glyph badge + caption, no id row.
- Nothing else. No spec file (task-134's boundary), no story and no
  `storybook/stories/index.ts` registration (task-133's), no catalog edit
  (task-135's), and `VotingPowerDelegation.tsx` byte-identical — mounting is
  cv-2/task-139.

**Review method (four lenses, three-skeptic adversarial refutation).** Four
independent lenses were run in parallel over the diff: guide fidelity; locked
invariants plus the sanitization floor plus the scope fence; runtime/React
correctness; and conventions/hygiene. Every raw finding any lens produced was
then handed to three blind skeptics working distinct angles — reproduce,
guide authority, scope — and a finding died once two or more refuted it.
Nothing survived: zero confirmed blockers and zero confirmed nits, in one
iteration.

Per-lens outcome. Guide fidelity — clean; all three files byte-identical to
the Step blocks and every acceptance box discharged. Invariants / floor /
scope — clean; all three boundary greps empty and the floor suite green at 23
of 23. Runtime and React correctness — clean; no candidate reproduced.
Conventions and hygiene — clean; eslint and prettier both silent on the new
files.

**Candidate findings adjudicated (none survived).** The notable ones:

1. *Refuted — "the DRep state under-renders against the design."* Design
   `:185` specifies name, the live active/inactive/expiring badge, an in-app
   details link and an anchor URL link for `kind === 'drep'`; the component
   renders the id row only. Not a defect: §9.1's own heading is scoped
   `(task-132, task-136)`, the guide's inline invariants at `:1439-1442` and
   `:1460-1461` defer the badge to cv-2/task-136 and name/anchor to
   anchor-1/anchor-2, and tracker AC-3 and AC-5 say the same inside the row
   itself. The under-render is the specification.
2. *Refuted — "no live badge should still mean a status caption."* Design
   `:189`'s "Status unavailable" caption belongs to the badge feature; with
   the badge deferred there is no unavailable state to caption. Producing one
   would have required reading `GovernanceStore.drepIndex`, which invariant 14
   forbids outright — the proposed fix was itself the violation.
3. *Refuted — "the CTA `Button` carries no `onClick`."* Guide `:1481-1485` is
   an explicit do-not-revisit ruling: props are pinned to `{ currentVote }` by
   design `:177-179`, cv-2/task-139 mounts and wires the panel, and
   `ButtonSkin` spreads only DOM-safe props onto a plain `<button>`, so the
   absent handler is inert, not an error.
4. *Refuted — "the sentinel branch is not exhaustive."* The final return
   derives `isAbstain` from `kind === 'abstain'` and treats everything else as
   `no_confidence`. `WalletVotingTarget` (api/wallets/types.ts:86-93) is a
   closed three-member union whose `drep` arm already returned two branches
   earlier, so the residue is exactly the two sentinels; `tsc` is the guard if
   a fourth kind is ever minted, and the shape is guide-verbatim.
5. *Refuted — "the twelve new ids are absent from en-US.json and ja-JP.json."*
   Confirmed absent (grep: 0 hits in both), and that is the designed order:
   guide `:1467-1473` places the messages module here and assigns catalog
   population to task-135, and `:2091-2093` already warns task-134 to expect
   react-intl missing-message output until then. No spec asserts catalog
   coverage and the unfiltered suite is green.
6. *Refuted — "raw glyph literals in JSX breach the no-English-literals
   rule."* `!`, `●`, `⊘` and `✕` are `aria-hidden="true"` decorations paired
   with localized text in every case (guide `:1489-1491`); D-4 governs label
   copy, and every user-visible string in the file passes through
   `intl.formatMessage`.
7. *Refuted — "rendering `drep.raw` into the DOM widens the redaction
   surface."* Rendering ids in the DOM is explicitly permitted by invariant 2
   (`:1462-1463`); the component adds no logger, analytics or store sink — the
   first boundary grep proves it — and the floor suite still passes 23 of 23.
   The pre-existing `filterLogData` key-name gap is already on record as F-15
   and is untouched here.

**Acceptance criteria.** AC-1 met — `<DRepIdDisplay
drepId={currentVote.drep.raw} />` and `<DRepSourceLabel source="on-chain" />`
in the `drep` branch; `DRepSourceLabel` declares the optional `className` this
call passes (`DRepSourceLabel.tsx:20-24`). AC-2 met — the `null` branch
renders the reward-withdrawal warning, the no-auto-delegation subline and the
CTA; the panel is never hidden and never collapsible (invariant 9). AC-3 met —
the `drep` branch renders the id and nothing else: no `givenName`, no anchor
URL, no view-details link, though both link keys exist in the messages module
for task-135's catalog pass. AC-4 met — both sentinel states render a badge
plus a caption with no id row (invariant 13). AC-5 met — no live badge
anywhere; boundary grep 1 returns nothing. AC-6 met — the status labels render
from the component-local `defineMessages` set, which is exactly the
"CurrentVoteSummary-local renderer that consumes react-intl directly" branch
the criterion authorizes: `DRepSourceLabelVariant` is only `'on-chain' |
'on-chain-anchor-reference'` (`DRepSourceLabel.tsx:18`) and its prop contract
was not widened. The guide's two extra boxes hold too — props are exactly
`{ currentVote }` plus injected `intl` with no store, IPC or cli access, and
tsc, lint and prettier are clean on all three new files.

**Status vocabulary.** `complete`, and never `verified`: task-132 ships no
test of its own by design — its specs are task-134 (guide `:2091-2093`) — so
the dedicated proof beyond a task's own unit tests that `verified` demands
does not exist yet and must not be claimed for this row.

**Comment convention.** One comment in the whole change set,
`CurrentVoteSummary.tsx:16-18`: three plain lines stating why the status
labels bypass `DRepSourceLabel` (its variant union cannot express them). That
is the D-4 invariant, not change history; no task id, no review label, no
ALL-CAPS. It is guide-verbatim.

**No unnecessary complexity.** 251 lines across three files, all of it
required output. No helper module, no local `IntlProvider`, no barrel export,
no memoization, no defensive `try`/`catch`, and no normalization at all —
boundary grep 2 finds no `toLowerCase`, `toUpperCase`, `trim` or `normalize`,
so the wire string reaches `DRepIdDisplay` untouched and byte-equality
(invariant 10) survives the render path. Scope fence clean in both directions:
no tracked file modified, and none of task-133 / 134 / 135 / 139's files
created.

**Verification commands run (results as observed).**

1. `node_modules/.bin/tsc --noEmit` → exit 0; `error TS` line count 0. No
   missing `*.scss.d.ts` failure occurred, so `typed-scss-modules` was not
   needed and was not run — `import styles from './CurrentVoteSummary.scss'`
   resolves through the global `declare module '*.scss'` exactly as the guide
   predicts at `:1791-1794`.
2. `node_modules/.bin/eslint` on `CurrentVoteSummary.tsx` and
   `CurrentVoteSummary.messages.ts` → exit 0 with completely empty output: no
   problem lines and no summary line, i.e. 0 errors and 0 warnings on the new
   files, meeting the bar without needing a HEAD-baseline comparison.
3. `node_modules/.bin/jest --runInBand` — **unfiltered, whole tree, no path
   filter** → exit 0. Verbatim: "Test Suites: 1 skipped, 81 passed, 81 of 82
   total" / "Tests:       12 skipped, 1038 passed, 1050 total" /
   "Snapshots:   2 passed, 2 total" / "Time:        43.032 s" / "Ran all test
   suites." Zero `FAIL` lines and zero `●` failure blocks across 1128 lines of
   output. That is the stated baseline unchanged, which is the correct result
   for a task that adds no test.
4. `node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts
   --no-coverage --runInBand` → exit 0; "Test Suites: 1 passed, 1 total" /
   "Tests:       23 passed, 23 total" / "Snapshots:   0 total". The inherited
   23-of-23 floor holds exactly.
5. `node_modules/.bin/prettier --check` on `CurrentVoteSummary.tsx`,
   `CurrentVoteSummary.messages.ts` and `CurrentVoteSummary.scss` →
   "Checking formatting..." / "All matched files use Prettier code style!",
   exit 0. `--write` was not run, and `yarn prettier` was not run.
6. Boundary greps — all three returned nothing (each exit 1), which is the
   required result. (a) `GovernanceStore|drepIndex|DRepStatusBadge|givenName|
   anchorUrl|logger\.|console\.|analytics` over the tsx → no store, badge,
   name or anchor coupling and no logging or telemetry sink. (b)
   `toLowerCase|toUpperCase|\.trim\(|normalize\(` over the tsx → no local
   normalization. (c) `CurrentVoteSummary` over `VotingPowerDelegation.tsx` →
   not wired, which is correct for cv-1.
7. `git status --porcelain=v1 --untracked-files=all` → exactly three `??`
   lines (`CurrentVoteSummary.messages.ts`, `.scss`, `.tsx`) and nothing else;
   `git diff --name-only` → empty, so no tracked file moved. The `coverage/`
   directory the full jest run produced was confirmed ignored rather than
   assumed: `git check-ignore -v coverage` → `.gitignore:25:coverage
   coverage`, exit 0. Not a breach. No file under the main checkout was
   created, edited or deleted; every command ran against the isolated
   worktree.

**Out-of-scope observations carried forward.** Nothing new is recorded — no
candidate survived refutation, so this entry mints no finding. Still carried
from earlier entries and unmoved by this diff: F-5 — `nix fmt` cannot run in
this devcontainer, the explicit-path `node_modules/.bin/prettier` is the
substitute and the format pass remains owed pre-merge; and F-15 —
`filterLogData`'s `sensitiveData` list is keyed to the wire shape, so the
renderer-side names stay unguarded until a consumer exists. This component
adds no sink of any kind, so it neither advances nor worsens F-15.

Decision: approved

---

## Code Review: task-133 — iteration 1 (2026-07-28)

**Scope reviewed.** The working tree against the guide section "task-133:
Storybook entry, 4 core knobs" (cv-1-implementation-guide.md:1833-2015 — the
two-file "Files touched" list at `:1835-1838`, the Storybook-6.4 context block
at `:1840-1848`, the record-only orphan-story note at `:1850-1856`, the four
inline locked invariants at `:1858-1871`, the four resolved judgment calls at
`:1873-1881`, Step 1's verbatim file body at `:1889-1966`, Step 2's single
import line at `:1972-1979`, the Step 3 verify block at `:1981-2001` and the
five-item acceptance checklist at `:2003-2013`), the knob table in
designs/current-vote-display-ux.md:200-211, and task-133's two acceptance
criteria in governance-drep-discovery-plan-tasks.json (pre-scribe
`:1025-1028`). Two files and nothing else: one new untracked story of 78 lines
and one added import line in a tracked file. `git diff --name-only` →
`storybook/stories/index.ts` alone; HEAD unchanged at 23f443b76 on
`wt/cv-1-task-133`. One review round.

**What landed.**

- `storybook/stories/governance/CurrentVoteSummary.stories.tsx` (new, 78
  lines) — one `storiesOf('Governance / Current Vote Summary', module)` kind
  with one story, `Core states`. `PANEL_STYLE` top-level const (`:9-13`);
  the three checksum-verified vector consts (`:17-21`); the label→id map
  `CURRENT_VOTE_OPTIONS` (`:23-28`) carrying exactly four entries; the
  `resolveCurrentVote` switch (`:30-52`) whose `case 'noDelegation':` falls
  through to `default:` returning `null`; decorators in house order —
  `StoryProvider` wrapping `StoryDecorator` first (`:58-62`), `withKnobs`
  second (`:63`); one `select('Current vote (mock)', CURRENT_VOTE_OPTIONS,
  'noDelegation')` (`:65-69`); and a `<div style={PANEL_STYLE}>` wrapper
  around `<CurrentVoteSummary key={option} currentVote={…} />` (`:71-76`).
- `storybook/stories/index.ts:18` — `import
  './governance/CurrentVoteSummary.stories';`, inserted directly after
  `import './governance/DRepDirectory.stories';` (`:17`), inside the
  `// Voting` group and ahead of `// Settings` (`:19`). The whole diff is that
  one line. This is the load-bearing half: `storybook/main.ts:8` is `stories:
  ['../storybook/stories/index.ts']`, so an unregistered story never renders.
- Nothing else. No spec (task-134's boundary), no catalog edit (task-135's),
  no `_utils/fixtures.ts` or `GovernanceWrapper.tsx` (cv-2 task-143/task-144),
  no touch of `CurrentVoteSummary.tsx` or any file under `source/` or `tests/`
  — `git status --porcelain source/ tests/` is empty.

The story body was compared byte-for-byte against the guide's Step 1 block
rather than by eye. The only delta in the whole file is the vector comment,
covered under "Comment convention" below.

**Review method (three lenses, adversarial refutation).** Three independent
lenses were run over the diff: (1) guide and design fidelity plus contract
conformance — the Step 1/Step 2 byte comparison, the ux §13 knob table, the
`WalletVotingTarget` / `DRepIdentity` literal, the `injectIntl` prop surface
and the decorator/`StoryDecorator` clone semantics; (2) locked invariants,
the sanitization floor and the scope fence — boundary greps plus a programmatic
bech32 decode and the floor suite; (3) simplicity, conventions and hygiene —
import order, exemplar shape, comment rules, and whether any construct in the
file earns its place. Every candidate any lens produced was then attacked on
the reproduce / guide-authority / scope axes and allowed to stand only if it
survived. **Zero blockers were confirmed, in one iteration**, and no change was
made to either file as a result of the review.

Per-lens decision. Fidelity — clean; the story is character-for-character the
guide's Step 1 block apart from the comment noted below, the registration line
is exactly where Step 2 puts it, and every knob label and id matches ux
`:201-209` verbatim including the em dash. Invariants / floor / scope — clean;
every boundary grep empty, both bech32 vectors decoded and proved byte-equal to
their committed fixtures, floor suite 23 of 23. Simplicity and conventions —
clean; nothing unused, and the three constructs that looked like candidates for
removal are all sibling-exemplar or guide-mandated shape.

**Candidates adjudicated (none survived as a blocker).**

1. *Refuted — "the vector comment deviates from the guide's verbatim block."*
   The guide's own text at `:1903` is `// Checksum-verified vectors from the
   cv-1 fixture set.`, which embeds a plan id. The shipped comment
   (`:15-16`) drops it and states the invariant instead. Following the guide
   here would have violated the comment convention; the deviation is the
   correct resolution, not drift.
2. *Refuted — "`source: 'onchain'` is inert, so the `drepUnverified` knob is
   wrong."* Accurate as an observation and wrong as a defect.
   `CurrentVoteSummary.tsx:65` hardcodes `<DRepSourceLabel source="on-chain"
   />` and never reads `currentVote.source`, but the field is mandatory on the
   `kind: 'drep'` arm of `WalletVotingTarget`
   (api/wallets/types.ts:86-93) so it cannot be dropped, and `'onchain'` is a
   do-not-revisit ruling in the guide at `:1875-1877` as the only source value
   the cv-1 pipeline emits. Kept, recorded below.
3. *Refuted — "the knob label 'DRep — unverified anchor' promises an anchor
   cv-1 does not render."* True of the render and irrelevant to the label: the
   labels are ux §13 wording (`:201-209`), the knob VALUES are the ids the
   acceptance criterion names, and the guide pins both at `:1873-1875`.
   Inventing new copy here would drift from the AC's own vocabulary and from
   the cv-2 story that converges on it.
4. *Refuted — "the ALL-CAPS `NOT` in the locale comment (`:54-56`) breaches the
   comment convention."* The three-line comment is house text, byte-identical
   in `DRepCategoryBadge.stories.tsx:38-40` and `DRepDirectory.stories.tsx:
   145-147` and reproduced word for word by the guide. Editing it in this one
   file would fork the same sentence across three governance stories for no
   gain. Its placement directly above `storiesOf` is also the better of the two
   precedents.
5. *Refuted — "`key={option}` is dead weight on a stateless component."*
   Reproduced: `CurrentVoteSummary.tsx:19-100` has no state, no effect and no
   ref, so React re-renders correctly on a prop change without a remount. But
   the guide pre-resolved it at `:1877-1879` ("applied … even though the
   component is stateless") inside a do-not-revisit block, and cv-2 task-144
   builds the same remount idiom into the shared wrapper. Cost is one prop.
6. *Refuted — "`CURRENT_VOTE_OPTIONS` + `resolveCurrentVote` could be one
   `Record` lookup."* It would save about ten lines and lose two things: the
   shape stops matching `DIRECTORY_STATE_OPTIONS` / `resolveDirectoryState`
   (DRepDirectory.stories.tsx:214-262), which is the pattern the guide cites,
   and the `default:` arm stops guaranteeing a rendered branch if a knob id
   ever drifts — a bare lookup would hand `undefined` to a prop typed
   `WalletVotingTarget | null`.
7. *Refuted — "`StoryProvider` is unnecessary for a storeless component."*
   True that `CurrentVoteSummary` consumes only `currentVote` plus injected
   `intl`, but the closest exemplar wraps an equally storeless component in the
   identical pair (`DRepCategoryBadge.stories.tsx:42-46`), and the same stack
   appears in the one governance story that is actually registered
   (`DRepDirectory.stories.tsx:264-272`), so it is runtime-proven rather than
   copied from an orphan.
8. *Refuted as a defect of this diff — "the 12 `voting.governance.currentVote.*`
   keys are absent from both catalogs."* Confirmed absent (grep count 0 in
   en-US.json and ja-JP.json against 12 in `CurrentVoteSummary.messages.ts`),
   and that is the designed order: the catalogs are task-135's, which itself
   waits on task-171. It is not a defect, but it does bear directly on AC-1 and
   is recorded there and as F-19 rather than dismissed.
9. *Refuted as attributable — `yarn storybook:build` exits 1.* The failure is
   in the MANAGER compile, at `storybook/addons/DaedalusMenu/register.tsx:12`.
   Structurally unreachable from either changed file: `main.ts:8` puts
   `../storybook/stories/index.ts` under `stories:` (preview graph) while
   `main.ts:13` puts `require.resolve('./addons/DaedalusMenu/register.tsx')`
   under `addons:` (manager graph), and the `swc-loader` rule for `/\.tsx?$/`
   is pushed inside `webpackFinal` (`main.ts:16-141`), which in Storybook 6.4
   configures the preview only — so the manager webpack genuinely has no loader
   for that file's JSX. Detail and the honesty qualifier are under verification
   command 4.
10. *Nit, not actionable — the `KEY_` prefix on `KEY_CIP129` / `KEY_CIP105` /
    `KEY_CREDENTIAL_HEX` (`:17-21`) encodes `credentialType: 'key'` but scans
    like "object key".* Guide-verbatim and byte-locked; renaming would break the
    byte comparison for no reader benefit.

**Acceptance criteria.** Neither is reported green, and the reasons differ.

AC-1 — "Four core knob values (noDelegation | drepUnverified | abstain |
noConfidence) render without console errors." **Partially demonstrated, not
met.** The four values are present, distinct and correctly wired: `noDelegation`
→ `null` → the warning + CTA branch (CurrentVoteSummary.tsx:20-47),
`drepUnverified` → the `kind: 'drep'` branch (`:57-70`) rendering exactly one id
row, `abstain` and `noConfidence` → the sentinel branch (`:74-98`) with no id.
The render path is type-clean, lint-clean, prettier-clean, and both files
transpile under the preview's real `swc-loader` options. What is NOT established
is the "without console errors" half, for two independent reasons. (a) The 12
message keys are absent from both catalogs, `StoryWrapper` passes `'en-US'` /
`'ja-JP'` into `<IntlProvider>` without a `defaultLocale`, and react-intl 2.9.0
routes every missing message through `console.error` in both locales — so the
clause is unsatisfiable until task-135 lands, in en-US as well as ja-JP. New
finding F-19 records the exact mechanism. (b) The build that would have
exercised a mount aborted for the unrelated manager reason, so there is no
runtime evidence at all. AC-1's console-error clause is **owed re-verification
after task-135**, in a running Storybook.

AC-2 — "Story renders in both en-US and ja-JP locales without overflow."
**Not verifiable in this environment.** It is a visual criterion and this
devcontainer has no browser; the automated floor the guide names at
`:1991-1994` (`yarn storybook:build` compiling) was run and did not pass, for
the pre-existing manager-side reason, so even the weak proxy is absent. The
ja-JP half is additionally premature: until task-135 supplies real Japanese
copy, ja-JP falls back to the `!!!`-marked English `defaultMessage`, so an
overflow judgement made now would not be the one that matters. Owed: a manual
pass in the main checkout driving the GLOBAL English/Japanese toggle across all
four knob values.

The guide's three extra acceptance boxes do hold, and were checked
independently. No local `IntlProvider`, no per-locale variants and no
`drepVerified` knob or fifth value — greps for `IntlProvider` and
`drepVerified` return only the word inside the `:54-56` comment and the
`drepUnverified` id at `:25` / `:32`. The story is registered
(`index.ts:18`). tsc, lint and prettier are clean on the new file.

**Status vocabulary.** `complete`, and never `verified`. The row ships no
executing assertion of its own — Jest cannot see it at all (`jest.config.js:129`
sets `roots: ['<rootDir>/tests','<rootDir>/source']`, so `storybook/` is outside
the test tree) — and its two criteria are adjudicated here by static reading
plus the compile/lint/transpile gate. `complete` matches the standing precedent
for browser-unverifiable Storybook work in this tracker: task-116 recorded five
stories as "tsc/eslint-verified only — no display in this devcontainer" and
task-122 recorded an acceptance path deferred to a later owner, both under
`complete`. The dedicated proof `verified` demands does not exist yet and must
not be claimed.

**Comment convention.** Two comments in the change set, both surviving the hard
test. `:15-16` states provenance and the actual invariant — the vectors are
byte-for-byte from the committed fixtures, and bech32 is case-insensitive so
they must never be re-cased or re-derived — in two plain sentence-case lines.
`:54-56` is the house locale comment, verbatim from two sibling stories. Neither
carries a task id, a review label or change history; `grep -nE
'task-[0-9]|cv-[0-9]|CAT-|CP-|ADR|DD-'` over the new file returns nothing.

**No unnecessary complexity.** 78 lines plus one import. One story, one knob, no
`withState`, no `action()` handler, no helper render function, no local
fixture module, no barrel export. Every const is referenced. The
`<div style={PANEL_STYLE}>` wrapper is load-bearing rather than decorative:
`StoryDecorator.tsx:31-39` clones children and injects `propsForChildren`
unless the child is a plain `div`, so keeping the div is what stops a stray
prop reaching `CurrentVoteSummary` — it must not be flattened in a later
cleanup. Scope fence clean in both directions: nothing under `source/` or
`tests/` moved, and none of task-134 / task-135 / cv-2's files were created.

**Verification commands run (results as observed).** Commands 1-3 and 4 were
re-run by this scribe pass and reproduced identically; 5-8 are as observed by
the verify pass.

1. `yarn compile` → exit 0. The `precompile` hook (`yarn typedef:sass`)
   regenerated the gitignored `*.scss.d.ts` set this fresh worktree lacked (316
   files), then `tsc --noEmit` produced zero diagnostics. `node_modules/.bin/tsc
   --noEmit` re-run directly → exit 0 in 19.186 s, no output. Consistent with
   F-16: the typings are not a compile precondition, they were simply generated
   by the hook.
2. `node_modules/.bin/eslint` on the two changed files → exit 0 with completely
   empty output: 0 errors AND 0 warnings from the new and changed files. Over
   the wider `storybook/stories/governance/` directory the run reports
   "2 problems (0 errors, 2 warnings)", both pre-existing `no-unused-vars` in
   `DRepDirectory.stories.tsx` at `140:23` (`drepId`) and `142:27` (`entry`) —
   delta against the baseline is exactly zero.
3. `node_modules/.bin/prettier --check storybook/stories/governance/
   CurrentVoteSummary.stories.tsx storybook/stories/index.ts` → "Checking
   formatting..." / "All matched files use Prettier code style!", exit 0.
   Explicit paths only; `--write` was never run and `yarn prettier` /
   `yarn prettier:check` were never invoked.
4. `yarn storybook:build` → **exit 1**, and the failure is not attributable to
   this task. Verbatim from the log: `ERR! => Failed to build the manager` /
   `ERR! Module parse failed: Unexpected token (12:18)` / `ERR! You may need an
   appropriate loader to handle this file type, currently no loaders are
   configured to process this file.` pointing at `render: () => <DaedalusMenu
   api={api} />`, with the offending module named in the stats as
   `storybook/addons/DaedalusMenu/register.tsx`. Four independent checks that
   it cannot originate here: the manager and preview are disjoint webpack
   graphs (`main.ts:8` vs `:13`) and the `/\.tsx?$/` swc rule is preview-only
   (pushed inside `webpackFinal`); `git status --porcelain storybook/addons/`
   is empty and `register.tsx` last changed in commit e39a29cf1; the diff is
   the two in-scope files only; and `grep -c` for `CurrentVoteSummary`,
   `stories/governance` and `stories/index` over the whole build log returns
   **0**. The preview compile had reached 55% / 18,761 modules when the manager
   aborted the run. **Honesty qualifier:** pre-existence is inferred from the
   graph separation plus the clean diff, not measured — reverting the story was
   forbidden and building at HEAD would have meant working outside the assigned
   worktree. Net effect: the automated stand-in for the visual pass is
   UNAVAILABLE, not green. `dist/storybook`, created by the aborted build, was
   proven ignored rather than assumed (`git check-ignore -v dist/storybook` →
   `.gitignore:57:dist`).
5. Compensating signal in place of the missing build: both in-scope files were
   transpiled through `@swc/core` with the exact `jsc` options `main.ts:72-94`
   hands to `swc-loader` for the preview → `OK …/CurrentVoteSummary.stories.tsx
   -> 2583 bytes`, `OK …/stories/index.ts -> 1390 bytes`, exit 0. That covers
   parse and transform under the real preview loader; it does NOT cover module
   resolution, scss loading or runtime render.
6. `node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts`
   → "Test Suites: 1 passed, 1 total" / "Tests: 23 passed, 23 total" in 4.0 s.
   The inherited floor holds exactly. The whole-tree run was not repeated: this
   task adds no spec and `jest.config.js:129` puts `storybook/` outside `roots`,
   so Jest cannot observe the diff.
7. Bech32 verified positively rather than string-matched. Both vectors were
   decoded with the repo's `bech32` package: `drep1y2sm9s75…23nmjy` → hrp
   `drep`, payload `22` + the 28-byte credential; `drep_vkh15xev…zu4a4l` → hrp
   `drep_vkh`, payload the same 28 bytes. So the two encode one credential,
   `KEY_CREDENTIAL_HEX` equals it, and `credentialType: 'key'` agrees with the
   CIP-129 header byte `0x22`. `Buffer.equals` against the fixtures confirms
   byte-identity with `tests/mocks/wallets/wallet-voting-drep.json:26` (58/58)
   and `tests/mocks/wallets/wallet-delegating-and-voting.json:27` (60/60), case
   untouched; the credential hex is the repo-canonical value already committed
   in `tests/jest/governance/normalizeDRepIdentity.spec.ts:11`. Invariant 10
   holds and the identity literal is internally coherent rather than a mash-up.
8. Boundary greps over the new file — all empty, which is the required result.
   `console\.|logger|Logger|analytics|electron-store|electronStore|ipcRenderer|
   localStorage` → no match (invariant 2). `drepVerified|givenName|anchorUrl|
   http|badge|status` → only the `drepUnverified` id and the vector comment
   (invariants 3, 8, 14). `defineMessages|defaultMessage|formatMessage|!!!` →
   no match, so the story authors zero copy and cannot touch a preliminary
   marker (invariant 11); `CurrentVoteSummary.messages.ts` still carries all 12
   and is unmodified. `store|drepIndex|actions|api\.` → no match.
   `git status --porcelain` before and after the full gate → exactly
   ` M storybook/stories/index.ts` and `?? storybook/stories/governance/
   CurrentVoteSummary.stories.tsx`, nothing staged, nothing else.

**Out-of-scope observations carried forward.**

- **The three orphan governance stories remain unregistered, deliberately.**
  `DRepCategoryBadge.stories.tsx`, `DRepDetail.stories.tsx` and
  `DRepDirectoryBanner.stories.tsx` exist under `storybook/stories/governance/`
  but are absent from `storybook/stories/index.ts`, so they never render. This
  is a pre-existing gap confirmed again at HEAD, explicitly out of cv-1 scope
  per the guide's record-only note at `:1850-1856`, and the implementer
  correctly did not silently register them. Recorded, not fixed. Note the
  second-order effect this had on the review: `DRepCategoryBadge.stories.tsx`
  is the PRIMARY style exemplar for this task and is itself unrendered, so the
  decorator stack was additionally cross-checked against
  `DRepDirectory.stories.tsx:264-272`, the one registered governance story.
- **Newly recorded as F-19** — the react-intl 2.9.0 missing-message path is
  `console.error`, not a warning, and it fires in en-US too because neither
  `StoryWrapper` nor `App.tsx` sets `defaultLocale`. This is the mechanism
  behind AC-1's undemonstrable clause and behind the task-171 → task-135 →
  AC-1 re-verify chain; the guide describes the symptom in four places but
  always as a "warning" and always framed around the Japanese toggle, which
  understates it.
- **Three guide-drift items, all cosmetic, none fixed** (cv-1 planning is
  closed and this scribe's mandate is its own task). The knob-pattern citation
  at `:1881` reads `:212-292`; the live `DRepDirectory.stories.tsx` block is
  comment `:212-213`, `DIRECTORY_STATE_OPTIONS` `:214-220`,
  `resolveDirectoryState` `:222-262`, decorators `:264-268`, the `select` call
  `:292`. The prose at `:1969-1970` says the `drep` literal typechecks
  structurally "no extra type import needed", while the same section's own code
  block at `:1896` imports `type { WalletVotingTarget }` — the code block is
  right, because `resolveCurrentVote` declares that return type; the prose
  presumably meant the separate `DRepIdentity` import, which is indeed
  unnecessary. And the Step 3 verify block at `:1981-2001` prescribes `yarn
  storybook`, which no devcontainer run can honour.
- Still carried and unmoved by this diff: **F-5** — `nix fmt` cannot run here,
  explicit-path `node_modules/.bin/prettier` is the substitute and the format
  pass is owed pre-merge; **F-15** — `filterLogData`'s key list is keyed to the
  wire shape, untouched here because the story adds no sink of any kind;
  **F-18** — design §9.1's `drep` paragraph is the combined cv-1+cv-2 card, and
  the knob label's "unverified anchor" wording is the same seam seen from the
  ux table rather than a second problem.

Decision: approved

---

## Planner: task-133 — post-review verification addendum (2026-07-28)

The review above recorded the `yarn storybook:build` failure as *inferred* to be
pre-existing, because reverting the story to test it was out of bounds. That
inference has since been replaced with a measurement, and the measurement also
produced better evidence for AC-1 than the review had.

**Measured.** A second worktree was checked out detached at `23f443b76` — the
commit this task branched from — with no changes at all and `node_modules`
symlinked. `yarn storybook:build` there exits 1 with the identical
`=> Failed to build the manager` / `Module parse failed: Unexpected token (12:18)`
at `storybook/addons/DaedalusMenu/register.tsx:12`. The break predates cv-1 and
is unreachable from any story file; the cause is `storybook/main.ts` registering
a `.tsx` addon into the manager graph at `:13` while the `swc-loader` rule at
`:71` sits inside the preview-only `webpackFinal` hook opened at `:16`.

**Better floor found.** `start-storybook` does not treat the manager error as
fatal. At clean HEAD it reports `manager … compiled with 1 error` and still comes
up with a successful `webpack built preview`. Re-run on this branch with the new
story present, the preview compiled clean in 35.9 s with zero `ERROR in`,
`Module not found` or `Failed to compile` lines. The preview graph is the one
`stories/index.ts` feeds, so this confirms the story links into the real
Storybook bundle — a stronger result than the `@swc/core` transpile the verifier
had fallen back on.

**Effect on the task-133 record.** No blocker, no code change. The tracker
`statusReason` was corrected to drop its now-false "no runtime evidence exists"
clause, and the mechanism plus its two spillovers — `yarn check:all` is red at
HEAD for this reason alone, and Storybook rows in this plan should use a clean
`yarn storybook` preview compile as their automated floor rather than
`yarn storybook:build` — are recorded as F-20 in `research/cv-1-findings.md`.
AC-2's owed item narrows to the human visual and overflow pass, which is not
blocked by this gap. AC-1 is unchanged: still owed after task-135, per F-19.

**Decision:** approved, unchanged.

---

## Code Review: task-134 — iteration 1 (2026-07-28)

**Scope reviewed.** The working tree against the guide section "task-134: Jest —
mapper, Wallet computeds, and `CurrentVoteSummary` core snapshots"
(cv-1-implementation-guide.md:2017-2552 — the four-file "Files created" list at
`:2019-2025`, the "Files modified" entry at `:2027-2031`, the jest/`roots`
context block at `:2033-2041`, the empirically verified import recipe at
`:2043-2061`, the four inline locked invariants at `:2063-2077`, the four
resolved judgment calls at `:2079-2095`, Step 1's verbatim file body at
`:2103-2279`, Step 2's at `:2285-2367`, Step 3's at `:2378-2473`, the Step 4
snapshot-review gate at `:2475-2486`, Step 5's one-case extension at
`:2488-2514`, the Step 6 verify block at `:2516-2530` and the eight-item
acceptance checklist at `:2532-2551`), and task-134's nine acceptance criteria
in governance-drep-discovery-plan-tasks.json (`:1055-1063`). Four files created
and one modified in place, and nothing else. HEAD unchanged at `051567976`
(detached, the task-133 commit `feat(gov): task-133 add CurrentVoteSummary
storybook entry`); `git status --porcelain` → exactly five entries, one ` M` and
four `??`. One review round.

**What landed.**

- `tests/jest/api/createWalletFromServerData.spec.ts` (new, 177 lines) — nine
  `it()` blocks in one `describe('_createWalletFromServerData voting
  mapping')`. Two hoisted `jest.mock` calls, of `utils/logging` (`:9-16`) and of
  `api/utils/request` (`:21-23`), which the guide's import recipe declares both
  required (`:2046-2057`); `const mockedWarn = logger.warn as jest.Mock`
  (`:25`); the four checksum-verified vector consts plus `POOL_ID` (`:28-33`);
  `loadFixture` (`:35-40`) and `withDelegation` (`:42-46`); `beforeEach`
  clearing the warn spy (`:49-51`). The five AC-1 mapping cases are the
  voting-only DRep (`:53-71`), `delegating_and_voting` (`:73-91`), `abstain`
  (`:93-100`), `no_confidence` (`:102-109`) and pending-`next` (`:111-118`); the
  three degradation cases are target-never-parsed (`:120-130`), unknown HRP
  (`:132-145`) and malformed value (`:147-160`); the ninth pins the untouched
  `delegating` / `not_delegating` paths (`:162-176`).
- `tests/jest/api/walletVotingComputeds.spec.ts` (new, 81 lines) — seven `it()`
  blocks in `describe('Wallet.currentVote / Wallet.isVoting')`: the three
  populated kinds (`:38-54`), explicit `null` and never-set (`:56-66`), and the
  two `update()` propagation directions (`:68-80`), which the guide names as
  "the executable form of the R-2 mitigation" (`:2369-2370`).
- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx`
  (new, 94 lines) — `renderSummary` (`:30-42`) wrapping the component in
  react-polymorph `ThemeProvider` + `IntlProvider locale="en-US"` with the
  en-US catalog, per the `VotingPowerDelegationConfirmationDialog.spec.tsx:30-55`
  precedent the guide cites at `:2374-2376`; four `it()` blocks, one per core
  state — `noDelegation` (`:47-53`), DRep (`:55-65`), `abstain` (`:67-79`),
  `no_confidence` (`:81-93`) — each ending in
  `expect(container.firstChild).toMatchSnapshot()`.
- `source/renderer/app/components/voting/voting-governance/__snapshots__/CurrentVoteSummary.spec.tsx.snap`
  (new, 176 lines, generated on the first run and committed as the guide
  requires at `:2481-2486`) — four entries.
- `tests/jest/governance/normalizeDRepIdentity.spec.ts` — EXTENDED in place, not
  re-created. `git diff` on the file is a single `+8`-line hunk appended after
  the eighth case and before the `describe`'s closing brace; the file goes from
  eight cases to nine and no existing line moves. This satisfies the tracker's
  own AC at `:1063`, which requires the spec be recorded as MODIFIED rather than
  created.
- Nothing else. No catalog edit (task-135's boundary), no touch of `api.ts`,
  `Wallet.ts`, `wallets/types.ts`, `CurrentVoteSummary.tsx` or any fixture under
  `tests/mocks/` — `git diff --name-only` names the normalizer spec alone.

The three authored spec bodies were compared against the guide's Step 1 / Step 2
/ Step 3 blocks rather than read for gist. Step 2 and Step 3 are byte-exact.
Step 1 carries exactly two added lines, both deliberate and both covered under
"The AC-7 deviation" below.

**Review method (three lenses, adversarial refutation).** Three independent
lenses were run over the diff: (1) guide and acceptance-criteria conformance —
the Step 1/2/3 byte comparison, the five-case AC-1 enumeration at guide
`:2087-2089`, the tracker's nine criteria at `:1055-1063`, and the Step 5
extend-don't-recreate constraint; (2) locked invariants and the sanitization
floor — the HRP-only payload and raw-string-absence assertions, byte-equality of
the bech32 vectors, the snapshot's negative constraints (no status badge, no
`givenName`, no anchor/view-details link) and the scope fence; (3) test quality
and repo convention — assertion strength, matcher choice, fixture-vs-synthetic
coverage, naming, and whether any construct earns its place. Every candidate any
lens produced was then attacked on the reproduce / guide-authority / scope axes
and allowed to stand only if it survived. **Twenty-four candidate observations
were logged across the three lenses — nineteen distinct once the four raised by
more than one lens are merged (the `nix fmt` obligation, the test-count
arithmetic, the AC-7 deviation and the task-135 snapshot hand-off). Zero
survived refutation as a blocker, in one iteration**, and no file was changed as
a result of the review.

Per-lens decision. Guide/AC conformance — clean; Steps 2 and 3 are
character-for-character the guide's blocks, Step 1 differs by the two
tracker-mandated assertions, and all nine tracker criteria are met. Invariants
and floor — clean; the sanitized-warning payload is asserted shape-exact, the
raw string is proven absent from the logged calls, the four snapshots carry no
`Active|Inactive|Expiring` text and no `givenName`/anchor/href, and the
task-111 floor suite is re-asserted at 23 of 23. Test quality and convention —
clean; nine hardening ideas were generated and every one was judged optional,
with the reasons recorded below rather than dropped.

**Candidates adjudicated (none survived as a blocker).**

1. *Refuted — "the two extra `expect(mockedWarn).not.toHaveBeenCalled()` lines
   deviate from the guide's verbatim Step-1 block."* Reproduced: the guide's
   `abstain` and `no_confidence` cases (`:2196-2210`) carry no such assertion,
   while the shipped cases do (createWalletFromServerData.spec.ts:99 and `:108`).
   The tracker outranks the guide here and names all four accepted-target cases
   explicitly (`:1061`). The code is right and the guide is stale; see below.
2. *Refuted — "the file ships nine tests where eight were predicted."* The
   guide's Step-1 block prescribes exactly nine `it()` blocks
   (`:2156-2277`) and the delivered file has those nine one-for-one and in the
   guide's order, ending with `maps delegating and not_delegating
   byte-identically to today` (`:162`). The prediction was arithmetic drift in
   the verify brief. An overshoot of one, never a shortfall.
3. *Refuted as a defect of this diff — "the four snapshots bake in the react-intl
   `!!!` fallback."* Confirmed: the 12 `voting.governance.currentVote.*` keys are
   absent from both catalogs, so every rendered string in the snapshot carries
   the leading marker (e.g. `!!!Your stake counts as Yes on every motion of
   no-confidence. Rewards can be withdrawn.` at
   `__snapshots__/CurrentVoteSummary.spec.tsx.snap:64`). That is the guide's own
   declared and accepted ordering (`:2090-2095`: the fallback is "byte-identical
   to what task-135 seeds, so the snapshots and text assertions are stable across
   the task boundary"). Not a defect; carried forward as a hand-off obligation.
4. *Refuted — "the tracker's task-134 description names a `Wallet.pendingVote`
   computed that no test covers."* The description does say it
   (governance-drep-discovery-plan-tasks.json:1044), and it is settled
   description drift: D-10 at cv-1-PRD.md:230-235 and the "Resolved (not open)"
   entry at `:470-471` both rule it out, task-131 forbids the computed in v1, no
   acceptance criterion requires it, and `grep pendingVote
   source/renderer/app/domains/Wallet.ts` returns nothing. There is no computed
   to test.
5. *Refuted — "AC-2's assertion is satisfied by fixture shape, not by the mapper
   branch."* Accurate and not a hole.
   createWalletFromServerData.spec.ts:57 asserts `delegatedStakePoolId` is null
   for the voting-only fixture, and `tests/mocks/wallets/wallet-voting-drep.json`
   has no `target` key under `delegation.active` (`:24-27`), so the default arm
   (`api.ts:3096`, `delegatedStakePoolId = target`) would also yield null. The
   invariant IS pinned — by the synthetic case at spec `:120-130`, which injects
   `{ status: 'voting', target: POOL_ID }` and still asserts null. Coverage is
   real; only the test that owns it differs from the one the AC names.
6. *Refuted — "no single case exercises `status: 'voting'` with BOTH `target` and
   `voting` present."* True. The VOTING arm sets `delegatedStakePoolId = null`
   unconditionally (`api.ts:3085-3088`), so spec `:120-130`
   (target-ignored-without-voting) and spec `:53-71` (voting-parsed-without-
   target) are jointly sufficient. A single collision case would be three lines
   and is recorded as optional hardening, not as missing coverage.
7. *Refuted — "the warning-payload assertions use `toEqual`, which tolerates
   extra keys."* `toEqual` at spec `:143` and `:156` tolerates an extra key only
   when its value is literally `undefined`; any DEFINED extra key — i.e. an
   actual leak — still fails. The floor is not holed. `toStrictEqual` would pin
   the shape harder and is recorded as optional.
8. *Refuted — "the raw-id absence sweep covers `logger.warn` only."* Confirmed:
   `JSON.stringify(mockedWarn.mock.calls)` at spec `:144` and `:157` scans the
   warn spy alone, while `debug`/`info`/`error` are mocked (`:11-14`) and never
   read. There is no gap today — `_createWalletFromServerData` contains no
   logging call, and the sole logging site on this path is `parseVoting`'s
   `logger.warn` (`api.ts:3025`). A sweep over the whole logger mock would be
   future-proof; recorded as optional.
9. *Refuted — "the malformed-value case omits `toHaveBeenCalledTimes(1)`."* Its
   sibling HRP case carries it (spec `:140`) and the malformed case (`:147-160`)
   does not, but it cannot silently pass: `const [, data] =
   mockedWarn.mock.calls[0]` at `:155` throws if the warn never fired.
10. *Refuted — "the DOM negative assertions match text nodes only, not
    attributes."* True of `queryByText(/drep1|drep_vkh|drep_script/)` (spec
    `:76`, `:90`) and of `queryByText(/Active|Inactive|Expiring/)` (`:62`).
    Coverage is nonetheless complete because the committed snapshot pins full
    markup including every `aria-label`, and the `abstain` / `no_confidence`
    entries contain no `drep` substring in any position. The `Active|Inactive|
    Expiring` regex is additionally meaningful on its own: a cv-2 status badge
    would render as `!!!Active` and the regex matches on substring.
11. *Refuted — "the DRep negative assertion is unfalsifiable."* Also true and
    also cheap: the `abstain` and `no_confidence` members of
    `WalletVotingTarget` (api/wallets/types.ts:92-93) carry no `drep` field, so
    there is no id those two states could print. Insurance against a future
    union change, not present-tense coverage.
12. *Refuted — "`toJS(wallet.currentVote)).toEqual(DREP_TARGET)` compares an
    object against itself."* walletVotingComputeds.spec.ts:38-41 does feed and
    compare the same `DREP_TARGET` const, but mobx deep-enhances into a NEW
    observable rather than mutating the source, so this is a genuine structural
    comparison. An independently written expected literal — the form the mapper
    spec uses at createWalletFromServerData.spec.ts:59-69 — would be a stronger
    pin; recorded as optional.
13. *Refuted — "the DRep test name says 'no badge' while the component renders
    one."* The name at CurrentVoteSummary.spec.tsx:55 reads "…and no badge
    (snapshot)", and `CurrentVoteSummary.tsx:59-64` does render `<span
    className={styles.statusBadge}>` with the `●` glyph and the `!!!Delegated to
    DRep` label, visible in the snapshot at `.snap:83-91`. The assertion the name
    describes is `queryByText(/Active|Inactive|Expiring/)` — "no LIVE status
    badge", the cv-2 task-136 element. The guide's Step-4 wording "NO badge
    markup" (`:2483`) has the same looseness. Wording only; the intended
    constraint is met and the `statusBadge` span is task-132's vote-kind
    indicator, not the deferred status badge.
14. *Refuted — "invariant 14's `givenName` and anchor clauses are pinned only by
    the snapshot."* Correct, and the guide's own Step-3 block has the identical
    gap, so it is not a deviation. A regression that started rendering
    `drepViewDetails` or `drepAnchorMetadata` — both already declared in
    `CurrentVoteSummary.messages.ts` and rendered by neither branch — would
    surface as a snapshot diff, which fails the suite. The sibling
    `VotingPowerDelegationConfirmationDialog.spec.tsx:84-96` chose an explicit
    negative `queryByText` for this class of invariant; recorded as optional.
15. *Refuted — "the DRep snapshot is brittle."* It embeds roughly 35 lines of
    react-polymorph Tooltip and Button internals (`.snap:102-135`), including an
    invalid `label="…"` DOM attribute on `<button>` (`.snap:131`). That is the
    honest output of `container.firstChild` over the real tree and exactly what
    Step 3 asks for; the cost is sensitivity to a react-polymorph bump rather
    than to `CurrentVoteSummary` alone. Noted, not fixed.
16. *Refuted — "`loadFixture`'s `path.join` + dynamic `require` defeats TS
    checking of the JSON."* True (`:35-40`), and `resolveJsonModule` is
    demonstrably on, since CurrentVoteSummary.spec.tsx:8 statically imports
    `en-US.json`. Four static imports cast through `unknown` would be smaller.
    The guide prescribes the `require` form verbatim (`:2138-2143`), so this is a
    guide-level preference, not drift.
17. *Refuted — "the comment at spec `:27` embeds a planning identifier."* It
    reads `// Checksum-verified vector set shared with the cv-1 fixtures.`;
    `cv-1` is a slice id rather than a task id, the guide prescribes the line
    verbatim (`:2130`), the fixtures themselves already carry `cv1` in their
    `name` values, and the sentence states a real invariant — these vectors must
    stay byte-identical to the committed fixtures. Kept.
18. *Nit, not actionable — the DRep snapshot carries the full raw id in three
    positions, not one.* The guide's Step-4 phrasing (`:2484-2485`) speaks of the
    visible text being truncated while the `aria-label` carries the full id; both
    hold (`.snap:125` is `drep1y2s…23nmjy`, `.snap:122` the full id), but the
    full id also appears as a text node inside the react-polymorph tooltip bubble
    at `.snap:115`. `getByLabelText(KEY_CIP129)` still resolves uniquely, and no
    listed constraint covers tooltip content. Flagged because a future
    `getByText` assertion on that id would be ambiguous.
19. *Nit, not actionable — `beforeEach` clears only `mockedWarn`* (spec
    `:49-51`). Same mechanism as candidate 8 and the same conclusion: correct
    today because `parseVoting` only ever calls `logger.warn`.

**The AC-7 deviation, and why the code is right.** The only substantive delta
between the shipped Step-1 file and the guide's block is two lines:
`expect(mockedWarn).not.toHaveBeenCalled();` as the last statement of the
`abstain` case (createWalletFromServerData.spec.ts:99) and of the
`no_confidence` case (`:108`). The guide's block omits both (`:2196-2210`). The
tracker requires both: acceptance criterion seven at
governance-drep-discovery-plan-tasks.json:1061 reads "The accepted-target mapper
cases (voting-only DRep, delegating_and_voting, abstain, no_confidence) assert
`expect(mockedWarn).not.toHaveBeenCalled()`, pinning the never-logs floor on the
accepted-id path and not only on the rejection paths" — four cases, not two. The
tracker is authoritative and the implementers followed it; both assertions pass.
Root cause is a partial edit: commit `2ee5f74cf` added exactly two of the four
owed single-line hunks. Two places in the guide are now stale in consequence and
were NOT edited by this review (cv-1 planning is closed and this task's mandate
is its own diff): the Step-1 code block itself, and the prose at `:2511-2512`
which still reads "Together with the two `expect(mockedWarn).not.toHaveBeenCalled()`
assertions in the Step-1 valid-DRep cases" where there are now four. The
acceptance bullet at `:2546-2547` carries the same narrower "valid-DRep mapper
cases" wording. Guide drift only; it changes nothing in the diff.

**Acceptance criteria.** All nine of the tracker's criteria (`:1055-1063`) are
met, and eight of them by an executing assertion rather than by reading.

AC-1 "All five mapping cases pass" — met. The five the guide enumerates at
`:2087-2089` are present as distinct cases: drep voting-only (`:53-71`), abstain
(`:93-100`), no_confidence (`:102-109`), delegating_and_voting (`:73-91`) and
pending (`:111-118`, asserting `pendingDelegations` length 1 plus both `last*`
fields). AC-2 "voting-only fixture asserts `delegatedStakePoolId === null`" —
met at `:57`, with the caveat under candidate 5 that the synthetic case at
`:120-130` is what makes the assertion load-bearing. AC-3 "Wallet computeds
covered for every `WalletVotingTarget` kind plus null" — met by
walletVotingComputeds.spec.ts, three kinds plus explicit-null plus never-set
plus both `update()` directions, seven cases. AC-4 "core-state snapshots cover
noDelegation / drepUnverified / abstain / noConfidence" — met; four snapshots
written and committed, with `DREP_VOTE`'s `source: 'onchain'` being the
drepUnverified state per the guide's own gloss at `:2543`. AC-5 — met and
measured: the wrong-length `drep_vkh` case is appended at
normalizeDRepIdentity.spec.ts:106-113 and the coverage run's "Uncovered Line
#s" column for `normalizeDRepIdentity.ts` is now EMPTY, where the baseline
showed `48` — the `bytes.length !== CREDENTIAL_BYTE_LENGTH` return inside the
`drep_vkh` / `drep_script` branch (`normalizeDRepIdentity.ts:47-49`), which is
exactly the guard the criterion names. AC-6 "the added vector is checksum-valid
… a 29-byte payload under a CIP-105 HRP" — met by construction rather than by
hand-authored string: `bech32.encode('drep_vkh', bech32.toWords(new
Array(29).fill(7)))`, so the checksum is valid by construction and the input
reaches the length guard instead of the decode `catch`; `grep -c "from
'bech32'"` over the spec returns 1, so no duplicate import was introduced. AC-7
— met, and it is the deviation discussed above; all four accepted-target cases
carry the assertion (`:70`, `:90`, `:99`, `:108`). AC-8 "the existing eight
cases and the sanitization floor stay green" — met: nine of nine and 23 of 23.
AC-9 "the spec is recorded as MODIFIED rather than created" — met in both
places: the guide's file list says so at `:2029-2031` and the diff is a pure
append.

The guide's own eighth acceptance box ("Floor suite green; whole-tree jest
green; `tsc` clean", `:2551`) holds on all three counts.

**Status vocabulary.** `verified` is defensible for this row, and it is the
first cv-1 row for which it is. Every preceding row shipped `complete, NOT
verified` with the same sentence — that the behavioural proof "arrives with
task-134" (recorded verbatim in the `statusReason` of task-126, task-127,
task-128, task-129 and task-130 in governance-drep-discovery-plan-tasks.json).
This row IS that proof: it is all executing assertion, its own gate is
mechanical, and the mapper, the normalizer's length guard, the Wallet computeds
and the four component states are now pinned by tests that fail loudly. What it
does not carry is any human-visual or running-app evidence, and it makes no such
claim.

**Comment convention.** Three comments in the change set, all surviving the hard
test. createWalletFromServerData.spec.ts:7-8 and `:18-20` state why each
`jest.mock` is structurally required (`global.electronLog` absent under jest;
`request.ts` builds a `global.https.Agent` at module scope) — non-obvious
mechanism, two and three plain lines. CurrentVoteSummary.spec.tsx:58 states the
invariant behind the `getByLabelText` choice: "DRepIdDisplay truncates the
visible text but exposes the full raw id." All three are guide-verbatim. None
carries a task id, a review label, an ALL-CAPS banner or change history; `grep
-nE 'task-[0-9]|CAT-|CP-|ADR|DD-'` over the three authored files returns
nothing, and the single `cv-1` occurrence is candidate 17, kept deliberately.

**No unnecessary complexity.** 352 authored lines across three specs plus a
176-line generated snapshot and an eight-line append. No test helper beyond
`loadFixture` / `withDelegation` / `makeWallet` / `renderSummary`, each used
three or more times; no shared fixture module; no `beforeAll`; no snapshot of
anything but `container.firstChild`. Every const is referenced. Scope fence
clean in both directions: nothing under `source/renderer/app/api/`,
`domains/`, `i18n/` or `tests/mocks/` moved, and none of task-135's or cv-2's
files were created.

**Verification commands run (results as observed).** All six command groups are
as measured by the dedicated verify pass; this scribe re-read the files and the
diff on disk and re-ran no command.

1. `yarn compile` → exit 0, zero diagnostics, `Done in 29.61s.` The only output
   ahead of `$ tsc --noEmit` is the `typed-scss-modules` `[GENERATED TYPES]`
   lines from the `precompile` hook. This also closes R-4: cv-1-PRD.md:462-466
   frames Node v24 `yarn compile` failure as a live risk and the guide repeats
   the framing at `:93-97` and in the Step-6 comment at `:2519` ("Node v24
   fallback"); the fallback was not needed here or in the preceding two rows, so
   the risk can be closed rather than merely carried.
2. `node_modules/.bin/jest tests/jest/api --runInBand` → "Test Suites: 3 passed,
   3 total" / "Tests: 18 passed, 18 total", exit 0. Per-suite:
   `createWalletFromServerData.spec.ts` 9, `walletVotingComputeds.spec.ts` 7,
   and the pre-existing `walletDelegationStatuses.spec.ts` 2. `grep -c "  it("`
   confirms 9 and 7 in the two new files.
3. `node_modules/.bin/jest tests/jest/governance/normalizeDRepIdentity.spec.ts
   --runInBand` → 1 suite / 9 tests passed, exit 0. The coverage row for
   `normalizeDRepIdentity.ts` reads 100 / 100 / 100 / 100 with an EMPTY
   "Uncovered Line #s" column; the baseline `48` is absent. This is the AC-5
   headline and the one gate result that could not have been inferred from
   reading.
4. `node_modules/.bin/jest …/CurrentVoteSummary.spec.tsx --runInBand` → 1 suite
   / 4 tests / "Snapshots: 4 passed, 4 total", exit 0. Component coverage in
   that run: `CurrentVoteSummary.tsx` and `CurrentVoteSummary.messages.ts` both
   100/100/100/100 with an empty uncovered column.
5. `node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts
   --runInBand` → "Test Suites: 1 passed, 1 total" / "Tests: 23 passed, 23
   total", exit 0. The task-111 floor is re-asserted exactly, with the new
   mapper spec's own logger mock in the tree.
6. Whole tree, no path argument, `--runInBand --coverage=false` → "Test Suites:
   1 skipped, 84 passed, 84 of 85 total" / "Tests: 12 skipped, 1059 passed,
   1071 total" / "Snapshots: 6 passed, 6 total", exit 0 in 42.02 s. Against the
   82 suites / 1050 tests / 2 snapshots pre-task baseline that is +3 suites, +21
   tests and +4 snapshots. The +21 (rather than +20) is candidate 2: the
   guide-mandated ninth mapper case.
7. `yarn lint` → exit 0 at "5591 warnings", `Done in 65.10s.` — delta against
   the pre-existing 5591-warning / 0-error baseline is exactly zero, and no
   warning in the output is attributable to any of the three added files.
8. `node_modules/.bin/prettier --check` on the three created files → "All
   matched files use Prettier code style!", exit 0. Explicit paths only;
   `yarn prettier` was never invoked, per the standing ~240-file drift hazard.
9. `git status --porcelain` → exactly five entries: ` M
   tests/jest/governance/normalizeDRepIdentity.spec.ts` plus four `??` paths
   (the two `tests/jest/api/` specs, the component spec, and the
   `__snapshots__/` directory containing only
   `CurrentVoteSummary.spec.tsx.snap`). No stray file, nothing staged, nothing
   committed, and no `*.scss.d.ts` noise (gitignored, as expected).

**Expected noise, recorded rather than counted as failure.** The two runs that
mount the component print react-intl `console.error` missing-message lines, e.g.
verbatim `[React Intl] Missing message:
"voting.governance.currentVote.noConfidence.caption" for locale: "en-US", using
default message as fallback`. Ten DISTINCT `voting.governance.currentVote.*` ids
appear across the four rendered states; the other two of the twelve belong to
states these snapshots do not render. Both runs still exit 0. This is F-19
(research/cv-1-findings.md:652) reproducing exactly as predicted — the
missing-message path is `console.error` and fires in en-US too — and it clears
when task-135 seeds the catalogs.

**Out-of-scope observations carried forward.**

- **Hand-off obligation for task-135, declared and accepted in advance.** The
  four committed snapshots capture the `!!!` `defaultMessage` fallback verbatim.
  When task-135 seeds `en-US.json`, the values must be byte-identical to the
  defaults in `CurrentVoteSummary.messages.ts` — including the leading `!!!` —
  or all four snapshots break at that task. The guide pre-authorised exactly
  this at `:2090-2095`. It is the intended invariant-11 canary, not a defect,
  but whoever runs task-135 needs to know the snapshots are watching.
- **Guide drift from CONFLICT-1, recorded not fixed** — the Step-1 block at
  `:2196-2210`, the prose at `:2511-2512` and the acceptance bullet at
  `:2546-2547` all still describe two `not.toHaveBeenCalled()` assertions where
  the tracker requires and the code ships four. Detail under "The AC-7
  deviation".
- **Record-only correction to this file's own history.** The task-170 planning
  entry at cv-1-code-review.md:1141-1143 states that "the cv-1 PRD and
  implementation guide are not rewritten by this entry". `git show --stat
  2ee5f74cf` reports `cv-1-implementation-guide.md` changed by 531 lines and
  `cv-1-PRD.md` by 109. The guide WAS rewritten. This resolves in the safe
  direction — the guide on disk is current and complete for task-134, which is
  what this review read — so it is noted for the record and nothing follows from
  it.
- **R-4 can be closed.** cv-1-PRD.md:462-466 and the guide at `:93-97` and
  `:2519` still treat `yarn compile` under Node v24.16.0 as a flakiness risk
  with a `node_modules/.bin/tsc --noEmit` fallback. Measured green again here
  (verification command 1), as in the two preceding rows. The fallback has never
  been needed.
- Still carried and unmoved by this diff: **F-5** — `nix fmt` cannot run in this
  devcontainer, explicit-path `node_modules/.bin/prettier` was the substitute
  and the format pass remains an owed pre-merge obligation; **F-19** — the
  react-intl `console.error` mechanism above; **F-15** — `filterLogData`'s
  `sensitiveData` list is keyed to the wire shape, untouched here because this
  diff adds no sink, only spies on one.

**Optional hardening, none of it required.** Recorded so the ideas are not lost:
a `{ status: 'voting', target: POOL_ID, voting: KEY_CIP129 }` collision case
(candidate 6); `toStrictEqual` on the two warning payloads (7); stringifying the
whole logger mock rather than `warn` alone (8, 19); an independently written
expected literal in the computeds spec (12); an explicit `queryByText` negative
for `givenName` and the anchor, matching
`VotingPowerDelegationConfirmationDialog.spec.tsx:84-96` (14).

**Blockers.** None.

Decision: approved

---

## Code Review: task-171 — iteration 1 (2026-07-28)

**Scope reviewed.** The working tree against the guide section "task-171:
Restore the ja-JP `!!!` markers and guard them"
(cv-1-implementation-guide.md:2947-3106 — the two-entry "Files touched" list at
`:2949-2954`, the twenty-key Context measurement at `:2956-2969`, the three
inline locked invariants at `:2971-2981`, the three resolved judgment calls at
`:2983-2994`, Step 1's nineteen-key list at `:2998-3027`, Step 2's verbatim
spec body at `:3029-3069`, the Step 3 verify block plus its bite-test
instruction at `:3071-3086`, and the six-item acceptance checklist at
`:3088-3106`), and task-171's five acceptance criteria in
governance-drep-discovery-plan-tasks.json (`:1087-1091`). One file created, two
modified in place, and nothing else. HEAD unchanged at `a3e352841` on branch
`wt/cv-1-build`; `git status --short` → exactly three entries, two ` M` and one
`??` directory. One review round. The main checkout `/workspaces/daedalus` was
never read, edited or run against.

**What landed.**

- `source/renderer/app/i18n/locales/ja-JP.json` — `git diff --numstat` reports
  exactly `19 19`. Verified by parsing HEAD and the working copy as ordered
  JSON rather than by reading the hunks: 1599 keys before and 1599 after, key
  ORDER byte-identical, exactly 19 values changed, and every one of the 19
  satisfies `cur[k] === '!!!' + head[k]` — a pure prefix with nothing else in
  the string touched. The set of keys whose HEAD value started `!!!` and whose
  new value does not is EMPTY, so nothing was stripped anywhere in the file.
  The 19 changed keys are the guide's 19 exactly (`:3004-3024`): the
  seventeen `governance.drepDirectory.*` keys, `governance.tabs.directory`
  (`:369`) and `sidebar.categoryTooltip.governance` (`:633`). File mode
  `100755` preserved and no whitespace-only hunk, so the tool-managed catalog
  was not reformatted.
- `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` (new, 26 lines, creating
  `tests/jest/i18n/`) — compared against the guide's Step 2 fence rather than
  read for gist: the fence was extracted programmatically from
  cv-1-implementation-guide.md:3038-3062 and is BYTE-EQUAL to the file on
  disk, including the four-line comment wording, the
  `REVIEWED_JA_JP_EXCEPTIONS` constant name, the `describe('preliminary copy
  markers')` string and the `it()` string. Path, directory placement alongside
  `tests/jest/{api,governance,security}`, and the no-config-change assumption
  all match `:2985-2988`; jest picks it up through `roots`
  (`jest.config.js:129`) with no edit.
- `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
  — 3 added / 3 removed, NOT in the guide's "Files touched" list and discussed
  under candidate 3 below. Three exact-text ja-JP assertions gain the prefix:
  `'!!!DRepディレクトリ'` (`:280`), `'!!!アクティブ'` (`:282`) and
  `'!!!オンチェーン'` (`:283`). The neighbouring `getByText(/投票権/)` at `:281`
  is a regex and was correctly left alone.
- Nothing else. `en-US.json`, `defaultMessages.json` and
  `translations/messages.json` are untouched on the delivered tree, which is
  the half of AC-4 this row owns and the explicit inline invariant at guide
  `:2978-2981`.

**Review method (three lenses, adversarial refutation).** Three independent
lenses were run over the diff: (1) guide and acceptance-criteria conformance —
the Step 1 nineteen-key enumeration, the Step 2 byte comparison, and the five
tracker criteria at `:1087-1091`; (2) locked invariants and the sanitization
floor — whether any marker was stripped or mutated, whether a localized label
can bleed into a status or identity comparison, and whether a stale hardcoded
Japanese literal survives anywhere in the tree; (3) tests, docs and complexity
— whether the guard bites or passes vacuously, whether the allow-list is padded,
and whether any construct earns its place. Every candidate was then attacked on
the reproduce / guide-authority / scope axes. **Three of the candidates raised
were dropped on checking and are recorded below with the reason. Zero survived
as a blocker, in one iteration**, and no file was changed as a result of the
review.

Per-lens decision. Guide/AC conformance — clean; Step 2 is character-for-
character the guide's block and Step 1 is the guide's exact nineteen keys.
Invariants and floor — clean; invariant 11 is strengthened rather than
weakened, and the task-111 floor suite re-runs at 23 of 23. Tests and
complexity — clean; the guard demonstrably bites and the allow-list is exact
rather than padded.

**Candidates adjudicated (three dropped, none survived as a blocker).**

1. *Dropped — "`yarn i18n:manage` is not clean, so AC-4 fails" (verify pass
   gate 4).* Reproduced exactly as the verifier measured: the run exits 0 but
   WRITES, reporting 12 added keys, all `voting.governance.currentVote.*`, and
   mutating four tracked files (`defaultMessages.json` +65,
   `translations/messages.json` +65, `en-US.json` +12, `ja-JP.json` +12).
   Dropped as a task-171 blocker for two independently checked reasons. First,
   it is pre-existing: `git show HEAD:<path> | grep -c
   voting.governance.currentVote` returns **0 for all four catalogs** at
   `a3e352841`, while `git ls-tree -r --name-only HEAD` confirms
   `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.messages.ts`
   IS committed there — so tasks 130-134 shipped the component without
   regenerating the catalogs and the same 12 additions appear on a pristine
   HEAD checkout. Second, those 12 keys are verbatim the NEXT row's stated
   deliverable: item 12 of the guide's Implementation Order
   (cv-1-implementation-guide.md:57-61, whose section body opens at `:2559`)
   defines task-135 as "`yarn i18n:manage` extracts the component's message
   definitions into both catalogs, keeping the leading `!!!`", and
   task-171's own inline invariant (`:2978-2981`) FORBIDS this row from editing
   the two generated catalogs. AC-4's first clause is therefore unsatisfiable at
   this position by the guide's own construction — a spec self-contradiction,
   not an implementation defect. Carried forward as an owed obligation below,
   not held against the commit.
2. *Dropped — "the new spec's `key in ja` filter silently skips the 12
   `voting.governance.currentVote.*` keys, so invariant 11 is unguarded for
   exactly the slice that introduced them" (verify pass, observations).* The
   premise does not hold. Measured on the delivered tree: en-US and ja-JP are
   perfectly key-symmetric — 1599 keys each, **0 en-only and 0 ja-only** — so
   `key in ja` is currently a no-op and masks nothing. And the currentVote keys
   are absent from **en-US as well**, so `Object.keys(en)` never yields them and
   the `key in ja` clause is never reached; tightening that filter would catch
   nothing. The real reason they are unguarded is candidate 1 — task-135 has not
   run. The gap closes on its own, because the i18n runner seeds a missing key
   into BOTH catalogs simultaneously (the verifier's own gate-4 measurement
   shows `en-US.json +12` AND `ja-JP.json +12`), so the 12 land inside the
   guard's domain the moment task-135 commits. Also covered by the guide's
   explicit do-not-revisit at `:2993-2994`.
3. *Dropped as a defect, recorded as a deviation — "the diff touches a third
   file the guide's 'Files touched' list omits."* True and confirmed:
   `DRepDirectory.spec.tsx` changed 3/3, and the guide's Step 3 even asserts
   `git diff --stat  # ja-JP.json and the new spec only` (`:3077`). It is
   nonetheless the smallest truthful change and the guide's own final
   acceptance (`:3106`, the unfiltered jest run green) is unreachable without
   it: the three assertions are exact-text `getByText` matchers that cannot
   match once the marker is restored. The edit also matches the file's own
   established local convention rather than inventing one — the same spec
   already hardcodes `'!!!DRep Directory'` (`:169`), `'!!!Active'` (`:171`),
   `'!!!Page 1 of 2'` (`:267`), `{ name: '!!!Previous' }` (`:269`) and
   `'!!!Loading DRep data…'` (`:292`) for en-US. Guide oversight, not
   implementer overreach; recorded here so the file list reconciles.
4. *Refuted — "the guard could pass vacuously."* Checked directly: the
   asymmetric guard fires only when the en-US value starts `!!!`, so any of the
   19 lacking the en-US marker would be silently unprotected. All nineteen
   carry `!!!` in en-US. Reach is real rather than token: 129 en-US keys
   currently start with `!!!`, 84 of them under `governance.*`. The guard was
   additionally proved to bite two ways — the verify pass stripped the marker
   from `governance.drepDirectory.title` with Edit and the focused spec exited
   1 naming that exact key, then returned to green and to `19 19` on restore;
   this review re-derived the same result non-destructively by running the
   spec's predicate over both catalogs in memory (`[]` as delivered,
   `['governance.drepDirectory.title']` with the marker removed from a copy).
5. *Refuted — "the allow-list whitewashes a governance key."* The predicate was
   re-run with the allow-list removed entirely and returns exactly one key,
   `wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement`,
   whose en-US value is literally `!!!ヶ月,か月` against a ja-JP `ヶ月,か月` — the
   pre-existing month-unit oddity the guide names as the twentieth key
   (`:2963-2969`). Exact, not padded, and not governance.
6. *Refuted — "a stale hardcoded Japanese literal survives elsewhere in the
   tree."* Every one of the 19 pre-change values was grepped fixed-string
   across `source/`, `storybook/`, `tests/` and `translations/`. Four files
   outside the catalogs matched and all four are coincidental substring hits on
   generic vocabulary, not consumers of these message ids:
   `source/main/locales/ja-JP.json` (`コピー`, the main-process Edit menu),
   `source/renderer/app/config/newsfeed-files/news-automatic-update.dummy.json`
   (`更新`, `ガバナンス`) and
   `source/renderer/app/i18n/locales/terms-of-use/ja-JP.md` (`更新`).
   `DRepDirectory.spec.tsx` is the only real consumer and it is already fixed
   in this diff. No `.feature` file, no story and no runtime module hardcodes
   any of them.
7. *Refuted — "a prefixed label could perturb filter or identity semantics."*
   `DRepDirectoryFilters.tsx:148-153` gives the status `<option>` elements
   hardcoded literal values (`'all'` / `'active'` / `'inactive'`) with
   `intl.formatMessage` supplying only the child text, so
   `status.active`/`status.inactive` cannot reach a comparison.
   `governance.drepDirectory.copyId` is a button label;
   `_shared/DRepIdDisplay.tsx:58` copies `drepId` itself and its two
   `logger.warn` payloads (`:52`, `:62`) carry only `drepIdLength` / `error`.
   Invariants 10 and 14 are untouched.
8. *Nit, not actionable — the spec's comment is four lines against the
   convention's "1-3 plain lines"* (preliminaryCopyMarkers.spec.ts:4-7). It
   states the invariant and the why, carries no task id, review label, ALL-CAPS
   marker or change history, and is guide-verbatim. Left as is.
9. *Recorded, not a defect — the guard is one-directional.* By design
   (`:2989-2992`) it never fires for a key minted with NO `!!!` in en-US at all,
   a case invariant 11 also binds. Risk is low today because en-US markers are
   generated from each component's source `defaultMessage` and all 12
   `CurrentVoteSummary` defaults carry the marker. Noted so cv-2 and anchor-2
   authors know the direction the guard does not cover. The opposite asymmetry
   — 11 pre-existing non-governance keys marked in ja-JP but not en-US, e.g.
   `staking.stakePools.tableHeader.roi` — is the benign one and is ignored on
   purpose.
10. *Not re-litigated — the guide's "all 82 suites stay green" (`:3072`,
    `:3106`).* Known-stale; the measured baseline is 85 at HEAD and 86 with
    this slice's new suite. Doc hygiene for whoever closes cv-1, not a finding
    against this diff.

**Ordering note.** The guide's "Implementation Order" section
(cv-1-implementation-guide.md:18-61) lists task-170 at item 10 (`:49-52`),
task-171 at item 11 (`:53-56`) and task-135 at item 12 (`:57-61`) — 170 → 171 →
135.
The orchestration ran 171 → 135 → 170. Both are valid topological orders of the
same graph: in governance-drep-discovery-plan-tasks.json task-171's
`dependencies` is `[]`, task-135's is `["task-132", "task-171"]`, and
task-170's is `["task-130", "task-109"]` — task-170 is independent of both
others, so it may sit anywhere after task-130. The orchestrator's rule
(dependency order first, then the tracker's own JSON listing order, which runs
task-134, task-171, task-135, task-170) plus research finding F-19
(research/cv-1-findings.md:652 — task-133's AC-1 stays unsatisfiable until
task-135 seeds the catalogs) yields 171 → 135 → 170, and it puts the
timing-critical guard first: task-171 is the only thing that stops task-135 from
minting twelve fresh governance strings into ja-JP unmarked, which the guide
itself argues at `:2967-2969` ("A guard landing after the mints protects
nothing") and at item 11 of its own ordering. This is a recorded deviation from
the guide's ordering, not a conflict, and it changes nothing in any task's
content.

**Acceptance criteria.** Four of the tracker's five criteria (`:1087-1091`) are
fully met; the fifth is met in the half this row owns and is unsatisfiable in
the other half by the guide's own ordering.

AC-1 "all nineteen feature-introduced keys carry the leading `!!!`" (`:1087`) —
met and machine-verified: 19 changed values, all pure prefixes, the changed-key
set identical to the guide's list. AC-2 "a Jest guard asserts … with a
documented allow-list containing only the one pre-existing non-feature
exception" (`:1088`) — met; the guard is byte-equal to the guide's block and the
allow-list was proved exact under candidate 5. AC-3 "the guard demonstrably
fails when a newly marked en-US key has an unmarked ja-JP counterpart"
(`:1089`) — met, and it is the one criterion that could not have been inferred
from reading: the verify pass's five-step bite test made the focused spec exit 1
and print `Received Array [ "governance.drepDirectory.title" ]` against
`Expected Array []`, then returned it to green and the numstat to exactly
`19 19`, all via Edit with `git restore` never used on ja-JP.json. AC-4 "`yarn
i18n:manage` runs clean and `defaultMessages.json` / `translations/messages.json`
are unchanged by the restoration" (`:1090`) — the second clause is met and
measured (`git diff --stat` on both files is EMPTY on the delivered tree); the
first clause is NOT met and cannot be at this row, per candidate 1. AC-5 "the
task restores markers only and never strips one" (`:1091`) — met and
machine-verified: the set of keys that lost a marker is empty, and no en-US
value was touched at all.

The guide's own sixth acceptance box ("`tsc` clean; the UNFILTERED jest run is
green", `:3106`) holds on both counts.

**Comment convention.** One comment in the change set, the four-line block at
preliminaryCopyMarkers.spec.ts:4-7. `grep -nE 'task-[0-9]|CAT-|CP-|ADR|DD-'`
over the created file returns nothing; no ALL-CAPS emphasis, no change history.
See candidate 8 for the line-count nit.

**No unnecessary complexity.** 26 authored lines plus 19 catalog value edits and
3 consequential assertion updates. No helper, no abstraction, no option, no
jest config change, no new fixture, and no second guard. The scope fence is
clean in both directions: no runtime module, no IPC contract, no type, no
logging/analytics/electron-store path and no story is touched, and none of
task-135's or task-170's files were created.

**Verification commands run (results as observed).** Gates 1-10 are as measured
by the dedicated verify pass. This review independently re-ran the unfiltered
jest gate and re-derived the catalog measurements on disk; every other command
it ran was read-only.

1. `yarn compile` → exit 0, `tsc --noEmit` clean, `Done in 25.22s` (verify
   pass). The Node v24 fallback was again not needed.
2. `yarn lint` → exit 0 at exactly 5591 warnings, 0 errors — zero delta against
   the 5591 baseline, `Done in 45.97s` (verify pass). Note lint does not cover
   `tests/`, so the new spec is gated by `tsc` alone, and `tsc` is green.
3. `node_modules/.bin/jest tests/jest/i18n/preliminaryCopyMarkers.spec.ts
   --runInBand` → 1 suite / 1 test passed, exit 0.
4. `yarn i18n:manage` → exit 0 but NOT clean: 12 added keys, 0 deleted, four
   tracked files mutated. This is the one red gate; see candidate 1. The verify
   pass reverted the mutation surgically (`git restore` on the three files that
   were clean at HEAD, plus an Edit removing only the 12 inserted lines from
   ja-JP.json, since restoring that file would have destroyed the 19
   restorations) and re-confirmed the tree.
5. `git diff --stat -- source/renderer/app/i18n/locales/defaultMessages.json
   translations/messages.json` → EMPTY on the delivered tree.
6. `node_modules/.bin/prettier --check
   tests/jest/i18n/preliminaryCopyMarkers.spec.ts` → "All matched files use
   Prettier code style!", exit 0. Explicit path only; `yarn prettier` was never
   invoked, per the standing ~240-file drift hazard, and no pre-existing file —
   least of all a locale catalog — was passed to prettier.
7. `git diff --numstat -- source/renderer/app/i18n/locales/ja-JP.json` → exactly
   `19 19`.
8. Whole tree, no path argument, `node_modules/.bin/jest --runInBand` →
   **re-run by this review**: "Test Suites: 1 skipped, 85 passed, 85 of 86
   total" / "Tests: 12 skipped, 1060 passed, 1072 total" / "Snapshots: 6
   passed, 6 total", exit 0 in 31.06 s. Against the measured 85 suites / 1071
   tests / 6 snapshots baseline at `a3e352841` that is exactly +1 suite and +1
   test, with the skip counts unchanged. (The guide's "82 suites" is stale; see
   candidate 10.)
9. `node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts
   … --runInBand` → 23 of 23, the task-111 floor re-asserted exactly. The new
   guard is not itself a leak surface: `expect(unmarked).toEqual([])` prints
   KEY NAMES only, never catalog values, and none of the 19 changed values
   contains a DRep id, a bech32 string or an `abstain`/`no_confidence` literal.
10. `git status --short` → exactly three entries: ` M DRepDirectory.spec.tsx`,
    ` M ja-JP.json`, `?? tests/jest/i18n/`. Nothing staged, nothing committed,
    the tracker JSON untouched, and `/workspaces/daedalus` re-confirmed clean at
    `a3e352841`.

**Owed obligations (not faked).**

- **AC-4's first clause, owed at cv-1 close.** `yarn i18n:manage` will stay
  dirty until task-135 runs it and commits the 12
  `voting.governance.currentVote.*` keys across all four catalogs. Owner is
  whoever closes cv-1 if task-135 somehow does not absorb it. Immediately after
  that commit the task-171 guard should be re-run, because the freshly
  generated ja-JP values are English placeholders whose `!!!` must survive —
  which is precisely the canary this row exists to be.
- **Guide drift, recorded not fixed** (cv-1 planning is closed and this task's
  mandate is its own diff): the "Files touched" list at `:2949-2954` omits
  `DRepDirectory.spec.tsx`; the Step 3 comment at `:3077` claims "ja-JP.json
  and the new spec only"; the Step 3 gate at `:3075` and the acceptance bullet
  at `:3101-3103` state an `i18n:manage`-clean condition that this row's own
  ordering makes unreachable; and `:3072` / `:3106` still say 82 suites where
  the measured figure is 86.
- **F-5 — `nix fmt` cannot run in this devcontainer**; explicit-path
  `node_modules/.bin/prettier --check` on the single created file was the
  substitute and the `nix fmt` pass remains an owed pre-merge obligation.
- **Human visual/locale pass in the running app** — no browser here. The
  release-end manual review that CLEARS these markers stays user-owned per
  invariant 11 and README.md:16, `:18`, and nothing in this row anticipates it.
- `yarn check:all` and `yarn storybook:build` were NOT run; both are red at HEAD
  for the unrelated storybook manager-side JSX loader reason (F-20) and are not
  valid gates.
- The tracker row for task-171 is still `"status": "pending"`
  (governance-drep-discovery-plan-tasks.json:1080) and needs flipping at commit
  time. This review did not touch the tracker.

**Blockers.** None.

Decision: approved

---

## Code Review: task-135 — iteration 1 (2026-07-28)

**Scope reviewed.** The uncommitted working tree against the guide section
"task-135: Add i18n keys for CurrentVoteSummary core states"
(cv-1-implementation-guide.md:2559-2717 — the four-entry "Files touched" list at
`:2560-2568`, the Context block at `:2570-2583`, the three inline locked
invariants at `:2585-2598`, the resolved judgment calls at `:2600-2612`, Step 1's
verbatim twelve-key en-US block at `:2624-2635`, Step 2's verbatim twelve-key
ja-JP block at `:2644-2655`, the Step 3 clean-re-run gate at `:2661-2669`, the
Step 4 verify block at `:2671-2683`, the Step 5 Storybook overflow pass at
`:2685-2696`, and the six-item acceptance checklist at `:2698-2717`), and
task-135's three acceptance criteria in
governance-drep-discovery-plan-tasks.json (`:1116-1118`). Four files modified,
none created, nothing else. HEAD is `523141760` (the task-171 commit) on branch
`wt/cv-1-build`; `git status --short` → exactly four ` M` entries and zero
untracked paths. One review round. The main checkout `/workspaces/daedalus` was
never read, edited or run against.

**What landed.** `git diff --numstat` is exactly four lines — 154 insertions, 0
deletions, no mode change, no new or deleted file, `git diff --summary` empty:

- `source/renderer/app/i18n/locales/en-US.json` (`12 0`) — the 12
  `voting.governance.currentVote.*` keys, seeded by the runner from the
  `defaultMessage` values.
- `source/renderer/app/i18n/locales/ja-JP.json` (`12 0`) — the same 12 keys with
  the hand-authored preliminary Japanese drafts.
- `source/renderer/app/i18n/locales/defaultMessages.json` (`65 0`) — one
  regenerated descriptor group for path
  `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.messages.ts`.
- `translations/messages.json` (`65 0`) — the identical group from
  `yarn i18n:extract`.

Nothing else. No `.ts`/`.tsx`, no store, no IPC contract, no story, no doc and no
tracker file is touched, which is correct: `CurrentVoteSummary.messages.ts` was
created by task-132 (`23f443b76`) and this row only runs the catalog workflow.
The two whitelist files are absent from `git status --short` and
`whitelist_en-US.json` is still the 3-byte empty array the repo convention
requires.

**Copy verified byte-exact against the guide, not read for gist.** The 12 en-US
lines were parsed programmatically out of cv-1-implementation-guide.md:2624-2635
and the 12 ja-JP lines out of `:2644-2655`, JSON-unescaped, and compared by
string equality against the parsed catalogs on disk: **24 of 24 MATCH, zero
mismatches**. The two risky characters survive exactly — the U+2014 em dash in
`noDelegation.subline` and the U+2197 arrow in `drep.anchorMetadata` — as does
the ASCII apostrophe in `wallet's`. The namespace contains exactly 12 keys in
each locale and no extras (`set(delivered) - set(guide)` is empty in both).
Independently, all 12 en-US values are identical to the `defaultMessage` of their
descriptor in the regenerated `defaultMessages.json`, and
`defaultMessages.json == translations/messages.json` as whole parsed documents,
with 0 duplicate ids across the entire file. `grep -c
"voting.governance.currentVote"` is 12 in each of the four catalogs, and
`CurrentVoteSummary.messages.ts` carries exactly 12 `id: '` entries.

**Review method (three lenses, adversarial refutation).** Three independent
lenses were run over the diff: (1) guide and acceptance-criteria conformance —
the Step 1/Step 2 byte comparison, the Step 4 grep set, and the three tracker
criteria at `:1116-1118`; (2) locked invariants and the sanitization floor —
whether any of the 154 added lines can reach a logger, analytics or
electron-store payload, and whether invariants 2, 9, 10, 11 and 13 hold; (3)
tests, docs and unnecessary complexity — whether the inherited guards actually
bite the new values or pass vacuously, and whether anything added earns its
place. **All three lenses returned zero blockers.** Every candidate they raised
was re-checked against the diff by this consolidation before being carried or
dropped; five were dropped and are recorded below. **Zero survived as a blocker,
in one iteration**, and no file was changed as a result of the review.

Per-lens decision. Guide/AC conformance — clean; 24 of 24 catalog values are
character-for-character the guide's blocks and the "Files touched" list matches
the diff exactly with nothing extra. Invariants and floor — clean; the slice adds
no executable code at all, and the task-111 floor suite re-runs at 23 of 23.
Tests and complexity — clean; the task correctly adds no test of its own because
two inherited guards demonstrably bite the seeded values.

**Candidates adjudicated (five dropped, none survived as a blocker).**

1. *Dropped — "the tracker row description is wrong: task-135 does not create
   `CurrentVoteSummary.messages.ts`."* True as stated but not a defect in this
   diff. governance-drep-discovery-plan-tasks.json:1106 still opens with "Create
   CurrentVoteSummary.messages.ts with the CORE-state keys", while the module was
   committed by task-132 at `23f443b76`. The guide — which is the SPEC — resolves
   this in two places: its task-132 section states "The messages module is
   created HERE, not in task-135" (`:1467`), and the task-135 Context block
   states "The message DEFINITIONS already exist (`CurrentVoteSummary.messages.ts`,
   task-132 Step 1) — this task runs the catalog workflow and lands the ja-JP
   drafts" (`:2570-2572`). Implementation Order item 12 (`:57-61`) agrees. The
   delivered diff follows the guide. The drift predates this diff; recorded, not
   rewritten, and this review did not touch the tracker.
2. *Dropped — "Step 1 says the run reports 12 added keys, but the delivered tree
   shows a clean run with zero added."* Not a contradiction. Step 1 (`:2657-2659`)
   describes the FIRST seeding run; Step 3 (`:2661-2669`) is the re-run that must
   report zero added and zero deleted, and the delivered end state is the Step 3
   state. Both are as specified.
3. *Dropped — "neither guard detects outright DELETION of one of the 12 keys."*
   Accurate and re-confirmed: react-intl falls back to the `defaultMessage` when a
   message id is missing, `jest.config.js` has both `setupFiles` (`:135`) and
   `setupFilesAfterEnv` (`:138`) commented out so no missing-message console error
   is promoted to a failure, and the guard's `key in ja` filter
   (preliminaryCopyMarkers.spec.ts:19) skips any key absent from ja-JP. Dropped
   because the failure mode is self-healing — `yarn i18n:manage` re-seeds a
   deleted key into both catalogs — and the guide calls for no presence test.
   Adding one would be exactly the unnecessary machinery this phase's convention
   polices.
4. *Dropped — "the task-171 guard is asymmetric: a key marked `!!!` in ja-JP but
   not in en-US passes silently."* True of the guard's shape
   (preliminaryCopyMarkers.spec.ts:17-24), but that shape is task-171's design,
   was approved on its own review (this log, the task-171 entry), and is out of
   scope for a catalog-seeding row. Moot in practice here: all 12 of task-135's
   keys are marked in BOTH locales.
5. *Dropped — "react-intl's missing-message `console.error` is an id-echo
   surface."* Checked and it runs the other way. `App.tsx:76-81` sets no `onError`
   on `IntlProvider`, so before this slice the 12 ids were absent from both
   catalogs and would be echoed to the console whenever `CurrentVoteSummary`
   rendered; seeding both locales REMOVES that echo. It is a console path, not the
   Daedalus logger or electron-store, so it was never an invariant-2 breach in
   either direction — task-135 narrows it rather than widening it.

**Locked invariants.** No invariant is touched by executable code, because the
slice adds none. Checked positively rather than assumed:

- **Invariant 11 (preliminary copy)** — all 24 new values carry the leading
  `!!!`. `grep "voting.governance.currentVote" <locale> | grep -v ': "!!!'`
  returns nothing for BOTH locales, and a parsed `startswith('!!!')` over the
  namespace is True in both. Machine-enforced from here on by the task-171 guard.
- **Invariant 2 (sanitization floor)** — none of the 154 added lines contains a
  bech32 string, a DRep id, or the snake_case wire literals `abstain` /
  `no_confidence`; the new ids are camelCase message ids
  (`statusAbstain`, `noConfidence.caption`), never payload values. The only
  runtime consumer of the seeded catalogs is react-intl via
  `source/renderer/app/i18n/translations.ts` → `App.tsx:76`; every other importer
  of `en-US.json`/`ja-JP.json` is a spec file. `tests/jest/security/
  governance-sanitization.spec.ts` re-runs at 23 of 23, the documented floor.
- **Invariant 10 (byte-equality)** — no identity string, CIP-129 or CIP-105 value
  is introduced; the 12 en-US values are nonetheless identical to their extracted
  `defaultMessage`, verified key by key with 0 mismatches.
- **Invariants 9 and 13** — reinforced by the copy rather than threatened by it.
  `noDelegation.warning` carries the CIP-1694 reward-withdrawal warning,
  `noDelegation.cta` supplies the CTA, and `noDelegation.subline` states
  "Daedalus will not pick a DRep for you" verbatim. The Abstain / No Confidence
  keys are wallet-own delegation-status labels under `currentVote.*`, not DRep
  directory entries.
- **No cv-2 leakage** — the guide's exact Step 4 grep, `grep -n
  "sameVoteHint\|currentVote.status\.\|previousVote\|newVote"`, run over BOTH
  locale files (broader than the verifier's anchored variant, since it also
  catches any bare `previousVote`/`newVote` token) returns no match. The reserved
  `confirmationDialog.previousVote`/`.newVote` keys stay unextracted because no
  `defineMessages` defines them.

**Structural integrity of the catalogs.** Both locale files are still fully
key-sorted after insertion (`list(keys) == sorted(keys)` for each), their key
SETS are identical to each other at 1611 keys apiece, there are zero duplicate
ids, and `git diff | grep '^-' | grep -v '^---'` is EMPTY across all four files —
so no pre-existing entry was modified, reordered, reformatted or removed. `git
diff --check` reports no whitespace errors and the `100755` modes on the two
locale files are preserved. The 12 new keys land between
`voting.governance.confirmationDialog.vote` and `voting.governance.delegateToDRep`
exactly where the guide's resolved judgment call (`:2607-2612`) predicts — runner
ordering, not hand placement.

**Task-171's deferred AC-4 first clause is DISCHARGED here.** The task-171 entry
in this log recorded AC-4's first clause as owed "until task-135 runs it and
commits the 12 `voting.governance.currentVote.*` keys across all four catalogs".
All four catalogs are now seeded and the clause closes at this row; it must NOT
be carried forward to cv-1 close. Evidence, from the verify pass and corroborated
non-destructively here:

- `yarn i18n:manage` → exit 0 and CLEAN. Zero added keys, zero deleted keys — the
  runner output contains no `added`/`deleted`/`removed` line at all (`grep -inE
  'added|deleted|removed'` returns nothing), only the informational
  Untranslated-keys report the repo convention leaves in place. Non-mutation was
  proved by sha256-ing all four catalog files immediately before and after the
  run: byte-identical, and `git diff --stat` afterwards is unchanged at
  `154 insertions, 0 deletions`. The hand-authored Japanese values were NOT
  rewritten.
- Corroborating signal from the runner itself: the 12 currentVote keys appear 12x
  in the en-US untranslated report (expected — they all carry the `!!!` marker)
  and 0x in the ja-JP report, i.e. the runner classifies all 12 Japanese values as
  genuine translations rather than seeds.
- This review re-derived the same conclusion without running the writing command:
  parsing all descriptor ids out of `defaultMessages.json` gives 1611 ids against
  1611 keys in each locale, with **0 would-be-added and 0 would-be-deleted for
  both en-US and ja-JP** — precisely the state in which the runner is a no-op.
- The second AC-4 clause ("`defaultMessages.json` / `translations/messages.json`
  unchanged by the RESTORATION") was already met at task-171 and is not
  contradicted here: `git show --stat 523141760` shows task-171 touched
  `ja-JP.json` alone among the catalogs, so the `+65/+65` now present in the two
  generated files is task-135's own regeneration, not task-171 spill.

**The task-171 guard stayed green through the ja-JP translation pass.**
`tests/jest/i18n/preliminaryCopyMarkers.spec.ts` passes over the seeded tree —
1 suite / 1 test, and re-run again by this review inside a 3-suite batch. The
canary the task-171 entry asked for is therefore discharged: the 12 freshly
landed ja-JP values pass the marker guard. Note the values are not the English
placeholders task-171 anticipated — guide Step 2 replaced the seeds with real
Japanese — and each was checked individually to start with `!!!`, contain
Japanese script, and differ from its en-US counterpart, so none is a runner
passthrough.

**The inherited guards genuinely bite this slice.** Task-135 correctly adds no
test of its own. The en-US half is bitten by
`source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx`,
which imports `en-US.json` directly (`:8`) into `<IntlProvider locale="en-US"
messages={translations}>` (`:38`) and asserts exact literals — `'!!!No governance
delegation'` (`:49`), `'!!!Choose a delegation'` (`:51`), `'!!!Delegated to DRep'`
(`:57`), `'!!!Abstain'` (`:69`), `'!!!No Confidence'` (`:83`) — so from this
commit onward those assertions read the CATALOG value rather than the
`defaultMessage`, and any drift in a seeded en-US string fails the suite. The
ja-JP half is bitten by the task-171 guard as described above.

**Comment convention.** No comment was added anywhere in the change set — the
diff is 154 JSON data lines. Nothing to check.

**No unnecessary complexity.** Zero code, zero helpers, zero options, zero new
abstractions, zero new spec files, zero config changes. This is the smallest
truthful change Steps 1-3 permit. The `.spec.ts`/`.spec.tsx` naming rule does not
apply because no spec is created. The two unrendered `drep.*` link labels were
extracted as the guide's Context (`:2578-2583`) said they would be — expected,
not scope creep.

**Acceptance criteria.** Two of the tracker's three criteria (`:1116-1118`) are
fully met; the third is met in one half and owed in the other by the
environment's own limits, exactly as the guide pre-authorises.

AC-1 "core keys present in en-US.json and ja-JP.json: headerCurrent,
statusDelegatedToDRep, statusAbstain, statusNoConfidence, noDelegation
title/warning/subline/cta, DRep link labels, abstain/noConfidence captions"
(`:1116`) — met; all 12 named keys are present in both locales and byte-equal to
the guide's blocks. AC-2 "preliminary ja-JP copy is reviewed for length / layout
overflow while retaining the leading `!!!` marker" (`:1117`) — the marker half is
met and machine-verified on all 12 ja-JP values; the overflow-review half is NOT
discharged and is carried below as owed, per the guide's explicit instruction at
`:2694-2696` ("If the devcontainer cannot open a browser, record this review as a
main-checkout follow-up in the code-review log rather than skipping it"). AC-3
"`yarn i18n:manage` runs clean" (`:1118`) — met and measured, with sha256
non-mutation proof; see the AC-4 discharge section above.

The guide's own six acceptance boxes (`:2698-2717`) hold, with the same single
exception: the 12-key presence box, the clean-re-run box, the Step 4 cv-2/reserved
grep box, the "no pre-existing catalog entry modified; whitelist files untouched"
box and the "task-134 snapshots unchanged after catalog seeding" box are all met;
the ja-JP box is met on `!!!` retention and owed on the overflow review.

**Verification commands run (results as observed).** Gates 1-13 are as measured
by the dedicated verify pass. This review independently re-derived the diff shape,
the byte comparison against the guide, the catalog structural checks and the
runner no-op proxy on disk, and re-ran the three invariant-relevant suites
together; every other command it ran was read-only.

1. `yarn compile` → exit 0, `tsc --noEmit` clean, `Done in 25.57s` (verify pass).
   The Node v24 fallback was not needed.
2. `yarn lint` → exit 0 at exactly 5591 warnings, 0 errors — zero delta against
   the 5591 baseline (verify pass).
3. `yarn i18n:manage` → exit 0 and CLEAN; zero added, zero deleted, all four
   catalog sha256s byte-identical before and after (verify pass). See the AC-4
   section.
4. `node_modules/.bin/jest .../CurrentVoteSummary.spec.tsx --runInBand` → 1 suite
   / 4 tests passed, **4 snapshots passed with zero written, zero updated, zero
   obsolete** (verify pass). The seeding canary held: because the catalog values
   are byte-identical to the `defaultMessage` fallbacks, the task-134 snapshots
   did not move and no `jest -u` was run or needed.
5. `node_modules/.bin/jest tests/jest/i18n/preliminaryCopyMarkers.spec.ts
   --runInBand` → 1 suite / 1 test passed (verify pass).
6. **Re-run by this review**, three suites in one batch —
   `CurrentVoteSummary.spec.tsx` + `preliminaryCopyMarkers.spec.ts` +
   `governance-sanitization.spec.ts` → "Test Suites: 3 passed, 3 total" /
   "Tests: 28 passed, 28 total" (4 + 1 + 23) / "Snapshots: 4 passed, 4 total",
   exit 0 in 2.224 s, zero snapshots written or obsolete. `git status --short`
   immediately afterwards was unchanged, so the run mutated nothing.
7. Whole tree, no path argument, `node_modules/.bin/jest --runInBand` → "Test
   Suites: 1 skipped, 85 passed, 85 of 86 total" / "Tests: 12 skipped, 1060
   passed, 1072 total" / "Snapshots: 6 passed, 6 total", exit 0 in 34.109 s
   (verify pass) — identical to the post-task-171 baseline of 86 / 1072 / 6, i.e.
   zero delta, as a catalog-only slice should be. (The guide's "82 suites" is
   stale; the measured figure is 86.)
8. `node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts
   --runInBand` → 23 of 23, the task-111 floor re-asserted exactly.
9. **Re-derived by this review**: `git diff --numstat` → `65 0`, `12 0`, `12 0`,
   `65 0`; `git diff --summary` empty; `git status --short` → four ` M` entries,
   zero untracked; `git diff | grep '^-' | grep -v '^---'` empty across all four
   files; `git diff --check` clean.
10. **Re-derived by this review**: a parsed comparison of the delivered catalogs
    against cv-1-implementation-guide.md:2624-2635 and `:2644-2655` → 24 of 24
    match, 0 mismatches, 0 extra namespace keys; both locales fully key-sorted
    with identical 1611-key sets; `defaultMessages.json ==
    translations/messages.json`; 0 duplicate ids; the single descriptor group for
    `CurrentVoteSummary.messages.ts` holds exactly 12 descriptors, all matching
    en-US.
11. **Re-derived by this review**: the runner no-op proxy — 1611 descriptor ids
    against 1611 keys per locale, 0 would-be-added and 0 would-be-deleted in both,
    corroborating gate 3 without re-running the writing command.
12. `grep -n "sameVoteHint\|currentVote.status\.\|previousVote\|newVote"` over
    both locale files → no match (**re-run by this review** in the guide's exact
    unanchored form).
13. `git status --short source/renderer/app/i18n/locales/whitelist_*.json` → empty;
    `whitelist_en-US.json` still the 3-byte `[]`.

**Prettier.** No prettier command was run and none applies. Task-135 creates no
new file, and all four touched files are on the standing do-not-reformat list
(`en-US.json`, `ja-JP.json`, `defaultMessages.json`, `translations/messages.json`)
under the ~240-file pre-existing drift hazard. `yarn prettier` was never invoked.

**Owed obligations (not faked).**

- **Guide Step 5 — the ja-JP overflow review, owed as a main-checkout
  follow-up.** `yarn storybook`, "Governance / Current Vote Summary → Core
  states", global locale switched to Japanese, cycling all four knob values,
  confirming every string renders fully (the `.scss` wraps — no ellipsis, no
  clipped line), no missing-message console warnings remain, and the panel height
  grows naturally. There is no browser in this devcontainer, so this half of AC-2
  is NOT discharged and must not be reported green. The likeliest overflow
  candidate is `noDelegation.subline` at 58 characters; `noDelegation.warning` and
  the two captions are the next candidates. Explicitly pre-authorised at guide
  `:2694-2696`.
- **F-5 — `nix fmt` cannot run in this devcontainer**; the pass remains an owed
  pre-merge obligation. No prettier substitute was applicable to this slice, since
  it creates no file.
- **Human visual/locale pass in the running app** — no browser here. The
  release-end manual review that CLEARS the `!!!` markers stays user-owned per
  invariant 11; nothing in this row anticipates it.
- **Guide/tracker drift, recorded not fixed** (cv-1 planning is closed and this
  row's mandate is its own diff): the task-135 tracker description at
  governance-drep-discovery-plan-tasks.json:1106 still says the row creates
  `CurrentVoteSummary.messages.ts`, which task-132 did — see candidate 1; and the
  guide's "82 suites" references remain stale against the measured 86.
- `yarn check:all` and `yarn storybook:build` were NOT run; both are red at HEAD
  for the unrelated storybook manager-side JSX loader reason (F-20) and are not
  valid gates.
- The tracker row for task-135 is still `"status": "pending"`
  (governance-drep-discovery-plan-tasks.json:1107) and needs flipping at commit
  time. This review did not touch the tracker.
- **Closing note for cv-1 close:** task-171's owed AC-4 first clause is discharged
  by this row and should be struck from the carry-forward list, not repeated.

**Blockers.** None.

Decision: approved

---

## Code Review: task-170 — iteration 1 (2026-07-28)

**Scope reviewed.** The uncommitted working tree against the guide section
"task-170: Redact raw wallet payloads at the AdaApi wallet-list log sites"
(cv-1-implementation-guide.md:2718-2945 — the two-entry "Files touched" list at
`:2720-2724`, the Context block at `:2726-2737`, the four inline locked
invariants at `:2739-2751`, the three resolved judgment calls at `:2753-2768`,
Step 1's verbatim replacement block at `:2770-2790`, Step 2's verbatim block at
`:2792-2806`, Step 3's audit grep and six-row table at `:2808-2836`, Step 4's
two verbatim fences at `:2838-2909`, the Step 5 verify block at `:2911-2925`,
and the six-item acceptance checklist at `:2927-2945`), and task-170's five
acceptance criteria in governance-drep-discovery-plan-tasks.json. Two files
modified in place, nothing created, nothing else touched. `git status --short`
→ exactly two entries, both ` M`. HEAD is `d3729994a` on branch
`wt/cv-1-build` (task-135 and task-171 already committed ahead of this row).
One review round. The main checkout `/workspaces/daedalus` was never read,
edited or run against.

**What landed.** `git diff --stat` → 2 files, 62 insertions / 20 deletions
(api.ts 32, the spec 50).

- `source/renderer/app/api/api.ts` — six `logger.debug` argument sites and
  nothing else. Read hunk by hunk against the guide: the Step 1 replacement is
  character-identical to guide `:2784-2789`, including the multi-line call
  formatting, and the Step 2 replacement is character-identical to guide
  `:2804-2806`. The four further wraps follow the same single-line form. No
  import was added — `filterLogData` was already imported at `api.ts:99`, as
  guide `:2745-2747` requires. No request, no return value, no
  `_createWalletFromServerData` call and no `map(legacyWallets, …)` line is
  altered anywhere in the diff.
- `tests/jest/security/governance-sanitization.spec.ts` — two module-scope
  `jest.mock` blocks added beside the existing `delegateVotes` mock (`:17-31`)
  and one new `it` inside the existing `describe('Governance sanitization —
  call boundaries', …)`, placed immediately after the `delegateVotes` case it
  is modelled on (`:254-288`). Both blocks reproduce the guide's Step 4 fences
  verbatim, down to the `FIXTURE_DREP` literal, the `it()` string and the
  `// eslint-disable-next-line global-require` directive (which mirrors the
  identical directive already at `:234`).
- Nothing else. No i18n catalog, no `translations/messages.json`, no doc, no
  tracker, no story, no runtime module outside `api.ts`, no IPC contract, no
  type declaration.

**Review method (three lenses, adversarial refutation).** Three independent
lenses were run over the diff: (1) guide and acceptance-criteria conformance —
the five steps, the six-row table and the five tracker criteria; (2) locked
invariants and the sanitization floor — whether any new leak path opens,
whether the wrap is a behavioural change, and whether byte-equality survives;
(3) tests, docs and complexity — whether the new case bites, whether the ACs
imply a missing test, and whether anything is over-built. Every candidate was
then re-checked against the actual tree on the reproduce / guide-authority /
scope axes before being admitted. **Six candidates were raised across the three
lenses; all six were dropped or downgraded on checking, one of them because it
is factually wrong. Zero survived as a blocker, in one iteration**, and no file
was changed as a result of the review.

Per-lens decision. Guide/AC conformance — clean; every guide-supplied code
block is byte-identical on disk. Invariants and floor — clean; the change is
strictly more redaction on six existing lines and opens no new sink. Tests and
complexity — clean; the new case demonstrably bites and the diff is the
smallest truthful change.

**AC-3 audit list (recorded here — this entry is the "task evidence" AC-3
requires).** Fifteen whole-payload wallet `logger.*` sites exist in `api.ts`.
Line numbers below are POST-change and were re-derived by this review with
`grep -n "filterLogData({"`, `grep -n "logger.debug($"` and
`grep -nE "^ +wallets?,$"`, not copied from the guide (guide anchors `:379`,
`:458`, `:870`, `:1588`, `:1628`, `:2077` drifted by up to 6 lines because the
file is now 8 lines shorter; the guide's own header rule at `:9-11` directs
re-anchoring by content).

Wrapped — six Shelley `AdaWallet` / `AdaWallets` payloads that can carry
`delegation.active.voting` or `delegation.next[*].voting`:

| Call site | Payload |
| --- | --- |
| `api.ts:379` `AdaApi::getWallets success` | `filterLogData({ wallets, legacyWallets, hwLocalData })` (`:381`) |
| `api.ts:457` `AdaApi::getWallet success` | `filterLogData({ wallet })` |
| `api.ts:867` `AdaApi::createWallet success` | `filterLogData({ wallet })` |
| `api.ts:1583` `AdaApi::restoreWallet success` | `filterLogData({ wallet })` |
| `api.ts:1621` `AdaApi::createHardwareWallet success` | `filterLogData({ wallet })` (`:1623`) |
| `api.ts:2071` `AdaApi::updateWallet success` | `filterLogData({ wallet })` |

Deliberately unwrapped — seven legacy/Byron sites, per the guide's resolved
judgment call at `:2765-2768`: `:922` `createLegacyWallet`, `:1699`
`restoreLegacyWallet`, `:1762` `restoreByronRandomWallet`, `:1816`
`restoreByronIcarusWallet`, `:1870` `restoreByronTrezorWallet`, `:1924`
`restoreByronLedgerWallet`, `:1970` `restoreExportedByronWallet`. The
justification was re-verified rather than taken on trust: `LegacyAdaWallet`
(`source/renderer/app/api/wallets/types.ts:57-71`) has **no `delegation` field
at all**, and each of these sites logs `{ ...legacyWallet,
...extraLegacyWalletProps }` where `extraLegacyWalletProps` injects
`delegation: { active: { status: NOT_DELEGATING } }` locally around
`api.ts:915-920` with no `voting` key. No vote target is reachable.

Audited and left unwrapped, NOT in the guide's table — two sites the guide's
Step 3 grep structurally cannot see, recorded here so the audit is complete
rather than merely conformant: `api.ts:1995` `AdaApi::importWalletFromKey
success` and `api.ts:2025` `AdaApi::importWalletFromFile success`, both logging
`{ importedWallet }` where the local is declared `const importedWallet:
AdaWallet` (`:1991`, `:2019`). See candidate 1 below for why these are out of
scope for this row.

**Candidates adjudicated (six raised, none survived as a blocker).**

1. *Dropped as a task-170 blocker, recorded as a new out-of-scope finding —
   "two Shelley-TYPED sites are still unwrapped, so the guide's own inline
   invariant at `:2739-2742` ('no DRep id … reaches a logger payload from any
   Shelley-wallet api.ts call site') is not fully achieved"* (raised
   independently by all three lenses and by the verify pass).
   `api.ts:1995` / `:2025` log `{ importedWallet }` unwrapped, and
   `AdaWallet.delegation.active` is a `WalletDelegation` carrying
   `voting?: WalletVotingTarget` (types.ts:44-47, `:113-118`), so statically
   they fit AC-3's "can carry `delegation.*.voting`". Dropped for three
   independently checked reasons. First, **pre-existing and byte-identical at
   HEAD** — this review ran `git show HEAD:source/renderer/app/api/api.ts |
   grep -n "importedWallet,"` and got `2002` and `2032`, the same unwrapped
   form; task-170 neither introduced nor worsened it. Second, **the guide never
   classified them and structurally could not**: Step 3's grep filter
   (`:2810-2813`) is `grep -B1 "^[0-9]*-        wallets\?,$"`, which matches
   only lines that are exactly `wallet,` or `wallets,`, so `importedWallet,`
   can never appear — which is precisely why the guide counts "thirteen sites"
   (6 + 7) and stops there. Third, **the runtime exposure is type-level, not
   demonstrated**: `importWalletAsKey` POSTs `/api/internal/import-wallet`
   (`source/renderer/app/api/wallets/requests/importWalletAsKey.ts:16`) and
   `importWalletAsJSON` POSTs `/api/backup/import`
   (`importWalletAsJSON.ts:12`) — both Byron/V0 legacy import endpoints, which
   do not return a governance delegation — and neither is on a poll, unlike
   `getWallets`. Recommended for `research/cv-1-findings.md` as **F-26**
   (next free id; F-25 is the highest in that file today). This review did NOT
   edit the findings file — its mandate is this diff and this entry.
2. *Dropped as a defect, recorded as a stated limitation — "two of the four
   new assertions are vacuous."* True and confirmed by reading the fixture:
   `tests/mocks/wallets/wallet-voting-drep.json` carries `"voting":
   "drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy"` under
   `delegation.active` and contains no `abstain` and no `no_confidence`
   substring anywhere, so `expect(payload).not.toContain('abstain')` and
   `.not.toContain('no_confidence')` (spec `:286-287`) cannot fail for any
   implementation. Only `not.toContain(FIXTURE_DREP)` bites — which the verify
   pass's negative probe proved independently, failing exactly 1 of 24 with the
   wrap reverted. Not a defect: both lines are copied verbatim from the guide's
   Step 4 fence (`:2899-2901`) and AC-4 names both classes, so deviating would
   cost guide fidelity for no coverage. The sentinel gap was closed by
   inspection instead: `tests/mocks/wallets/wallet-voting-abstain.json` and
   `wallet-voting-no-confidence.json` place their sentinel at the SAME
   `delegation.active.voting` key that `filterLogData` strips, so the single
   outer call covers all three shapes. Recorded here so nobody reads the case
   as proving more than it does — it proves bech32 redaction, and the sentinel
   classes rest on the `filterLogData` unit cases at
   governance-sanitization.spec.ts:133-151.
3. *Dropped — "no positive assertion, so a payload-to-`{}` regression would
   pass" and "only 1 of the 6 wrapped sites is tested."* Both true as stated
   and both in-spec. AC-4 asks for exactly one `getWallets` call-boundary case
   worded purely as a negative, and gets it; the other five wraps are gated by
   `tsc` plus this audit. The suggested hardening (one
   `expect(payload).toContain('"hwLocalData"')` line, or an assertion on the
   fixture wallet id) is cheap and would close AC-1's "hwLocalData is still
   filtered" clause behind an assertion rather than by inspection, but it is a
   deviation from a guide-verbatim block for a regression class nothing in cv-1
   makes likely. Left as delivered; noted so the coverage claim is stated
   accurately rather than as "six sites tested".
4. *Dropped — "the new case leaks `global.daedalus` / `global.https` and never
   restores the `rendererLogger.debug` stub."* Both facts confirmed:
   `jest.config.js:17` sets `clearMocks: true` only, with `resetMocks` and
   `restoreMocks` commented out, so `clearMocks` wipes recorded calls but
   leaves the mock implementation in place; and the describe's cleanup is
   `afterEach(() => jest.restoreAllMocks())` (spec `:219-221`), which does not
   unset globals. Latent only, and identical to the pre-existing
   `delegateVotes` case directly above it (spec `:223-227`), which leaks
   `environment` and `https` the same way. Nothing later in the file reads
   `global.daedalus`, the whole file is green at 24/24 and the whole tree is
   green. Guide-verbatim; matching the file's convention beats inventing a
   teardown here.
5. *Refuted as factually wrong — "the `createHardwareWallet` wrap is at
   `api.ts:1618`, not `:1621`"* (raised by the tests/docs lens as a correction
   to the verify pass). The verify pass was right and the correction is wrong.
   `grep -n "logger.debug($" source/renderer/app/api/api.ts` returns exactly
   `379`, `767` (a commented-out line) and `1621`, and reading `:1615-1624`
   shows `:1618` is the closing `}` of the `walletInitData` argument object,
   `:1620` is `const wallet = { ...hardwareWallet, isHardwareWallet: true };`
   and `:1621` is `logger.debug(`. The AC-3 table above uses `:1621`. Recorded
   because a wrong anchor in the evidence would have propagated into every
   later audit of this file.
6. *Refuted — "wrapping the payload is a behavioural change."* Checked by
   reading the implementation rather than assuming: `redact`
   (source/common/utils/logging.ts:51-71) is purely constructive — it maps
   arrays and rebuilds plain objects key by key with `Object.keys(…).reduce`,
   and never writes to its input. So at all six sites the ORIGINAL object, not
   the filtered copy, is what continues downstream: `wallets` is still the
   array `.push()`ed and mapped at `api.ts:383-407`, and
   `_createWalletFromServerData(wallet)` still receives the unfiltered payload
   at `:458`, `:868`, `:1584`, `:1625`, `:2072`. The filtered value's only
   consumer is `logger.debug`. Invariant 10 (byte-equality) is untouched.
   One forward-looking caveat surfaced while checking and is recorded rather
   than acted on: `redact` flattens any non-plain object (a `Date`, a class
   instance, a `BigNumber`) to `{}`, because such a value has no own
   enumerable keys. Nothing is flattened today — all six payloads are raw wire
   JSON or plain spreads of it — but a future task wrapping a payload that
   carries a domain object would silently empty it in the log.

**Log-shape delta.** Measured against `AdaWallet` / `LegacyAdaWallet`
(types.ts:32-71) and the `sensitiveData` list (logging.ts:24-49): the only
collisions are `voting` (the intended redaction) and `passphrase`
(`{ last_updated_at }`), which is the ONE deliberate non-governance omission
the guide authorises at `:2759-2763`. Note it now applies to the
`legacyWallets` array too, which the guide's wording covers. `votingKey`,
`stakeKey`, `accountPublicKey`, `withdrawal` and the rest of the list do not
appear in the wire wallet shape. `hwLocalData` filtering is unchanged: `redact`
recurses uniformly, so the single outer call is exactly equivalent to the
previous nested `hwLocalData: filterLogData(hwLocalData)`.

**Acceptance criteria.** All five tracker criteria are met, AC-3 by virtue of
this entry.

AC-1 "`wallets` and `legacyWallets` … wrapped in `filterLogData`; `hwLocalData`
filtering unchanged" — met; see the Step 1 hunk and the equivalence argument
above. AC-2 "the `AdaApi::getWallet success` call site receives the same
treatment" — met at `api.ts:457`. AC-3 "every remaining whole-payload
`logger.*` call site … is audited; those that can are wrapped, and the audit
list is recorded in the task evidence" — the code half was met on delivery
(six wrapped, seven classified); the evidence half was NOT on disk when the
review opened and is discharged by the fifteen-site audit list above, which
also classifies the two `importedWallet` sites the guide's grep could not see.
AC-4 "a `getWallets` call-boundary case driving a voting-wallet fixture …
asserting no CIP-129/CIP-105 bech32 string and no `abstain` / `no_confidence`
literal reaches the emitted logger payload" — met literally; see candidate 2
for what the case does and does not prove. AC-5 "INHERITED sanitization floor:
the full governance-sanitization suite is green with the new case;
non-governance log shapes for the wallet-list flow are otherwise unchanged" —
met; 24 of 24 green and the only extra shape delta is the authorised
`passphrase` omission.

The guide's own sixth acceptance box ("`tsc` clean; lint clean; the UNFILTERED
jest run is green", `:2943-2945`) holds on all three counts. Its "82 suites"
figure is the known staleness — the measured figure is 86 — and was not
treated as a mismatch.

**Invariant sweep.** Invariant 2 (sanitization floor) is strengthened, not
merely preserved: `WalletDelegation` and `WalletNextDelegation`
(types.ts:113-126) expose governance ONLY through the `voting` key, which
`filterLogData` strips at any depth, and `source/renderer/app/api/utils/
request.ts` contains no logger at all, so there is no lower HTTP layer logging
the raw response body behind `api.ts`'s back. `delegation.active.status` may
legitimately hold the string `"voting"`, but that is a status enum, not an
identity. Invariant 10 (byte-equality) holds per candidate 6. Invariant 11 is
untouched — no catalog is dirty in this diff. Invariants 1, 3, 4, 5, 6, 7, 8,
9, 12, 13 and 14 are untouched: no IPC channel, no main-process file, no type
declaration, no analytics call, no electron-store write, no new abstraction and
no new import appears anywhere in the change set.

**Comment convention.** The diff adds no explanatory code comment at all. The
single added comment is `// eslint-disable-next-line global-require` (spec
`:22`), matching the identical directive at `:234`. No task id, no review
label, no ALL-CAPS marker, no change history.

**No unnecessary complexity.** Six one-to-three-line call-site rewrites, two
module-scope mocks (both load-bearing — without the `getLegacyWallets` stub the
real request fires) and one test case. No helper, no abstraction, no option, no
per-field redactor, no bespoke second sanitizer on a security seam — the shared
`filterLogData` is reused exactly as the floor demands. This is the smallest
truthful change.

**Verification commands run (results as observed).** Gates 1-8 are as measured
by the dedicated verify pass, which reported `passed: true` with no failures.
This review independently re-ran the focused floor suite and re-derived every
line anchor and every type/fixture claim on disk; every other command it ran
was read-only.

1. `yarn compile` → exit 0, `tsc --noEmit` clean, `Done in 18.77s` (verify
   pass). The Node v24 fallback was not needed.
2. `yarn lint` → exit 0 at exactly 5591 warnings, 0 errors — zero delta against
   the 5591 baseline (verify pass). `api.ts` is under `source/` and therefore
   covered; the spec is not, since lint's scope is `source storybook utils`
   (package.json:43), so the new test file is gated by `tsc` alone and `tsc` is
   green.
3. `node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts
   --runInBand` → **re-run by this review**: "Test Suites: 1 passed, 1 total" /
   "Tests: 24 passed, 24 total", 0 snapshots, exit 0 in 3.608 s. Baseline was
   23; the delta is exactly the new case.
4. Negative proof (verify pass). `api.ts:379-382` was Edited back to its HEAD
   form and the focused spec exited 1 with
   `Expected substring: not "drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy"`
   at `governance-sanitization.spec.ts:284`, printing the raw payload with
   `"delegation":{"active":{"status":"voting","voting":"drep1…"}}` intact —
   **1 failed / 23 passed of 24**, so the probe was precisely targeted and no
   pre-existing case is incidentally coupled to it. After the restoring Edit:
   24 passed, exit 0, and `git diff --stat` byte-identical to the pre-probe
   reading. `git restore` and `git stash` were never used.
5. Log-site audit (verify pass, **fully re-derived by this review**) — six
   wrapped, seven legacy unwrapped, two `importedWallet` sites classified. See
   the AC-3 table above. Method note for whoever repeats it: `:379` and `:1621`
   are multi-line calls and do NOT match `grep -n "logger.debug('AdaApi::"`;
   they were found with `grep -n "logger.debug($"` and cross-checked with
   `grep -n "filterLogData({"` (6 hits) and `grep -nE "^ +wallets?,$"` (7
   hits). A reviewer running only the guide's scripted grep would wrongly
   conclude two sites were missed.
6. Whole tree, no path argument, `node_modules/.bin/jest --runInBand` (verify
   pass) → "Test Suites: 1 skipped, 85 passed, 86 total" / "Tests: 12 skipped,
   1061 passed, 1073 total" / "Snapshots: 6 passed", exit 0 in 29.252 s.
   Against the post-task-171 baseline (86 / 1072 / 6) that is exactly +1 test
   with the suite count and both skip counts unchanged.
7. `yarn i18n:manage` (verify pass, run 3x) → exit 0 and CLEAN: zero added,
   zero deleted, no Added/Deleted sections, only the expected "Untranslated
   keys" `!!!` listings. `translations/messages.json` was rewritten on disk but
   came out byte-identical to HEAD, so no revert was needed or taken. This is
   task-135 discharging task-171's owed AC-4 first clause, now re-confirmed at
   this row.
8. `git status --short` → exactly two entries, ` M
   source/renderer/app/api/api.ts` and ` M
   tests/jest/security/governance-sanitization.spec.ts`. No untracked file, so
   nothing is hidden from `git diff`. Nothing staged, nothing committed, the
   tracker JSON untouched.
9. `git diff --stat` → 2 files, 62 insertions, 20 deletions
   (**re-measured by this review**). The tests/docs lens additionally reported
   `git diff -w --stat` byte-identical to `git diff --stat` and `git diff
   --check` silent — positive proof that no pre-existing file was
   whitespace-reformatted.

**Prettier.** No prettier command applies and none was run: this slice creates
no file, and both touched paths are on the standing do-not-reformat list
(`source/renderer/app/api/api.ts`, `tests/jest/security/
governance-sanitization.spec.ts`) under the ~240-file pre-existing drift
hazard. `yarn prettier`, `yarn prettier:check` and `yarn prettier:format` were
never invoked. The guide says the same at `:2923-2925`.

**Out-of-scope observations carried forward.**

- **F-15 is NOT closed by this task, by design.** The renderer-side domain key
  names `votingTarget` and `currentVote` remain absent from `filterLogData`'s
  `sensitiveData` list (source/common/utils/logging.ts:24-49), which is keyed
  to the WIRE shape — `drepId`, `dRepId`, `vote`, `voting`. Task-170 wraps wire
  payloads whose key is `voting`; it adds no `sensitiveData` entry and the diff
  correctly does not attempt one. The domain shape stays unguarded the moment
  cv-2 gives it a consumer, exactly as recorded at `:1428-1435` of this file
  and at research/cv-1-findings.md:472. No conflict, no regression.
- **The `HardwareWalletsStore` raw `{ error }` sink is NOT closed by this task,
  by design.** The `[HW-DEBUG]` calls log raw `{ error }`
  (research/slice-3-findings.md:71-77, re-stated at cv-1-findings.md:316);
  that is the message-SUBSTRING class — a DRep id embedded in an error string
  — which `filterLogData` structurally cannot reach, since it deletes by key
  name and never inspects string contents. The guide fences it out explicitly
  at `:2749-2751`, and the tracker's task-170 description fences it out again.
  Out of scope, still open.
- **Recommended as F-26** — the two `importedWallet` sites of candidate 1.
  Pre-existing, type-level, Byron import endpoints, not on a poll. For the
  reviewer to accept or schedule; not a task-170 blocker.
- Still carried from earlier entries: **F-5** — `nix fmt` cannot run in this
  devcontainer.
- **Guide/tracker anchor drift, recorded not fixed** (cv-1 planning is closed
  and this row's mandate is its own diff): `api.ts` is now 8 lines shorter, so
  the guide's Step 3 table (`:870`, `:1588`, `:1628`, `:2077`) and the
  tracker's task-170 description (`:379-383`, `:458-460`) hold pre-change
  anchors. The guide's own header rule at `:9-11` makes this expected rather
  than a defect, so no doc edit is owed; the post-change anchors are in the
  AC-3 table above. Separately, the guide's Step 3 grep is under-inclusive in
  two independent ways (it cannot see multi-line calls, and its
  `^[0-9]*-        wallets\?,$` filter cannot match `importedWallet,`), and its
  "82 suites" references at `:2917` / `:2945` remain stale against the measured
  86.

**Owed obligations (not faked).**

- **`nix fmt`** — impossible here, nix is not installed in this devcontainer.
  No prettier substitute was applicable to this slice either, since it creates
  no file and both touched files are on the never-reformat list. The `nix fmt`
  pass remains an owed pre-merge obligation.
- **Human visual pass in the running app** — no browser here. Nothing in this
  row is visual (it changes log payloads only), so the exposure is limited to
  confirming that developer log files still carry enough diagnostic context
  after the `passphrase` omission. Recorded, not faked.
- `yarn check:all` and `yarn storybook:build` were NOT run; both are red at
  HEAD for the unrelated storybook manager-side JSX loader reason (F-20) and
  are not valid gates.
- The tracker row for task-170 is still `"status": "pending"` and needs
  flipping at commit time. This review did not touch the tracker.
- **Closing note for cv-1 close:** task-135 discharged task-171's owed AC-4
  first clause and gate 7 above re-confirms `yarn i18n:manage` clean at this
  row, so that carry-forward is settled and should not be repeated again.

**Blockers.** None.

Decision: approved

---

## Planner: cv-1 slice close (2026-07-28)

**Status: cv-1 is closed.** All twelve rows — task-126 … task-135 plus the two
post-approval additions task-170 and task-171 — are `complete` in
`governance-drep-discovery-plan-tasks.json`, each on its own commit, each
code-reviewed in this log and **approved on iteration 1 with zero blockers**.
No row required a second review round anywhere in the slice.

**What shipped.** Commits in landing order: `35a8a57d0` task-126 (four authored
cardano-wallet voting fixtures) → `f948845a5` task-127 (the latent
`voting_and_delegating` → `delegating_and_voting` wire-literal fix, constant
export name preserved) → `83edc15fa` task-128 (`voting?: WalletVotingTarget` on
both delegation shapes) → `40bcd990a` task-129 (the pure `normalizeDRepIdentity`
helper, no new dependency) → `1d33baa2c` task-130 (`delegation.active.voting`
mapped through `_createWalletFromServerData` with a sanitized rejection warning)
→ `2baed760c` task-131 (`Wallet.votingTarget` plus `currentVote` / `isVoting`,
including the `update()` pick-list entry R-2 demanded) → `23f443b76` task-132
(`CurrentVoteSummary`'s four CORE states and, note, `CurrentVoteSummary.messages.ts`
itself) → `051567976` task-133 (the four-knob Storybook entry) → `d8f71319c`
task-134 (mapper cases, Wallet computeds, four snapshots, sanitized-warning
cases, the ninth normalizer vector) → `a3e352841` (a docs-only reconciliation of
the guide's task-134 Step-1 block against AC-7) → `523141760` task-171 →
`d3729994a` task-135 → `fb4f07f6c` task-170.

**Gates at close, re-measured for this entry rather than copied.** The
unfiltered `node_modules/.bin/jest --runInBand`, no path argument, at
`fb4f07f6c`: **86 test suites (1 skipped, 85 passed), 1073 tests (12 skipped,
1061 passed), 6 snapshots, exit 0** in 29.735 s. `jest --listTests` reports 86
suites unfiltered against 10 under a `tests/jest` filter — the trap the guide
warns about, now with current numbers. Per-row gates as recorded above:
`yarn compile` exit 0 on Node v24.16.0 at every row with the fallback never
needed (which is why F-23 closes PRD R-4 outright), `yarn lint` exit 0 at
exactly 5591 warnings with zero delta at every row, `yarn i18n:manage` clean
from task-135 onward, and the task-111 floor suite green at every row — 23 tests
through task-135, 24 from task-170.

**Ordering deviation from the guide.** The guide's own "Implementation Order"
section (`cv-1-implementation-guide.md:18-61`) lists task-170 at item 10,
task-171 at item 11 and task-135 at item 12, i.e. **170 → 171 → 135**. The
orchestration executed **171 → 135 → 170**, following the tracker's JSON listing
order. Both are valid topological orders of the same graph: task-171's
`dependencies` is `[]`, task-135's is `["task-132", "task-171"]`, and task-170's
is `["task-130", "task-109"]`, so task-170 is independent of the other two and
may sit anywhere after task-130. The executed order also preserves the one
constraint the guide argues for in prose — the marker guard must land *before*
the copy mint, because "a guard landing after the mints protects nothing" — and
it satisfies F-19, which makes task-133's AC-1 unsatisfiable until task-135
seeds the catalogs. Recorded as a deviation for the record; it changed no task's
content and cost nothing.

**task-171's deferred AC-4 clause is DISCHARGED, and must not be carried
forward.** task-171's AC-4 has two clauses. The second — `defaultMessages.json`
and `translations/messages.json` unchanged by the restoration — was met and
measured at that row. The first — `yarn i18n:manage` runs clean — was
**unsatisfiable at task-171's position by the guide's own construction**: the
twelve `voting.governance.currentVote.*` keys were already missing from all four
catalogs at `a3e352841` (tasks 130-134 shipped the component without
regenerating), while task-171's own inline invariant forbids it from editing the
generated catalogs, and seeding those keys is verbatim the next row's stated
deliverable. A spec self-contradiction, not an implementation defect. **task-135
discharged it in full**: `yarn i18n:manage` exits 0 and is a genuine no-op —
zero added, zero deleted, all four catalog sha256s byte-identical before and
after, `git diff --stat` unchanged — and task-170's gate 7 re-ran it three times
and re-confirmed the same. The canary task-171 asked for also held: the twelve
freshly minted ja-JP values (real Japanese, not runner passthrough) all carry
`!!!` and pass the guard. This carry-forward is settled and is struck here.

**Cross-phase bookkeeping task-170 triggered.** Two slice-1 rows were updated as
a direct consequence, and both changes are already in the tracker:

- **task-109** ("Redact governance vote targets in filterLogData") is
  **re-promoted from `complete` back to `verified`**. Its AC-2 was the criterion
  that had forced the earlier demotion: `filterLogData` only redacts where a call
  site invokes it, and `api.ts:379-383` logged the whole `wallets` /
  `legacyWallets` arrays with only `hwLocalData` filtered — so on the 5000 ms
  `WalletsStore` poll a raw DRep id reached the log file roughly every five
  seconds for any wallet that had voted. task-170 closed exactly that gap at six
  Shelley call sites and supplied the proof task-109 lacked, from a different
  module at a call boundary that row never touched.
- **task-111** (the sanitization spy suite) keeps `verified` with its recorded
  caveat **cleared**. That caveat was that the suite's call-boundary cases covered
  `delegateVotes` and the two hardware-wallet paths only, with no `getWallets`
  case. task-170 AC-4 added it, taking the suite 23 → 24, and the case was proved
  non-vacuous: reverting only the `getWallets` wrap failed exactly that one case
  of 24 and named the fixture DRep id. task-111's reusable module-scope
  `jest.mock` harness pattern held for a fourth time, which is AC-3's
  reusability clause holding again.

**auditSummary check — a verified no-op.** The tracker's fourteen phase objects
were enumerated programmatically: **`slice-1` is the only phase carrying an
`auditSummary` field**; `slice-2` … `slice-8`, `cv-1`, `cv-2`, `anchor-1`,
`anchor-2`, `standing` and `ux-refinement` all carry exactly
`id, name, description, riskLevel, tasks`. cv-1 therefore matches the convention
of the other twelve phases and **no `auditSummary` was invented for it**. This
was verified, not assumed, and the tracker JSON is untouched by this entry.

**Guide drift reconciled at close.** Five factual corrections were applied to
`cv-1-implementation-guide.md`, all plain restatements of measured fact, none
touching a code block or an acceptance criterion's substance:

1. The stale suite count. `all 82 suites stay green` in four verify blocks
   (task-131, task-132, task-170, task-171) → `every suite stays green (86 at
   cv-1 close)`; `the UNFILTERED 82-suite jest run` in three acceptance bullets →
   `the UNFILTERED whole-tree jest run (86 suites at cv-1 close)`; and the
   Cross-Cutting gate's `all 82 suites` → `all 86 suites at cv-1 close`.
2. The whole-tree-gate trap note, which read "the unfiltered tree is 82 suites,
   `tests/jest` selects 7 of them" — re-measured with `jest --listTests` to 86
   and 10, with the authoring-time figure kept in parentheses.
3. task-171's "Files touched" list, which omitted
   `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`.
   Added, with the one-line reason: three exact-text ja-JP `getByText`
   assertions cannot match once the marker is restored, so the row's own
   whole-tree gate is unreachable without them.
4. task-171 Step 3's `git diff --stat  # ja-JP.json and the new spec only`,
   which contradicted the delivered three-file diff. Corrected to name all three.
5. task-171 Step 3's `yarn i18n:manage  # zero added, zero deleted keys`, which
   is unreachable at that position. Annotated in place to state that the run is
   *expected dirty* there, why (the twelve currentVote keys are missing at HEAD
   and are task-135's deliverable), and where the clean run is actually gated.

The task-134 drift a previous review recorded — the Step-1 block and its
acceptance bullet describing two `not.toHaveBeenCalled()` assertions where four
ship — was **already reconciled by commit `a3e352841`** and needed nothing here;
that was verified against the commit diff rather than assumed.

**Guide/tracker drift deliberately left, and owed.** Each of these needs a
phase-owner judgment call, not a factual patch, so none was edited:

- task-171's **acceptance bullet AC-4** still states the `i18n:manage`-clean
  condition that its own ordering makes unreachable at that row. The wording
  mirrors the tracker's acceptance criterion, which is authoritative; rewording
  an approved criterion is not a scribe's call. The Step 3 annotation above now
  explains it in place, and the discharge is recorded here and in the PRD.
- The **task-135 tracker description** still says the row creates
  `CurrentVoteSummary.messages.ts`; task-132 did, and both the guide and the
  delivered diff are correct.
- The guide's **task-170 Step 3 table anchors** are pre-change (`api.ts` is now
  8 lines shorter), and its audit grep is under-inclusive in two independent
  ways — it cannot see multi-line `logger.debug(` calls, and its
  `^[0-9]*-        wallets\?,$` filter can never match `importedWallet,`. The
  guide's own header rule directs re-anchoring by content, so this is expected
  rather than defective; the post-change anchors are in the task-170 entry's AC-3
  table above.

**Owed at cv-1 close (the complete list, nothing faked green).**

1. **One human browser session in the main checkout**, covering two open
   acceptance halves and nothing else: **task-133 AC-1** — all four
   `CurrentVoteSummary` knobs render in en-US AND ja-JP via the global locale
   toggle with no console errors and no missing-message warnings; and **task-135
   AC-2's overflow half** — the same four knobs with the locale switched to
   Japanese, every string rendering fully with no clipping and the panel growing
   naturally, `noDelegation.subline` at 58 characters being the likeliest
   clipper. AC-2's marker half is met and machine-verified on all twelve ja-JP
   values. Explicitly pre-authorised as a follow-up by the guide; there is no
   browser in this devcontainer. No new task row is needed.
2. **The `nix fmt` pre-merge pass (F-5).** `nix` is not installed here. The
   substitute throughout was `node_modules/.bin/prettier --check` on explicit
   paths and only on files this slice created — never `yarn prettier*`, and never
   on a pre-existing file, under the standing ~240-file drift hazard.
3. **The release-end `!!!` copy review**, user-owned by invariant 11 and never a
   per-slice task. cv-1 enlarged its surface: the strip now touches twelve more
   values, and each is pinned by a committed snapshot and by exact-text matchers,
   so the strip is a code change rather than a copy edit.
4. **The three guide/tracker prose items above**, left as judgment calls.
5. **`yarn check:all` and `yarn storybook:build` remain unrun and are not valid
   gates** — both are red at HEAD for the unrelated storybook manager-side JSX
   loader reason (F-20). The working automated floor for Storybook rows is a
   clean `yarn storybook` preview compile.

**What cv-2 inherits.**

- **Built and green to build on:** `Wallet.currentVote` / `isVoting`, the
  `WalletVotingTarget` shape, `normalizeDRepIdentity` (with full branch
  coverage), and `CurrentVoteSummary`'s four core states. The live status badge,
  `VotingPowerDelegation` pre-fill and same-vote prevention are cv-2's, exactly
  as scoped.
- **F-15, which task-170 explicitly does not close.** `filterLogData`'s
  `sensitiveData` list is keyed to the WIRE shape (`drepId`, `dRepId`, `vote`,
  `voting`); the renderer-side domain names `votingTarget` and `currentVote` are
  absent from it. task-170 wraps wire payloads whose key is `voting` and correctly
  adds no `sensitiveData` entry. **The domain shape stays unguarded the moment
  cv-2 gives it a consumer — the first such consumer owns closing it.**
- **F-26**, recommended at task-170 and now recorded: `api.ts:1995`
  `importWalletFromKey` and `:2025` `importWalletFromFile` log
  `{ importedWallet }` unwrapped and are Shelley-`AdaWallet`-typed. Pre-existing
  and byte-identical at HEAD, type-level rather than demonstrated (both POST
  Byron/V0 legacy import endpoints, neither on a poll). For the reviewer to
  accept or schedule.
- **The `HardwareWalletsStore` `[HW-DEBUG]` raw `{ error }` sink** — the
  message-SUBSTRING leak class a key-based redactor structurally cannot reach.
  Still open, fenced out of task-170 by both the guide and the tracker.
- **The i18n key-DELETION blind spot (F-25).** The `!!!` guard filters on
  `key in ja`, react-intl falls back to the `defaultMessage`, and
  `jest.config.js` has both `setupFiles` and `setupFilesAfterEnv` commented out,
  so no console error becomes a failure. Self-healing across an `i18n:manage`
  run but silent between them. Unowned; the cheap form is a key-set symmetry
  assertion added to the existing `preliminaryCopyMarkers` suite, not a new file.
- **The guard's deliberate asymmetry.** It never fires for a key minted with no
  `!!!` in en-US at all. Low risk today because en-US markers derive from each
  component's source `defaultMessage`, but task-146 and anchor-2 authors should
  know the direction it does not cover.
- **The copy-mint procedure task-135 established (F-25), to be repeated
  verbatim by task-146:** define the messages in source first, let the runner
  seed both catalogs, replace only the ja-JP *values* by hand, keep every `!!!`,
  and never hand-edit an en-US value away from its `defaultMessage` while a
  snapshot bakes the fallback in.

**Scope of this entry.** Documentation only. It edited
`cv-1-PRD.md` (Final Outcome filled, slice status flipped to closed) and
`cv-1-implementation-guide.md` (the five factual reconciliations above), and
appended this entry. No source file, no test, no story, no locale catalog and no
tracker JSON was touched, and nothing was committed — commit is a separate
owner's step. `/workspaces/daedalus` was never read, edited or run against.

Decision: cv-1 closed.

---

## Planner: 2026-07-28 — comparator-note correction discharged (cv-2 task-140)

**Discharges** the "Correction owed on the comparator note" promise at
`:1224-1234`.

**The note.** `:736-738` offers `cip129` or the (`credentialHex`,
`credentialType`) pair as the same-vote key. Read as a plain string comparison the
`cip129` half is case-unstable: BIP-173 permits an all-uppercase encoding,
`normalizeDRepIdentity` returns `raw` / `cip129` byte-untouched
(`normalizeDRepIdentity.ts:39-40`, `:56`), and the form gate
`Cardano.DRepID.isValid` (`VotingPowerDelegation.tsx:221`) accepts it. The note's
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
