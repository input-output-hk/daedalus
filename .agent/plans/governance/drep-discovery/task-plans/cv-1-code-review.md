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
