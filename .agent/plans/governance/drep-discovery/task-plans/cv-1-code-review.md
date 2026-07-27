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
