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
