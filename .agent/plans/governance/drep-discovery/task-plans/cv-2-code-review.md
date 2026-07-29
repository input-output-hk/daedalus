# CV-2 Code Review Log

> Append-only transcript: `Planner:` entries (planning open/close), one `Critiquer:` entry
> (required review pass over the PRD + guide), and per-task `Code Review:` entries.
> Companion docs: [cv-2-PRD.md](./cv-2-PRD.md) ·
> [cv-2-implementation-guide.md](./cv-2-implementation-guide.md)

---

## Planner: 2026-07-28 — cv-2 planning complete (status: in_review)

**Scope planned.** Fifteen tasks, all `pending` at HEAD `504b44c1a`, all classified
`autonomous` (none is in the locked non-autonomous set — task-125, the task-166
remainder, task-158, and the release-end `!!!` copy review — and no blocking decision
survived planning). cv-2 closes Track V's enrichment half: it puts the live lifecycle
badge on the cv-1 `CurrentVoteSummary`, turns the delegation form into something that
knows what the wallet already voted for, and stops the user re-submitting the vote they
already hold.

Task by task:

- **task-143** creates `storybook/stories/governance/_utils/fixtures.ts` — a pure module
  exporting `currentVoteOptions`, `useCurrentVoteKnob`, `resolveCurrentVote`,
  `makeGovernanceWallets`, `makeDRepIndex` — with checksum-verified bech32 vectors and no
  module-level mutable state.
- **task-136** adds the live status badge to `CurrentVoteSummary` from the
  slice-1-populated `drepIndex`: a renderer-local `expiring` badge, reuse of the shared
  `DRepStatusBadge` for `active` / `inactive`, four new message descriptors, a rewritten
  spec with regenerated snapshots, and a rewritten component story.
- **task-137** replaces the selected `Wallet` object in `VotingPowerDelegation` state with
  `selectedWalletId`; the wallet is derived on every render.
- **task-138** pre-fills the form from the wallet's current on-chain delegation through a
  module-scope `deriveFormSeed`, applied at both seed sites, plus a reactive re-seed that
  never overwrites a dirty input.
- **task-139** mounts `CurrentVoteSummary` between the wallet picker and the vote-type
  controls, unconditionally (so `noDelegation` still renders), and wires `drepIndex`
  through the container.
- **task-140** adds `source/renderer/app/utils/governance/isSameVoteTarget.ts` plus its
  spec, disables submit when the chosen vote equals the current one, and renders a visible
  hint paragraph wired with `aria-describedby`. It also discharges four documentation
  obligations (design-doc sentence, an appended cv-1 code-review correction, an in-place
  cv-1 F-9 correction, and one tracker AC re-anchor).
- **task-173** replaces the prefix heuristic in `VotingGovernancePage` with
  `normalizeDRepIdentity`, and pins the result through a dialog-props recorder in the
  container spec.
- **task-141** is a pin: no production change survives review, so it appends the
  no-historical-props assertion instead.
- **task-142** is the verification row for the confirmation dialog — section-scoped pins
  for the fee, hardware-status and passphrase blocks, authored to survive task-175.
- **task-175** renders the pre-anchor shared-design-tokens §7 identity block (CIP-129
  primary line, CIP-105 and signed-payload renderings, source label, sentinel suppression)
  behind two new descriptors.
- **task-144** adds `GovernanceWrapper` with key-based remount so a knob change rebuilds
  the fixture wallets instead of mutating them.
- **task-145** wires the `currentVote` knob across the wallet-bearing governance stories
  and deletes the module-level fixture array.
- **task-146** mints the seven remaining enrichment keys into both catalogs, hand-writes
  the ja-JP values, and widens the preliminary-copy guard with two assertions.
- **task-147** is the slice's regression harness: current-vote flows, the hardware-wallet
  path, the comparator's letter-case vectors, and the widened sanitization floor.
- **task-148** pins the `same_vote` server path at the store level and in the render path,
  changing none of its six existing sites.

Everything is renderer-and-storybook only: no new IPC channel, no new cardano-wallet
endpoint, no signing-path change, no `GovernanceStore` read from `VotingStore`, no new
logging anywhere.

**Canonical build order (binding).**

```
143 → 136 → 137 → 138 → 139 → 140 → 173 → 141 → 142 → 175 → 144 → 145 → 146 → 147 → 148
```

This is the tasks-JSON listing order with one amendment (D-13): task-143 is hoisted to
position 1 because task-136 AC-4 requires a `drepVerified` Storybook knob that does not
exist at HEAD, and building task-136 first would force a throwaway local knob edit that
task-143 / task-145 would then delete. task-143's only dependency is task-131
(`complete`), so the hoist breaks no edge. The constraints that are not free: 137 before
138 (138's AC-3 is only expressible once selection is an id); 139 after 136 and 138 (it
mounts the finished panel); 173 before 141 (stated in task-173's own description — same
container); 175 after 142 (D-3); 145 after 139 and 144; 136 + 140 + 175 before 146 (146
mints from their descriptors, D-9); 140 / 142 / 145 / 146 before 147.

**Numbered decisions resolved during planning (binding; full text in the PRD).**

- **D-1** — The `expiring` badge is renderer-local. `DRepStatus` is a closed two-value
  union and must not be widened (invariant 14); `active` / `inactive` reuse the shared
  `DRepStatusBadge` unchanged, so the two consumers outside cv-2's fence are untouched.
- **D-2** — The expiry threshold is `≤12` epochs, not the cohort's `7–12`.
  `CurrentVoteSummary` renders the user's own delegation, which is not cohort-scoped and
  can sit at `drepActivity` 1…6 — exactly where a `7–12` gate would show nothing.
- **D-3** — task-142 stays before task-175; its assertions are section-scoped, whole-dialog
  snapshots are forbidden, and task-175 re-runs them as its own gate.
- **D-4** — task-140's and task-173's documentation obligations: one pre-discharged, one
  re-anchored, two corrections owed (the cv-1 code-review comparator note is discharged by
  an appended entry because that file is append-only; cv-1 F-9 is corrected in place).
- **D-5** — task-139's AC-3 is split: the reactivity clause is retained, the `givenName`
  clauses are struck and deferred to anchor planning. No store field carrying a DRep name
  exists today (`AppDRepDirectoryEntry` has `drepId`, `votingPower`, `status`,
  `drepActivity`, `anchor` and nothing else).
- **D-6** — `CurrentVoteSummary` receives its directory entry as an optional prop; the
  container reads the store. An `@inject` read would break all four committed specs, which
  render the component with no MobX provider.
- **D-7** — `drepVerified` ships as an option id with cv-2-renderable semantics: all five
  ids exist, the two DRep options differ by the lifecycle state cv-2 can actually render,
  and the verified-anchor affordances are documented as arriving in anchor-2.
- **D-8** — "Every governance story" is scoped to the stories that render a wallet.
- **D-9** — Message descriptors are minted by their consumers (task-136, task-140,
  task-175); task-146 owns the catalogs. Seven new keys total, every one carrying `!!!` in
  both locales.
- **D-10** — The same-vote hint is a visible inline paragraph carrying a stable `id` with
  `aria-describedby` on the button, not a hover tooltip — react-polymorph's `Button` takes
  a plain `disabled` prop shared with three other disable reasons.
- **D-11** — task-138 takes the re-seed branch, not the "data changed" indicator. The
  re-seed applies only while `drepInputState.dirty === false`; a dirty input is never
  overwritten.
- **D-12** — The three unregistered governance story files stay unregistered. This is a
  cv-1 deferral, not a new discovery, and no cv-2 task owns it.
- **D-13** — task-143 is hoisted to build position 1 (see the build order above).
- **D-14** — The `!!!` guard is widened in place with two assertions in the existing spec:
  key-set symmetry between the catalogs, and namespace marker coverage over
  `voting.governance.currentVote.`. Both are green on arrival at HEAD (1611 keys each,
  zero asymmetry; 12 keys in the namespace, zero unmarked), so neither needs an allow-list.
  The `confirmationDialog.` namespace is deliberately excluded — seven of its eight keys
  legitimately predate the feature — and task-175's two new keys are pinned individually.
- **D-15** — task-148 pins the `same_vote` path and changes none of its six sites.

**Seam contract (compressed; full text in the PRD's "Cross-Task Seam Contracts").**

- **S-1 — `VotingPowerDelegation` state shape** (137 → 138, 139, 140). `selectedWalletId:
  string | null` replaces the `Wallet` object in `FormData` / `Form`; the wallet is derived
  immediately after the `useState` call. No store read is introduced — `wallets` stays the
  existing prop, which the request layer replaces with fresh instances on every poll.
- **S-2 — `CurrentVoteSummary` props and mount point** (136 → 139). `drepEntry?:
  AppDRepDirectoryEntry | null`, optional and defaulting to `null` so the four committed
  specs keep compiling; the type is imported as a type from `GovernanceStore`, never
  re-declared and never via the store class. The panel mounts between the wallet picker
  and the vote-type controls, unconditionally, outside the existing `selectedWallet` gates.
- **S-3 — Pre-fill derivation** (138). One pure module-scope `deriveFormSeed(wallet,
  inheritedDRepId)` applied at both seed sites, with the fallback chain current DRep vote
  (`raw` verbatim — no trim, no re-encode) → sentinel kind → inherited directory id →
  blank form.
- **S-4 — The same-vote comparator** (140 → 147, 148). A new module
  `utils/governance/isSameVoteTarget.ts`, not a closure: `null` current vote is `false`,
  sentinels compare as sentinels, DRep equality is `credentialHex` case-insensitive **and**
  `credentialType` equal, and a missing `credentialHex` on either side is never equal. It
  takes strings and returns a boolean — it mutates nothing (invariant 10). task-140 creates
  the spec; task-147 extends the same file with the letter-case vector. No second spec.
- **S-5 — Confirmation-dialog identity block** (173 → 141, 142, 175). The container derives
  the identity with `normalizeDRepIdentity` instead of a prefix heuristic; sentinels yield
  `null`; the `null` branch renders the raw string verbatim; the dialog stays
  current-target only, with no historical-target prop and no store-backed comparison state.
- **S-6 — Storybook fixture module** (143 → 144, 145, 136). The named exports above; the
  knob label is `'Current vote (mock)'`; `makeGovernanceWallets` constructs `Wallet`
  instances directly and returns a fresh array per call.
- **S-7 — Story wiring** (145, plus 136's component story). Wallet-bearing stories read the
  knob and render through `GovernanceWrapper`, which supplies `{ wallets, drepIndex }` and
  remounts by key. At least one story keeps the input different from the wallet's
  `currentVote` so the error knob stays exercisable.
- **S-8 — Test file map** (140, 141, 142, 147, 148, 175). One new spec file
  (`tests/jest/governance/isSameVoteTarget.spec.ts`); everything else extends existing
  colocated specs. task-147 and task-148 carry `targetPath: "tests/jest/"` but extend the
  colocated harnesses under `source/` rather than duplicating ~150 lines of scaffolding —
  the cv-1 task-134 precedent — and record the deviation in `statusReason`.
- **S-9 — Sanitization surface cv-2 widens** (147, discharging cv-1 F-15). Every renderer
  domain name cv-2 makes live (`votingTarget`, `currentVote`, `drepIdentity` and its
  members, `chosenOption`) is unguarded by `filterLogData`'s exact-match key list, and the
  sentinels ride `kind` on `WalletVotingTarget`, so the domain shape defeats the guard too.
  cv-2's discharge is the stricter invariant — no domain `Wallet` and no `DRepIdentity`
  ever enters a logger or analytics payload from a cv-2 code path — asserted with the
  task-111 spy pattern, not a two-key patch. The key-list extension is specified as a
  whole-surface fallback so a reviewer does not improvise one.
- **S-10 — Guide-authoring shards** (suggested, not binding). The guide was in fact
  authored in six shards and assembled: task-136 · task-137 + task-138 · task-139 +
  task-140 · task-173 + task-141 + task-142 + task-175 · task-143 + task-144 + task-145 ·
  task-146 + task-147 + task-148.

**Residual risks carried into the build phase (PRD R-1…R-9).**

- **R-1 (high)** — the deferred `givenName` work is orphaned unless anchor planning acts.
  D-5 strikes two clauses from task-139 AC-3 and points them at anchor-2, but no anchor
  task owns them today and none adds the store field they need. The two required tracker
  edits are named explicitly in D-5 and must be actioned by the anchor-1 / anchor-2
  planning passes. Recorded as an option, not a stop condition — cv-2 completes without it.
- **R-2 (medium)** — task-136's badge breaks committed assertions and four colocated
  snapshots the moment it renders; the spec rewrite and snapshot regeneration are inside
  task-136's scope, not a surprise at review.
- **R-3 (medium)** — task-140's client-side disable can hide the `same_vote` server net;
  task-148 pins it at the store level where the UI gate does not apply, plus a render
  assertion.
- **R-4 (medium)** — four independent copies of an expiry window already exist; D-2 fixes
  the value and forbids importing `DRepCategoryBadge`'s constants.
- **R-5 (medium)** — task-142 and task-175 edit the same file in sequence; D-3 scopes 142's
  assertions and forbids whole-dialog snapshots.
- **R-6 (medium)** — the domain-object logging surface is wide; S-9's stricter invariant
  plus task-147's spies, with the whole-surface fallback pre-specified.
- **R-7 (low)** — react-polymorph's `Button` may not forward `aria-*`; D-10 pre-decides the
  fallback (the visible hint alone satisfies AC-2).
- **R-8 (low)** — ja-JP overflow is unverifiable here (no browser in this container).
  Recorded as OWED at slice close, exactly as cv-1 did; the longest new string, and so the
  specific overflow candidate, is the expiring badge label.
- **R-9 (low)** — `resolveExactDRepMatch` is used outside the directory it lives in; the
  import direction mirrors the existing cross-directory `_shared/DRepIdDisplay` import, and
  task-136 pins both encodings with unit cases so the behaviour is proven wherever it lives.

**Environment deviations that must be reported, not silently absorbed.** `nix` is absent
in this container, so `nix fmt` cannot run — `node_modules/.bin/prettier --write` on
explicit, newly created paths is the recorded substitute and the user must run `nix fmt`
before merge. `gh` and push credentials are absent, so all work stays local.
`prettier --check` is already red at HEAD on `VotingPowerDelegation.tsx`,
`VotingPowerDelegationConfirmationDialog.tsx`, `VotingGovernancePage.tsx` and
`storybook/stories/voting/Governance.stories.tsx`; `yarn check:all` is red at HEAD for
unrelated reasons (the `storybook:build` manager-webpack JSX loader gap). Neither may be
read as a cv-2 regression.

**Verification contract.** After every task: `node_modules/.bin/tsc --noEmit` zero errors
(`yarn compile` when a new `.scss` module needs its declarations regenerated); focused Jest
via `node_modules/.bin/jest --testPathPattern=<pattern> --no-coverage --runInBand`; the
task-111 sanitization floor suite re-asserted green where the guide calls it — task-138,
task-173, task-175 and task-147; `yarn lint` clean for the touched surfaces; `yarn i18n:manage` clean and
idempotent after task-146, with every file that was clean at HEAD restored via
`git restore` if the run writes; prettier only on files cv-2 creates; exactly one
subject-only Conventional Commits line per task with explicit paths staged.

**Guide assembly note.** `cv-2-implementation-guide.md` was assembled from the six authored
shards without rewriting, shortening or re-ordering their prose or code. Three structural
normalizations were applied: the task-143 block was hoisted to document position 1 so the
document order matches the binding build order (task-136's first instruction requires
task-143 to be committed already); the later Storybook group heading was narrowed to
`(task-144, task-145)` so the task-143 id is not claimed by two headings; and `---`
separators were inserted at the six shard joins, which the raw concatenation lacked. The
guide's preamble carries the slice id, the binding build order, all fourteen locked
invariants in full, the measured environment and verification commands, and the formatting
/ commit / comment conventions, with pointers to the PRD, the tracker and
`research/cv-2-findings.md`.

Planning status set to `in_review` — awaiting the required Critiquer pass over the PRD and
the guide.

---

## Critiquer: 2026-07-28 — required planning review of cv-2 PRD + implementation guide

**Scope reviewed.** `cv-2-PRD.md` (1637 lines) and `cv-2-implementation-guide.md`
(5685 lines) in full, plus `research/cv-2-findings.md` and this log, against the cv-2
phase of `governance-drep-discovery-plan-tasks.json` (`:1162-1457`), the parent plan,
`designs/current-vote-display-design.md`, `designs/shared-design-tokens.md`, the cv-1
corpus, `prompt.md`, and live code in this worktree at `504b44c1a`.

**Method.** Five independent lens passes — task/plan/design coverage · consistency
with the tasks JSON and the fourteen locked invariants · implementability and
conciseness for a small model · internal consistency across the four cv-2 docs ·
tests / i18n / stories / docs / hidden manual checkpoints — consolidated here. Every
finding was re-opened and re-verified against the live worktree before promotion;
what did not survive is listed under **Dropped findings**.

**Checks that came back clean.**

- **Coverage.** All 15 tasks appear in both docs, none dropped or merged. All **61**
  acceptance criteria are quoted verbatim (4+3+7+3+7+6+2+3+5+4+2+4+4+5+2 = 61,
  counted off the tracker) and each has a per-AC checklist entry in the guide. Every
  task is `autonomous` on a defensible basis (none is in the locked non-autonomous
  set). The canonical order `143 → 136 → 137 → 138 → 139 → 140 → 173 → 141 → 142 →
  175 → 144 → 145 → 146 → 147 → 148` satisfies every `dependencies` edge, and all ten
  cross-slice prerequisites are `complete`/`verified`.
- **Anchors.** ~50 cited `path:line` anchors opened; the load-bearing ones are exact
  (`VotingPowerDelegation.tsx:133`/`:135-137`/`:139-143`/`:160-163`/`:231-237`,
  `VotingGovernancePage.tsx:75-83`, `ConfirmationDialog.tsx:151-172`/`:174-177`/
  `:179-185`/`:186-202`, `helpers.ts:139-153`, `GovernanceStore.ts:100`,
  `governance.types.ts:20-31`, `Governance.stories.tsx:63-97` + the four
  `GOVERNANCE_WALLETS` reuse sites `:233`/`:420`/`:457`/`:492`, `.stylelintrc`,
  `.gitignore:141`, `preview.tsx:8`). The off-by-ones found are all cosmetic (M-2).
- **Bech32 provenance.** All nine vectors in tasks 140/143/147/175 decode exactly as
  the guide predicts (re-decoded here with the repo's `bech32@2.0.0`), and
  `Cardano.DRepID.isValid` / `toCip129DRepID` behave as claimed on the CIP-105,
  legacy-28-byte and uppercase forms.
- **Invariants 4, 5, 8, 9, 10, 12, 13, 14.** `drepIndex` reaches
  `VotingPowerDelegation` by prop-drill only; `VotingStore` gains no `GovernanceStore`
  reference; `DRepStatus` stays the closed two-value union with `expiring` derived
  locally; the comparator, the pre-fill and the §7 identity block are all read-only.
- **Storybook discipline.** No story wraps a local `IntlProvider`, no per-locale
  variants, the `Connected flow` exemplar is preserved, and story ids are not renamed.
- **Environment baselines.** Re-measured and correct as recorded: jest
  `(governance|Governance|voting|Voting|DRep)` 17 passed / 1 skipped / 269 passed;
  `tsc --noEmit` exit 0; 1611 keys per catalog with 12 `currentVote` / 8
  `confirmationDialog`; `stylelint CurrentVoteSummary.scss` 12 pre-existing errors.

---

### Blockers (ranked, most severe first)

**B-1 (blocker, guide) — task-138's `rerenderWithStores` cannot run; its AC-3 pin
throws before the assertion.** Step 6(b) (`:1998-2048`) builds a fresh store object
and re-renders the whole tree with it —
`rerenderWithStores: (nextStores) => rerender(tree(nextStores))` — and Step 6(c) case
4 (`:2110-2134`) calls it with `buildStores({ wallets: [...] })`. `tree()` renders
`<Provider stores={currentStores as any} …>`; mobx-react is 6.3.1 and its development
build throws by construction when the provided store set changes identity
(`node_modules/mobx-react/dist/mobxreact.cjs.development.js:481-497`,
`if (!shallowEqual(value, newValue)) throw new Error("MobX Provider: The set of
provided stores has changed…")`). Measured in this worktree with a throwaway probe
spec: replacing the `stores` object on rerender throws that exact error; re-rendering
with the *same* `stores` object after mutating `stores.wallets.all` does not. Case 4
is the designated AC-3 pin ("Pre-fill is reactive … pinned by Step 6 case 4",
`:2176-2178`), so the criterion has no working proof and a small model gets an opaque
failure with no diagnosis path in the guide.
*Fix:* keep the `stores` object identity stable. Return
`rerenderWithWallets: (wallets: any[]) => { stores.wallets.all = wallets; rerender(tree(stores)); }`
instead of `rerenderWithStores` (`wallets.all` is a plain array on a plain object,
`VotingGovernancePage.spec.tsx:121`, and the container re-reads it every render,
`VotingGovernancePage.tsx:63`), and rewrite case 4 to call it with the vote-carrying
wallet. Add one line to the task's resolved judgment calls recording that the stores
object must not be replaced, citing the Provider guard.

**B-2 (blocker, PRD + findings) — the binding `drepIndex` lookup expression is
measurably wrong, and the guide has to contradict a "binding, not re-derivable" seam
to be correct.** PRD D-6 prescribes
`resolveExactDRepMatch<AppDRepDirectoryEntry>(currentVote.drep.raw, drepIndex)`
(`:536`, repeated in the data-flow diagram at `:1254`) on the stated ground that the
helper "canonicalizes CIP-105 → CIP-129" (`:540`);
`research/cv-2-findings.md` F-1 carries the same reasoning (`:58`, `:61-74`). Live:
`helpers.ts:143` is `if (!Cardano.DRepID.isValid(full)) return null;` **before** any
canonicalization, and measured here
`Cardano.DRepID.isValid('drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l')`
returns `false`. So the prescribed call returns `null` for exactly the CIP-105 case
D-6 exists to fix — the silent "no record yet" miss on an indexed DRep. The guide is
right (`:2305-2312`, `:2374-2382`: `currentVote.drep.cip129 ?? currentVote.drep.raw`),
but PRD `:766-768` declares the seam contracts binding and calls any contradiction "a
guide defect", so a reviewer applying the PRD would revert correct code.
*Fix:* correct the PRD and the findings note, not the guide. D-6 `:536` and the
data-flow line `:1254` become
`resolveExactDRepMatch<AppDRepDirectoryEntry>(currentVote.drep.cip129 ?? currentVote.drep.raw, drepIndex)`;
the clause at `:540` is replaced with the measured behaviour (the helper canonicalizes
only what `Cardano.DRepID.isValid` accepts — `drep1…` and `drep_script1…` — and
rejects `drep_vkh1…` at `helpers.ts:143`), plus the supporting fact that `parseVoting`
(`api.ts:3009-3022`) returns `null` when `normalizeDRepIdentity` fails, so `cip129` is
always populated on the production path and `?? raw` covers hand-built fixtures only.
Apply the same correction to F-1's Resolution paragraph.

**B-3 (major, PRD + guide) — the `drepIndex → drepEntry → badge` wiring ships with no
test, and R-9's stated mitigation is assigned to a task forbidden from containing the
code.** PRD R-9 (`:1515-1516`, repeated verbatim in this log at `:204`) says "task-136
pins both encodings (CIP-129 and CIP-105 `raw`) with unit cases so the behaviour is
proven wherever it lives", and findings F-1's Disposition/Owner (`:76-85`) name
task-136 for the code. But D-6 puts the lookup solely in `VotingPowerDelegation`, and
the guide's task-136 lists `VotingPowerDelegation.tsx` under **Files this task must
NOT touch** (`:681-687`); task-136's seven new cases all pass `drepEntry` directly
(`:1099-1166`), task-139 "adds **no spec file**" (`:2319-2321`), and task-147's flow
cases assert only the `!!!Delegated to DRep` heading, the same-vote hint and the
disabled submit (`:5073-5159`) — never a badge or a status caption. So no cv-2 task
owns the mitigation, and B-2's wrong key would ship green.
*Fix:* add one executable pin, cheapest in task-147 Step 2's first case — the harness
already supplies `governance.drepIndex = new Map([[VALID_DREP_ID, drepEntry]])` with
`drepActivity: 12`, which the guide itself says renders the expiring badge
(`:4955-4958`) — asserting the badge label and its caption; optionally a second case
whose wallet carries a CIP-105 `raw` with `cip129` set, as the executable form of B-2.
Then re-point R-9 (and F-1's Disposition/Owner) from task-136 to the task that
actually owns the pin.

**B-4 (major, PRD) — seam S-4 prescribes an edit that cannot compile.** S-4
(`:932-939`) and the task-140 contract row (`:154`) require
`const isSameAsCurrent = isSameVoteTarget(chosenOption, …)` "appended to the
`submitButtonDisabled` composition at `:139-143`", with "`chosenOption` stays exactly
as derived at `:160-163`". Live `VotingPowerDelegation.tsx`: `submitButtonDisabled` is
`:139-143` and `chosenOption` is `:160-163`, 17 lines below — a temporal-dead-zone
`ReferenceError` at render. The guide catches it and moves the block (`:2635-2641`),
which the PRD's own precedence rule labels a guide defect.
*Fix:* amend S-4 and the task-140 row to state that `submitButtonDisabled` is deleted
from `:139-143` and re-declared immediately after the `chosenOption` derivation, with
`formIsValid` (`:135-137`) left in place, noting that `submitButtonDisabled` is
referenced only in the JSX so the move is safe. Leave the guide as written.

**B-5 (major, guide + PRD) — task-143's mandated fixture comment states a provenance
the vectors do not have, and AC-4's satisfied-in-part record scopes the gap to the
wrong half.** The guide dictates, as file content, `// CIP-119 example credentials,
decoded and checksum-verified before commit and used verbatim.` (`:427-429`). Decoded
here with the repo's `bech32`, the four vectors are
`220f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4` and
`22a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c` (and their CIP-105
pairs) — sequential nibble patterns, not any CIP-119 test vector. PRD D-7 says so
itself: the verified vector is "**synthesized** with the repo's `bech32` dependency"
(`:570-573`). Yet AC-4's record (guide `:651-656`, PRD `:572-575`) claims "the vectors
are CIP-119-shaped … and the module records that provenance in a comment" and scopes
the shortfall to "verified hash" alone. The named provenance is a design contract
(`designs/current-vote-display-design.md:227`; plan `:103`), and at least the Cardano
Academy preprod credential is available offline in this repo —
`research/drep-state-preprod-epoch295-sample.json` carries keyHash
`e68fb144f40ed30764fba34ca21cdea2400b1b7f02cb27c04a515bdc` with its anchor url and
`dataHash`. A false provenance line in shipped source is also a comment-convention
breach: comments state the invariant, never an unverifiable claim.
*Fix:* replace the comment's first clause with what is true and load-bearing (the
unverified pair is copied byte-for-byte from the committed story fixtures; the
verified pair was synthesized with the repo's `bech32` and decoded before commit;
bech32 is case-insensitive, so re-casing or re-deriving them breaks the `drepIndex`
lookup). Restate AC-4 as satisfied-in-part on **both** halves — named provenance and
Blake2b-256 verify — in the guide, in D-7 and in the task's `statusReason`. Preferred
(cheap, offline): derive the `drepVerified` id from the committed Cardano Academy
preprod keyHash and record the sample path in the comment, which converts one third of
the named provenance from OWED to delivered.

**B-6 (major, PRD) — a locked-invariant narrowing lives only in a guide judgment
call.** The PRD asserts invariant 2 whole, including the sentinel literal, in the NFRs
(`:1217-1221`) and in "Locked Invariants Touched" (`:1391-1397`), and quotes task-147
AC-5 verbatim at `:302`; the DoD (`:1531-1534`) does not list AC-5 among its
exceptions. The guide narrows it: "The analytics vote-kind field is a deliberate
exception to 'no `abstain` literal' … Never write an assertion that `'abstain'` is
absent from an analytics payload — it will fail" (`:5006-5011`) and "AC-5 is read as:
no DRep identifier in any payload, and no sentinel literal in any **logger** payload"
(`:5394-5398`). The guide is right on the facts — `VotingStore._getVoteKind`
(`VotingStore.ts:196-202`) returns `'abstain'` / `'no_confidence'` and it is the third
analytics argument at `:399-402` and `:430-433` — and the tension is a pre-existing
recorded decision (`research/slice-3-findings.md:132-141`, F-5). The defect is
placement: the document a reviewer checks the slice against claims a floor the slice
cannot meet.
*Fix:* carry the carve-out into the PRD where the invariant is stated (`:1217-1221`,
`:1391-1397`, and S-9 at `:1153-1160`) with its evidence and the operative reading —
no DRep identifier in any logger/analytics/electron-store payload; no sentinel literal
in any *logger* payload; the analytics vote *kind* is the one reviewed exception — and
add task-147 AC-5 to the DoD's scoped list. Because task-147 Step 5 pins the literal
as a required argument (`:5341-5345`), record that pin as an accepted deviation in
`research/cv-2-findings.md` cross-referencing slice-3 F-5, so a later slice
revisiting the analytics payload finds a decision rather than an unexplained red test.

**B-7 (major, PRD) — the slice DoD's exception ledger is false as written, and one
gate is unachievable in this container.** The DoD says "All 61 verbatim acceptance
criteria above pass, except the four explicitly scoped by this PRD" (`:1531-1534`),
naming 139 AC-3, 142 AC-3, 143 AC-4, 145 AC-1. The guide declares at least seven more
partial / OWED / not-satisfied: task-136 AC-4 (`:1357-1366`, no browser — "OWED and
must never be asserted green"), task-138 AC-3 (`:2176-2182`, D-11 alternative not
built), task-139 AC-2 (`:2520-2524`, visual OWED), task-144 AC-2 (`:4256-4259`, whose
only stated proof is "Proved visually in task-145's Step 8" and which — alone among
the browser-dependent criteria — is *not* flagged OWED), task-145 AC-4 (`:4642-4646`,
"not satisfiable in this container"), task-146 AC-3 second half (`:4923-4925`, "NOT
satisfied"), and task-147 AC-5 (B-6). The same DoD section then asserts as a slice
gate the very observation the guide says cannot be made: "Storybook: all five knob
values plus the four `DRep status (mock)` values render without console errors …; the
ja-JP overflow pass is OWED" (`:1567-1569`) — the console-clean pass needs the same
absent browser as the overflow pass.
*Fix:* expand the DoD exception list to every AC the guide actually scopes, each with
its reason; split task-144 AC-2 like its siblings (structural half proved by code +
typecheck, visual remount recorded OWED and discharged with task-145's pass); and
rewrite the Storybook DoD bullet so the console-error observation is OWED alongside
the overflow pass. Mirror the same list in the "OWED at close" placeholder.

**B-8 (major, PRD) — task-140 AC-7's first conjunct is declared satisfied while the
design sentence still offers the key AC-4 forbids.** D-4 (`:409-415`) states
"task-140 AC-7's first conjunct is therefore satisfied at `:97`" and "**No rewrite.**";
guide Step 7 (`:2933-2944`) only appends a sentence and explicitly forbids touching
the alternative. Live `designs/current-vote-display-design.md:97` still reads: "The
same-vote comparator (`task-140`) must key on a case-stable form: the
(`credentialHex`, `credentialType`) pair, **or a case-insensitive `cip129`
comparison**." task-140 AC-4 in the tracker says the comparator keys on the pair
"never on raw, cip129 or cip105". So the design doc continues to sanction a comparison
key the same task's AC-4 bans — precisely the design/code drift AC-7 exists to close.
*Fix:* either delete the "or a case-insensitive `cip129` comparison" alternative from
`:97` in the same task-140 edit that appends the new sentence, so the design agrees
with AC-4; or, if the alternative is deliberately kept, record AC-7's first conjunct
as UNMET with that reason in D-4, in the guide's task-140 Acceptance entry and in the
task's `statusReason` — do not label it satisfied.

---

### Minor (non-blocking; absorb in the same fix pass)

**M-1 (PRD) — the per-task contract table and two D-sections drift from the sections
they summarize.** (a) task-138 row `:152` names `deriveInitialFormState(wallet,
initialFormState)`; S-3 `:868` and the guide `:1783`/`:1792` name
`deriveFormSeed(wallet, inheritedDRepId)` — different name *and* different second
parameter. (b) task-136 row `:150` says "mint five message descriptors (D-9)" while
D-9's table `:627-635` gives task-136 four and assigns `sameVoteHint` to task-140;
the guide mints four in 136 and `sameVoteHint` in 140 — as written the row invites a
duplicate mint. (c) task-137 row `:151` says "the six read sites" and then lists
seven. (d) D-14 `:728`/`:737` and the task-146 row `:161` say "two assertions" while
the guide adds three (`:4665`, `:4852-4877`) — the third is described but uncounted at
PRD `:750-751`.

**M-2 (PRD + guide) — cosmetic anchor and count slips.** Guide task-137 `:1463-1467`
"All eight read `state.selectedWallet`" — the grep returns seven reads (`:136`,
`:174`, `:239`, `:244`, `:260`, `:286`, `:333`); `:231-237` *writes* it through a
shadowing local, which is why Step 5(c) renames that local. Guide task-143 `:330-333`
"the three `drep1…` vectors below" — the module declares two. Guide
`governance.types.ts:19-30` → `:20-31` (its own task-138 block cites it correctly);
`parseApiCode (:74-95)` → `:74-94`; "`VotingGovernancePage.spec.tsx` (392 lines at
HEAD)" → 391 (both docs). Guide task-136 Step 2 cites `DRepCategoryBadge.scss:33-40`
and `DRepCategoryBadge.tsx:50-51` with no directory — the files are under
`source/renderer/app/components/governance/_shared/`, and the `.threshold` rule is
`:34-41`. PRD S-7 `:1078` `renderGovernancePanel (:203-234)` → `:204-236`, and `:1082`
cites the `initialFormState` region as `:429-433`, outside the `:403-423` story range
it names — live it is `:411-415`. Guide `:4123` "All three tasks are Storybook-only"
under a heading scoped to two tasks.

**M-3 (PRD) — D-8 assigns a migration that does not exist.** D-8 `:602-606` (echoed in
the guide's task-145 judgment calls `:4393-4395`) says the two confirmation-dialog
stories "**and `Unavailable while syncing`** migrate off `GOVERNANCE_WALLETS`".
`grep -n GOVERNANCE_WALLETS storybook/stories/voting/Governance.stories.tsx` returns
four sites (`:233`, `:420`, `:457`, `:492`); `Unavailable while syncing` (`:497-507`)
renders only `<VotingUnavailable syncPercentage={…} />` and references none of them.
Drop it from the AC-3 migration sentence in both places.

**M-4 (guide) — task-136's new SCSS block breaks the rule task-140 makes binding.**
Step 2 (`:884-905`) dictates `.expiringBadge` in the order `display; align-items; gap;
font-size; line-height; padding; border-radius; font-weight; color; background`, and
task-136's verify step never runs stylelint; task-140 Step 5 (`:2823-2825`) states
"Properties **must** be alphabetical: `.stylelintrc` enables
`order/properties-alphabetical-order`" and gates on it. Measured:
`stylelint CurrentVoteSummary.scss` reports exactly 12 pre-existing order errors at
HEAD, and the dictated block adds more. Either reorder the declarations
alphabetically, or state in Step 2 that the file is already stylelint-red with 12
pre-existing errors and the block deliberately matches the file's existing order —
and add the single-file stylelint run to Step 6 with the expected count either way.

**M-5 (guide) — task-145's "prefilled from directory" claim holds only for the default
knob value.** Step 6 `:4546-4548` says `selectedWalletId: 'governance-wallet-1'` means
"the pre-fill still resolves". But `deriveFormSeed` puts the wallet's own
`currentVote` ahead of the inherited directory id (guide `:1798-1816`; PRD S-3
`:876-882`), and task-143 gives `governance-wallet-1` a knob-derived `votingTarget`
(`:502-512`), so for `drepVerified` / `drepUnverified` / `abstain` / `noConfidence`
the story demonstrates the current-vote precedence, not the directory hand-off. Narrow
the claim to `noDelegation`, or point that story's `initialFormState.selectedWalletId`
at `governance-wallet-2` (always built with `votingTarget: null`, `:513-521`).

**M-6 (guide) — the per-task `yarn i18n:manage` gate is deferred without being
recorded as a deviation.** Three rows mint copy (136, 140, 175), and the guide forbids
the gate on them (task-140 Step 11 `:3051-3052`: "never `yarn i18n:manage` … as a
gate"; task-136 judgment call 8 `:836-839`: "The catalogs are not edited here"), while
`prompt.md:196-199` requires "`yarn i18n:manage` whenever copy changed". The deferral
is correct under D-9, but unstated it reads as a skipped gate. Add one line to
"Environment and Verification Commands": the gate is deliberately deferred to
task-146 by D-9, the interim state is descriptor-present / catalog-absent with
react-intl falling back to the `!!!`-carrying `defaultMessage`, and it is discharged
once in task-146 Step 4.

**M-7 (PRD) — the DoD's i18n restore rule, read literally, deletes task-146's mint.**
`:1547-1548`: "`yarn i18n:manage` runs clean and idempotent after task-146; every file
it rewrites that was clean at HEAD is `git restore`d." `en-US.json`, `ja-JP.json`,
`defaultMessages.json` and `translations/messages.json` are all clean at HEAD, so the
rule as phrased removes exactly the entries task-146 AC-1 requires. The guide's
task-146 Step 4 (`:4826-4831`) has the correct rule. Scope the DoD bullet to files
*outside* those four, and to any task other than task-146 that incidentally runs the
manager.

**M-8 (PRD) — front matter.** `:3` reads "**Planning Status:** draft" while this log's
Planner entry (`:10`) records `status: in_review`; the cv-1 sibling uses the
`draft → in_review → approved` vocabulary, so the two must agree. `:7` says
`research/cv-2-findings.md` is "not present at planning time; created during the
slice", but the file exists (794 lines, untracked) and describes itself as
"Durable findings from cv-2 planning (2026-07-28)"; the guide `:10-11` treats it as a
planning artifact.

---

### Dropped findings (raised by a lens, not promoted)

1. **"Trim the guide's duplication: delete the per-task locked-invariant blocks and
   cut the global list to the invariants cv-2 can violate."** Dropped — it contradicts
   the authoring mandate. `prompt.md:86` requires each task to "state the **locked
   invariants the change must not break** inline, not by reference only", and
   `prompt.md:93` carries all fourteen into *every* task. The measured repetition (17
   "Locked invariants" headings, "Byte-equality" 18×, "Sanitization floor" 15×) is the
   rule being followed, not a defect. Recorded as a note: the guide is long (5685
   lines for 15 tasks), and the only trim that costs nothing is the authoring-shard
   table of contents at `:18-20`/`:45-53`, which S-10 already labels non-binding — an
   optional tidy, not a blocker.
2. **"task-143 must ship the SIPO mainnet and canonical CIP-119 vectors."** Dropped as
   a requirement, kept as the preferred half of B-5. cv-2 renders no anchor at all, and
   anchor fetch plus Blake2b-256 verification arrive in anchor-1 (task-149 / task-150);
   the two remote vectors are unobtainable in this offline container. What survives is
   the *record*: the comment must not claim a provenance the bytes lack, and AC-4's
   partial record must name the provenance gap, not only the hash gap. The Cardano
   Academy credential is offline-available and is offered as the cheap improvement.
3. **"task-147's new floor case must not assert `'abstain'` as a required analytics
   argument."** Dropped as a standalone finding and folded into B-6's fix. Pinning the
   shipped three-argument vote-kind payload is legitimate; what was missing is
   traceability, which B-6 supplies by recording the deviation in the PRD and the
   findings note rather than by reshaping the assertion.

**Decision: requires_changes** — 2 blockers (B-1, B-2) and 6 major findings (B-3 …
B-8), plus 8 minors for the same fix pass. One fix pass addressing every entry above
across `cv-2-PRD.md`, `cv-2-implementation-guide.md` and `research/cv-2-findings.md`,
then re-run this critique's B-1 / B-2 / B-3 checks before build.

---

## Critiquer: 2026-07-28 — fix-pass verification over B-1 … M-8

**Method.** Every finding re-opened in the file it was raised against, in this worktree at
`504b44c1a`. No fixer claim taken on report. Twenty-two `path:line` anchors introduced or
touched by the fix pass were re-read against live source, and three measured claims were
re-executed (`Cardano.DRepID.isValid`, the four bech32 vectors, `stylelint` on
`CurrentVoteSummary.scss`).

**Per-finding outcome.**

- **B-1 (blocker, guide) — fixed.** `cv-2-implementation-guide.md:2069-2135` now extracts
  the JSX into `tree(currentStores)` and returns
  `rerenderWithWallets: (wallets) => { stores.wallets.all = wallets; rerender(tree(stores)); }`,
  with the `Provider` guard cited inline; Step 6(c) case 4 (`:2193-2213`) calls it. The
  store object is never replaced, so the mobx-react throw is out of reach. Verified:
  `VotingGovernancePage.spec.tsx:121` is `wallets: { all: wallets },`;
  `VotingGovernancePage.tsx:63` is `wallets={wallets.all}`;
  `node_modules/mobx-react/dist/mobxreact.cjs.development.js:481-497` is the `Provider`
  body whose `shallowEqual` mismatch throws `The set of provided stores has changed`;
  `renderFlow` is `:126-163` and the HW describe `:304-391` as the guide states.
- **B-2 (blocker, PRD + findings) — fixed.** PRD `:547-549` is now
  `resolveExactDRepMatch<AppDRepDirectoryEntry>(currentVote.drep.cip129 ?? currentVote.drep.raw, drepIndex)`;
  the false "canonicalizes CIP-105 → CIP-129" clause is replaced by the measured behaviour
  at `:558-568`; the data-flow line is corrected at `:1334`; `research/cv-2-findings.md:75-88`
  carries the same correction in F-1's Resolution. Re-measured here:
  `Cardano.DRepID.isValid('drep_vkh15xev…')` is `false`, `isValid('drep1…')` is `true`.
  `normalizeDRepIdentity.ts:40` / `:55` populate `cip129` on both branches and
  `api.ts:3009-3023` is `parseVoting`, returning `null` when normalization fails — both
  cited correctly. One residual anchor slip carried in from the finding text: see
  **Residual 1**.
- **B-3 (major, guide + PRD + findings) — fixed.** Guide `:5202-5219` adds
  `!!!Expiring in 12 epochs` and the `status.expiring` caption to task-147 Step 2's first
  case, and `:5222-5241` adds the CIP-105 lookup case asserted through `getByLabelText`.
  Verified the case can pass and can fail for the right reason:
  `VotingGovernancePage.spec.tsx:89` supplies `drepIndex = new Map([[VALID_DREP_ID, drepEntry]])`,
  `:64-70` gives `drepActivity: 12`, `CurrentVoteSummary.tsx:68` renders
  `<DRepIdDisplay drepId={currentVote.drep.raw} />` and `DRepIdDisplay.tsx:74` sets
  `aria-label={drepId}`, so a `raw`-keyed lookup yields the `status.unavailable` caption and
  both assertions fail. Copy checks out too: `en-US.json:964` is `!!!Delegated to DRep` and
  `:981` is `Submit` (no marker), so the quoted `getByText` / `getByRole` targets are real.
  PRD R-9 (`:1598-1608`) and findings F-1 Disposition/Owner (`:89-101`) are re-pointed to
  task-147.
- **B-4 (major, PRD) — fixed.** PRD S-4 `:988-998` now states that `submitButtonDisabled` is
  deleted from `:139-143` and re-declared after the `chosenOption` derivation, `formIsValid`
  (`:135-137`) staying put, and the task-140 contract row `:154` matches. Live
  `VotingPowerDelegation.tsx` confirms every anchor, including that `submitButtonDisabled` is
  read only at `:313`. Guide unchanged, as required.
- **B-5 (major, guide + PRD) — fixed, with the preferred improvement taken.** The mandated
  comment (guide `:437-439`) no longer claims CIP-119 provenance; the `drepVerified` vectors
  are re-derived from the Cardano Academy preprod key hash. Decoded here with the repo's
  `bech32`: `drep1ytnglv2y…` → `22e68fb144f40ed30764fba34ca21cdea2400b1b7f02cb27c04a515bdc`,
  `drep_vkh1u68mz…` → the same 28 bytes without the `0x22` header, matching guide Step 3's
  expected output byte-for-byte; the key hash is committed at
  `research/drep-state-preprod-epoch295-sample.json:2849` with the `Cardano Academy.jsonld`
  anchor at `:2852-2853`. The unverified pair is genuinely byte-identical to
  `storybook/stories/governance/CurrentVoteSummary.stories.tsx:17-21`. AC-4 is restated as
  satisfied-in-part on both halves in the guide (`:679-696`) and in PRD D-7 (`:598-615`).
  Omitting the `.agent/…` path from the source comment is correct — shipped source must not
  cite plan artifacts — and the path is recorded in the guide and the PRD instead. See
  **Residual 2** for the one place the old scoping survives.
- **B-6 (major, PRD) — fixed.** The carve-out is now stated where the invariant is stated:
  NFR/S-9 (`:1217-1225`), the sanitization bullet (`:1288-1301`) and "Locked Invariants
  Touched" (2) (`:1472-1487`), each carrying `VotingStore.ts:196-202`, `:399-403`, `:430-434`
  and `research/slice-3-findings.md:132-141`. All four anchors re-read and correct — the
  fixer's off-by-one correction to `:430-434` is right. task-147 AC-5 appears in the DoD
  exception table (`:1638`) and the deviation is recorded as findings F-14 (`:804-830`).
- **B-7 (major, PRD) — fixed.** The four-item list is now a thirteen-row table
  (`:1626-1640`) covering 136 AC-4, 138 AC-3, 139 AC-2, 139 AC-3, 142 AC-3, 143 AC-4,
  144 AC-2, 145 AC-1, 145 AC-4, 146 AC-3 (second half), 147 AC-5, 173 AC-2 and 140 AC-7
  (first conjunct), each with its reason. task-144 AC-2 is split in the PRD and in the
  guide's Acceptance entry (`:4353-4362`); the Storybook DoD bullet (`:1681-1687`) now OWEs
  the console-clean pass alongside the ja-JP overflow pass. Adding the two rows the finding
  did not enumerate is within the fix instruction ("every AC the guide actually scopes").
- **B-8 (major, PRD + guide) — fixed by the second option offered, correctly.**
  `designs/current-vote-display-design.md:97` does still offer the case-insensitive `cip129`
  alternative, and the planning seam contract
  (`tmp/cv-2-planning/cv-2-seam-contract.md` R-10) binds task-140 to *appending one sentence,
  no rewrite* — so deleting the clause was not available. The conjunct is recorded as
  satisfied-in-part in PRD D-4 (`:401-425`), the guide's task-140 AC-7 entry (`:3175-3184`)
  and the DoD table, and Step 7's appended sentence (`:3024`) explicitly retires the
  alternative citing AC-4. Not labelled satisfied anywhere. See **Residual 3** for a wording
  tension in Step 8's appended cv-1 log entry.
- **M-1 — fixed.** task-138 row now `deriveFormSeed(wallet, inheritedDRepId)`; task-136 row
  four descriptors with `sameVoteHint` marked task-140's (matching guide Step 1's four);
  task-137 row seven read sites, which is exactly what `grep -n "state.selectedWallet"`
  returns (`:136`, `:174`, `:239`, `:244`, `:260`, `:286`, `:333`); D-14 and the task-146 row
  three assertions.
- **M-2 — fixed, with one sub-item correctly declined.** Verified in live source:
  `governance.types.ts:20-31` is `DRepIdentity`; `DRepCategoryBadge.scss:34-41` is
  `.threshold` and both `_shared` paths are now fully qualified; `DRepCategoryBadge.tsx:4`
  and `:50-51` are right; `VotingGovernancePage.spec.tsx` is 391 lines in both docs;
  `Governance.stories.tsx:204` is `renderGovernancePanel`, `:411-415` is the
  `initialFormState` object inside the prefilled story, `:403/:420/:457/:492/:497` all match;
  the guide reads "eight sites (seven reads plus the `onChange` write)" and "Both tasks are
  Storybook-only". The declined item is **right to decline**: `VotingStore.ts:74` opens
  `parseApiCode` and `:95` is its closing `};` — `:74-95` is the whole declaration and
  `:74-94` would truncate it.
- **M-3 — fixed.** PRD D-8 (`:643-653`) and the guide's task-145 judgment call (`:4460-4464`)
  both drop `Unavailable while syncing` from the AC-3 migration and state the four reuse
  sites. Live `Governance.stories.tsx:497` renders `<VotingUnavailable …>` only.
- **M-4 — fixed and measured.** The `.expiringBadge` block (guide `:937-948`) is alphabetical;
  `node_modules/.bin/stylelint source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.scss`
  reports exactly **12** `order/properties-alphabetical-order` errors at HEAD, the number the
  guide records, and the single-file run with the "13+ means your block" diagnosis is in
  Step 6 (`:1349-1356`). The `.statusBadge` (`:24-31`), `.glyph` (`:33-35`) and `.caption`
  (`:47-52`) anchors the step cites are all correct.
- **M-5 — fixed.** Guide `:4655-4665` narrows the claim to the `noDelegation` default and
  explains current-vote precedence for the other four values.
- **M-6 — fixed.** The deferred-i18n-gate paragraph is in "Environment and Verification
  Commands" (`:188-200`), naming `prompt.md:196-199`, D-9, the descriptor-present /
  catalog-absent interim and the single discharge in task-146 Step 4.
- **M-7 — fixed.** The DoD i18n bullet (`:1656-1663`) keeps task-146's four files and binds
  the restore rule to every other file and task.
- **M-8 — fixed.** PRD `:3` is `in_review`; `:7` links `research/cv-2-findings.md` as written
  during planning, carrying F-1 … F-14 (the file is now 853 lines, up from the 794 this log
  recorded).

**Residuals (none blocking; fold into the next doc touch).**

1. `helpers.ts:143` is misattributed. Live, `:143` is
   `const { full } = normalizeDRepQuery(rawQuery);` and `:144` is
   `if (!Cardano.DRepID.isValid(full)) return null;`. The finding text carried the slip and
   the fix pass propagated it verbatim to `cv-2-PRD.md:560`, `research/cv-2-findings.md:77`
   and `cv-2-implementation-guide.md:2467` / `:5320`. The guide gets it right once, at
   `:2388` (`helpers.ts:143-144`). Quoted content and prescription are unaffected; only the
   number is wrong.
2. `cv-2-PRD.md:267-269` still reads "AC-4 is satisfied-in-part by D-7: provenance **is**
   named in the fixture module; the 'verified hash' half has no mechanism in cv-2" — the
   pre-fix scoping B-5 asked to retire. D-7 (`:598-615`), the guide's AC-4 record and the DoD
   row now all say satisfied-in-part on *both* halves, so the acceptance-criteria section is
   the one place left contradicting them.
3. The cv-1 log entry the guide dictates in task-140 Step 8 (`:3055-3057`) says "The
   acceptable keys are the pair, or an explicitly case-insensitive `cip129` comparison". As a
   2026-07-28 discharge entry that re-endorses the alternative Step 7 retires and AC-4 bans.
   The following "What shipped" paragraph is correct; the sentence needs to read as a
   description of the cv-1 note, not a current endorsement.

**Both questions answered.** *Small-model-implementable end to end:* yes for all fifteen
rows. The two mechanical traps the critique found are closed — the `Provider` store-identity
throw (B-1) and the `raw`-keyed lookup that would have shipped green (B-2/B-3) — and every
step now carries its own verify block, expected counts and failure diagnosis. *Remaining
contradictions with live code or the tracker:* none that change an instruction. Residual 1 is
a wrong line number under a correct quotation; residual 2 is a PRD self-contradiction on a
disposition already recorded correctly in three other places; residual 3 is wording in a
dictated doc entry. Nothing in either document now prescribes an action the live code rejects.

**Environment deviations, unchanged.** `nix` absent, so `nix fmt` stays the user's pre-merge
obligation and `node_modules/.bin/prettier --write` on explicitly named new files is the
substitute; `gh` and push credentials absent, work stays local; `prettier --check` and
`yarn check:all` red at HEAD for pre-existing reasons. This fix pass touched only `.md`
files, so no formatting run was owed.

**Decision: approved** — all 8 blockers/majors (B-1 … B-8) discharged, 7 of 8 minors applied
and the eighth correctly declined on verified evidence. Three minor residuals recorded above
for the next doc touch; none gates the build phase.

---

## Planner: 2026-07-28 — `helpers.ts` anchor correction (residual 1)

**Discharges** residual 1 at `:703-709`.

**The slip.** The Critiquer entry above anchors the form gate at `helpers.ts:143`
(`:322`, `:335`). Live, `:143` is `const { full } = normalizeDRepQuery(rawQuery);` and
`:144` is the `if (!Cardano.DRepID.isValid(full)) return null;` gate the entry meant. The
finding's substance and its prescription are unaffected; only the number is wrong.

**What shipped.** Commit `b8a14e708` corrected the anchor to `helpers.ts:144` in
`cv-2-PRD.md`, `research/cv-2-findings.md` and `cv-2-implementation-guide.md` (both
sites). This file is append-only, so `:322` and `:335` stand as written; `:703-708` quote
`:143` deliberately — the verifier describing the misattribution — and are correct as they
stand.

Decision: anchor correction recorded.

---

## Code Review: task-143 — iteration 1 (2026-07-28)

**Scope reviewed.** The uncommitted working tree against the guide section
"Storybook current-vote fixtures (task-143)"
(`cv-2-implementation-guide.md:241-700` — the files-created / files-touched list
at `:245-251`, the Context block at `:253-355`, the seven locked invariants at
`:357-378`, the six resolved judgment calls at `:380-398`, Step 1 at `:400-404`,
Step 2's verbatim `ts` block at `:406-575`, Step 3's bech32 provenance check and
its expected output at `:577-640`, Step 4 at `:642-655`, Step 5's structural grep
at `:657-665`, and the four-item acceptance checklist at `:667-700`), plus
task-143's four acceptance criteria in `governance-drep-discovery-plan-tasks.json`
and the per-task Definition of Done at `cv-2-PRD.md:1619-1623`. HEAD is
`427b9a487`; one review round; the main checkout `/workspaces/daedalus` was never
read, edited or run against.

**What landed.** `git status --porcelain -uall` → exactly one line,
`?? storybook/stories/governance/_utils/fixtures.ts`. Zero modified files, zero
staged files, so no pre-existing file was edited or reformatted and none of the
three files the guide forbids touching (`CurrentVoteSummary.stories.tsx`,
`Governance.stories.tsx`, `storybook/stories/index.ts`) is in the change set. The
new module is 169 lines and byte-identical to the guide's Step 2 block (extracted
and diffed: identical, 169 lines on both sides). The tasks tracker is **not** in
the change set.

**Review method (three lenses, adversarial refutation).** Three independent
lenses ran over the diff: (1) guide and acceptance-criteria conformance, with the
Step 3 and Step 5 proofs re-executed rather than trusted; (2) locked invariants
and the sanitization floor; (3) tests, simplicity and drift. Every candidate was
re-opened against the live worktree on the reproduce / guide-authority / scope
axes before promotion. **Two candidates were raised across the three lenses; one
survived, ranked below, and one was dropped.** Per-lens decision: conformance —
`requires_changes` on the tracker row alone, code clean; invariants and floor —
clean; tests, simplicity and drift — clean.

**What came back clean, re-verified here.** The four bech32 constants decode
exactly as guide `:626-633` predicts — both CIP-129 forms carry the `0x22`
key-hash header and each CIP-105 partner decodes to its `credentialHex` — and
`Cardano.DRepID.toCip129DRepID` returns both `drep1…` map keys unchanged, so the
`makeDRepIndex` key form (`fixtures.ts:149`, `:159`) is the one
`resolveExactDRepMatch` looks up (`helpers.ts:139-153`). The unverified trio at
`fixtures.ts:43-48` matches the committed story vector at
`CurrentVoteSummary.stories.tsx:17-21` byte-for-byte, and the verified credential
is the Cardano Academy preprod key hash committed at
`research/drep-state-preprod-epoch295-sample.json:2849`. Purity holds
structurally: one `new Wallet(` at `:95`, six `export` lines at `:14`, `:21`,
`:29`, `:66`, `:108`, `:143`, no `export default`, no `let`, no `.push(`, no
`GOVERNANCE_WALLETS` (AC-1, AC-2). `currentVoteOptions` (`:21-27`) enumerates
exactly the five ids AC-3 names. Invariants hold: `noDelegation` returns `null`
with no fallback DRep (`:78-80`); `abstain` / `no_confidence` return sentinels and
an empty index Map (`:143-169`); both index entries are `status: 'active'`, never
the renderer-derived `'expiring'`; both ship `anchor: null`, the correct reading
of D-7; no logger, analytics or electron-store sink exists in the module, so the
sanitization floor cannot be moved. `WalletProps` accepts every key `buildWallet`
passes, `delegatedStakePoolId` included (`Wallet.ts:113-134`). One comment,
`:33-35`, three plain sentence-case lines stating provenance and the lower-case
map-key invariant — no task id, no process label, no ALL-CAPS, no change history.
The module contains no JSX, so no local `IntlProvider` and no per-locale variant
question arises.

### Blockers (ranked, most severe first)

**CR-1 (major, tracker) — task-143's row is still `pending`, so AC-4's
satisfied-in-part discharge is nowhere recorded.** The guide's own acceptance
record for AC-4 (`cv-2-implementation-guide.md:679-700`) ends: "Record **both**
shortfalls in the task's tracker `statusReason`; do not claim the criterion whole
and do not scope the shortfall to the hash half alone." The per-task Definition
of Done (`cv-2-PRD.md:1619-1623`) requires "tasks JSON synchronized (`status`,
`statusReason`, `evidence`, `updatedAt`) · exactly one commit" — one commit per
task, so the tracker edit has to be in the working tree before task-143 is
committed, and no later cv-2 row owns it (task-148 is the `same_vote` store
regression, not a closeout). Live, the task-143 object still reads
`"status": "pending"` with no `statusReason`, no `evidence` and no `updatedAt`,
and the tracker is absent from `git status --porcelain -uall`. The convention is
per-task and rides the task's own commit: `2baed760c` ("feat(gov): task-131 add
currentVote and isVoting to the Wallet domain model") and every governance commit
back to `f948845a5` carry a `governance-drep-discovery-plan-tasks.json` hunk in
the same commit. The shape to follow is task-133's row, which inserts
`statusReason`, `evidence`, `updatedAt` between `status` and `priority`, with
`evidence` an array of repo-relative paths, source files first then plan docs,
and `updatedAt` as `2026-07-28`.
*Fix:* set the row to `complete`, add `evidence` beginning with
`storybook/stories/governance/_utils/fixtures.ts`, and write a `statusReason` that
records both AC-4 shortfalls explicitly — (a) only the `drepVerified` pair carries
a plan-named provenance (the Cardano Academy preprod key hash at
`research/drep-state-preprod-epoch295-sample.json:2849`), while `drepUnverified`
is the repo's own committed story vector from
`CurrentVoteSummary.stories.tsx:17-21` and is neither the SIPO mainnet nor the
canonical CIP-119 example; and (b) "verified hash" has no mechanism in cv-2 — no
anchor fetch, no Blake2b-256 path, both index entries ship `anchor: null`. Neither
pair may be described as a "CIP-119 test vector" (D-7, `cv-2-PRD.md:611-616`). Do
not run prettier on the tracker — it is tool-managed JSON.

### Dropped findings (raised by a lens, not promoted)

1. *Dropped as re-litigation — "the knob labels `DRep — verified anchor` /
   `DRep — unverified anchor` (`fixtures.ts:23-24`) name an anchor-verification
   distinction cv-2 cannot render."* The observation is factually true, and the
   lens that noticed it raised it only to decline it. D-7
   (`cv-2-PRD.md:573-625`) resolves exactly this: all five ids ship, the two DRep
   options are differentiated by the lifecycle state cv-2 *can* render
   (`drepActivity: 30` vs `4`), and renaming would force a knob-id churn in
   anchor-2. The labels are the guide's Step 2 verbatim text, they are
   Storybook-knob-only and never shipped copy, and the module's comment states
   nothing stronger than the true provenance. Re-opening a resolved judgment call
   is out of scope for a code review of this diff.

**Notes, not defects.** The file is still untracked, so the commit must
`git add storybook/stories/governance/_utils/fixtures.ts` explicitly — a bare
`git commit -a` misses it. No Jest spec is added, correctly: `jest.config.js:129`
sets `roots: ['<rootDir>/tests', '<rootDir>/source']`, so a spec under
`storybook/` would never execute; acceptance rests on the Step 3 and Step 5 proofs
and on task-145's visual pass. `nix fmt` could not be run (nix is absent from this
container); `node_modules/.bin/prettier --check` on the single newly created path
is the recorded substitute, and the `nix fmt` pass stays an owed pre-merge
obligation.

**Verification gate.** Green, with nothing attributable to this task or to HEAD.
Change set exactly the one new file; scss codegen correctly skipped (no `.scss` in
the diff); `node_modules/.bin/tsc --noEmit` exit 0, and `--listFiles` confirms the
new module is really in the program rather than silently excluded; `yarn lint`
exit 0 at exactly the 5591-warning baseline, with zero warnings mentioning
`fixtures.ts`; `node_modules/.bin/jest --testPathPattern='(governance|voting)'
--no-coverage --runInBand` exit 0 — 17 passed / 1 skipped suites, 269 passed / 12
skipped tests, 6 snapshots — identical to the wave baseline, the one skipped suite
being the environment-gated `GovernanceCliArgvSmoke.spec.ts`. `prettier --check`
clean on the newly created file; the four prettier-red-at-HEAD files were neither
checked nor touched. i18n was not applicable and `yarn i18n:manage` was correctly
never invoked, so no catalog needed restoring.

**Decision: requires_changes** — one major finding, CR-1. The code deliverable is
accepted as delivered; the tracker row must be synchronized before task-143 is
committed.

---

## Code Review: task-143 — round 2 (2026-07-28)

**Scope reviewed.** The uncommitted working tree after the CR-1 fix pass.
`git status --porcelain -uall` now reads three lines — `?? storybook/stories/
governance/_utils/fixtures.ts`, ` M governance-drep-discovery-plan-tasks.json`
and ` M cv-2-code-review.md` — the code deliverable plus the two doc artifacts
CR-1 asked for, and nothing else. HEAD is unchanged at `427b9a487`. Three
independent lenses ran again (guide and acceptance conformance; locked
invariants and the sanitization floor; tests, simplicity and drift); every
candidate was re-opened against the live files here before promotion.

**CR-1 is discharged.** The task-143 row is `complete`, with `statusReason`,
`evidence` (six repo-relative paths, source file first) and
`updatedAt: "2026-07-28"` inserted between `status` and `priority` — task-133's
shape. AC-4 is recorded satisfied-in-part on **both** halves and names each
shortfall: only the `drepVerified` pair carries a plan-named provenance, and
"verified hash" has no mechanism in cv-2 (both entries ship `anchor: null`),
which is what the guide's AC-4 record at `cv-2-implementation-guide.md:679-696`
demands. The tracker still parses as JSON and was not run through prettier; the
log append is 135 additions / 0 deletions, genuinely append-only.

**Code deliverable: clean on all three lenses. Not one line of `fixtures.ts`
changed this round.** Re-measured, not trusted: the module is byte-identical to
the guide's Step 2 fenced block (fences `:407` / `:577`, content `:408-576`) —
extracted and diffed, 169 lines on both sides. Step 3's decode reproduces the
guide's expected output at `:617-620` line for line, both CIP-129 forms carry
the `0x22` key-hash header, each CIP-105 partner decodes to its `credentialHex`,
and `Cardano.DRepID.toCip129DRepID` returns both `drep1…` strings unchanged, so
the `makeDRepIndex` keys (`fixtures.ts:149`, `:159`) are the exact form
`resolveExactDRepMatch` looks up (`drep-directory/helpers.ts:139-153`). Step 5's
grep still yields one `new Wallet(` (`:95`), six `export` lines, no `let`, no
`.push(`, no `GOVERNANCE_WALLETS`, no `export default`. `WalletProps`
(`Wallet.ts:113-134`) accepts every key `buildWallet` passes, `votingTarget`
included (`:130`). `noDelegation` returns `null` with no fallback DRep
(`:78-80`); abstain and no-confidence return form-only sentinels with an empty
Map; both index entries are `status: 'active'` with `anchor: null` (`:152-154`,
`:162-164`) and the renderer-derived `'expiring'` appears nowhere. A grep for
`logger|console.|analytics|electron-store|localStorage|ipc` over the module
exits 1 — no sink exists, so the sanitization floor cannot be moved from here.
The lower-case map-key comment at `:33-35` is accurate rather than aspirational:
`normalizeDRepQuery` lower-cases at `helpers.ts:29`.

### Minor (non-blocking; absorb before the task-143 commit)

**CR2-1 (minor, `cv-2-code-review.md`) — the iteration-1 "Scope reviewed"
citation list mis-anchors twelve guide ranges.** The entry at `:761-892` is
still uncommitted, so this is a pre-commit correction, not a rewrite of a
committed entry. Measured heading map of the task-143 section, verified line by
line: `:241` `## Storybook current-vote fixtures (task-143)`; `:245-251` files
created / files touched; `:253` `#### Context`, running to `:353`; `:354`
`#### Locked invariants…`, bullets `:356-376`; `:378` `#### Resolved judgment
calls`, bullets `:380-394`; `:396` `#### Step 1` with its fence `:398-400`;
`:402` `#### Step 2`, fence `:407` / `:577`; `:581` `#### Step 3`, ending `:638`;
`:639` `#### Step 4`, ending `:650`; `:652` `#### Step 5`, its grep fence
`:654-656`, ending `:665`; `:667` `#### Acceptance`, ending `:696`; `:700` is
already `## Group 1 — task-136`. Against that map the appended ranges are:
`:241-700` → `:241-696`; Context `:253-355` → `:253-353` (`:354` is the next
heading); invariants `:357-378` → `:354-377` (`:357` starts mid-first-bullet,
`:378` is the next heading); judgment calls `:380-398` → `:378-395` (`:398` is
Step 1's bash fence); Step 1 `:400-404` → `:396-400` (`:400` is Step 1's closing
fence, `:402-404` is Step 2's prose); Step 2 block `:406-575` → `:407-577`, or
`:408-576` for content (`:406` is blank, `:575` is `  return index;`, two lines
short of the block's end); Step 3 `:577-640` → `:581-638`; Step 4 `:642-655` →
`:639-650` (`:642` is inside Step 4's bash block, `:655` is inside Step 5's);
Step 5 `:657-665` → `:652-665` (the cited range starts *after* the grep it
names); acceptance `:667-700` → `:667-696`. Two further slips outside that list:
`:796` reads "decode exactly as guide `:626-633` predicts", but the sentence it
paraphrases ("Both CIP-129 forms carry header byte `0x22` (key-hash) …") is
`:623-625` — `:629-635` is the optional `@cardano-sdk/core` check; and CR-1 at
`:824` cites the AC-4 record as `:679-700`, which should be `:679-696`. Only
`:245-251` is exact. No instruction or quotation changes; the substance of the
entry survives untouched. *Fix:* correct the ranges in place before the commit.
Markdown only, no prettier run is owed.

**CR2-2 (minor, `governance-drep-discovery-plan-tasks.json`) — two mis-anchored
guide ranges in the new task-143 `statusReason`.** It reads "byte-identical to
the guide's Step 2 verbatim block at `:406-575`" — the fences are `:407` and
`:577`, content `:408-576`, so `:406` is the blank line before the fence and
`:575` is `  return index;`, mid-`makeDRepIndex`. The byte-identity claim itself
holds (extracted and diffed here: identical, 169 lines both sides); only the
range is wrong. It also reads "`makeDRepIndex` ships here … by the guide's
resolved judgment call at `:382-384`", but that bullet is `:380-382`; `:383-384`
is the unrelated knob-label bullet ("Knob **label** is exactly `Current vote
(mock)`; the default is `'noDelegation'`"). Every other citation in the same
`statusReason` was checked here and is exact — `:245-251`, `:617-620`,
`jest.config.js:129`, `drep-directory/helpers.ts:139-153`, `cv-2-PRD.md:1619-1623`,
`governance-drep-discovery-plan.md:103`, `designs/current-vote-display-design.md:227`,
`research/drep-state-preprod-epoch295-sample.json:2849`,
`CurrentVoteSummary.stories.tsx:17-21`, and the `fixtures.ts` anchors `:14`,
`:21-27`, `:29-30`, `:33-35`, `:36-41`, `:43-48`, `:66`, `:78-80`, `:95`,
`:143`, `:149`, `:153-154`, `:159`, `:163-164` — so these two are outliers, not
a house convention. Optional tightening in the same pass: the D-7 citation
`cv-2-PRD.md:573-625` points one line before the heading and stops three lines
short; D-7 is `:574-628`. *Fix:* hand-edit the two ranges. The tracker is
tool-managed JSON — do not run prettier on it.

### Merged and dropped

1. *Merged.* Two lenses raised the same two tracker anchors independently
   (`:406-575` and `:382-384`), one as a pair of one-token findings and one as a
   single finding. They are one edit to one field and are consolidated as CR2-2,
   keeping both lenses' measurements.
2. *Nothing dropped as unfounded.* Every promoted range was re-opened here and
   every one failed as reported; nothing survived that a lens had merely
   asserted. The invariants-and-floor lens raised no finding at all, and its
   clean result was re-derived rather than accepted (sink grep exit 1, case /
   normalize grep exit 1, both `status: 'active'`, `DRepStatus` at
   `source/common/types/governance.types.ts:34`).
3. *Not promoted, recorded instead.* Neither survivor is a defect in
   `fixtures.ts`; the code deliverable needs no edit this round, and re-opening
   the knob-label question (dropped in iteration 1 as re-litigation of D-7)
   remains out of scope.

**Verification gate.** Green, with nothing attributable to this task or to HEAD.
Change set exactly the three artifacts above; scss codegen correctly skipped (no
`.scss` in the diff); `tsc --noEmit` exit 0 with `--listFiles` confirming the new
module is in the program; `yarn lint` exit 0 at exactly the 5591-warning
baseline, zero warnings naming `fixtures.ts`; `jest --testPathPattern='(governance|voting)'
--no-coverage --runInBand` exit 0, 17 passed / 1 skipped suites and 269 passed /
12 skipped tests, identical to the wave baseline, the one skip being the
environment-gated `GovernanceCliArgvSmoke.spec.ts`; `prettier --check` clean on
the single newly created path. The four files that are prettier-red at HEAD
(`VotingPowerDelegation.tsx`, `VotingPowerDelegationConfirmationDialog.tsx`,
`VotingGovernancePage.tsx`, `Governance.stories.tsx`) and the red
`yarn check:all` are pre-existing at the slice baseline, were neither checked nor
touched, and are notes rather than findings. `nix` is absent, so `nix fmt` stays
an owed pre-merge obligation with `prettier --check` on the explicit new path as
the recorded substitute. i18n was not applicable and `yarn i18n:manage` was
never invoked, so no catalog needed restoring.

**Decision: approved** — both survivors are minor citation-accuracy defects in
the two doc artifacts; neither changes an instruction and neither touches
`storybook/stories/governance/_utils/fixtures.ts`. A fix pass should absorb
CR2-1 and CR2-2 before the single task-143 commit, which must
`git add storybook/stories/governance/_utils/fixtures.ts` explicitly (the path is
untracked, so `git commit -a` would miss it) under the subject
`feat(gov): task-143 add governance storybook current-vote fixtures`.

---

## Code Review: 2026-07-28 — task-136 round 1

**Scope reviewed.** The uncommitted working tree against the guide section
"Group 1 — task-136: live DRep status badge on `CurrentVoteSummary`"
(`cv-2-implementation-guide.md:700-1434` — files-touched and must-not-touch at
`:712-727`, Context at `:729-816`, locked invariants at `:817-848`, resolved
judgment calls at `:850-883`, Steps 1-5 at `:885-1321`, Step 6's verification
block at `:1322-1392`, the commit subject at `:1391`, and the acceptance record
at `:1394-1434`), plus task-136's four acceptance criteria in
`governance-drep-discovery-plan-tasks.json` and the per-task Definition of Done
at `cv-2-PRD.md:1619-1623`. HEAD is `0fc92fcab` (task-143), one commit ahead of
the wave baseline `427b9a487`, which is what the canonical build order predicts.
The main checkout `/workspaces/daedalus` was never read, edited or run against.

**What landed.** `git status --porcelain -uall` → exactly the six paths Step 6
predicts and nothing else: `CurrentVoteSummary.messages.ts`,
`CurrentVoteSummary.scss`, `CurrentVoteSummary.spec.tsx`,
`CurrentVoteSummary.tsx`, `__snapshots__/CurrentVoteSummary.spec.tsx.snap` and
`storybook/stories/governance/CurrentVoteSummary.stories.tsx` — 486 insertions,
46 deletions. **The tasks tracker is not in the change set**, which is the one
survivor ranked below.

**Review method (three lenses, adversarial refutation).** Three independent
lenses ran over the diff: (1) guide and acceptance-criteria conformance; (2)
locked invariants and the sanitization floor; (3) tests, simplicity and drift.
All three returned `approved`; one raised a single minor. Every candidate was
re-opened against the live worktree here before promotion, and the consolidation
also re-applied the per-task Definition of Done, which no lens was scoped to
check. **Two findings are promoted, one raised by a lens and one derived from
the DoD; two lens/gate observations were dropped as misreads or notes.**

**What came back clean, re-verified here.** Steps 1-5 are the prescribed code:
the four descriptors at `CurrentVoteSummary.messages.ts:73-97` carry the exact
ids, the ICU argument named `n`, and the leading `!!!` on every
`defaultMessage`; the `.expiringBadge` block at `CurrentVoteSummary.scss:33-44`
is alphabetical as written; `CurrentVoteSummary.tsx` adds
`EXPIRING_MAX_REMAINING_EPOCHS = 12` (`:19`), the unexported module-scope
`deriveCurrentVoteBadgeState` (`:26-38`) and the badge/caption render
(`:91-120`); the story (`CurrentVoteSummary.stories.tsx:8-9`, `:17-51`,
`:63-80`) deletes the inline vectors and consumes task-143's
`useCurrentVoteKnob` / `resolveCurrentVote`. Invariants hold: the only
`GovernanceStore` reference is the `import type` at `:9` — no `@inject`, no
`observer`, no `drepIndex` read — status enters solely through the `drepEntry`
prop (`:15`); the shared badge receives only `drepEntry.status` (`:92`) so the
`active | inactive` union is not widened; a null entry yields the neutral
caption and never an Active fallback; the id row still renders
`currentVote.drep.raw` verbatim (`:106`); the sentinel branch carries no badge;
and no logger, analytics, electron-store, IPC or network sink appears in any
added line. The "active + `drepActivity` 0 renders Expiring in 0 epochs" hazard
was chased and is unreachable: `source/main/governance/GovernanceQueryService.ts:507-511`
sets `inactive` whenever `expiry <= currentEpoch` and derives
`drepActivity = Math.max(0, expiry - currentEpoch)`, so `active` implies `>= 1`.
The snapshot file is regenerated, not hand-edited — 7 `exports[` keys for
exactly the 7 snapshot-taking tests, the old "no badge" key pruned, no orphan.
Exactly one comment was added across the production diff, the three-line one the
guide sanctions at `CurrentVoteSummary.tsx:23-25`; no added line in source or in
a test name cites a task id, a `CAT-`/`CP-` label, a plan name or ALL-CAPS. The
story adds no `IntlProvider` and no per-locale variant, and keeps the
`storiesOf` and `.add` ids unchanged.

### Blockers (ranked, most severe first)

**CR3-1 (major, `governance-drep-discovery-plan-tasks.json`) — task-136's row is
still `pending`, so AC-4's satisfied-in-part discharge and AC-1's prop-chain
scoping are nowhere recorded.** Live, the row reads `"status": "pending"` with
no `statusReason`, no `evidence` and no `updatedAt`, and the tracker is absent
from `git status --porcelain -uall`. The per-task Definition of Done
(`cv-2-PRD.md:1619-1623`) requires "tasks JSON synchronized (`status`,
`statusReason`, `evidence`, `updatedAt` as `YYYY-MM-DD`) · exactly one commit",
so the tracker edit has to be in the working tree before task-136 is committed —
no later cv-2 row owns it. The convention is per-task and rides the task's own
commit: `0fc92fcab` (task-143), `d3729994a` (task-135) and `fb4f07f6c`
(task-170) each carry a `governance-drep-discovery-plan-tasks.json` hunk in the
same commit, and this is the identical defect promoted as CR-1 for task-143 one
round ago. The guide's own AC-4 record (`cv-2-implementation-guide.md:1417-1426`)
names what must be written: the knob itself is delivered through
`useCurrentVoteKnob()`, but (a) there is no browser in this container, so
"renders without console errors" cannot be observed here — OWED, never asserted
green; and (b) until task-146 seeds both catalogs the four new ids legitimately
log `[React Intl] Missing message`, so the console-clean observation can only be
made after task-146 lands. AC-1's record (`:1396-1405`) is equally explicit that
the "sourced from `drepIndex`" half is satisfied **through the prop chain** —
task-139 resolves the entry — and the row must say so rather than claim the
criterion whole.
*Fix:* set the row to `complete` and insert `statusReason`, `evidence`,
`updatedAt: "2026-07-28"` between `status` and `priority` (task-133's and
task-143's shape), with `evidence` an array of repo-relative paths, source files
first then plan docs, beginning with
`source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx`.
Record AC-2 and AC-3 satisfied, AC-1 satisfied for the render half and
satisfied-through-the-prop-chain for the source half, and AC-4 satisfied in part
with both OWED clauses named. The tracker is tool-managed JSON — do not run
prettier on it.

### Minor (non-blocking; absorb before the task-136 commit)

**CR3-2 (minor, `CurrentVoteSummary.tsx:40-42`) — the retained comment's first
clause stopped being exhaustive the moment the shared badge landed.** It reads
"Status labels render through the local message set because DRepSourceLabel's
variant union cannot express them; DRepSourceLabel is reused only for the
on-chain source label on the DRep state." That was true at HEAD, where every
`status*` label lived in `CurrentVoteSummary.messages.ts`. This diff adds
`<DRepStatusBadge status={drepEntry.status} />` at `:92`, whose labels come from
the shared badge's own descriptors — `governance.drepDirectory.status.active` /
`.inactive` at `DRepStatusBadge.tsx:8-17`, which the guide records as already
shipping in both catalogs (`cv-2-implementation-guide.md:790-792`), so this task
mints no key for them. A reader auditing key ownership from the comment would
look for Active/Inactive in the local message file and not find them.
*Fix, if taken:* narrow the subject rather than add a second sentence of "what",
e.g. "The vote-kind chip and the status captions render through the local
message set because DRepSourceLabel's variant union cannot express them." Guide
Step 3c pins the comment's **position** ("leave that comment exactly where it
is", `:990`), not its wording, so the edit must stay in place and change
nothing else; declining it as guide-conformant is also defensible, in which case
it should be absorbed at the next edit of this file.

### Merged and dropped

1. *Merged.* Only one finding was raised across the three lenses (the stale
   comment at `:40-42`, from the tests/simplicity/drift lens). It is promoted as
   CR3-2 with the shared badge's descriptor ids re-opened here and the guide's
   Step 3c wording checked, so the "leave it alone" reading is recorded next to
   the fix rather than left implicit. Nothing else needed merging — the
   conformance lens and the invariants/floor lens both returned clean, and both
   clean results were re-derived here (sink grep over the added lines exits 1;
   the only `GovernanceStore` reference is the `import type`; `_shared/` and
   `governance.types.ts` are absent from the diff).
2. *Dropped as a misread — "the guide's Step 6 note `3 suites / 25 tests green
   at HEAD` is stale arithmetic; the real number is 32."* Raised by the
   conformance lens and echoed by the verification gate as a discrepancy worth
   recording. Measured here: the parenthetical is explicitly scoped **at HEAD**,
   and at HEAD it is exactly right. The pattern runs 32 tests now
   (re-run here: 3 suites / 32 tests / 7 snapshots, exit 0), and
   `CurrentVoteSummary.spec.tsx` went from 4 `it(` blocks at HEAD to 11, so the
   HEAD total is `32 - 11 + 4 = 25`. Nothing in the guide is wrong; the number
   simply is not a post-change expectation. No doc edit is owed.
3. *Not promoted, recorded instead.* The guide's parenthetical "one rewritten
   name is pruned, three are added" describes the **net** key delta correctly
   (4 keys at HEAD → 7 now, `+3`); the literal breakdown is 1 pruned and 4
   written, because the rewritten test name takes a snapshot under its new name.
   The end-state figure the verifier actually gates on (7) is right. A one-token
   doc nit with no reader action, not worth an edit to a committed guide.
4. *Not promoted — AC-4's console-clean and ja-JP clauses, and AC-1's "sourced
   from `drepIndex`" half.* Both are pre-recorded by the guide as not
   dischargeable at this commit (`:1396-1405`, `:1417-1426`); demanding either
   as a code defect would be demanding work the guide deliberately excluded and
   task-139 / task-146 own. They convert into the CR3-1 `statusReason`
   obligation instead.
5. *No findings note is owed.* task-143 needed F-15 because its AC-4 partiality
   was not dispositioned anywhere; task-136's is written out in the guide's own
   acceptance record, and the `nix` absence is already F-12.

**Verification gate.** Green on every prescribed gate, with the one red proven
pre-existing. `tsc --noEmit` exit 0 (run both bare and with the 316 regenerated
`*.scss.d.ts` present, so `styles.expiringBadge` type-checks under strict
per-file scss typing too; all 316 are gitignored at `.gitignore:141`, none
existed beforehand, and all were deleted afterwards). `yarn lint` exit 0 at
exactly the 5591-warning baseline, zero errors.
`jest --testPathPattern=CurrentVoteSummary --no-coverage --runInBand` → 1 suite,
11 tests, 7 snapshots, no obsolete snapshots; the neighbour sweep
`--testPathPattern="voting-governance|VotingGovernancePage"` → 3 suites / 32
tests / 7 snapshots (reproduced during this consolidation); the wave sweep
`--testPathPattern="(governance|voting)"` → 17 passed / 1 skipped suites and 276
passed / 12 skipped tests, `+7` tests and `+3` snapshots against the baseline,
both fully accounted for by this suite going 4 → 11 and 4 → 7, with the skip
count unmoved at 12. `prettier --check` clean on the five source paths, with no
`--write` run anywhere, so none of the four HEAD-red files was rewritten.
`yarn i18n:manage` exit 0 and idempotent, adding exactly the four
`voting.governance.currentVote.status.*` ids and nothing else; task-136 is not
task-146, so all four written files were reverted with `git restore` and
re-checksummed byte-identical. **Attribution of the one red:** `stylelint
CurrentVoteSummary.scss` exits 2 with 12 `order/properties-alphabetical-order`
errors, and the same 12 rule messages in the same order exit 2 against the HEAD
copy of the file; the six errors above the insertion point are unmoved and the
six below shift by exactly the 13 inserted lines. The count did not move, which
is the invariant Step 6 states, and zero errors fall inside `:33-44`. It is
pre-existing at HEAD, not attributable to this task. `nix` is absent, so
`nix fmt` stays an owed pre-merge obligation with explicit-path `prettier` as
the recorded substitute (F-12).

**Decision: requires_changes** — the code deliverable is clean on all three
lenses and needs no edit; the single major is the unsynchronized tracker row,
which the Definition of Done requires in the working tree before the one
task-136 commit. A fix pass should write CR3-1, optionally absorb CR3-2, and
then commit all six code paths plus the tracker, this appended log entry and
nothing else under the subject
`feat(gov): task-136 render the live DRep status badge in the current-vote summary`.

---

## Code Review: 2026-07-28 — task-136 round 2

**Scope reviewed.** The uncommitted working tree after the round-1 fix pass,
against the same guide section "Group 1 — task-136: live DRep status badge on
`CurrentVoteSummary`" (`cv-2-implementation-guide.md:700-1434`, acceptance record
at `:1394-1434`), task-136's four acceptance criteria in
`governance-drep-discovery-plan-tasks.json`, the per-task Definition of Done
(`cv-2-PRD.md:1619-1623`) and the slice-level disposition of task-136 AC-4
(`cv-2-PRD.md:1632`). The round's specific question is whether the two round-1
findings — CR3-1 (major, unsynchronized tracker row) and CR3-2 (minor, stale
comment subject) — are discharged without collateral drift. HEAD is unchanged at
`0fc92fcab` (task-143); the main checkout `/workspaces/daedalus` was never read,
edited or run against.

**What landed.** `git status --porcelain` → eight paths: the six code paths from
round 1, unchanged in count, plus the two the fix pass added —
`governance-drep-discovery-plan-tasks.json` (`+12/-1`) and this log
(`+190`, the round-1 entry). The six code paths are still
`CurrentVoteSummary.messages.ts` (`+25`), `.scss` (`+13`), `.spec.tsx` (`+88/-6`),
`.tsx` (`+55/-4`), `__snapshots__/CurrentVoteSummary.spec.tsx.snap` (`+268/-1`)
and `storybook/stories/governance/CurrentVoteSummary.stories.tsx` (`+40/-38`).
Nothing outside the eight, and no untracked file.

**Review method (three lenses, adversarial refutation).** The same three lenses
re-ran over the post-fix tree: (1) guide and acceptance-criteria conformance;
(2) locked invariants and the sanitization floor; (3) tests, simplicity and
drift. **All three returned `approved` with zero blockers.** One lens raised and
self-dropped a re-open of CR3-2; that candidate was re-adjudicated here rather
than accepted on the lens's word. Every material claim below was re-derived in
this worktree.

**CR3-1 (major) — discharged.** The tracker row now reads `"status": "complete"`
with `statusReason`, `evidence` and `updatedAt` inserted between `status` and
`priority`, which is exactly the committed shape: the key order
`id,title,description,status,statusReason,evidence,updatedAt,priority,estimatedHours,dependencies,targetPath,acceptanceCriteria`
is byte-identical to the `task-143` and `task-135` rows, `updatedAt` is
`"2026-07-28"`, and the file still parses as JSON. `evidence` is seven
repo-relative paths, source first and beginning with `CurrentVoteSummary.tsx`,
plan docs last. The two scoping obligations round 1 named are both written: AC-1
is recorded satisfied on the render half and **satisfied through the prop chain**
on the `drepIndex` half, matching the guide at `:1396-1405`; AC-4 is recorded
**satisfied in part** with both OWED clauses spelled out (no browser here; the
four ids stay unseeded until task-146), matching the guide at `:1417-1426` and
the PRD's slice-level row at `:1632`. The row closes "Complete, NOT verified",
which is the honest reading. The row's own anchors were spot-checked and all
land: `governance.types.ts:35` really is `export type DRepStatus = 'active' |
'inactive';`; `DRepStatusBadge.tsx:8-17` really are the
`governance.drepDirectory.status.active` / `.inactive` descriptors; those two ids
really do already ship at `en-US.json:355-356` and `ja-JP.json:355-356`;
`fixtures.ts:14-27` really does carry the five options including `drepVerified`;
`declaration.d.ts:1` really is `declare module '*.scss';`. The numstat figures in
the row (55/4, 88/6, 268/1, 25/0, 13/0, 40/38) match `git diff --numstat`
exactly. The tracker was not run through prettier.

**CR3-2 (minor) — discharged.** The comment at `CurrentVoteSummary.tsx:40-42`
now reads "The vote-kind chip and the status captions render through the local
message set because DRepSourceLabel's variant union cannot express them;
DRepSourceLabel renders only the on-chain source label on the DRep state." — the
narrowing round 1 prescribed, applied in place. Its position is unchanged
(directly above the component function), which is what guide Step 3c pins
(`:990`); the net comment count in the file is unchanged; no other line moved.

**What came back clean, re-verified here.** Steps 1-5 are unchanged from round 1
and still the prescribed code — the fix pass touched no verbatim guide block. The
four descriptors at `CurrentVoteSummary.messages.ts:73-97` carry the exact ids,
the ICU argument `n`, the leading `!!!` and the double-quoted `DRep's` string. The
`.expiringBadge` block at `CurrentVoteSummary.scss:33-44` is the guide's block
character-for-character, alphabetical, inserted immediately after `.statusBadge`
and immediately before `.glyph`. `CurrentVoteSummary.stories.tsx:17-51` and
`:63-80` match guide Steps 5c and 5d statement for statement, including the bare
`key={option}`, the `'DRep status (mock)'` knob defaulting to `'none'`, and the
locale comment kept verbatim at `:53-55` — no `IntlProvider`, no per-locale
variant, no new story id. Invariants hold on re-derivation: `git diff --stat`
over `source/renderer/app/components/governance/_shared`,
`source/common/types/governance.types.ts` and `source/renderer/app/stores` is
empty, so the shared badge and the closed `DRepStatus` union are unwidened and
`expiring` exists only as the unexported local `CurrentVoteBadgeState` (`:21`);
the sink grep (`logger|console.|analytics|electron-store|localStorage|ipcRenderer`)
over every added line in `source/` and `storybook/` returns nothing; the id row
still renders `currentVote.drep.raw` untouched (`:106`); a null entry
short-circuits to `'unavailable'` (`:29`) with no default-to-Active branch. The
`drepEntry.status` dereference at `:92` is guarded by the
`active || inactive` test at `:91`, so it is never reached on a null entry. The
one hazard worth chasing — `status: 'active'` with `drepActivity: 0` rendering
"Expiring in 0 epochs" — is unreachable at the producer:
`source/main/governance/GovernanceQueryService.ts:506-511` sets `inactive`
whenever `expiry <= currentEpoch` and derives
`drepActivity = Math.max(0, expiry - currentEpoch)`, so `'active'` implies `>= 1`,
and `deriveCurrentVoteBadgeState` tests status (`:30`) before the epoch window
(`:31-36`) regardless. The eleven tests all execute and none is vacuous — the
negative queries have teeth because the same strings are asserted present in
sibling cases (`!!!Expiring in 4 epochs` at `:122`, `!!!Active` at `:114`), and
the `12 → expiring` / `13 → active` / `null → active` triple pins the threshold
rather than restating it. The seven `exports[` keys match the seven
snapshot-taking test names, with the HEAD "no badge" key pruned and no orphan. No
added line in source or in a test name cites a task id, a `CAT-`/`CP-`/`AC-`
label, a plan name or a `TODO`.

### Blockers (ranked, most severe first)

**None.** Both round-1 findings are discharged, all three lenses returned
`approved` with no blockers, and no new candidate survived adjudication.

### Merged and dropped

1. *Merged.* Nothing needed merging — the three lenses raised no overlapping
   finding, because they raised no finding. Their three clean results were not
   accepted on assertion: the conformance claim was re-derived by diffing the
   messages, scss and story blocks against guide Steps 1, 2 and 5c/5d; the
   invariants claim by re-running the `_shared` / `governance.types.ts` /
   `stores` diff, the sink grep and the `drep.raw` check; the tests claim by
   re-running the suite and recounting the snapshot keys.
2. *Dropped as churn — the tests/simplicity/drift lens re-opened
   `CurrentVoteSummary.tsx:40-42`, arguing the narrowed subject "the vote-kind
   chip and the status captions" omits the local `statusExpiringBadge` label at
   `:99`.* The observation is factually correct: `:99` does render a local
   descriptor that is neither the vote-kind chip nor a caption. It is still
   dropped. The sentence claims no exhaustivity, nothing it states is false, and
   the wording under attack is the exact remedy the previous round prescribed for
   CR3-2 — re-opening it would spend a second round narrowing a comment that was
   narrowed on this log's own instruction. The lens reached the same conclusion
   independently and declined to promote it.
3. *Re-dropped — "the guide's `3 suites / 25 tests green at HEAD` is stale."*
   Round 1 already adjudicated this as a misread (item 2 of that round): the
   parenthetical is explicitly scoped **at HEAD** and is exactly right there,
   since this suite went 4 → 11 and `32 - 11 + 4 = 25`. The tracker's
   `statusReason` now states it correctly, citing the guide's `:1333` and
   attributing the `+7` to this suite's own new cases. No doc edit is owed and
   none was made.
4. *Not promoted — AC-4's console-clean and ja-JP clauses and AC-1's
   `drepIndex` half.* Unchanged from round 1: both are pre-dispositioned as not
   dischargeable at this commit (`cv-2-implementation-guide.md:1396-1405`,
   `:1417-1426`; `cv-2-PRD.md:1632`) and are owned by task-139 and task-146.
   Demanding either as a code defect would demand work the guide deliberately
   excluded. They are discharged as `statusReason` prose, which is now written.
5. *Note, not a finding.* The task-143 round-2 entry above cites `DRepStatus` at
   `governance.types.ts:34`; the live declaration is `:35`. That entry is
   committed and this log is append-only, so it stays as written; the task-136
   tracker row cites `:35` correctly.

**Verification gate.** Green on every prescribed gate, with the one red proven
pre-existing and no gate attributable to this task. Re-run during this
consolidation on the post-fix tree: `node_modules/.bin/tsc --noEmit` exit 0 with
zero diagnostics; `node_modules/.bin/jest --testPathPattern=CurrentVoteSummary
--no-coverage --runInBand` → 1 suite, 11 tests, 7 snapshots, all green, no
obsolete snapshot; `node_modules/.bin/prettier --check` clean on all five source
paths, with no `--write` run anywhere, so none of the four HEAD-red files
(`VotingPowerDelegation.tsx`,
`VotingPowerDelegationConfirmationDialog.tsx`, `VotingGovernancePage.tsx`,
`Governance.stories.tsx`) was rewritten. Carried from the round's gate agent and
unchanged by the fix pass, which touched no source file: `yarn lint` exit 0 at
exactly the 5591-warning baseline with zero errors; the neighbour sweep
`--testPathPattern="voting-governance|VotingGovernancePage"` → 3 suites / 32
tests; the wave sweep `--testPathPattern="(governance|voting)"` → 17 passed / 1
skipped suites and 276 passed / 12 skipped tests, `+7` tests and `+3` snapshots
against the baseline and the skip count unmoved at 12; `yarn i18n:manage` exit 0,
idempotent, adding exactly the four `voting.governance.currentVote.status.*` ids,
after which all four files it writes were restored with `git restore` and
re-checksummed byte-identical — task-146, not this task, owns those writes.
**Attribution of the one red:** `stylelint CurrentVoteSummary.scss` exits 2 with
12 `order/properties-alphabetical-order` errors, the same 12 rule messages in the
same order as against the HEAD copy of the file, the six above the insertion
point unmoved and the six below shifted by exactly the 13 inserted lines, with
zero errors inside `:33-44`. The count did not move, which is the invariant Step
6 states; it is pre-existing at HEAD and is a note, not a finding. `nix` is
absent, so `nix fmt` stays an owed pre-merge obligation with explicit-path
`prettier` as the recorded substitute (F-12). `yarn check:all` and
`yarn storybook:build` were deliberately not run: both are red at HEAD for the
unrelated storybook manager-webpack JSX loader reason and neither is a valid
gate.

**Decision: approved** — the code deliverable was clean in round 1 and is
unchanged; the round-1 major (CR3-1) is discharged by a tracker row that matches
the committed shape and records both scoped acceptance dispositions truthfully,
and the round-1 minor (CR3-2) is discharged in place. The per-task Definition of
Done is met except its last clause, which is the next action rather than a
finding: one commit, subject-only, carrying all eight working-tree paths — the
six code paths, `governance-drep-discovery-plan-tasks.json` and this log — under
`feat(gov): task-136 render the live DRep status badge in the current-vote summary`.

---

## Code Review: 2026-07-28 — task-137 round 1

**Scope reviewed.** The uncommitted working tree against the guide section
"task-137: Replace the selected `Wallet` object state with `selectedWalletId`"
(`cv-2-implementation-guide.md:1449-1747`), its locked invariants (`:1534-1545`),
its four resolved judgment calls (`:1547-1566`), its six ordered steps
(`:1570-1729`) and its acceptance record (`:1731-1745`); task-137's three
acceptance criteria in `governance-drep-discovery-plan-tasks.json`; and the
per-task Definition of Done (`cv-2-PRD.md:1619-1623`). HEAD is `4880c963d`
(task-136). The main checkout `/workspaces/daedalus` was never read, edited or
run against.

**What landed.** `git status --porcelain -uall` → exactly one path:
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`,
`19+/15-`. That is precisely the surface the guide declares — one edited file, no
spec change, no new file, no i18n key, no prettier run (`:1451-1458`). No
untracked file. **The tracker is absent from the change set**, which is CR137-1
below.

**Review method (three lenses, adversarial refutation).** Three lenses ran over
the diff — (1) guide and acceptance-criteria conformance; (2) locked invariants
and the sanitization floor; (3) tests, simplicity and drift. **All three returned
`approved`**, two with zero findings and one with a single minor. Their clean
results were not accepted on assertion: every load-bearing claim was re-derived
in this worktree, and one defect none of the three raised was found during that
re-derivation and is promoted as CR137-1.

**The code deliverable is accepted as delivered.** Re-derived here: all six steps
are applied as written — the Step 1 type block matches `:1574-1588`, `initialState`
carries `selectedWalletId: null` (`:96`), the initializer's local is renamed
`initialWallet` and stores `initialWallet?.id ?? null` (`:117-123`) with the
byte-equality comment carried across verbatim (`:125-126`), the derived local sits
at `:133-134` with no `useMemo`, no store read and no comment, and all eight sites
are migrated (`:139`, `:177`, `:234-239`, `:242`, `:247`, `:263`, `:289`, `:337`).
The Step 6 grep `state\.selectedWallet([^I]|$)` re-run here returns no output and
exits 1, so AC-1 holds. The one sanctioned guard is present at the confirmation
render (`:327`) and the deliberately-omitted `initiateTransaction` null guard is
correctly absent — both pre-dispositioned at `:1549-1559`; the dialog's
unconditional dereference is real and is in fact two sites, not one
(`VotingPowerDelegationConfirmationDialog.tsx:143` and `:179`, where the record
cites only `:179`), which strengthens rather than weakens the guard. The
sanitization floor holds: the file references no `logger`, `analytics`,
`electron-store`, `localStorage` or `console.` sink, and the only outbound payload
touched (`:289`) keeps its shape and resolves to React Router `location.state`. No
store read, no `@inject`, no `GovernanceStore`/`VotingStore` import was added — the
`wallets` prop remains the only wallet source (`:1535-1539`). `chosenOption`
(`:163-166`) and `drepInputState.value` are outside every hunk, so byte-equality is
untouched. The diff adds no comment at all, so no task id or process artifact
leaked into source.

### Blockers (ranked, most severe first)

**CR137-1 (major, `governance-drep-discovery-plan-tasks.json`) — task-137's row is
still `pending`, so AC-2's deliberate deviation is nowhere recorded.** Live, the
row's keys are `id, title, description, status, priority, estimatedHours,
dependencies, targetPath, acceptanceCriteria` — `"status": "pending"` with no
`statusReason`, no `evidence` and no `updatedAt` — and the tracker is absent from
`git status --porcelain -uall`. The per-task Definition of Done
(`cv-2-PRD.md:1619-1623`) requires "tasks JSON synchronized (`status`,
`statusReason`, `evidence`, `updatedAt` as `YYYY-MM-DD`) · exactly one commit", so
the tracker edit has to be in the working tree before task-137 is committed, and no
later cv-2 row owns it. The convention is per-task and rides the task's own commit:
`0fc92fcab` (task-143) and `d3729994a` (task-135) each carry a
`governance-drep-discovery-plan-tasks.json` hunk in the same commit, and this is
the identical defect promoted as CR-1 for task-143 and as CR3-1 for task-136 in the
two rounds above. It matters here for a specific reason and not merely as
bookkeeping: task-137's AC-2 reads "derived reactively from
`stores.wallets.all.find()`", and the implementation deliberately does **not** do
that — it runs `find()` over the `wallets` prop (`:133-134`), which the guide
records as a deliberate deviation at `:1736-1741` because a store read inside a
presentational component would break the container split and the no-second-backend
invariant. A row set to `complete` with no `statusReason` would claim, in the
tracker's own words, a `stores.wallets.all` read that is not in the code.
*Fix:* set the row to `complete` and insert `statusReason`, `evidence` and
`updatedAt: "2026-07-28"` between `status` and `priority` — the byte-identical key
order already used by the `task-135` and `task-143` rows — with `evidence` an array
of repo-relative paths, source first, beginning with
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`
and plan docs last. Record AC-1 and AC-3 satisfied (citing the Step 6 grep and the
type change), and AC-2 **satisfied with a recorded deviation**, naming the `wallets`
prop, its container feed at `VotingGovernancePage.tsx:63` and the guide's
`:1736-1741`, rather than claiming the criterion whole. The tracker is tool-managed
JSON — do not run prettier on it.

### Minor (non-blocking; absorb before the task-137 commit)

**CR137-2 (minor, `VotingPowerDelegation.tsx:63-66`) — the `Form` alias now
`Omit`s and reintroduces an identical field.** It reads
`type Form = Omit<FormData, 'selectedWalletId'> & { selectedWalletId: string | null; status: 'form'; }`
while `FormData:54` already declares `selectedWalletId: string | null`. At HEAD the
`Omit` did real work — it widened `selectedWallet: Wallet` to `Wallet | null`
(`git show HEAD:…:63-64`) — and Step 1 dissolved the widening by making the
`FormData` field nullable, so the construct is now an identity operation. The file
carries the simpler idiom for exactly this shape at `:73-75`
(`type StateFormComplete = FormData & { … }`). Confirmed behaviour- and
typecheck-neutral: `tsconfig.json:79` sets `"strict": false` with
`strictNullChecks` commented out at `:81`, and `tsc --noEmit` exits 0.
*Disposition: leave it as written.* The guide prescribes this text verbatim
(`:1585-1588`), and the same identity-`Omit` idiom already pre-exists in this file
at two other untouched sites — `:68` `Omit<FormData, 'status'>` where `FormData`
declares no `status`, and `:77` `Omit<FormData, 'fee'>` where the field is `fees` —
both present at HEAD. So task-137 did not introduce the pattern; it converted the
one load-bearing `Omit` into a third vestige. Deviating byte-for-byte from a
prescribed block to no behavioural end, in a file task-138 is about to edit, is
churn. Recorded so task-138 and task-139 do not propagate the pattern, and so a
later cleanup can collapse all three at once.

### Merged and dropped

1. *Merged.* Only one finding was raised across the three lenses — the identity
   `Omit` at `:63-66`, from the tests/simplicity/drift lens, which raised it as
   minor and self-declined it. It is promoted as CR137-2 with the two pre-existing
   sibling vestiges at `:68` and `:77` re-derived here and the HEAD form of `:63-64`
   checked, so the "the `Omit` used to do real work" reading is recorded next to
   the finding rather than left implicit.
2. *Not raised by any lens, promoted here — the unsynchronized tracker row.* All
   three lenses scoped themselves to the single source file and none opened
   `governance-drep-discovery-plan-tasks.json`. The per-task Definition of Done and
   two prior rounds of this same log make it a review-time obligation, so it is
   promoted as CR137-1 rather than deferred.
3. *Dropped as a non-defect — the `useEffect` deps `[initiateTransaction, intl, state]`
   (`:194`) do not list the derived `selectedWallet`.* Raised and self-dismissed by
   two lenses; re-derived here and the dismissal is right. The effect returns
   immediately unless `state.status === 'form-submitted'` (`:169`), so it does its
   work in the render pass that set that status and closes over that pass's derived
   wallet — strictly fresher than the `state.selectedWallet` it replaced, which was
   captured at selection time. `chosenOption` (`:163-166`) is likewise absent from
   the deps and was absent at HEAD too, so no new pattern is introduced. There is no
   lint surface either: `react-hooks` is not among the configured plugins
   (`.eslintrc:96`).
4. *Not promoted — AC-2's literal `stores.wallets.all.find()` wording.* The
   implementation reads the `wallets` prop instead, which is a deliberate deviation
   the guide pre-dispositions at `:1736-1741` and the PRD echoes. Demanding a store
   read would demand work the guide deliberately excluded and would break the
   no-second-backend invariant (`:1535-1539`). It is not a code defect — but it is
   exactly what CR137-1 requires the tracker `statusReason` to say, so it folds
   into that finding rather than being dropped outright.
5. *Not promoted — the absent null guard on the `initiateTransaction` call
   (`:176-177`).* Pre-dispositioned "do not revisit" at `:1549-1554`: an early
   return there would strand `status: 'form-submitted'` and permanently disable the
   submit button, and `formIsValid` (`:139`) already reads the derived wallet.
6. *Not promoted — `:289` sends `selectedWallet?.id ?? null`, so an id whose wallet
   has left the snapshot degrades to `null` instead of being echoed back.* This is
   the guide's mandated Step 5f text (`:1717-1718`) and matches what the dropdown
   itself renders in that state (`:242`). Consistent, not a defect.
7. *Re-dropped for the third time — "the guide's focused-run expectation
   `3 suites / 25 tests / 4 snapshots` is stale."* Live is 32 tests / 7 snapshots.
   Already adjudicated in the task-136 round-1 (item 2) and round-2 (item 3)
   entries above. Recorded again as a gate note below, not as a finding.

**Verification gate.** Green on every prescribed gate, with the one red proven
pre-existing and nothing attributable to this task. Carried from the gate agent and
spot-re-run during this consolidation: `node_modules/.bin/tsc --noEmit` exit 0 with
zero diagnostics (`tsconfig.json` has no `include` key, so
`storybook/stories/voting/Governance.stories.tsx` was typechecked against the new
state shape too); `yarn lint` exit 0 at exactly the 5591-warning baseline with zero
errors, so the `initialWallet`/`nextWallet` renames introduced no `no-shadow`
warning; the guide's two Step 6 greps both behave as specified — `state.selectedWallet`
returns no output and exits 1, and `selectedWalletId` returns 12 lines that are
exactly the enumerated set, with `:43`, `:48`, `:117` and `:119` confirmed present at
HEAD via `git show`; the focused run
`--testPathPattern="voting-governance|VotingGovernancePage"` re-run here → 3 suites
/ 32 tests / 7 snapshots, all green, 2.315 s; the unfiltered closing gate
`jest --runInBand` → 85 passed / 1 skipped of 86 suites, 1068 passed / 12 skipped
tests, zero FAIL lines, the one skipped suite being the environment-gated
`GovernanceCliArgvSmoke.spec.ts`; the wave sweep `--testPathPattern="(governance|voting)"`
→ 17 passed / 1 skipped suites, 276 passed / 12 skipped tests. `yarn i18n:manage`
was correctly never invoked — the diff defines no message and edits no catalog — so
no file needed restoring. `typed-scss-modules` was correctly skipped: no `.scss` in
the change set. **Attribution of the two discrepancies, both proven pre-existing:**
(a) the guide's `25 tests / 4 snapshots` predates the committed task-136
(`4880c963d`), which added 94 lines to `CurrentVoteSummary.spec.tsx` and 269 to its
snapshot file; `git show HEAD:…/__snapshots__/CurrentVoteSummary.spec.tsx.snap`
already contains 7 `exports[` keys, and task-137's diff touches no spec or snapshot
file, so it cannot move a test count — downstream verifiers should expect 32/7.
(b) `prettier --check` is red on `VotingPowerDelegation.tsx`, at exactly one line,
`:86` (`(typeof messages)[keyof typeof messages]`); piping the HEAD copy through the
identical `--stdin-filepath` command yields the identical single-line delta, and
`:86` lies outside every hunk (the first begins at `:51`). No `--write` was run
anywhere, so none of the four HEAD-red files was rewritten and task-137 added zero
formatting debt. `nix` is absent, so `nix fmt` stays an owed pre-merge obligation
with explicit-path `prettier` as the recorded substitute. `yarn check:all` and
`yarn storybook:build` were deliberately not run: both are red at HEAD for the
unrelated storybook manager-webpack JSX loader reason and neither is a valid gate.

**Decision: requires_changes** — one major finding, CR137-1. The code deliverable
is accepted as delivered and needs no edit: all six guide steps are applied as
written, all three acceptance criteria hold on the code, every gate is green, and
the only code-level finding (CR137-2) is a guide-prescribed, typecheck-neutral
vestige that is explicitly dispositioned to stay. What is missing is the tracker
row, which must be synchronized — with AC-2's deviation recorded truthfully rather
than claimed whole — before task-137 is committed, in the same single commit,
subject-only, under
`refactor(gov): task-137 hold selectedWalletId instead of the wallet object in VotingPowerDelegation`.

---

## Code Review: 2026-07-28 — task-137 round 2

**Scope reviewed.** The uncommitted working tree after the round-1 fix pass,
against the guide section "task-137: Replace the selected `Wallet` object state
with `selectedWalletId`" (`cv-2-implementation-guide.md:1449-1747`) — its locked
invariants (`:1532-1545`), its four resolved judgment calls (`:1547-1566`), its
six ordered steps (`:1568-1729`) and its acceptance record (`:1731-1745`) —
task-137's three acceptance criteria in
`governance-drep-discovery-plan-tasks.json`, and the per-task Definition of Done
(`cv-2-PRD.md:1619-1623`). HEAD is `4880c963d`. The main checkout
`/workspaces/daedalus` was never read, edited or run against.

**What landed.** `git status --porcelain` → three paths, which is the full
per-task surface and not a scope creep: the one code file the guide declares
(`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`,
`19+/15-`, byte-identical to the round-1 state — the fix pass changed no line of
it), plus the two round-1 remediation paths,
`governance-drep-discovery-plan-tasks.json` (`8` lines) and this log (a pure
append at `:1402`, `197+/0-`). `git diff --check` exit 0, `git diff --summary`
empty. No untracked file, no catalog write, no `.scss`.

**CR137-1 is closed.** Re-derived here rather than accepted on assertion. The
task-137 row now reads `"status": "complete"` and carries `statusReason`,
`evidence` and `"updatedAt": "2026-07-28"` inserted between `status` and
`priority` — parsed programmatically, its key order is
`id, title, description, status, statusReason, evidence, updatedAt, priority,
estimatedHours, dependencies, targetPath, acceptanceCriteria`, byte-identical to
the `task-135`, `task-143` and `task-136` rows. The file parses as JSON and the
hunk is minimal, so the tool-managed formatting is untouched. Most importantly the
`statusReason` records AC-2 as **satisfied with a recorded deviation** and names
the `wallets` prop, its container feed at `VotingGovernancePage.tsx:63` and the
guide's pre-disposition at `:1736-1741`, rather than claiming a
`stores.wallets.all` read that is not in the code — which was the substantive half
of CR137-1, not the bookkeeping half. `evidence` is source-first and lists only
paths this task actually touched; it omits `research/cv-2-findings.md`, and that
is correct, not an inconsistency with the `task-143` and `task-136` rows: those
two commits appended real findings (`0fc92fcab` `+61`, `4880c963d` `+43`) whereas
task-137 produced none — F-12 already owns the absent `nix` and F-16 already owns
the 32/7 count basis.

**The code deliverable is re-confirmed, not re-litigated.** Every prescribed block
was diffed against the guide text again in this round: Step 1 against `:1574-1588`
(live `:53-66`), Step 3 against `:1608-1626` (live `:115-131`, the byte-equality
comment carried across verbatim at `:125-126`), Step 4 against `:1636-1639` (live
`:133-134`, no `useMemo`, no store read, no comment), Step 5a-g against
`:1650-1702` (live `:138-140`, `:175-178`, `:234-242`, `:247`, `:263`, `:289`,
`:326-337`). All match. The two Step 6 greps re-run here:
`state\.selectedWallet([^I]|$)` → no output, exit 1; `selectedWalletId` → exactly
the 12 lines the guide enumerates at `:1717-1719`.

### Blockers (ranked, most severe first)

**None.** No finding from any of the three lenses survived adjudication, and none
was promoted during the independent re-derivation.

### Merged and dropped

1. *Nothing to merge.* All three lenses — (1) correctness-versus-guide, (2) locked
   invariants and the sanitization floor, (3) tests, simplicity and drift —
   returned `approved` with **zero** blockers. There were no duplicate findings to
   reconcile, so this round's work was refutation, not merging.
2. *Dropped, already dispositioned — the identity `Omit` at `:63-66`.* Lens 3
   raised it only as an explicit non-finding. It is CR137-2 from round 1 and its
   disposition stands unchanged: the guide prescribes the text verbatim
   (`:1585-1588`), the same idiom pre-exists untouched at `:68` and `:77` (both at
   HEAD), and it is typecheck-neutral under `tsconfig.json:79` `"strict": false`.
   Re-raising it in a file task-138 is about to edit would be churn. It stays a
   carry-forward note for task-138 and task-139 — do not propagate the pattern —
   and a later cleanup collapses all three sites at once.
3. *Dropped — "task-137's AC-2 deviation is missing from the PRD's DoD exception
   table" (`cv-2-PRD.md:1630-1644`).* Lens 3 raised and self-declined it; the
   self-decline is right. That table sits under **"Per slice:"** (`:1625`), not
   under the per-task Definition of Done (`:1619-1623`), and the PRD reserves
   "Deviations from this PRD and its guide" (`:1736`) for the slice-close Final
   Report. The deviation is already recorded in the two places a per-task reviewer
   can check — the guide's own acceptance record (`:1736-1741`) and now the tracker
   `statusReason`. Carried forward to task-148 as a fourteenth row for that table;
   it is not a task-137 defect and editing the approved PRD mid-slice is not
   task-137's to do.
4. *Dropped — the `useEffect` deps `[initiateTransaction, intl, state]` (`:194`)
   omit the derived `selectedWallet`.* Raised and self-dismissed by two lenses;
   already adjudicated as a non-defect in round 1 (item 3) and re-derived once
   more: the effect early-returns unless `state.status === 'form-submitted'`
   (`:170`), so it runs in the commit of the render that set that status and closes
   over that render's derived wallet — strictly fresher than the
   `state.selectedWallet` it replaced. `chosenOption` (`:163-166`) is likewise
   absent from the deps and was absent at HEAD.
5. *Dropped — AC-2's literal `stores.wallets.all.find()` wording.* Demanding a
   store read would demand work the guide deliberately excludes and would break the
   no-second-backend invariant (`:1534-1539`). It folded into CR137-1's
   `statusReason` requirement, which is now satisfied.
6. *Re-dropped for the fourth time — "the guide's `3 suites / 25 tests / 4
   snapshots` (`:1723`) is stale."* Recorded as a gate note below, never as a
   finding. F-16 in `research/cv-2-findings.md` is the standing disposition.

**Precision note, not a finding and needing no edit.** The tracker `statusReason`
says four of the twelve `selectedWalletId` hits (`:43`, `:48`, `:117`, `:119`) are
present at HEAD. `git show HEAD:…VotingPowerDelegation.tsx | grep -n` returns
five: those four plus `:286`, the browse-DReps payload **key**, whose value
expression (not its key) is what Step 5f changed. So the net-new count is seven
lines plus one modified line rather than eight new lines. The claim as written is
true of the four it names and the conclusion it supports — that no unexplained
occurrence exists — is unaffected, since the 12 live lines are exactly the guide's
enumerated set.

**Verification gate.** Green, with the one red proven pre-existing. Carried from
the gate agent and independently re-run in this consolidation against the current
three-path tree: `node_modules/.bin/tsc --noEmit` → exit 0, zero diagnostics;
`node_modules/.bin/jest --testPathPattern="voting-governance|VotingGovernancePage"
--no-coverage --runInBand` → exit 0, **3 suites / 32 tests / 7 snapshots**, all
green. From the gate agent, unchanged by the docs-only fix pass: `yarn lint` exit 0
at exactly the **5591**-warning baseline with zero errors, so the
`initialWallet`/`nextWallet` renames introduced no `no-shadow` warning;
`jest --runInBand` → 85 passed / 1 skipped of 86 suites, 1068 passed / 12 skipped
tests, zero FAIL lines, the one skipped suite being the environment-gated
`GovernanceCliArgvSmoke.spec.ts`; the wave sweep
`--testPathPattern="(governance|voting)"` → 17 passed / 1 skipped suites, 276
passed / 12 skipped. `yarn i18n:manage` correctly never invoked — no message
definition, no catalog edit, nothing to restore. `typed-scss-modules` correctly
skipped — no `.scss` in the change set. **Attribution, both discrepancies proven
pre-existing:** (a) the guide's `25 tests / 4 snapshots` (`:1723`) was measured at
`504b44c1a`, before `0fc92fcab` and `4880c963d` landed; `git show
HEAD:…/__snapshots__/CurrentVoteSummary.spec.tsx.snap` already carries 7 `exports[`
keys and task-137's diff touches no spec or snapshot file, so 32/7 is the correct
basis. (b) `prettier --check` red on `VotingPowerDelegation.tsx` at exactly one
line, `:86`, identical when the HEAD copy is piped through the same
`--stdin-filepath` command, and `:86` lies outside every hunk (the first begins at
`:51`); no `--write` was run anywhere and `yarn prettier` was never invoked, so
task-137 added zero formatting debt. **One stale gate statement, recorded so it is
not read as drift:** the gate report asserts `git status --porcelain` returns
"exactly one entry". That was true when it ran, before the round-1 fix pass added
the tracker and this log; both are docs-only and cannot move a compile or a test,
which the tsc and Jest re-runs above confirm. `nix` is absent, so `nix fmt` stays
an owed pre-merge obligation (F-12). `yarn check:all` and `yarn storybook:build`
were deliberately not run: both are red at HEAD for the unrelated storybook
manager-webpack JSX loader reason and neither is a valid gate.

**Decision: approved** — zero surviving blockers. The code deliverable is
accepted as delivered, CR137-1 is closed with the tracker row synchronized in the
sibling key order and AC-2's deviation recorded truthfully rather than claimed
whole, and CR137-2 remains dispositioned to stay as written. The Definition of
Done is met except its last clause, which is the next action rather than a
finding: one commit, subject-only, carrying all three working-tree paths, under
`refactor(gov): task-137 hold selectedWalletId instead of the wallet object in VotingPowerDelegation`.

---

## Code Review: 2026-07-28 — task-138 round 1

**Scope reviewed.** The uncommitted working tree against the guide section
"task-138: Pre-fill `VotingPowerDelegation` from the current on-chain delegation"
(`cv-2-implementation-guide.md:1751-2277`) — its locked invariants (`:1795-1814`),
its seven resolved judgment calls (`:1816-1850`), its seven ordered steps
(`:1852-2246`) and its acceptance record (`:2248-2274`); task-138's seven
acceptance criteria in `governance-drep-discovery-plan-tasks.json:1223-…`; seam
contract S-3 (`cv-2-PRD.md:918-946`), decision D-11 (`:740-750`), the slice
disposition for task-138 AC-3 (`:1633`) and the per-task Definition of Done
(`:1619-1623`). HEAD is `31cadffd9` (task-137). The main checkout
`/workspaces/daedalus` was never read, edited or run against.

**What landed.** `git status --porcelain` → exactly two paths, both named by the
guide's Files-touched list (`:1753-1759`):
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`
(75+) and `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`
(128+), `192 insertions, 11 deletions`. No new file, no untracked artifact, no
i18n catalog, no `translations/`, no `package.json`/`yarn.lock`, no prettier run —
exactly the declared surface (`:1761`). **The tracker is absent from the change
set**, which is CR138-1 below.

**Review method (three lenses, adversarial refutation).** Three lenses ran over
the diff — (1) correctness against the guide and the acceptance criteria; (2)
locked invariants and the sanitization floor; (3) tests, simplicity and drift.
**All three returned `approved`**, carrying four minors between them. Their
results were not accepted on assertion: every load-bearing claim below was
re-derived in this worktree, one lens minor was dropped to a recorded
no-action disposition, one was re-scoped, and the defect none of the three
promoted — the unsynchronized tracker row — is promoted here as CR138-1.

**The code deliverable is accepted as delivered.** Re-derived here: all seven
steps are applied as written. `deriveFormSeed` sits at module scope between
`initialState` and the component (`VotingPowerDelegation.tsx:104-137`) with the
four fallback branches in the guide's order — current `drep` vote, current
sentinel, inherited directory id, blank — pure, unexported, no logging and no
store access. The lazy initializer carries the currentVote-wins `voteType`
precedence verbatim (`:150-165`), and the byte-equality comment moved onto the
helper rather than being duplicated (the HEAD copy at the initializer, `git show
HEAD:…:117-118`, is gone). `WalletsDropdown.onChange` spreads the seed **after**
`...initialState` (`:296-303`), replacing HEAD's unconditional
`setState({ ...initialState, selectedWalletId })` (`git show HEAD:…:234-240`). The
re-seed effect (`:170-196`) keeps all four locked properties: the two-primitive
dependency array `[currentVoteKind, currentVoteDRepId]`, the
`currentVoteKind === null` short-circuit, the identity bail-out returning
`previous` by reference, and the `status !== 'form'` guard; no `eslint-disable`
was added and none was needed (`react-hooks` is not a configured plugin). The
spec's `WalletsDropdown` mock gains only `onChange` plus text-free, name-free
option buttons (`VotingGovernancePage.spec.tsx:34-54`) and the `ItemsDropdown`
mock is untouched (`:56-60`); `renderFlow` hands the **same** `stores` object to
`tree` on every render and mutates `stores.wallets.all` (`:162-209`), so
`mobx-react`'s provided-store-set check cannot fire; the four prescribed cases are
appended in their own trailing describe (`:439-509`). Invariants: no store
crossover (`grep -n "GovernanceStore" source/renderer/app/stores/VotingStore.ts`
returns nothing), no id mutation (no `toLowerCase|trim(|normalizeDRepIdentity` in
the component), no logging sink (no `logger|analytics|console.|electron-store|
localStorage`), no auto-delegation (branch 4, `:133-136`, returns the blank form),
and no story drift owed — `grep -c currentVote
storybook/stories/voting/Governance.stories.tsx` returns `0`, so the
"prefilled from directory" story still describes true behaviour and the wrapper
migration remains task-145's. The two added comments (`:104-105`, `:175-176`)
state an invariant and a why in plain sentence case with no process artifact, and
no test or describe name carries one.

### Blockers (ranked, most severe first)

**CR138-1 (major, `governance-drep-discovery-plan-tasks.json:1223`) — task-138's
row is still `pending`, so AC-3's deliberate partiality is nowhere recorded.**
Live, the row's keys are `id, title, description, status, priority,
estimatedHours, dependencies, targetPath, acceptanceCriteria` — `"status":
"pending"` with no `statusReason`, no `evidence`, no `updatedAt` — and the tracker
is absent from `git status --porcelain`. The per-task Definition of Done
(`cv-2-PRD.md:1619-1623`) requires "tasks JSON synchronized (`status`,
`statusReason`, `evidence`, `updatedAt` as `YYYY-MM-DD`) · exactly one commit", so
the tracker edit has to be in the working tree before task-138 is committed and no
later cv-2 row owns it. This is the identical defect promoted as CR-1 for
task-143, CR3-1 for task-136 and CR137-1 for task-137, and it bites harder here
because task-138 is one of the thirteen scoped criteria: the slice DoD table
(`cv-2-PRD.md:1633`) records task-138 AC-3 as **satisfied in part**, and the
guide's acceptance record (`:2255-2261`) says in as many words "Record both in the
task's `statusReason`". A row flipped to `complete` with no `statusReason` would
claim, in the tracker's own words, that "the form re-seeds (or surfaces a 'data
changed' indicator)" whole — when the re-seed fires only while
`drepInputState.dirty === false` (`VotingPowerDelegation.tsx:180-182`) and the
indicator alternative was deliberately not built (D-11, `cv-2-PRD.md:740-750`).
*Fix:* set the row to `complete` and insert `statusReason`, `evidence` and
`updatedAt: "2026-07-28"` between `status` and `priority` — the byte-identical key
order the `task-136` and `task-137` rows already use — with `evidence` an array of
repo-relative paths, source first
(`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`,
then `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`), plan
docs last. Report AC-1, AC-2, AC-4, AC-5, AC-6 and AC-7 satisfied, and AC-3
**satisfied in part**, naming the `dirty === false` gate, the not-built indicator,
and the two consequences in CR138-2 and CR138-3 below. The tracker is tool-managed
JSON — do not run prettier on it.

### Minor (non-blocking; absorb before the task-138 commit)

**CR138-2 (minor, `VotingPowerDelegation.tsx:175-176` versus `:315-321`) — the
re-seed's comment claims a guarantee the effect does not give: a user-chosen vote
type can be overwritten.** The effect's only user-protection gate is the DRep
input's flag, `if (previous.status !== 'form' || previous.drepInputState.dirty) {
return previous; }` (`:180-182`), but the comment above it reads "re-seed only
while the DRep input is untouched **so user input is never overwritten**"
(`:175-176`). The vote-type dropdown is user input that never sets that flag:
`handleChange={(option) => setState({ ...state, selectedVoteType: option.value,
status: 'form' })}` (`:315-321`) leaves `drepInputState` alone. Re-derived
sequence: a wallet whose on-chain vote is `{ kind: 'abstain' }` seeds
`selectedVoteType: 'abstain'` with `dirty: false` (branch 2, `:119-124`); the user
moves the dropdown to `drep` without typing; a poll then changes the chain vote's
`kind`, `currentVoteKind` changes, the effect re-runs, `dirty` is still `false`,
and `return { ...previous, ...seed }` (`:194`) reverts the user's choice. D-11
(`cv-2-PRD.md:740-750`) decides only the `drepInputState` case and the guide's
judgment call (`:1821-1828`) reasons only about destroying typing mid-edit, so the
vote-type consequence is unstated rather than sanctioned. *Fix:* do **not**
restructure the effect — the guide locks its four properties (`:1980-1991`) and a
fifth bail-out is a behavioural change no criterion asks for. Narrow the second
clause of the comment so it names what it actually guards (the DRep input), and
record the vote-type consequence in the CR138-1 `statusReason` alongside the
partial-re-seed and no-indicator disclosures AC-3 already requires.

**CR138-3 (minor, `VotingPowerDelegation.tsx:112-116` consumed at `:198` and
`:362-365`) — a CIP-105 on-chain delegation now shows the DRep-input error on a
form the user never touched.** Branch 1 seeds `drepInputState: { dirty: true,
value: currentVote.drep.raw }`. `raw` is the wire string byte-untouched:
`parseVoting` (`source/renderer/app/api/api.ts:3009-3022`) hands
`delegation.active.voting` straight to `normalizeDRepIdentity`, whose CIP-105
branch (`normalizeDRepIdentity.ts:46-59`, `prefix === 'drep_vkh' || prefix ===
'drep_script'`) returns `{ raw, cip105: raw, … }`. The form gate is
`Cardano.DRepID.isValid(state.drepInputState.value)` (`:198`), measured again in
this worktree: `isValid('drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l')`
→ `false`, `isValid('drep1ygqqq…7vlc9n')` → `true`. Because the seed also sets
`dirty: true`, the error branch `state.drepInputState.dirty && !drepInputIsValid`
(`:362-365`) fires unprompted and `formIsValid` (`:200-202`) disables submit. The
path is new: at HEAD `dirty: true` could only come from
`initialFormState.selectedDRepId` (`git show HEAD:…:126-129`), an id sourced from
the CIP-129-keyed index. The PRD records the CIP-105 `raw` fact only against the
**index lookup** (`cv-2-PRD.md:540-546`, `:566-567`; guide `:2384-2390`), never
against the form gate. *Fix:* do **not** change the seed — re-encoding would
violate invariant 10 and the guide's "Nothing else may be added to it" (`:1898`).
Record the consequence in the CR138-1 `statusReason` and raise it to the slice
owner so a later cv-2 or anchor task decides whether the CIP-105 form should pass
the gate; if the wallet API is confirmed to emit CIP-129 only for
`delegation.active.voting`, record that measurement in
`research/cv-2-findings.md` and close the item there.

**CR138-4 (minor, `VotingGovernancePage.spec.tsx:32-33`) — the mock's comment
still calls it a pass-through.** It reads "the flow tests assert the values they
RECEIVE, so plain pass-through mocks are enough". After Step 5 the
`WalletsDropdown` mock renders one option button per wallet and all four new cases
drive behaviour through its `onChange` (`:43-50`, `:444-509`), so the second
clause is no longer true of it. The guide's Step 5 says "the comment above it is
unchanged" (`:2010`), so this is inherited wording rather than an implementer
error. *Fix:* adjust the second clause only — e.g. "…assert the values they
receive and drive selection through the mock's `onChange`, so lightweight mocks
are enough." The ALL-CAPS `RECEIVE` violates the comment convention but is
pre-existing (`git show HEAD:…:32-33`); folding it in is optional and outside
task-138's surface.

### Merged and dropped

1. *Merged, with the strongest evidence from two lenses.* CR138-2 comes from the
   correctness lens (the abstain → dropdown-move → poll sequence) joined to the
   invariants lens's reading of the gate; the comment-accuracy half is scored
   against the slice's own comment convention rather than against the guide, since
   the guide prescribes the wording verbatim.
2. *Re-scoped, not dropped.* The invariants lens filed the CIP-105 case as a
   "newly reachable state", explicitly not an invariant breach. Re-derived here it
   is a user-visible consequence with a documentation fix, so it is promoted as
   CR138-4's sibling CR138-3 at minor severity with the seed left untouched.
3. *Recorded, no action — the redundant vote-type assertion in case 1.* The
   tests lens is right that `expect(screen.getByTestId('vote-type-dropdown'))
   .toHaveTextContent('drep')` cannot discriminate: `initialState.selectedVoteType`
   is already `'drep'` (`VotingPowerDelegation.tsx:97`) and HEAD's `onChange`
   spread `...initialState` (`git show HEAD:…:234-240`). The case's real pin is the
   following `getByDisplayValue(VALID_DREP_ID)` line, which does fail on revert, and
   case 2 supplies the discriminating sentinel assertion. The line is guide-verbatim
   (`:2158`); deleting prescribed assertion text to no behavioural end is churn, so
   it stays as written — the same disposition given CR137-2.
4. *Recorded, no action — the inert second argument in the effect's
   `deriveFormSeed(selectedWallet, initialFormState?.selectedDRepId)` (`:183-186`).*
   Re-derived and correct: the effect returns at `:178` unless
   `currentVoteKind !== null`, so inside the updater the wallet always has a
   `currentVote` and branch 3 (`:126-131`) is unreachable from this call site. It is
   guide-prescribed (`:1964-1967`) and keeps the three call sites uniform, and the
   argument is live at the other two. Leave it.
5. *Dropped — "the wallet's `currentVote` outranks a freshly directory-selected
   DRep id".* Raised and self-declined by the correctness lens. The precedence is
   contractual: S-3's chain order (`cv-2-PRD.md:918-946`) puts `currentVote` first,
   and the guide dispositions the visible Storybook consequence as "correct
   behaviour, not a defect — say so in the review notes rather than 'fixing' the
   seed order" (`:4654-4664`). Said here.
6. *Dropped as pre-existing — `prettier --check` red on
   `VotingPowerDelegation.tsx`.* Baseline item 4; see the gate note below.

**Verification gate.** Green, with the one red proven pre-existing. Carried from
the gate agent and independently re-run in this consolidation against the current
two-path tree: `node_modules/.bin/tsc --noEmit` → exit 0, zero diagnostics;
`node_modules/.bin/jest --testPathPattern="voting-governance|VotingGovernancePage"
--no-coverage --runInBand` → exit 0, **3 suites / 36 tests / 7 snapshots**, all
green. From the gate agent: `jest --testPathPattern=governance-sanitization` → 24
passed, the inherited floor at its stated number; the closing
`node_modules/.bin/jest --runInBand` → 85 passed / 1 skipped of 86 suites, 1072
passed / 12 skipped tests, zero FAIL lines; the wave sweep
`--testPathPattern="(governance|voting)"` → 17 passed / 1 skipped suites, 280
passed / 12 skipped, the one skipped suite being the environment-gated
`GovernanceCliArgvSmoke.spec.ts`; `yarn lint` exit 0 with 5595 warnings and zero
errors. `typed-scss-modules` correctly skipped (no `.scss` in the change set) and
`yarn i18n:manage` correctly never invoked — the diff defines no message and
`git status --porcelain source/renderer/app/i18n translations` is empty, so
nothing needed restoring. **Attribution, three discrepancies all proven
pre-existing or accounted for:** (a) the guide's "grows from 8 to 12 tests"
(`:2232`) holds for this spec, and the suite total is 36 rather than 29 because
task-136's `CurrentVoteSummary.spec.tsx` landed at `4880c963d` after the guide's
counts were measured — the standing F-16 disposition, not a regression; (b) the
`+4` lint delta was measured, not assumed — each file's HEAD content piped through
`eslint --stdin --stdin-filename` gives 10 + 5 = 15 against 19 now, the four being
three `@typescript-eslint/no-explicit-any` matching the file's existing `as any`
fixture style and one `no-unused-vars` on the `walletId` parameter name in the
guide's verbatim Step 5 type, all warnings; (c) `prettier --check` red on
`VotingPowerDelegation.tsx` at exactly one line, `:86`
(`(typeof messages)[keyof typeof messages]`), byte-identical when the HEAD copy is
piped through the same `--stdin-filepath` command and outside every hunk, so
task-138 added zero formatting debt — no `--write` was run and `yarn prettier` was
never invoked. `VotingGovernancePage.spec.tsx` passes `prettier --check` cleanly.
The focused run's `[React Intl] Missing message:
"voting.governance.currentVote.status.expiring"` console noise comes from
`CurrentVoteSummary.tsx:99,:111`, a file outside this change set, and closes with
task-146's catalog run. `nix` is absent, so `nix fmt` stays an owed pre-merge
obligation (F-12). `yarn check:all` and `yarn storybook:build` were deliberately
not run: both are red at HEAD for the unrelated storybook manager-webpack JSX
loader reason and neither is a valid gate.

**Decision: requires_changes** — one major survivor, CR138-1, plus three minors to
absorb in the same pass. The code deliverable itself is accepted as delivered:
every guide step is applied verbatim, every locked invariant holds, and the four
prescribed cases each fail for the right reason on revert. What is missing is the
tracker synchronization the per-task Definition of Done requires **before** the
single commit, and it is the same edit that carries CR138-2's and CR138-3's
disclosures. Once the row is written — `complete`, with AC-3 recorded as satisfied
in part — and the two comment clauses in CR138-2 and CR138-4 are narrowed, the
task closes with one subject-only commit,
`feat(gov): task-138 pre-fill the delegation form from the wallet current on-chain vote`.

---

## Code Review: 2026-07-28 — task-138 round 2

**Scope reviewed.** The uncommitted working tree after the round-1 fix pass,
against the guide section "task-138: Pre-fill `VotingPowerDelegation` from the
current on-chain delegation" (`cv-2-implementation-guide.md:1751-2277`) — its
locked invariants (`:1795-1814`), its seven resolved judgment calls
(`:1816-1850`), its seven ordered steps (`:1852-2246`) and its acceptance record
(`:2248-2274`) — task-138's seven acceptance criteria in
`governance-drep-discovery-plan-tasks.json:1223-1234`, decision D-11
(`cv-2-PRD.md:740-750`), the slice DoD row for task-138 AC-3 (`:1633`) and the
per-task Definition of Done (`:1619-1623`). HEAD is `31cadffd9`. The main
checkout `/workspaces/daedalus` was never read, edited or run against.

**What landed.** `git status --porcelain` → three paths: the two source files the
guide's task-138 section declares
(`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`
`69+/6-`, `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`
`125+/6-`) plus this log (`246+/0-`, a pure append at `:1752`). No untracked
file, no `.scss`, no catalog or `translations/` write, no `package.json` or
`yarn.lock`. The tracker is **not** in the change set, which is this round's one
major.

**Round 1's two comment minors are closed, re-derived rather than accepted.**
CR138-2: `VotingPowerDelegation.tsx:175-176` now reads "re-seed only while the
DRep input is untouched so **a typed DRep id** is never overwritten" — a claim the
`previous.drepInputState.dirty` clause at `:180-182` actually delivers, where the
round-1 wording ("so user input is never overwritten") over-claimed against the
vote-type dropdown handler at `:316-323`, which still never sets that flag.
CR138-4: the spec's mock comment (`VotingGovernancePage.spec.tsx:31-33`) no longer
calls the mock a "pass-through" and no longer carries the ALL-CAPS `RECEIVE`; it
now names the `onChange` drive path. Both fixes are the wording change only — the
effect's four locked properties (`cv-2-implementation-guide.md:1980-1991`) are
untouched, as CR138-2 required. CR138-3 stays dispositioned as a disclosure, not a
code change, and is folded into CR138R2-1's `statusReason` below.

**The code deliverable is re-confirmed.** All seven steps are applied as written:
`deriveFormSeed` at module scope between `initialState` and the component with
the four branches in the prescribed order (`:104-137`, guide `:1856-1900`); the
lazy initializer's currentVote-wins `voteType` precedence (`:150-165`, guide
`:1904-1926`); `WalletsDropdown.onChange` spreading the seed after
`...initialState` (`:296-303`); the re-seed effect keeping all four locked
properties (`:177-196`); the Step 5 mock, the Step 6 fixtures, the
`rerenderWithWallets` hook and the four cases (`:446`, `:459`, `:474`, `:489`)
verbatim. Locked invariants re-measured here, not inherited: no `toLowerCase`,
`trim(` or `normalizeDRepIdentity` in the component; no `logger`, `analytics`,
`console.`, `electron-store` or `localStorage` sink anywhere in the diff; no
`GovernanceStore` read added to `VotingStore.ts`; branch 4 (`:133-136`) keeps the
no-vote-no-inheritance form blank, so no auto-delegation; branch 2 (`:119-124`)
blanks `drepInputState`, so a sentinel never enters the id field.

### Blockers (ranked, most severe first)

**CR138R2-1 (major, `governance-drep-discovery-plan-tasks.json:1223-1234`) —
task-138's row is still `pending`, so AC-3's deliberate partiality is nowhere
recorded.** Carried from round 1 as CR138-1 and verified still open in this round
rather than restated: the row's keys are still `id, title, description, status,
priority, estimatedHours, dependencies, targetPath, acceptanceCriteria` with
`"status": "pending"` (`:1226`) and no `statusReason`, no `evidence`, no
`updatedAt`, and the tracker is absent from `git status --porcelain`. The sibling
`task-137` row (`:1199-1210`) carries all four keys, so the convention is
established and this row is the outlier. The gap is substantive, not clerical: the
row's own AC-3 text (`:1229`) promises the form "re-seeds (or surfaces a 'data
changed' indicator)", while the delivered effect returns `previous` whenever
`previous.drepInputState.dirty` is true (`VotingPowerDelegation.tsx:180-182`) and
no indicator exists (D-11, `cv-2-PRD.md:740-750`; guide `:1816-1820`). Flipping
the row to `complete` with no `statusReason` would report a scoped criterion
green, which the slice DoD table forbids in as many words (`cv-2-PRD.md:1633`),
and the guide's acceptance record says "Record both in the task's `statusReason`"
(`:2255-2261`). The per-task DoD (`cv-2-PRD.md:1619-1623`) requires the row
synchronized **before** the single commit, and no later cv-2 row owns it.
*Fix:* set the row to `complete` and insert `statusReason`, `evidence` and
`"updatedAt": "2026-07-28"` between `status` and `priority`, matching the
`task-136` / `task-137` key order. Report AC-1, AC-2, AC-4, AC-5, AC-6 and AC-7
satisfied with path anchors, and AC-3 **satisfied in part**, naming (a) the
`dirty === false` gate at `:180-182`, (b) the deliberately unbuilt "data changed"
indicator, (c) CR138-2's consequence — the effect's only user-protection gate is
the DRep input's dirty flag, so a vote type chosen through the dropdown handler at
`:316-323` can be reverted by a later poll — and (d) CR138-3's consequence — branch
1 seeds `dirty: true` with `currentVote.drep.raw`, so a CIP-105 on-chain id fails
`Cardano.DRepID.isValid` (`:198`) and surfaces the input error on an untouched
form. `evidence` source-first, plan docs last. The tracker is tool-managed JSON —
do not run prettier on it.

**CR138R2-2 (major, `VotingPowerDelegation.tsx:177-196`, unpinned by
`VotingGovernancePage.spec.tsx:440-509`) — the dirty gate is the mechanism of
task-138's one deliberately-partial criterion and no test defends it.** Measured
in this worktree by mutation, not argued: rewriting `:180` from
`if (previous.status !== 'form' || previous.drepInputState.dirty) {` to
`if (previous.status !== 'form') {` leaves
`jest --testPathPattern="voting-governance|VotingGovernancePage"` at **3 suites /
36 tests / 7 snapshots, all green**. It cannot be otherwise with the four
prescribed cases: case 1 (`:446`) and case 2 (`:459`) are already `dirty: true` or
byte-identical after the click, so the identity bail-out at `:187-193` returns
`previous` anyway; case 3 (`:474`) never enters the effect body because
`softwareWallet` (`:66-70`) has no `currentVote` and `currentVoteKind === null`
returns at `:178`; case 4 (`:489`) runs with `dirty: false`, where the clause is
inert by construction. The guide locks the clause as load-bearing
(`cv-2-implementation-guide.md:1980-1991`, "keep all four properties") and the
slice DoD records AC-3 as satisfied in part precisely because of it
(`cv-2-PRD.md:1633`) — so the one behaviour the slice is obliged to disclose is
the one behaviour no test can catch a regression in. The guide's own coverage plan
pins AC-3 by case 4 only (`:2257`), so this is test debt the guide did not
foresee rather than a step skipped; it is promoted anyway because a fifth case is
cheap, the harness already supports it, and the alternative is an unenforceable
disclosure. *Fix:* add one case to the `Delegation form pre-fill from the selected
wallet` describe — render with `{ wallets: [votingWallet] }`, click
`wallets-dropdown-option-${VOTING_WALLET_ID}`, type over the seeded input with
`fireEvent.change` (the harness already drives an input this way at `:331-333`),
then `rerenderWithWallets` with a different on-chain `drep.raw`, and assert the
typed value survives and the new id is absent. The mutation above then turns that
case red. Do not restructure the effect.

### Minor (non-blocking; absorb before the task-138 commit)

**CR138R2-3 (minor, `VotingPowerDelegation.tsx:156-163` against
`VotingGovernancePage.spec.tsx:218-361`) — the initializer's currentVote-wins
precedence is untested.** Measured by the same method: replacing the Step 2 body
`selectedVoteType: initialWallet?.currentVote ? seed.selectedVoteType : voteType
|| seed.selectedVoteType, drepInputState: seed.drepInputState` with HEAD's
`selectedVoteType: voteType || initialState.selectedVoteType, drepInputState:
selectedDRepId ? { dirty: true, value: selectedDRepId } :
initialState.drepInputState` also leaves the focused run at **3 / 36 / 7 green**.
The reason is that `selectedWalletId` appears in the spec only as `WALLET_ID` or
`HW_WALLET_ID` (`:222`, `:232`, `:245`, `:264`, `:285`, `:300`, `:314`, `:361`)
and neither fixture (`:66-70`, `:74-78`) carries a `currentVote`, so
`initialWallet?.currentVote` is falsy in every mount in the file and both branches
of the ternary reduce to the same value. This is the precedence the guide argues
about explicitly (`cv-2-implementation-guide.md:4654-4664` — the wallet's own
`currentVote` sits ahead of the inherited directory id, and the Storybook
consequence is "correct behaviour, not a defect"), so it is the branch most likely
to be flipped later by someone reading that consequence as a bug. *Fix:* one
mount-path case — `renderFlow` with
`state: { selectedWalletId: VOTING_WALLET_ID, voteType: 'abstain' }` and
`{ wallets: [votingWallet] }`, asserting the vote-type dropdown reads `drep` and
`VALID_DREP_ID` is displayed. That case is red against the HEAD initializer
(`'abstain'` and a blank input) and green against `:156-163`.

### Merged and dropped

1. *Merged — the unsynchronized tracker row.* Lens 1 raised it as `SPEC-2-1` and
   lens 3 as `QUALITY-2-3`; they are the same defect at the same anchor and are
   consolidated as CR138R2-1, keeping lens 1's fuller `statusReason` content
   requirement (the four named disclosures) and lens 3's DoD citation.
2. *Promoted after independent measurement, not on the lens's word —
   CR138R2-2 and CR138R2-3.* Both lens-3 findings assert that a mutation leaves
   the suite green. Neither was accepted as written: each mutation was applied to
   the working copy in this worktree, the focused pattern re-run, and the file
   restored byte-identically (`cmp` clean) before the entry was written. Both
   claims held at 3 / 36 / 7. CR138R2-3 is kept at **minor** rather than lens 3's
   own minor→ambiguous framing because it pins a mount-time precedence with no
   user-visible partiality attached to it; CR138R2-2 is **major** because the
   untested clause is the exact mechanism the slice DoD forces the team to
   disclose.
3. *Dropped — lens 2's `filterLogData` gap.* `source/common/utils/logging.ts:24-49`
   redacts `drepId`, `dRepId`, `vote` and `voting` but not `votingTarget`,
   `currentVote`, `drep` or `raw`. True, and correctly labelled a forward note by
   the lens that raised it: task-138 adds no sink at all, so nothing in this diff
   can reach that filter, and the renderer-domain `Wallet.currentVote` accessor
   (`domains/Wallet.ts:254-257`) is not handed to a logger anywhere in the change
   set. It is not a task-138 defect. Recorded here so task-147 (AC-5, the
   sanitization sweep) picks it up rather than re-discovering it.
4. *Not re-promoted — CR138-3, the CIP-105 form gate.* Round 1 dispositioned it as
   a disclosure plus a slice-owner escalation, explicitly **not** a code change,
   because re-encoding `raw` would breach the byte-equality invariant. Lens 2
   independently re-derived it this round and reached the same conclusion ("real,
   but not an invariant breach — the code correctly refuses to re-encode"). Its
   owed action is a clause inside CR138R2-1's `statusReason`, so it is carried
   there rather than re-listed as a finding.
5. *Not a blocker — the pre-existing prettier redness.* `prettier 2.1.2 --check`
   is red on `VotingPowerDelegation.tsx` at exactly `:86`
   (`(typeof messages)[keyof typeof messages]`), a line outside every hunk; the
   verification gate proved the delta byte-identical before and after task-138 by
   piping both revisions through the same `--stdin-filepath`. Baseline item 4,
   not a regression. `--write` was correctly never run on either file, and
   `yarn prettier` was never invoked.
6. *Not a blocker — the `[React Intl] Missing message` console noise.* The focused
   run logs `voting.governance.currentVote.status.expiring` from
   `CurrentVoteSummary.tsx:99` and `:111`. That file is untouched by this change
   set, the ids were minted by task-136 at `4880c963d`, and task-146 owns the
   `yarn i18n:manage` run that closes it. Tests pass through the `defaultMessage`
   fallback.

**Gate result and its attribution.** The verification pass returned **PASS with
zero failures**: `tsc --noEmit` exit 0; `yarn lint` exit 0 at 5595 warnings /
0 errors, the `+4` delta over the 5591 baseline measured per-file through
`git show HEAD:<path> | eslint --stdin` and fully attributed to the spec file
(3 `no-explicit-any` matching the file's existing `as any` fixture style, 1
`no-unused-vars` on the `walletId` parameter name in the guide's verbatim Step 5
code) with the production file unchanged at 10; the four prescribed Jest runs
green, including the closing unfiltered `--runInBand` at 85 passed / 1 skipped of
86 suites and 1072 passed / 12 skipped tests; the three invariant greps clean;
`typed-scss-modules` correctly skipped with no `.scss` in the change set; and
`yarn i18n:manage` correctly never run, verified by an empty
`git status --porcelain source/renderer/app/i18n translations`. One attribution
correction: the gate's `git status` snapshot reports two modified files and
`128+` on the spec, whereas the tree reviewed here has three paths and `131`
changed lines on that file — the gate ran **before** the round-1 fix pass landed
the CR138-2 / CR138-4 comment narrowings and before this log was appended. The
three-line delta is exactly those two comments. So the gate's substantive results
were re-run against the reviewed tree rather than inherited: `tsc --noEmit` exit
0, `jest --testPathPattern="voting-governance|VotingGovernancePage"` at 3 / 36 /
7 green, `jest --testPathPattern=governance-sanitization` at 24 / 24 green. The
mutation runs above left the tree byte-identical (`cmp` against a pre-mutation
copy, clean) and moved neither HEAD nor the change set. `nix` is absent, so
`nix fmt` stays an owed pre-merge obligation (F-12). `yarn check:all` and
`yarn storybook:build` were deliberately not run: both are red at HEAD for the
unrelated storybook manager-webpack JSX loader reason and neither is a valid gate.

**Decision: requires_changes** — two major survivors, CR138R2-1 and CR138R2-2,
plus one minor to absorb in the same pass. The production code is accepted as
delivered for the second round running: every guide step is applied verbatim,
every locked invariant re-measured holds, and round 1's two comment minors are
genuinely closed. What is missing is the tracker synchronization the per-task
Definition of Done requires **before** the single commit — which is also the only
place CR138-2's and CR138-3's disclosures can land — and a regression pin on the
one behaviour the slice is obliged to disclose as partial. Neither fix touches the
effect or the seed. Once the row is written (`complete`, AC-3 satisfied in part)
and the two spec cases are added, the task closes with one subject-only commit,
`feat(gov): task-138 pre-fill the delegation form from the wallet current on-chain vote`.

---

## Code Review: 2026-07-28 — task-138 round 3

**Scope reviewed.** The uncommitted working tree after the round-2 fix pass,
against the guide section "task-138: Pre-fill `VotingPowerDelegation` from the
current on-chain delegation" (`cv-2-implementation-guide.md:1751-2277`) — its
locked invariants (`:1795-1814`), its seven resolved judgment calls
(`:1816-1850`), its seven ordered steps (`:1852-2246`) and its acceptance record
(`:2248-2274`) — task-138's seven acceptance criteria in
`governance-drep-discovery-plan-tasks.json:1222-1243`, decision D-11
(`cv-2-PRD.md:740-750`), the slice DoD row for task-138 AC-3 (`:1633`), the
per-task Definition of Done (`:1619-1623`) and F-16
(`research/cv-2-findings.md:906-947`). HEAD is `31cadffd9`. Three independent
lenses were consolidated here — guide conformance, locked invariants and
sanitization, and quality plus comment convention. The main checkout
`/workspaces/daedalus` was never read, edited or run against, and this round
fixed no code.

**What landed.** `git status --porcelain` → the same three paths as round 2:
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`
(`75+`), `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`
(`181+`, up from round 2's `131` changed lines — the two review-mandated cases),
and this log (`468+/0-`, a pure append). `git diff --stat` totals 3 files,
`712` insertions, `12` deletions. No untracked file, no `.scss`, no
`package.json` / `yarn.lock`, and `git status --porcelain source/renderer/app/i18n
translations` is empty. The tracker is **not** in the change set, for the third
round running.

**Round 2's two test-debt findings are closed, and both pins discriminate.**
Neither claim was accepted on a lens's word, and neither was re-mutated in this
round — a mutate-and-restore cycle on an uncommitted tree buys nothing here
because both cases are decidable by reading the shipped code:

- CR138R2-2 is closed by `'leaves a typed DRep id untouched when a refreshed
  snapshot carries a new vote'` (`VotingGovernancePage.spec.tsx:513-540`). The
  click seeds `{ dirty: true, value: VALID_DREP_ID }` through branch 1
  (`VotingPowerDelegation.tsx:112-117`), `fireEvent.change` overwrites the value
  with `dirty: true`, and the re-render swaps `drep.raw` to `OTHER_DREP_ID`, so
  `currentVoteDRepId` changes and the effect body runs. With `:180`'s
  `|| previous.drepInputState.dirty` present the updater returns `previous` at
  `:181`; with it removed the identity bail-out at `:187-193` cannot fire —
  `previous.drepInputState.value` is the typed id and `seed.…value` is
  `OTHER_DREP_ID` — so `{ ...previous, ...seed }` lands and
  `getByDisplayValue(typedDRepId)` at `:538` goes red. The clause is now pinned.
- CR138R2-3 is closed by `'prefers the wallet current vote over the inherited vote
  type and DRep id on mount'` (`:542-559`). `location.state` carries
  `voteType: 'abstain'` and `selectedDRepId: OTHER_DREP_ID` while the wallet
  carries a `drep` current vote, so the ternary at `:160-162` is the only thing
  separating the two outcomes: against HEAD's initializer the dropdown reads
  `abstain` and the input holds `OTHER_DREP_ID` with `dirty: true`, and the mount
  effect then bails at `:180`, leaving both assertions at `:557-558` red.

**The code deliverable is re-confirmed for the third round.** All seven steps
remain applied as written: `deriveFormSeed` at module scope with the four
branches in order (`:106-137`; branch 1 `:112-117`, branch 2 `:119-124`, branch 3
`:126-131`, branch 4 `:133-136`); the initializer's currentVote-wins precedence
(`:150-165`); the seed spread **after** `...initialState` in
`WalletsDropdown.onChange` (`:301`); the re-seed effect with all four locked
properties intact (`:177-196`); the Step 5 mock, the Step 6 fixtures — which do
reuse `VALID_DREP_ID` as Step 6(a) requires (`VotingGovernancePage.spec.tsx:84-100`)
— and the `rerenderWithWallets` hook (`:206`). Locked invariants re-measured in
this worktree, not inherited: `grep -n "GovernanceStore"
source/renderer/app/stores/VotingStore.ts` → nothing; `grep -nE
"toLowerCase|trim\(|normalizeDRepIdentity"` on the component → nothing; `grep -nE
"logger|analytics|Analytics|console\.|electron-store|localStorage"` on the
component → nothing. Branch 4 keeps the no-vote-no-inheritance form blank, so no
auto-delegation, and `governance-sanitization` is 24/24 green.

### Blockers (ranked, most severe first)

**CR138R3-1 (major, `governance-drep-discovery-plan-tasks.json:1222-1243`) —
task-138's row is still `pending`, so AC-3's deliberate partiality is recorded
nowhere.** Carried unresolved from CR138-1 (round 1) and CR138R2-1 (round 2), and
re-verified here rather than restated: the row's keys are still `id, title,
description, status, priority, estimatedHours, dependencies, targetPath,
acceptanceCriteria`, with `"status": "pending"` at `:1226` and no `statusReason`,
no `evidence`, no `updatedAt`; `git status --porcelain` on that path returns
nothing, so the file is not in the change set at all. The sibling `task-137` row
(`:1199-1210`) carries all four keys in the order `status, statusReason,
evidence, updatedAt, priority`, so the convention is established and this row is
the outlier. The gap is substantive: the row's own AC-3 text promises the form
"re-seeds (or surfaces a \"data changed\" indicator)" — **anchor corrected, that
text is at `:1237`; rounds 1 and 2 both cited `:1229`, which is the `dependencies`
key** — while the delivered effect returns `previous` whenever
`previous.drepInputState.dirty` is true
(`VotingPowerDelegation.tsx:180-182`) and no indicator exists (D-11,
`cv-2-PRD.md:740-750`; guide `:1816-1820`). Flipping the row to `complete` with no
`statusReason` would report a scoped criterion green, which the slice DoD forbids
in as many words (`cv-2-PRD.md:1633`), and the guide's acceptance record says
"Record both in the task's `statusReason`" (`:2255-2261`). The per-task DoD
(`cv-2-PRD.md:1619-1623`) requires the row synchronized **before** the single
commit, and no later cv-2 row owns it.
*Fix:* set the row to `complete` and insert `statusReason`, `evidence` and
`"updatedAt": "2026-07-28"` between `status` and `priority`, matching the
`task-136` / `task-137` key order. Report AC-1, AC-2, AC-4, AC-5, AC-6 and AC-7
satisfied with source-first path anchors, and AC-3 **satisfied in part**, naming
(a) the `dirty === false` gate at `:180-182`; (b) the deliberately unbuilt "data
changed" indicator; (c) CR138-2's consequence — the effect's only user-protection
gate is the DRep input's dirty flag, so a vote type chosen through the
`ItemsDropdown` handler at `:315-321`, which never sets that flag, can be reverted
by a later poll; and (d) CR138-3's consequence — branch 1 seeds `dirty: true` with
`currentVote.drep.raw`, so a CIP-105 on-chain id fails `Cardano.DRepID.isValid`
(`:198`) and surfaces the input error on an untouched form, which is the
byte-equality invariant working as specified rather than a defect. Two further
clauses this round adds: the spec file ships **14** `it(` blocks against the
guide's predicted 12 (CR138R3-3), and branch 2's blanking ships structurally
unpinned (CR138R3-2). `evidence` source-first, plan docs last. The tracker is
tool-managed JSON — do not run prettier on it, and use `2026-07-28` for
`updatedAt` to match every sibling row in this slice.

### Minor (non-blocking; absorb in the same fix pass)

**CR138R3-2 (minor, `VotingGovernancePage.spec.tsx:461-474`) — the sentinel
case's second assertion cannot fail, so branch 2's blanking ships unpinned.**
`'seeds the vote type and no DRep id from a sentinel on-chain vote'` closes with
`expect(screen.queryByDisplayValue(VALID_DREP_ID)).toBeNull();` (`:473`), but the
DRep input is rendered only behind `{selectedWallet && state.selectedVoteType ===
'drep' && (<Input` (`VotingPowerDelegation.tsx:326`). Once the first assertion has
established that the dropdown reads `abstain`, `state.selectedVoteType !== 'drep'`,
the input is not in the DOM, and any display-value query returns null — so the
second assertion passes on every outcome in which the first one passes. Round 2
recorded branch 2 (`:119-124`) as blanking `drepInputState` "so a sentinel never
enters the id field" (`cv-2-code-review.md:2032-2035`); that claim is true of the
code and untested by this case. Kept at **minor**, and the lens's first proposed
fix — delete `:473` — is **not** adopted: the line is guide-verbatim (Step 6(c)
case 2), and the guide also closes off the obvious pin by locking the
`ItemsDropdown` mock ("This is the **only** permitted change to this mock; the
`ItemsDropdown` mock (`:40-44`) is untouched"), so the vote type cannot be driven
back to `drep` from a test.
*Fix (either is acceptable):* record in CR138R3-1's `statusReason` that branch 2's
blanking is structurally unpinned and why the mock constraint blocks a direct pin;
or add one case that observes it through a permitted seam — select the abstain
wallet, then `rerenderWithWallets` the same id carrying
`{ kind: 'drep', drep: { raw: VALID_DREP_ID, credentialType: 'key' }, source:
'onchain' }` and assert `VALID_DREP_ID` is displayed, which goes red if branch 2
ever leaks a sentinel with `dirty: true`, because the `:180` guard would then
suppress the re-seed. The second option is a partial pin only (a leak with
`dirty: false` still re-seeds), which is why neither option is mandated.

**CR138R3-3 (minor, `cv-2-implementation-guide.md:2232`, plus Step 6(c)'s case
list and the acceptance record's AC-3 line at `:2257`) — the guide's verification
expectation is stale by exactly +2 cases.** Step 7 predicts
"`VotingGovernancePage.spec.tsx` grows from 8 to 12 tests" and Step 6(c)
prescribes four cases; the delivered file carries 14 `it(` blocks against 8 at
HEAD (`git show HEAD:… | grep -c "  it("` → 8; working copy → 14), and the focused
pattern reports 38 tests where the stale gate reported 36. This is **not** an
F-16-style misread — the guide's parenthetical is correct at HEAD and its `+4` is
correct for the four prescribed cases; the `+2` is over-delivery mandated by
CR138R2-2 and CR138R2-3 in this log (`:2073-2115`). Reconciled here, so the count
is derivable from the log alone: `8 + 4 (guide) + 2 (round-2 review) = 14`, and
the slice-wide full-suite figure moves `1072 → 1074` for the same reason.
*Fix:* add the same arithmetic to CR138R3-1's `statusReason` so the row does not
read as a regression against Step 7. Editing the guide itself (Step 7's comment to
`grows from 8 to 14 tests`, Step 6(c)'s case list, and AC-3's "pinned by Step 6
case 4" to name the dirty-gate case as the second pin) is **deferred, not
mandated**: the per-task DoD allows task-138 exactly one commit, and the cv-1
precedent for this kind of reconciliation is its own docs commit
(`a3e352841`, "docs(gov): reconcile the cv-1 guide task-134 step-1 block with
acceptance criterion AC-7"). Whoever runs the slice-close doc pass owns it.

**CR138R3-4 (minor, `VotingGovernancePage.spec.tsx:32-34`) — the mock preamble
claims an `onChange` drive path for both dropdowns, and only one has one.** The
comment reads "The wallet and vote-type dropdowns are react-polymorph-heavy; the
flow tests assert the values they receive and drive selection through the mock's
onChange, so lightweight mocks are enough." Its subject is both mocks, but
`ItemsDropdownMock` (`:57-61`) is `function ItemsDropdownMock(props: { value:
string })` returning a single `div` — no handler at all, and the guide requires it
stay that way. The wording is round 1's CR138-4 fix, which correctly removed the
ALL-CAPS `RECEIVE` and the inaccurate "pass-through"; it over-corrected in the
other direction, and the capability it claims for the vote-type mock is exactly
the capability whose absence makes CR138R3-2 unfixable the obvious way.
*Fix:* split the clause so each half maps onto what that mock actually does — the
vote-type mock renders only the value the flow asserts, the wallet mock also
exposes `onChange` so a selection can be driven. Three lines, sentence case,
matched to the surrounding style by hand: this is a pre-existing file, so no
`prettier --write`.

### Merged and dropped

1. *Merged — the unsynchronized tracker row.* Lens 1 raised it as `SPEC-3-1` and
   lens 3 as `QUALITY-3-1`; same defect, same anchor, consolidated as CR138R3-1.
   Lens 1's four-part `statusReason` content requirement and lens 3's DoD citation
   are both kept, and two clauses are added from this round's minors.
2. *Corrected inside CR138R3-1 — two anchors.* Lens 3 prescribed
   `"updatedAt": "2026-07-29"`; every sibling row in this slice and both earlier
   rounds use `2026-07-28`, which is the date this round is filed under, so the
   fix text says `2026-07-28`. And the row's AC-3 string is at `:1237`, not the
   `:1229` that lenses 1 and 3 both cite (inherited from round 2's entry); `:1229`
   is the `dependencies` key. The finding is unaffected either way.
3. *Narrowed rather than dropped — CR138R3-2 and CR138R3-3.* Both lens-3 fixes
   were adopted only in their record-it form. Deleting the guide-verbatim
   assertion at `:473` and editing a frozen guide section are each drift against
   an approved contract, and neither is worth taking inside task-138's single
   commit; the disclosure route discharges both, and the optional pins are
   written down so a later pass can take them deliberately.
4. *Nothing dropped outright, and no production-code finding was raised.* Lens 2
   returned **approved** with zero blockers and its cleared items are recorded so
   they are not re-litigated: the added `OTHER_DREP_ID` (`:65-66`) does not breach
   Step 6(a)'s "do not mint a new id", which is scoped to the wallet fixtures and
   they do reuse `VALID_DREP_ID` (`:84-100`); `deriveFormSeed` sharing
   `initialState.drepInputState` by reference (`:122`, `:135`) is inert because
   every `setState` builds a fresh object and the aliasing pre-existed at HEAD;
   and the upstream sink at `api.ts:3016-3018` still logs a pattern-gated HRP
   only.
5. *Noted, not promoted — the unreachable third argument.* Inside the effect,
   `deriveFormSeed(selectedWallet, initialFormState?.selectedDRepId)` (`:183-186`)
   can never read `inheritedDRepId`: `currentVoteKind === null` returns at `:178`,
   so the wallet always has a `currentVote` by the time the call is made and only
   branches 1 and 2 are reachable. It is guide-verbatim (Step 4), harmless, and
   removing it would be a gratuitous deviation from an approved step.
6. *Not a blocker — the pre-existing prettier redness.* Re-measured in this round
   on both revisions through the identical
   `prettier --stdin-filepath <realpath>`: the working copy and the HEAD copy each
   yield the **same** single delta, `:86`
   `(typeof messages)[keyof typeof messages]` → `typeof messages[keyof typeof
   messages]`, a line outside every hunk. The 75 added production lines contribute
   zero new deltas, and the spec file passes `--check` outright. Baseline item 4;
   `--write` was correctly never run and `yarn prettier` never invoked.
7. *Not a blocker — the `[React Intl] Missing message` console noise.* The focused
   run still logs `voting.governance.currentVote.status.expiring` from
   `CurrentVoteSummary.tsx:99` and `:111`. That file is untouched by this change
   set, the ids were minted by task-136 at `4880c963d`, and task-146 owns the
   `yarn i18n:manage` run that closes it. Tests pass through the `defaultMessage`
   fallback.

**Gate result and its attribution.** The supplied verification gate reports
**PASS with zero failures**, but it is **stale** against the reviewed tree in the
same way round 2's was: it snapshots two modified files, `128+` on the spec and
`36` tests, whereas this tree has three paths, `181+` on the spec and `38` tests —
it ran before the round-2 fix pass landed CR138R2-2's and CR138R2-3's cases. Every
substantive gate was therefore re-run here rather than inherited:
`node_modules/.bin/tsc --noEmit` exit 0, zero diagnostics;
`jest --testPathPattern="voting-governance|VotingGovernancePage" --no-coverage
--runInBand` → **3 suites / 38 tests / 7 snapshots**, all green;
`jest --testPathPattern=governance-sanitization` → **24/24** green; the unfiltered
closing gate `jest --no-coverage --runInBand` → **85 passed / 1 skipped of 86
suites, 1074 passed / 12 skipped of 1086 tests, 9 snapshots**, zero FAIL lines —
`+2` tests over the gate's 1072/1084, attributable line-for-line to the two
review-mandated cases and to nothing else. `yarn lint` exit 0 at **5595 warnings /
0 errors**, identical to the number the gate measured and attributed, so the two
new cases added no warning of their own. The three invariant greps are clean.
`typed-scss-modules` was correctly skipped (no `.scss` in the change set) and
`yarn i18n:manage` correctly never run, verified by an empty
`git status --porcelain source/renderer/app/i18n translations`. The 12 skipped
tests are still `GovernanceCliArgvSmoke.spec.ts:28`'s environment self-skip. This
round moved neither HEAD nor the change set: no file was edited except this log.
`nix` is absent, so `nix fmt` stays an owed pre-merge obligation (F-12).
`yarn check:all` and `yarn storybook:build` were deliberately not run — both are
red at HEAD for the unrelated storybook manager-webpack JSX loader reason and
neither is a valid gate.

**Decision: requires_changes** — one major survivor, CR138R3-1, plus three minors
to absorb in the same pass. The production code is accepted as delivered for the
third round running, and the round-2 test debt is genuinely closed with two cases
that both discriminate their target branch. What remains is entirely
documentation: the tracker row the per-task Definition of Done requires **before**
the single commit, which is the only place AC-3's partiality, CR138-2's and
CR138-3's consequences, the unpinned branch 2 and the `8 → 14` test-count
reconciliation can land, plus one three-line comment correction in the spec. No
fix touches `deriveFormSeed`, the initializer, the `onChange` seed or the effect.
Once the row is written (`complete`, AC-3 satisfied in part) and CR138R3-4 is
applied, the task closes with one subject-only commit,
`feat(gov): task-138 pre-fill the delegation form from the wallet current on-chain vote`.

---

## Code Review: 2026-07-28 — task-139 round 1

**Scope reviewed.** The uncommitted working tree against the guide section
"task-139: Mount `CurrentVoteSummary` in the delegation form"
(`cv-2-implementation-guide.md:2306-2617`) — its no-spec-file scope boundary
(`:2398-2400`), its six build steps (`:2404-2555`), its Step 7 verification block
(`:2557-2589`) and its acceptance record (`:2595-2617`) — task-139's three
acceptance criteria in `governance-drep-discovery-plan-tasks.json:1252-1269`, the
PRD's task-139 row (`cv-2-PRD.md:153`), the AC-3 split under D-5 (`:206-209`,
`:450`), the two scoped-criterion rows for task-139 (`:1634-1635`) and the
per-task Definition of Done (`:1619-1623`). HEAD is `144c5153d`. Three independent lenses were
consolidated here — guide conformance and runtime correctness, locked invariants
and the sanitization floor, and quality plus comment convention. The main
checkout `/workspaces/daedalus` was never read, edited or run against, and this
round fixed no code.

**What landed.** `git diff --stat` → 3 files, `122` insertions, `2` deletions:
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx`
(`+24`), `source/renderer/app/containers/voting/VotingGovernancePage.tsx`
(`+12/-2`) and `research/cv-2-findings.md` (`+88`, the new F-18). No untracked
file, no `.scss`, no locale or catalog write, and the tracker JSON is **not** in
the change set.

**The code deliverable is clean, and all three lenses agree on that.** Steps 1-5
are applied: the `CurrentVoteSummary` and `resolveExactDRepMatch` imports plus the
`import type { AppDRepDirectoryEntry }` at `VotingPowerDelegation.tsx:22-24`; the
optional `drepIndex` prop at `:54` with the module-scope `EMPTY_DREP_INDEX`
default bound at `:149`, so no `new Map()` is minted per render; the lookup at
`:209-215`; the unconditional mount at `:329-332`, a sibling of `WalletsDropdown`
and **outside** the `{selectedWallet && (` gate at `:334`, so the `noDelegation`
branch renders with no wallet selected; and the exploded stores destructure plus
`drepIndex={governance.drepIndex}` in the container (`VotingGovernancePage.tsx:38-46`,
`:73`). Step 3 is applied differently from the guide's literal snippet and is
value-identical: it reuses task-138's `const currentVote = selectedWallet?.currentVote ?? null;`
(`:179`) instead of re-deriving the expression, which also keeps the type aligned
with `CurrentVoteSummary`'s `currentVote: WalletVotingTarget | null`. `submitButtonDisabled`
(`:223`), `chosenOption`, the form-state shape and the seed chain are
byte-identical to HEAD; `currentDRepEntry` is referenced exactly once, at `:331`,
as a display prop, so nothing from the index reorders, filters or gates the form.
Invariant 4 holds (`grep -n "GovernanceStore" source/renderer/app/stores/VotingStore.ts`
→ nothing, and the index is prop-drilled, never injected); invariant 10 holds
(`cip129 ?? raw` is a read-only lookup key, no `toLowerCase`/`trim`/`normalize`);
the sanitization floor is untouched — the diff adds no `logger.`, `analytics`,
`console.` or `electron-store` call, and the only sink the mount newly makes
reachable is `DRepIdDisplay.tsx:53`/`:64`, which logs `drepIdLength` and never the
bech32 string. No `reaction`, no `autorun`, no poll: reactivity comes from the
`@observer` container plus `GovernanceStore`'s reassignment of `drepIndex`
(`GovernanceStore.ts:254`, `:297`).

**What fails this round is documentation the diff itself introduces, plus the
tracker step the build skipped.**

### Blockers (ranked, most severe first)

**CR139-1 (blocker, `research/cv-2-findings.md:1026-1111`) — F-18's headline
claim is refuted by a green in-repo jsdom spec, and the root cause, the
prescription and the two downstream constraints built on it are all unproven.**
F-18 asserts as measured fact that "`resolveExactDRepMatch` returns `null` for
**every** input under the jsdom test environment, so no component spec can pin the
`drepIndex` → `drepEntry` → badge chain" (`:1026`), that under jsdom
`Cardano.DRepID.toCip129DRepID` "**throws** — `radix2.encode input should be
Uint8Array`" (`:1046-1060`), and concludes "Treat the positive chain as pinnable
only in a `@jest-environment node` spec" (`:1101`), with `task-173 (must re-plan
its badge cases)` and `task-147` named as owners (`:1108-1111`). Measured
here, first-hand: `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
carries **no** `@jest-environment` docblock — its first line is `import React from 'react';`
— so `jest.config.js:147`'s `testEnvironment: 'jest-environment-jsdom'` applies,
and `jest.config.js` defines no `projects` and no `testEnvironmentOptions` that
could vary it. `node_modules/.bin/jest --testPathPattern="DRepDirectory.spec"
--no-coverage --runInBand -t "canonicalizes an exact CIP-105 match"` → **1 passed**,
46 skipped. That case (`:540`) types `realCip105Id(1)` and asserts
`onViewDetails` was called once with `realDrepId(1)`; the only non-click call site
is `DRepDirectory.tsx:192-198`, `const match = resolveExactDRepMatch(searchQuery, drepIndex);
if (match) { … onViewDetails(match.drepId); }`. It can only pass if
`toCip129DRepID` succeeded under jsdom **and** the canonicalized key hit the map —
i.e. the exact positive chain F-18 declares impossible, already pinned by a
component spec. `:528` (exact CIP-129) passes for the same reason. The
symptom F-18 measured is real and reproduced here — the container suite logs
`voting.governance.currentVote.status.unavailable` **7** times and
`…status.expiring` **0** times — but the count is not a clean signal either: at
least one of those renders is a *correct* `unavailable`, since
`VotingGovernancePage.spec.tsx:532` re-renders with `drep.raw = OTHER_DREP_ID`,
which is deliberately not a key in `buildStores`' one-entry index (`:128`). And
the fixture id itself canonicalizes to itself — probed directly in this worktree,
`Cardano.DRepID.toCip129DRepID(Cardano.DRepID('drep1ygqqq…7vlc9n'))` returns the
same string with `isValid` true — so the encoder is not the discriminator the note
says it is. The finding is filed as a blocker not because task-139's code is
wrong, but because F-18 is written as a **standing constraint binding on task-173
and task-147** (`:1096-1111`) that would strip the guide's self-declared "only
executable pin on the `drepIndex` → `drepEntry` → badge chain"
(`cv-2-implementation-guide.md:5307-5314`) on a diagnosis a passing spec
contradicts.
*Fix:* rewrite F-18 to claim only what was measured — in
`VotingGovernancePage.spec.tsx` the lookup resolves `null` on the drep-branch
renders that use `VALID_DREP_ID` (7 `unavailable`, 0 `expiring`, with the
`OTHER_DREP_ID` re-render legitimately among them). Delete the "every input under
the jsdom test environment" headline, the `radix2.encode` root-cause paragraph and
the "pinnable only in a `@jest-environment node` spec" prescription; reconcile
against `DRepDirectory.spec.tsx:528` and `:540` before recording any constraint,
and state the cause as **unidentified**. Leave the disposition open and remove
task-173 and task-147 as owners of a remedy that a green jsdom spec refutes; a
narrower re-measurement is the right hand-off, not a re-plan.

**CR139-2 (major, `governance-drep-discovery-plan-tasks.json:1252-1269`) — guide
Step 6 was not performed, and the row is still `pending` with no `statusReason`.**
Two halves of one edit, both verified as undone: (a) Step 6 (`:2526-2555`)
instructs replacing task-139's third `acceptanceCriteria` string with "The panel
updates reactively when `drepIndex` is populated or updated; no wallet re-poll is
triggered." The old string — carrying the `givenName` read and the
unverified→verified Storybook clause — is still in the file at `:1267`, and the
acceptance record's own checkbox is written as "**AC-3 (as rewritten in Step 6)**"
(`:2606`), so the criterion cannot be reported against until the rewrite lands.
(b) The row's keys are `id, title, description, status, priority, estimatedHours,
dependencies, targetPath, acceptanceCriteria` with `"status": "pending"` at
`:1256` and no `statusReason`, no `evidence`, no `updatedAt`; the sibling
`task-137` row (`:1199-1210`) carries all four in the order `status,
statusReason, evidence, updatedAt, priority`, so the convention is established and
this row is the outlier. The per-task Definition of Done (`cv-2-PRD.md:1619-1623`)
requires the tracker synchronized **before** the single commit, and no later cv-2
row owns either half. Raised by the correctness lens as a deliberately
non-blocking hand-off; promoted here because the guide places the string rewrite
inside task-139's ordered steps rather than in the Scribe step — the Scribe owns
only the *reason* text — and because every prior round of this log has upheld the
unsynchronized row as blocking (CR136-1, CR137-1, CR138-1, CR138R2-1, CR138R3-1).
*Fix:* hand-edit the one `acceptanceCriteria` string per Step 6, preserving
surrounding formatting, then set the row to `complete` and insert `statusReason`,
`evidence` and `"updatedAt": "2026-07-28"` between `status` and `priority` in the
`task-136`/`task-137` key order. Report AC-1 satisfied (unconditional mount at
`VotingPowerDelegation.tsx:329-332`, outside the `selectedWallet` gate at `:334`),
AC-2 **satisfied in part** with the Storybook visual recorded **OWED** to task-145
(`cv-2-PRD.md:1634`; guide `:2601-2605`), and AC-3-as-rewritten satisfied via the
`@observer` container plus the `drepIndex` reassignment. Record the struck clauses
as deferred to `anchor-2`, naming `GovernanceStore.ts:20-31` as the evidence, per
the acceptance record's fourth checkbox (`:2611-2613`). Note also that the guide's
own `:1241` anchor for the string is stale — see CR139-3. This JSON is
tool-managed: never run prettier on it.

### Minor (non-blocking; absorb in the same fix pass)

**CR139-3 (minor, `research/cv-2-findings.md:1088-1094`) — F-18's AC-3 paragraph
cites a tracker line that is now a different key.** The note reads, at `:1093`,
"the AC-3 string at `governance-drep-discovery-plan-tasks.json:1241`". Verified:
`:1241` is
`"targetPath": "source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx"`,
and the `givenName` string is at `:1267`. The number is the guide's pre-137/138
numbering copied without re-verification, which is the trap the guide itself warns
about ("Re-anchor by the quoted content, never by the number",
`cv-2-implementation-guide.md:2291-2292`). Co-located with CR139-1's rewrite, so
it costs nothing to take in the same pass.
*Fix:* cite the quoted string, or `:1267`, so the hand-off cannot land on the
wrong row.

**CR139-4 (minor, `VotingPowerDelegation.tsx:207-208`) — the comment names the
wrong rejection mechanism.** It reads "The index is keyed by CIP-129, and a
CIP-105 raw id fails the **checksum** validation the lookup gates on, so the
CIP-129 form is queried when present." The gate is
`if (!Cardano.DRepID.isValid(full)) return null;` (`helpers.ts:144`), and the doc
comment on the very function the line calls says the opposite about checksums:
"checksum-valid full IDs of **either** encoding are canonicalized to CIP-129 (the
store's key form) before lookup" (`helpers.ts:134-137`). Probed here: the id the
rule actually excludes is `drep_vkh1…`, and it fails on the HRP, not a bad
checksum — `Cardano.DRepID.isValid('drep_vkh15xev84…u4a4l')` → `false`, while
`Cardano.DRepID.isValid(Cardano.DRepID.cip105FromCredential(…))` → `true` for the
`drep1…` form, which `helpers.spec.ts:192-194` pins as canonicalizing to the same
entry. The quality lens filed this as **major** on the strength of that spec;
recalibrated down here because the *encoding label* is guide-sanctioned — the
guide itself calls `drep_vkh1…` a "CIP-105 `raw`" (`cv-2-implementation-guide.md:2385-2391`)
and "the CIP-105 encoding" (`:5317`) — so the only unambiguous error is the
mechanism word, and the code it explains is correct.
*Fix:* replace "fails the checksum validation" with the real gate, e.g. "…and
`raw` can be the `drep_vkh1…` encoding, which the lookup's ID-validity gate
rejects, so the CIP-129 form is queried when present." — or drop the comment and
let `cip129 ?? raw` stand.

### Merged and dropped

1. *Nothing dropped outright from a filed blocker — all three lens findings
   survived adjudication,* though two were re-scoped. The correctness lens and the
   invariants/sanitization lens both returned **approved** with zero blockers, and
   their independent confirmations of the code deliverable are the reason no
   production-code finding appears above.
2. *Adjudicated between two lenses — the `:207-208` comment.* The invariants lens
   explicitly **cleared** it ("the comment mirrors the guide's own sanctioned
   wording"), the quality lens filed it as **major**. Both are partly right: the
   guide does use "CIP-105" for `drep_vkh1…`, so that half of the clearance holds,
   but no reading of "CIP-105" rescues "checksum" — `drep_vkh1…` has a valid
   checksum and fails on the HRP. Kept, narrowed to the mechanism word, filed
   minor (CR139-4).
3. *Promoted, not dropped — guide Step 6.* The correctness lens filed it as a
   "NON-BLOCKING HANDOFF … the tracker row is out of scope for this round". It is
   in scope: Step 6 is one of task-139's six ordered build steps, its acceptance
   checkbox is phrased "AC-3 (as rewritten in Step 6)", and the per-task DoD puts
   the tracker before the commit. Promoted to CR139-2 and widened to the whole
   row, consistent with every prior round of this log.
4. *Not a blocker — the mount is inert under jsdom in the container spec.* Both
   the gate and one lens escalated it. Confirmed as a symptom (7 `unavailable` /
   0 `expiring`, reproduced here) and recorded as a **note**, not a defect of this
   task: the guide states at `:2398-2400` that task-139 adds **no spec file** and
   that its gate is "every existing suite stays green" plus the Step 7 greps, so a
   missing pin is out of scope by contract. What *is* a defect this round is the
   diagnosis written into F-18 — see CR139-1.
5. *Not a blocker — the pre-existing prettier redness.* `VotingPowerDelegation.tsx`
   yields the same single delta on the working copy and on the HEAD blob, at
   `:90`'s `(typeof messages)[keyof typeof messages]`, a line outside every hunk;
   `VotingGovernancePage.tsx` went from 10 deltas at HEAD to 0, because the
   hand-written exploded destructure happens to match prettier 2.1.2's output.
   Both files are pre-existing, `--write` was correctly never run and
   `yarn prettier` never invoked. Baseline item 4.
6. *Not a blocker — the `[React Intl] Missing message` console noise.* The
   `voting.governance.currentVote.status.*` ids were minted by task-136 and the
   catalogs are seeded by task-146; tests pass through the `defaultMessage`
   fallback. The four `console.warn` records in `GovernanceQueryService.spec.ts`
   are the documented era-fallback path.
7. *Cleared and recorded so it is not re-litigated.* The invariants lens verified
   that no `abstain` / `no_confidence` literal leaves `intl.formatMessage`
   (`CurrentVoteSummary.tsx:139-141`), that the **full** `drepIndex` rather than
   the default cohort is queried so a top-35 DRep still resolves, that `expiring`
   stays renderer-derived (`CurrentVoteSummary.tsx:26-38`) and that no new copy
   string was minted.

**Gate result and its attribution.** The supplied verification gate reports
**PASS with zero failures** and matches the reviewed tree exactly — 3 modified
paths, `122`/`2`, HEAD `144c5153d` — so it was accepted, with the two load-bearing
numbers re-measured here rather than inherited.
`node_modules/.bin/jest --testPathPattern="VotingGovernancePage" --no-coverage
--runInBand` reproduces **7** `status.unavailable` records and **0**
`status.expiring`, and
`--testPathPattern="DRepDirectory.spec" … -t "canonicalizes an exact CIP-105 match"`
is **1 passed** — the two facts CR139-1 turns on. The gate's own greens stand:
`tsc --noEmit` exit 0; `jest --testPathPattern="voting-governance|VotingGovernancePage"`
→ 3 suites / 38 tests / 7 snapshots; `jest --testPathPattern=governance` → 15 of
16 suites, 260 passed / 12 skipped; the wave pattern `"(governance|voting)"` →
17 of 18 suites, 282 passed / 12 skipped / 9 snapshots, **numerically identical**
to the wave baseline; `yarn lint` exit 0 at 5595 warnings / 0 errors, with both
changed files proven warning-identical to their HEAD blobs through
`eslint --stdin --stdin-filename`, so the delta is `+0`. `typed-scss-modules` was
correctly skipped (no `.scss` in the change set) and `yarn i18n:manage` correctly
never run, verified by a clean `git status` on `source/renderer/app/i18n` and
`translations`. The 12 skipped tests are `GovernanceCliArgvSmoke.spec.ts`'s
environment self-skip. All four Step 7 greps pass, including the
`CurrentVoteSummary` one whose two hits are substring false positives
(`injectIntl`, `import type`) — `grep -n mobx` on that file returns nothing. This
round moved neither HEAD nor the change set: no file was edited except this log.
`nix` is absent, so `nix fmt` stays an owed pre-merge obligation (F-12).
`yarn check:all` and `yarn storybook:build` were deliberately not run — both are
red at HEAD for the unrelated storybook manager-webpack JSX loader reason and
neither is a valid gate.

**Decision: requires_changes** — one blocker, CR139-1, one major, CR139-2, and two
minors to absorb in the same pass. The production code is accepted as delivered
and no fix below touches it beyond a two-line comment: the mount, the lookup, the
prop plumbing and the untouched `submitButtonDisabled` / `chosenOption` / seed
chain are confirmed by all three lenses and by a re-measured gate. What must
change is what this task wrote *about* the repo — an F-18 whose universal claim a
green jsdom spec refutes and whose prescription would cost task-173 its only badge
pin — and the tracker step the build skipped, which is the only place AC-2's
**OWED** Storybook check and AC-3's D-5 split can be recorded truthfully. Once
CR139-1 and CR139-2 land and the two minors are absorbed, the task closes with one
subject-only commit,
`feat(gov): task-139 mount CurrentVoteSummary in the delegation form`.

---

## Code Review: 2026-07-28 — task-139 round 2

**Scope reviewed.** The uncommitted working tree after round 1's fix pass, against
the same contract as round 1 — the guide section
`cv-2-implementation-guide.md:2306-2617` (no-spec-file scope at `:2398-2400`, six
build steps at `:2404-2555`, Step 7 at `:2557-2589`, acceptance record at
`:2595-2617`), task-139's tracker row, and the PRD's D-5 split and per-task DoD
(`cv-2-PRD.md:1619-1623`, `:1634-1635`). HEAD is still `144c5153d`; no commit was
made. Three independent lenses ran again — guide conformance and runtime
correctness, locked invariants and the sanitization floor, and tests plus
simplicity and drift — and **all three returned `approved` with zero blockers**.
This round's work was therefore adjudicating the four round-1 findings against the
files rather than triaging new ones; every discharge below was re-verified
first-hand, not accepted on report. The main checkout `/workspaces/daedalus` was
never read, edited or run against, and this round fixed no code.

**What landed since round 1.** `git status --porcelain` → 5 modified, 0 untracked,
0 staged. `git diff --numstat`:
`governance-drep-discovery-plan-tasks.json` `10/2`,
`research/cv-2-findings.md` `128/0`,
`task-plans/cv-2-code-review.md` `263/0` (the round-1 entry, itself still
uncommitted),
`VotingPowerDelegation.tsx` `24/0`,
`VotingGovernancePage.tsx` `10/2`. The production diff is unchanged from round 1
apart from the two comment lines CR139-4 asked for; the tracker row and the F-18
rewrite are the new material.

### Blockers

**None.** All four round-1 findings are discharged.

**CR139-1 (was blocker) — discharged, and the diagnosis is now measured rather
than asserted.** F-18 (`research/cv-2-findings.md:1026-1153`) no longer claims the
lookup fails for *every* input under jsdom, and no longer prescribes
`@jest-environment node` as the only route. Its new thesis is a realm split that
an in-repo shim already defeats, and its four load-bearing anchors check out:
`DRepDirectory.spec.tsx` carries no `@jest-environment` docblock (line 1 is
`import React from 'react';`) and repoints the global at Node's realm at module
scope with its own explanatory comment (`:21-26`); `helpers.spec.ts:1-6` carries
`@jest-environment node` with the matching docblock; `VotingGovernancePage.spec.tsx`
carries neither (`grep -n "Uint8Array\|jest-environment"` → empty). Round 1
blocked because the previous diagnosis was contradicted by a green spec, so the
replacement was re-probed here rather than inherited — a throwaway spec in the
default jsdom environment, run and deleted, working tree confirmed back to the
same 5 modified paths:

```
PROBE[unshimmed] isValid=true canonical=THREW: radix2.encode input should be Uint8Array lookup=null
PROBE[shimmed]   isValid=true canonical=drep1ygqqq…7vlc9n                              lookup=HIT
```

Both probes queried `VALID_DREP_ID` against `new Map([[VALID_DREP_ID, entry]])`.
That single run reconciles the two measurements round 1 could not: the encoder
*does* round-trip the fixture id (which is why round 1's node-side probe saw it
canonicalize to itself and why `DRepDirectory.spec.tsx:528`/`:540` are green), and
it *does* throw inside `resolveExactDRepMatch`'s `try` (`helpers.ts:145-152`)
under an unshimmed jsdom global — `Cardano.DRepID.isValid` returning `true` first
is what makes the failure silent. F-18's task-173 hand-off is correspondingly
narrowed from "must re-plan its badge cases" to "install the three-line shim", so
the guide's self-declared "only executable pin on the `drepIndex` → `drepEntry` →
badge chain" (`cv-2-implementation-guide.md:5307-5314`) survives.

**CR139-2 (was major) — discharged.** Guide Step 6's tracker half is applied.
The `acceptanceCriteria` third string is replaced **verbatim** with the guide's
text and nothing else in that array moved
(`governance-drep-discovery-plan-tasks.json`, hunk `@@ -1264,7 +1272,7 @@`; the
removed `givenName` string was at `:1267`, confirming CR139-3's re-anchor). The
row is now `"status": "complete"` with `statusReason`, `evidence` and
`"updatedAt": "2026-07-28"` inserted between `status` and `priority`, matching the
`task-136`/`task-137`/`task-138` key order. It reports AC-1 satisfied, AC-2
**satisfied in part** with the Storybook visual **OWED** to task-145, and
AC-3-as-rewritten satisfied structurally, and it records the struck clauses as
deferred to `anchor-2` naming `GovernanceStore.ts:20-31` — the fourth acceptance
checkbox (`cv-2-implementation-guide.md:2611-2613`). Its supporting citations were
spot-checked rather than trusted: `givenName` really does appear exactly once
across `source`, `storybook` and `tests`, at
`VotingPowerDelegationConfirmationDialog.spec.tsx:89`, as a negative fixture. The
file still parses (`node -e "require(…)"` → `JSON OK`) and prettier was not run on
it.

**CR139-3 (was minor) — discharged.** F-18 now cites
`governance-drep-discovery-plan-tasks.json:1267` and flags the guide's `:1241` as
pre-137/138 numbering; `:1241` is indeed the `targetPath` key.

**CR139-4 (was minor) — discharged.** The comment at
`VotingPowerDelegation.tsx:207-208` now reads "``raw`` can be the `drep_vkh1...`
encoding, which the lookup's ID-validity gate rejects, so the CIP-129 form is
queried when present." The mechanism word is correct — the gate is
`Cardano.DRepID.isValid` (`helpers.ts:144`), not a checksum — and the comment is
two plain sentence-case lines stating a constraint.

### Independent re-checks of the code deliverable (nothing new found)

Re-anchored against the current file rather than round 1's numbers: imports
`:22-24` with `AppDRepDirectoryEntry` as `import type`; the optional prop `:54`;
the module-scope `EMPTY_DREP_INDEX` `:110` bound in the destructure at `:149`; the
lookup `:209-215`; the unconditional mount `:329-332`, a sibling of
`WalletsDropdown` and outside the `{selectedWallet && (` gate at `:334`; the
container destructure `:38-46` and `drepIndex={governance.drepIndex}` at `:73`.
Two hazards were checked directly rather than reasoned about. The default
parameter cannot be bypassed by a `null` prop: `GovernanceStore.ts:100` declares
`@observable drepIndex: Map<string, AppDRepDirectoryEntry> = new Map()` and both
refresh paths reassign a `new Map` (`:254`, `:297`), so the value is never
`null`/`undefined` and `.get` cannot throw. `governance` is a registered store
(`stores/index.ts:43`, `:68`, `:121`), so the prop is present in the app. Step 3's
deviation from the guide's literal snippet stands as value-identical — it reuses
task-138's `const currentVote = selectedWallet?.currentVote ?? null;` (`:177`),
which also matches `CurrentVoteSummary`'s `currentVote: WalletVotingTarget | null`
(`CurrentVoteSummary.tsx:14`). `DRepIdentity.cip129` is optional
(`common/types/governance.types.ts:22-24`), so `cip129 ?? raw` yields a string.
`submitButtonDisabled`, `chosenOption`, the form-state shape and the seed chain
are outside every hunk; `currentDRepEntry` is read exactly once, at `:331`, as a
display prop. `grep -rnE "task-1[0-9][0-9]|CR13[0-9]|CAT-|CP-[0-9]|AC-[0-9]"` over
`source storybook tests` returns nothing.

### Merged and dropped

1. *Nothing dropped, because nothing was filed.* All three lenses returned
   `approved` with zero blockers, and their three summaries agree on the same
   facts: the six build steps are applied, the mount is unconditional, no store
   read entered either component, and the two source comments are
   convention-compliant. Their overlapping claims were merged into the re-check
   paragraph above rather than restated three times.
2. *Not promoted — the mount is inert under jsdom in `VotingGovernancePage.spec.tsx`.*
   The gate escalated it again as a non-gate observation and F-18 now records it as
   **open**. It is a **note**, not a task-139 defect, for the reason round 1 gave:
   the guide assigns this task no spec file (`:2398-2400`) and gates it on "every
   existing suite stays green". Carried forward, now with a confirmed cause:
   **task-173 must install the `DRepDirectory.spec.tsx:21-26` shim at
   `VotingGovernancePage.spec.tsx` module scope before its badge and CIP-105 cases
   can pass**, and task-147 inherits the same constraint.
3. *Not promoted — AC-2 stays partial.* The Storybook visual belongs to task-145
   and no browser exists in this container; the guide records it **OWED**
   (`:2601-2605`) and the tracker row now says so. An acknowledged scope boundary
   is not a defect.
4. *Not promoted — prettier redness on `VotingPowerDelegation.tsx`.* Pre-existing
   at `:90`, outside every hunk, identical on the HEAD blob; `--write` was never
   run on either pre-existing file. Baseline item 4.
5. *Not promoted — `[React Intl] Missing message` noise for
   `voting.governance.currentVote.status.*`.* Catalog seeding is task-146's;
   the suites pass through the `defaultMessage` fallback.
6. *Not a finding — a one-line numbering slip in the gate report.* It places the
   second source comment at `:206-207`; it is at `:207-208`. No claim depends on
   it.

**Gate result and its attribution.** The supplied gate reports **PASS with zero
failures** — `tsc --noEmit` exit 0; `jest --testPathPattern="voting-governance|VotingGovernancePage"`
→ 3 suites / 38 tests / 7 snapshots; `--testPathPattern=governance` → 15 of 16
suites, 260 passed / 12 skipped; the wave pattern `"(governance|voting)"` → 17 of
18 suites, 282 passed / 12 skipped / 9 snapshots, numerically identical to the
wave baseline; `yarn lint` exit 0 at 5595 warnings / 0 errors with both changed
files proven warning-identical to their HEAD blobs; `typed-scss-modules` and
`yarn i18n:manage` correctly not run; all four Step 7 greps green. Zero reds, so
nothing needed attribution. One caveat recorded rather than inherited: the gate
was measured against the **pre-scribe 3-file tree**, and the reviewed tree is 5
files. The two things the scribe edits could have moved were re-measured here —
the tracker JSON still parses, and
`jest --testPathPattern="voting-governance|VotingGovernancePage" --no-coverage --runInBand`
is again **3 suites / 38 tests / 7 snapshots, exit 0**. `tsc` and `yarn lint` are
unaffected by definition: the delta is Markdown and a `.agent` JSON, and lint
scans only `source`, `storybook` and `utils`. `nix` is absent, so `nix fmt` stays
an owed pre-merge obligation (F-12). `yarn check:all` and `yarn storybook:build`
were deliberately not run — both are red at HEAD for the unrelated storybook
manager-webpack JSX loader reason and neither is a valid gate. This round moved
neither HEAD nor the change set: no file was edited except this log, and the
throwaway realm probe was deleted before this entry was written.

**Decision: approved** — zero blockers, zero majors, zero minors outstanding. The
production diff was accepted unchanged by both rounds; what round 1 blocked on was
documentation about the repo, and both halves are now discharged with the
diagnosis independently re-measured rather than restated. task-139 closes with one
subject-only commit,
`feat(gov): task-139 mount CurrentVoteSummary in the delegation form`, carrying
the two source files, the tracker row, the findings note and this log. Two items
travel forward as notes, not debts of this task: the jsdom realm shim that
task-173 must install before its badge cases can pass, and AC-2's Storybook visual
**OWED** to task-145.
