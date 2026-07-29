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

---

## Code Review: 2026-07-28 — task-140 round 1

**Scope reviewed.** The uncommitted working tree for task-140, against its guide
section `cv-2-implementation-guide.md:2619-3186` (file-ownership map at
`:2621-2641`, the eleven build steps, the acceptance record at `:3146-3186`), the
task-140 tracker row (`governance-drep-discovery-plan-tasks.json:1276-1299`), and
the PRD's D-4 / R-10 append-only rule. HEAD is `23e9899b0` (task-139 committed);
the wave baseline was recorded one commit earlier at `144c5153d`, and every
comparison below accounts for that. Three independent lenses ran — guide
conformance and runtime correctness, locked invariants and the sanitization floor,
and tests plus simplicity and drift — and **all three returned `approved`**, two of
them filing the same single minor. This round adjudicated those reports against
the files rather than accepting them; every claim promoted or dropped below was
re-verified first-hand. The main checkout `/workspaces/daedalus` was never read,
edited or run against, and this round fixed no code.

**What landed.** `git status --porcelain` → 6 modified, 2 untracked, 0 staged,
exactly the implementers' hand-off. `git diff --stat`:
`designs/current-vote-display-design.md` `1/1`,
`research/cv-1-findings.md` `6/6`,
`task-plans/cv-1-code-review.md` `30/0` (appended, nothing edited in place),
`CurrentVoteSummary.messages.ts` `7/0`,
`VotingPowerDelegation.scss` `7/0`,
`VotingPowerDelegation.tsx` `22/6`; plus
`source/renderer/app/utils/governance/isSameVoteTarget.ts` and
`tests/jest/governance/isSameVoteTarget.spec.ts` as new files. The tracker JSON and
all four i18n catalogs are clean — confirmed by an explicit
`git status --porcelain` over `source/renderer/app/i18n/locales`,
`translations/messages.json` and the tracker (empty output).

### Blockers

**None.** One minor survives, and it is documentation, not code.

**CR140-1 (minor) — guide Step 10 is undone: task-140's own AC-7 still cites the
wrong design-doc line.** Step 10 requires (`cv-2-implementation-guide.md:3108-3112`)
that in `governance-drep-discovery-plan-tasks.json`, **task-140's own AC-7 string
only**, `designs/current-vote-display-design.md:95` become
`designs/current-vote-display-design.md:97`, leaving task-173's AC — which
legitimately cites `:95` — alone. AC-7's own acceptance record repeats it: "the
tracker's AC-7 anchor is re-pointed to `:97`" (`:3172-3175`). The live AC-7 still
opens "The comparator sentence of designs/current-vote-display-design.md:95 no
longer offers a canonical CIP-129 string as an acceptable comparison key…"
(tracker `:1297`). The anchor is factually wrong and was verified against the
design doc in this worktree: `:95` is the header-byte classification sentence
(`credentialType` is derived during normalization… `0x22` → `'key'`, `0x23` →
`'script'`), which is task-173's subject and is correctly cited by the other AC at
`:1317`; the comparator sentence this task appended to is `:97` ("The same-vote
comparator (`task-140`) must key on a case-stable form…"). The guide's stated
anchors `1263` / `1283` are pre-137/138/139 numbering; the live targets are
`1297` (to change) and `1317` (to leave). This is an AC **content** edit, not the
`status` / `statusReason` / `evidence` / `updatedAt` row metadata the review brief
defers, but it is a one-token change inside a row the closing scribe pass must
open anyway, so it is filed **minor and absorbable** rather than sent back as a fix
round. *Fix:* hand-edit line `1297` only, `:95` → `:97`; leave `1317` untouched;
never run prettier on the tracker JSON. Both the correctness lens and the
tests/drift lens filed this independently; the verification gate escalated it as
its one outstanding non-gate item, noting the `[prod]` agent was out of surface and
the `[test]` agent was forbidden the tracker — so it fell between the two owners
rather than being skipped by either.

### Independent re-checks of the code deliverable (nothing new found)

Re-anchored against the current file, not the guide's pre-slice numbers. The
comparator (`isSameVoteTarget.ts:9-33`) matches the sentinels **before** any
bech32 decode (`:14-17`), so `abstain` / `no_confidence` are never treated as
directory entries (invariant 13); returns `false` when either side lacks
`credentialHex` (`:22-27`); compares the hex case-insensitively and requires
`credentialType` equality (`:28-32`). It is pure — no `console.`, no `logger.`, no
throw path, since `normalizeDRepIdentity` catches decode failures and returns
`null` (`normalizeDRepIdentity.ts:20-26`) — so the sanitization floor is untouched
(invariant 2). `toLowerCase()` is applied only to string copies inside the
comparison; `raw` / `cip129` / `cip105` are never read, and `chosenOption`
(`VotingPowerDelegation.tsx:245-248`) and the `delegateVotes` payload are outside
every hunk, so byte-equality holds (invariants 5, 10). `submitButtonDisabled` is
deleted from its old site and re-declared at `:249-255`, after `chosenOption` and
above both effects — no TDZ. Step 4's divergence is a strict simplification the
guide's own re-anchoring rule sanctions: it passes the existing `currentVote` local
(`:183`, itself `selectedWallet?.currentVote ?? null`) rather than re-deriving the
expression. The hint renders only under `isSameAsCurrent` with a stable id, and the
button's `aria-describedby` is `undefined` when it is absent (`:407-419`). The SCSS
block's four properties are alphabetical.

Two hazards were measured rather than reasoned about. **The `== null` hex guards
cannot mis-equate an empty string:** both success branches of
`normalizeDRepIdentity` populate `credentialHex` from a 28-byte slice via `toHex`
(`:38-44`, `:53-59`), so a success result always carries 56 hex characters and no
producer can emit `''`. **The spec's `drepVote` helper is type-sound at this
repo's settings:** `normalizeDRepIdentity` returns `DRepIdentity | null` and the
helper assigns it to a non-optional `drep`, which is legal because
`tsconfig.json` sets `"strict": false` with `strictNullChecks` commented out —
`tsc --noEmit` re-run here, exit 0. All five spec fixtures were decoded rather than
trusted: `KEY_CIP129` / `KEY_CIP105` genuinely share one credential hex,
`OTHER_KEY_CIP129` / `SCRIPT_CIP129` genuinely share credential bytes and differ
only in `credentialType`, and `UNDECODABLE_DREP` genuinely returns `null` — so no
assertion passes for the wrong reason. `grep -rn` for `task-1[0-9][0-9]`, `CR14x`,
`CAT-` and `AC-[0-9]` over `source storybook tests` returns nothing.

### Merged and dropped

1. *Merged.* Two lenses filed the same Step 10 finding (`SPEC-1-1`, `QUALITY-1-1`)
   with identical live anchors; they are one item, **CR140-1**, keeping the
   correctness lens's guide quotation and the drift lens's design-doc
   disambiguation of `:95` vs `:97`.
2. *Not promoted — AC-2 is partial by design.* The button keeps a real `disabled`
   rather than the UX doc's focusable `aria-disabled="true"`, because dropping it
   would re-enable submission and break AC-1. The guide records exactly this and
   directs it to `statusReason` (`:3155-3161`). An acknowledged, guide-sanctioned
   deviation is not a defect.
3. *Not promoted — AC-7's first conjunct is partial by design.* The design doc's
   `case-insensitive cip129` alternative is superseded by an appended clause rather
   than deleted, because this file's edit rule for the row is append-only (D-4,
   seam contract R-10); the guide states the outcome and orders it recorded
   (`:3176-3186`). Verified: the `1/1` diff appends to `:97` and deletes nothing,
   and `cv-1-code-review.md:736-738` is unedited with the discharge appended at
   end of file.
4. *Not promoted — the `credentialHex == null` guards would equate two empty
   strings.* Raised by one lens as explicitly non-blocking and dropped here on
   measurement: no producer can emit an empty hex (see the re-checks above), and
   the block is the guide's verbatim text.
5. *Not promoted — `[React Intl] Missing message: voting.governance.currentVote.sameVoteHint`.*
   Descriptor-present / catalog-key-absent is by design until task-146 seeds the
   catalogs; the `!!!`-prefixed default renders and no assertion depends on it.
   Same disposition as the task-136 `status.*` misses.
6. *Not promoted — `yarn lint` at 5595 warnings against a "roughly 5591" baseline.*
   Proven pre-existing by the gate, which linted the HEAD blobs through
   `eslint --stdin --stdin-filename`: `VotingPowerDelegation.tsx` is 10 problems at
   HEAD and 10 with task-140 applied (one `@ts-ignore` warning merely shifted
   `311` → `318`), the messages file is 0 → 0, and the new comparator is 0. The
   residual `+4` is the legacy-decorator false positive in
   `VotingGovernancePage.tsx`, identical at `144c5153d`. Warnings are not failures.
7. *Not promoted — prettier redness on the pre-existing files.* Red at HEAD and
   outside every hunk; `--write` was run only on the two newly created files, and
   `--check` on exactly those two is green here. Baseline item.
8. *Not a finding — the uppercase-input pre-check.* One lens decoded an
   all-uppercase `KEY_CIP129` through the installed `bech32` and confirmed it
   yields prefix `drep` and the same credential hex, so task-147's future
   `toUpperCase()` vector is already satisfied by the shipped comparator. That is a
   verification, not a defect, and AC-4 assigns the vector to task-147 anyway.
9. *Note, not a debt of this task.* `GovernanceCliArgvSmoke.spec.ts` still
   self-skips (no `cardano-cli`) and `GovernanceQueryService.spec.ts` still emits
   four era-fallback `console.warn` lines inside a passing suite — both reproduced
   unchanged from baseline.

**Gate result and its attribution.** The supplied gate reports **PASS with zero
failures**, and its load-bearing measurements were re-run here rather than
inherited: `tsc --noEmit` exit 0; `jest --testPathPattern=isSameVoteTarget` → 1
suite / **9 of 9** tests / 0 snapshots; `stylelint` on
`VotingPowerDelegation.scss` exit 0; `prettier --check` on the two new files
green. The gate additionally recorded
`--testPathPattern="voting-governance|VotingGovernancePage"` → 3 suites / 38 tests
/ 7 snapshots, `--testPathPattern=governance` → 269 passed + 12 skipped, and the
wave pattern `"(governance|voting)"` → 291 passed + 12 skipped / 9 snapshots
against a baseline of 282 passed + 12 skipped — a delta of exactly `+1` suite and
`+9` tests, i.e. the new comparator spec alone. Zero reds, so nothing needed
attribution. `typed-scss-modules` was correctly run for the changed `.scss` (its
output is gitignored and left the tree unchanged), and `yarn i18n:manage` was run
and then fully reverted with `git restore` on the four catalog paths — re-confirmed
clean here. No `git stash` anywhere. `nix` is absent, so `nix fmt` stays an owed
pre-merge obligation and prettier-on-explicit-paths is the recorded substitute.
`yarn check:all` and `yarn storybook:build` were deliberately not run: both are red
at HEAD for the unrelated manager-webpack JSX loader reason and neither is a valid
gate. This round moved neither HEAD nor the change set; no file was edited except
this log.

**Decision: approved** — zero blockers, zero majors, one minor (CR140-1) carried
into the closing pass rather than back into a fix round. task-140 closes with one
subject-only commit,
`feat(gov): task-140 disable submit when the chosen delegation matches the current one`,
carrying the two new files, the four modified source and doc files, this log, and
the tracker row. The scribe pass owes that row four things in a single edit: the
CR140-1 anchor change at `:1297` (`:95` → `:97`), and three `statusReason`
deviations — AC-2 partial (a real `disabled` kept over `aria-disabled` to preserve
AC-1), AC-7's first conjunct satisfied by append rather than deletion (D-4 /
R-10), and `nix fmt` unavailable with prettier-on-explicit-paths substituted. Two
items travel forward as notes, not debts: the catalog seeding of
`voting.governance.currentVote.sameVoteHint` **OWED** to task-146, and the
uppercase-input regression vector **OWED** to task-147 by AC-4's own text.

---

## Code Review: 2026-07-28 — task-173 round 1

**Scope reviewed.** The uncommitted working tree for task-173, against its guide
section `cv-2-implementation-guide.md:3212-3517` (Files-touched at `:3214-3219`,
Context at `:3221-3271`, the three locked invariants at `:3273-3287`, the four
resolved judgment calls at `:3289-3310`, Steps 1-6, and the acceptance record at
`:3500-3517`), the task-173 tracker row (`governance-drep-discovery-plan-tasks.json:1315-1340`,
still `"status": "pending"`), and the PRD's byte-equality and sanitization
invariants. HEAD is `08eeb719a` (task-140 committed); the wave baseline was
recorded at `144c5153d`, and tasks 139 and 140 have since landed, so every
comparison below is decomposed against that. Three independent lenses ran — guide
conformance and runtime correctness, locked invariants and the sanitization floor,
and tests plus simplicity and drift — and **all three returned `approved` with zero
blockers**. This round did not accept that on report: every load-bearing claim was
re-derived first-hand against the files, the three bech32 vectors were decoded
again here, and the container spec was re-run. The main checkout
`/workspaces/daedalus` was never read, edited or run against, and this round fixed
no code.

**What landed.** `git status --porcelain --untracked-files=all` → 3 modified, 0
untracked, 0 staged, exactly the guide's Files-touched list. `git diff --numstat`:
`VotingGovernancePage.spec.tsx` `75/0` (**purely additive**, so the pre-existing
byte-equality case and the hardware-wallet `describe` are provably untouched, as
Step 5 requires), `VotingGovernancePage.tsx` `5/9`, `Governance.stories.tsx` `2/1`.
`git diff --name-only -- .agent/` is empty: no tracker, catalog, design-doc or
dialog-component edit. Step 1 lands the helper import at
`VotingGovernancePage.tsx:12` with the `import type { DRepIdentity }` line kept at
`:13`; Step 2 replaces the derivation at `:81-87`, character-for-character the
guide's snippet, with the explicit
`chosenOption === 'abstain' || chosenOption === 'no_confidence'` guard retained
**ahead of** the decode; Step 3 rewrites `toStoryDRepIdentity` at
`Governance.stories.tsx:59-62` behind the import at `:54`, helper name and its three
call sites (`:283`, `:431`, `:469`) unchanged; Steps 4-5 add `mockDialogProps` at
`VotingGovernancePage.spec.tsx:63`, the recorder `jest.mock` at `:65-83` immediately
after the `ItemsDropdown` mock that ends at `:61`, `openConfirmation` at `:235-249`
immediately after `renderFlow`, and the two cases at `:599-635`.

### Blockers

**None.** No survivor at any severity. All three lenses filed zero, and
adjudication promoted nothing they left unfiled.

### Independent re-checks of the code deliverable (nothing new found)

Re-derived here, not inherited. **The two new cases are non-vacuous**: decoding the
guide's vectors through the installed `bech32` in this worktree gives
`drep1ydwykw3…` → prefix `drep`, 29 bytes, header `0x23`, and
`drep1pu0z60z…` → prefix `drep`, **28** bytes, header `0x0f`. Under the deleted
`startsWith('drep_script')` heuristic the first would have read `'key'` (the
assertion demands `'script'`) and the second would have produced a non-null identity
(the assertion demands `null`), so each case fails for the right reason.
`normalizeDRepIdentity.ts:28-30` is what rejects the 28-byte payload and `:32-34`
what gates the header. `VALID_DREP_ID` decodes to 29 bytes header `0x22` → `'key'`,
so the Storybook swap is behaviour-identical to the literal it replaces and no story
changes. **Byte-equality holds**: `chosenOption` reaches the decoder untouched — no
`trim`, no `toLowerCase`, no re-encode (`VotingGovernancePage.tsx:87`) — the helper
returns the same `raw` reference on both success branches
(`normalizeDRepIdentity.ts:39`, `:56`), and the `voting.delegateVotes({ chosenOption, … })`
payload at `VotingGovernancePage.tsx:97-104` is outside every hunk. **The
sanitization floor is untouched**: `grep -nE "logger|Logger|console\.|analytics|sendEvent|localStorage"`
over the container, the stories file and the consuming dialog exits 1, and the
decoder's only failure path is a bare `catch { return null; }`
(`normalizeDRepIdentity.ts:24-26`) — no message construction, no rethrow. The
identity object is genuinely richer now (`cip129` / `cip105` / `credentialHex`,
`:38-44` and `:53-59`), which would matter for a wire-keyed `filterLogData`, but its
only consumers are the JSX prop at `VotingGovernancePage.tsx:91` and two read-only
render sites at `VotingPowerDelegationConfirmationDialog.tsx:151` and `:160` — a
whole-tree `grep -rn "drepIdentity"` over `source/` and `storybook/` returns exactly
those plus the three story call sites. **The recorder drops nothing**: the mocked
module's only runtime export is `default` at
`VotingPowerDelegationConfirmationDialog.tsx:214` (the `:42` export is a type and is
erased), so the `{ __esModule, default }` shape is complete, and the recorder wraps
`actual.default` rather than stubbing it, leaving the DOM the other flow tests
assert on identical. **Comment convention clean**: the two added comments
(`VotingGovernancePage.tsx:81-83`, spec `:65-66`) are the guide's verbatim text,
2-3 plain lines each, stating an invariant and a mock constraint; no task id, no
change history, no ALL-CAPS, and no test name cites a process artifact.

### Merged and dropped

1. *Not promoted — the legacy 28-byte `drep1…` id now renders without its raw
   string.* One lens noted this and deliberately did not file it; the note is
   correct and this round sharpened it by opening the branch. With `drepIdentity`
   `null`, the dialog takes the `:` branch at
   `VotingPowerDelegationConfirmationDialog.tsx:163-172`, whose value is
   `intl.formatMessage(mapVoteToIntlMessage(chosenOption))`; `mapVoteToIntlMessage`
   (`:31-40`) has a `default` arm returning `sharedGovernanceMessages.delegateToDRep`,
   so the rendered text is the generic "delegate to DRep" label — **not** an
   `Abstain` / `No Confidence` mislabel and **not** a crash — and the raw id is
   simply absent from that surface. The predicate fix is task-175's by name
   (judgment call 1, `cv-2-implementation-guide.md:3886-3892`, which cites this exact
   legacy form), and this task is forbidden the dialog file
   (`:3302`). Guide-assigned deferral, not a defect of this row.
2. *Not promoted — the second case's name says "still submits it byte-for-byte"
   while it asserts `initializeVPDelegationTx`, not `delegateVotes`.* Filed as a
   non-blocking observation by one lens; dropped as a finding on three grounds. The
   name and both assertions are the guide's Step 5 text verbatim (`:3456-3463`);
   `initializeVPDelegationTx` is a real submission seam, reaching the form as
   `initiateTransaction` at `VotingGovernancePage.tsx:68` and fired by the Submit
   click inside `openConfirmation`; and `delegateVotes` byte-equality is already
   pinned by the untouched case at spec `:347-386`. Changing it would deviate from a
   verbatim snippet to no test-power gain.
3. *Not promoted — AC-2 is only half-satisfied at this commit.* By design, and
   recorded as such before the build: judgment call 2 (`:3296-3302`) and the AC text
   itself (`:3505-3509`) split it, leaving the rendering half to task-175 Step 3. The
   half that is this task's — null identity plus byte-equal submission — is asserted.
   A partial criterion the guide itself partitions is not a defect.
4. *Not promoted — AC-5 unedited.* `current-vote-display-design.md:95` already
   carries the header-byte rule ("`0x22` -> `'key'`, `0x23` -> `'script'`") and the
   grep prints it; judgment call 1 (`:3291-3295`) says verify, do not edit, and
   `git status --porcelain .agent/` is empty. Correct as built.
5. *Not promoted — `[React Intl] Missing message: voting.governance.currentVote.status.{inactive,unavailable}`
   in the spec run.* Pre-existing and unrelated: the ids are declared at
   `CurrentVoteSummary.messages.ts:87` / `:93` and rendered from
   `CurrentVoteSummary.tsx:114`, all files clean in the working tree, and catalog
   seeding is task-146's single responsibility. Same disposition as the task-139 and
   task-140 rounds.
6. *Not promoted — prettier redness on `Governance.stories.tsx`.* Re-measured
   non-destructively here by piping both the HEAD blob and the working copy through
   `prettier --stdin-filepath` and diffing against their own sources: **22 drift
   lines at HEAD, 22 in the working tree**, i.e. unchanged and entirely outside the
   hunks. The other two changed files are 0/0 both ways, so the hand-formatting
   matches prettier's output exactly and no whole-file reformat occurred. No
   `--write` was run on any file, per the guide's "Do not run prettier on any file in
   this task" (`:3498`) and F-10.
7. *Not promoted — `yarn lint` at 5595 warnings against a "roughly 5591" baseline.*
   Attributed by the gate file-by-file against the HEAD blobs via
   `eslint --stdin --stdin-filename`: 5 vs 5, 9 vs 9, 0 vs 0 on the three changed
   files. This task's delta is +0 warnings, +0 errors; the residual belongs to
   committed `23e9899b0` / `08eeb719a`. Warnings are not failures.
8. *Note, not a debt of this task.* `GovernanceCliArgvSmoke.spec.ts:28` still
   self-skips (no `cardano-cli` in this devcontainer) and `GovernanceQueryService.spec.ts`
   still emits era-fallback `console.warn` lines inside a passing suite — both
   reproduced unchanged from baseline.
9. *Superseded prediction, corrected here.* The task-139 round-2 close carried
   forward "the jsdom realm shim that task-173 must install before its badge cases
   can pass" (F-18). task-173 **as built ships no badge case**, and needs no shim:
   `normalizeDRepIdentity` decodes through the `bech32` npm package, which returns a
   plain `number[]` from `fromWords` (`normalizeDRepIdentity.ts:21-23`), not the
   `@scure` `radix2` path that trips the Node/jsdom `Uint8Array` realm split. `grep -n
   "Uint8Array"` over `VotingGovernancePage.spec.tsx` exits 1 and the suite is green
   at 16/16, including the `'script'` classification. F-18 stays accurate about the
   badge chain; its carry-forward to this row is discharged as not-applicable.

**Gate result and its attribution.** The supplied gate reports **PASS with zero
failures**, and its load-bearing measurements were re-run in this worktree rather
than inherited: `node_modules/.bin/tsc --noEmit` exit 0;
`jest --testPathPattern=VotingGovernancePage --no-coverage --runInBand` → 1 suite /
**16 of 16** tests / 0 snapshots, exit 0, with both new cases named in the run and
the pre-existing "propagates the selected DRep ID byte-for-byte: row select →
confirmation → `delegateVotes` payload" plus all three hardware-wallet cases green
unedited (AC-4); the three Step 6 greps behaving exactly as specified — the
`startsWith('drep_script')` grep and the `logger|console\.|Logger` grep both exit 1
(AC-1, AC-6), and the "leading header byte" grep prints
`current-vote-display-design.md:95` against an unmodified file (AC-5). The gate
additionally recorded `--testPathPattern=governance-sanitization` → 24 of 24 green
(AC-6's task-111 spy suite) and the wave pattern `"(governance|voting)"` → 18 passed
+ 1 skipped of 19 suites, 293 passed + 12 skipped of 305 tests, 9 snapshots, against
F-20's 291 / 12 / 303 basis — a delta of exactly the two cases added here, with the
skip and snapshot counts unmoved. `typed-scss-modules` was correctly skipped (no
`.scss` in the change set) and `yarn i18n:manage` correctly never invoked (no i18n
path in the diff), so no catalog needed restoring; no `git stash` anywhere. `nix` is
absent, so `nix fmt` stays an owed pre-merge obligation and prettier-on-explicit-paths
is the recorded substitute (F-12). `yarn check:all` and `yarn storybook:build` were
deliberately not run — both are red at HEAD for the unrelated manager-webpack JSX
loader reason and neither is a valid gate. This round moved neither HEAD nor the
change set; no file was edited except this log.

**Decision: approved** — zero blockers, zero majors, zero minors. task-173 closes
with one subject-only commit,
`fix(gov): task-173 derive the confirmation dialog identity via normalizeDRepIdentity`,
carrying the three modified files, this log and the tracker row. The scribe pass
owes that row (`:1315`) two things the guide names explicitly: the AC-2 partial
recorded in `statusReason` — "the dialog still renders the raw string verbatim" is
discharged by task-175 Step 3, which owns the branch predicate (`:3505-3509`) — and
the `nix fmt` unavailability with prettier-on-explicit-paths substituted. Edit that
row by content, never by the guide's pre-slice line numbers (F-19), and never run
prettier on the tracker JSON. One item travels forward as a note, not a debt: the
legacy 28-byte `drep1…` id renders without its raw string until task-175's
`isSentinelVote` predicate lands.

---

## Code Review: 2026-07-28 — task-141 round 1

**Scope reviewed.** The uncommitted working tree for task-141, against its guide
section `cv-2-implementation-guide.md:3521-3639` (Files-touched at `:3523-3527`,
Context at `:3529-3545`, the single locked invariant at `:3547-3551`, the two
resolved judgment calls at `:3553-3559`, Steps 1-3 at `:3561`, `:3570`, `:3620`, and
the acceptance record at `:3631-3637`), the task-141 tracker row
(`governance-drep-discovery-plan-tasks.json:1344-1360`, still `"status": "pending"`),
and the PRD's row charter at `cv-2-PRD.md:156` plus its acceptance restatement at
`:237-240`. HEAD is `2842d6fe9` (task-173 committed). Three independent lenses ran —
correctness against the guide, locked invariants and the sanitization floor, and
tests plus simplicity and drift — and **all three returned `approved` with zero
blockers**. As in the task-173 round, that was not accepted on report: the byte
comparison against the guide snippet, the prop-set derivation, the non-vacuity
argument and both jest patterns were re-derived first-hand here. The main checkout
`/workspaces/daedalus` was never read, edited or run against, and this round fixed
no code.

**What landed.** `git status --porcelain` → exactly one line,
` M source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`; nothing
staged, nothing untracked. `git diff --stat` → `1 file changed, 44 insertions(+)`,
**zero deletions**, so every pre-existing case in the file is provably untouched.
`git diff --stat -- source/renderer/app/containers/voting/VotingGovernancePage.tsx`
prints nothing, which Step 3 (`:3629`) explicitly requires of this verification row.
The appended block is `describe('Confirmation dialog prop contract', …)` at
spec `:637-679`, preceded by the separating blank line at `:636`. It is
**byte-identical to the guide's Step 2 snippet**: `diff` of guide `:3575-3617`
against spec `:637-679` returns nothing, 43 lines each.

### Blockers

**None.** No survivor at any severity. All three lenses filed zero, and
adjudication promoted nothing they left unfiled.

### Independent re-checks of the code deliverable (nothing new found)

Re-derived here, not inherited. **The ten-key pin matches production one-for-one**:
`VotingGovernancePage.tsx:89-114` renders `<VotingPowerDelegationConfirmationDialog>`
with exactly `chosenOption` (`:90`), `drepIdentity` (`:91`), `fees` (`:92`),
`hwDeviceStatus` (`:93`), `isTrezor` (`:94`), `onClose` (`:97`),
`onExternalLinkClick` (`:98`), `onSubmit` (`:99`), `redirectToWallet` (`:106`) and
`selectedWallet` (`:114`) — ten props, no more — and
`VotingPowerDelegationConfirmationDialogProps` at
`VotingPowerDelegationConfirmationDialog.tsx:54-70` declares those ten plus `intl`,
which `injectIntl` supplies rather than the container, and **no** historical field.
That is AC-1's first half, satisfied in full rather than in part. **Neither case is
vacuous**: `openConfirmation` (spec `:235-249`) ends with
`await screen.findByText('Confirm Transaction')`, so it throws rather than silently
no-oping if the dialog never mounts; `mockDialogProps` is therefore guaranteed
non-empty after the `beforeEach` reset at `:652`, and had it been empty
`Object.keys(undefined)` would throw rather than pass. Exact-set `toEqual` against a
ten-element array fails on any added, renamed or dropped prop, so the pin is
genuinely fail-on-addition rather than tautological, and `[...EXPECTED_DIALOG_PROPS]`
copies before the mutating `.sort()`, leaving the module-level array intact for the
second case. **No historical prop exists to remove**: Step 1's premise re-verified
live — `grep -rnE "previousVote|newVote|previousDRepId|historicalVote"
source/renderer/app storybook` returns exactly one hit, spec `:673`, the pin's own
forbidden-key list; zero production occurrences. **The sanitization floor is
untouched**: the same `grep -nE "logger|Logger|analytics|electron-store|localStorage|console\."`
over the container, its spec and the consuming dialog exits 1; the `VALID_DREP_ID`
in the diff never leaves a Jest assertion, so the wire-keyed `filterLogData` gap is
not exercised, and no i18n string, cohort/badge/status rule, BigNumber path or IPC
channel is touched. **No second delegation backend** (inv 4): the pin introduces no
`GovernanceStore` read and its exact-set equality makes any future store-backed
comparison prop a test failure by construction. **Byte-equality (inv 10) still
proven**: task-173's `:passes a null identity … still submits it byte-for-byte` and
both flow cases named by AC-2 pass unchanged in the same run. **Comment convention
clean**: the appended block contains **zero** comment lines, and both `it` names are
behavioural — no task id, CAT/CP label, plan name, PR number or ALL-CAPS emphasis.
**The pin will not go stale inside cv-2**: `grep -n "VotingGovernancePage.tsx"` over
the guide beyond `:3900` exits 1, and task-175's Files-touched list (`:3807-3814`)
covers only the dialog `.messages.ts` / `.tsx` / `.spec.tsx` trio, never the
container, so nothing later in this slice alters the prop set the pin fixes.

### Merged and dropped

1. *Merged — three separate statements of "byte-for-byte the guide's Step 2 block".*
   All three lenses asserted it; the strongest evidence is kept and strengthened by
   re-running the comparison as a real `diff` here (guide `:3575-3617` vs spec
   `:637-679`, 43 lines each, zero differences) rather than by reading.
2. *Merged — three statements that the ten-key list matches production.* Kept with
   the per-prop line anchors above; the lenses cited the JSX span variously as
   `:88-113` and `:88-114`, and the guide's Context line cites `:85-111`. The true
   span at this commit is `:89-114`. **Note, not a defect**: those are pre-slice /
   pre-task-173 numbers drifting under committed edits, exactly the hazard F-19
   records. Anchor by content in any later round.
3. *Not promoted — "the second `it` is strictly implied by the first".* One lens
   raised it and deliberately did not file it; adjudication agrees. An exact key-set
   equality does already forbid the four keys, but AC-1 (`:3633-3634`) demands "the
   ten-key pin **plus** the four negative-key assertions", and the block is a verbatim
   guide snippet. Deleting the redundancy would deviate from the contract for no
   test-power gain.
4. *Not promoted — "the pin constrains only top-level dialog props, so it would not
   catch the dialog reading history off `selectedWallet.currentVote`".* Correct as an
   observation — that shape exists (spec `:105-113`) — but out of row: AC-1 scopes
   this task to the **prop set**, and widening the pin into the wallet payload would
   exceed the row and pre-empt task-142/task-175. Recorded as a forward note, not a
   defect.
5. *Not promoted — the duplicated `beforeEach`/`afterEach` pair across the two
   adjacent `describe`s (spec `:605-612` and `:651-658`).* Hoisting them would rewrite
   task-173's `describe`, which resolved judgment call 1 (`:3555-3557`) forbids
   outright; the duplication is the contract's choice, not drift.
6. *Not promoted — Step 1's grep no longer prints nothing.* After the edit it prints
   spec `:673`. That is the pin naming what it forbids, not a leak; anyone re-running
   the Step 1 command as a premise check post-edit must expect the one self-hit.
7. *Not promoted — `[React Intl] Missing message: voting.governance.currentVote.status.unavailable`
   on stderr during the spec run, from `CurrentVoteSummary.tsx:114`.* Pre-existing
   and unrelated inside a fully green suite; catalog seeding is task-146's single
   responsibility. Same disposition as the task-139, task-140 and task-173 rounds.
8. *Not promoted — `yarn lint` at 5595 warnings against a "roughly 5591" baseline.*
   Attributed by the gate against the HEAD blob of the one changed file via
   `eslint --stdin --stdin-filename`: **9 problems (0 errors, 9 warnings) before and
   the identical 9 after**, all at `:37-227`, far above the appended block at
   `:637-679`. This task's delta is +0 warnings, +0 errors; the residual belongs to
   the seven wave tasks already at HEAD. Warnings are not failures.
9. *Note, not a debt of this task.* `GovernanceCliArgvSmoke.spec.ts` still self-skips
   (no `cardano-cli` in this devcontainer) and the React legacy-lifecycle warnings
   still print across renderer suites — both reproduced unchanged from baseline.

**Gate result and its attribution.** The supplied gate reports **PASS with zero
failures**, and its load-bearing measurements were re-run in this worktree rather
than inherited: `node_modules/.bin/tsc --noEmit` exit 0;
`jest --testPathPattern=VotingGovernancePage --no-coverage --runInBand` → 1 suite /
**18 of 18** tests / 0 snapshots, exit 0, with both new cases green — "hands the
dialog exactly the current-target prop set" and "passes no historical vote-target
prop" — and AC-2's named backward-compatibility cases passing unedited in the same
run: "propagates the selected DRep ID byte-for-byte: row select → confirmation →
`delegateVotes` payload" plus all three under "Hardware-wallet delegate flow via
location.state handoff". The wave pattern `"(governance|voting)"` → **18 passed + 1
skipped of 19 suites, 295 passed + 12 skipped of 307 tests, 9 snapshots**, against
the task-173 close basis of 293 / 12 / 305 — a delta of exactly the two cases added
here, with the skip and snapshot counts unmoved and no `FAIL` line anywhere. Prettier
was run **read-only**: `--check` on the one changed pre-existing file →
"All matched files use Prettier code style!", exit 0, so the hand-appended block did
not turn a green file red; no `--write` touched any file and the three
baseline-red files were not opened. `typed-scss-modules` was correctly skipped (no
`.scss` in the change set) and `yarn i18n:manage` correctly never invoked (no i18n
path in `git diff --name-only`), so no catalog needed restoring; no `git stash`
anywhere, and `yarn check:all` / `yarn storybook:build` were deliberately not run —
both are red at HEAD for the unrelated manager-webpack JSX loader reason and neither
is a valid gate. `nix` is absent, so `nix fmt` stays an owed pre-merge obligation and
prettier-on-explicit-paths is the recorded substitute (F-12). This round moved
neither HEAD nor the change set; no file was edited except this log.

**Decision: approved** — zero blockers, zero majors, zero minors. task-141 closes
with one subject-only commit,
`test(gov): task-141 pin the confirmation dialog to current-target props`, carrying
the single modified spec file, this log and the tracker row. The scribe pass owes
that row (json `:1344-1360`) two things: both acceptance criteria recorded as
satisfied **in full** — AC-1 by the ten-key pin plus the four negative assertions,
AC-2 by the pre-existing software and hardware flow cases passing unchanged — and
the `nix fmt` unavailability with prettier-on-explicit-paths substituted. Edit that
row by content, never by the PRD's pre-slice `json :1287-1303` citation (F-19), and
never run prettier on the tracker JSON. One item travels forward as a note, not a
debt: the pin fixes the **top-level** prop set only, so a later slice that
legitimately adds a dialog prop must update `EXPECTED_DIALOG_PROPS` deliberately —
that failure is the pin working, not a regression.

---

## Code Review: 2026-07-28 — task-142 round 1

**Scope reviewed.** The uncommitted working tree for task-142, against its guide
section `cv-2-implementation-guide.md:3641-3803` (Files-touched `:3643-3647` with the
explicit **No production file changes** at `:3648`, Context `:3650-3692`, the three
locked invariants at `:3694-3704`, the two resolved judgment calls at `:3706-3715`,
Step 1 at `:3717`, Step 2 at `:3779` and the acceptance record at `:3791-3803`), the
task-142 tracker row (`governance-drep-discovery-plan-tasks.json:1369`, still
`"status": "pending"`), and the PRD's row charter at `cv-2-PRD.md:157` with its
acceptance restatement at `:242` and the governing decision **D-3** at `:379`. HEAD is
`8004affd9` (task-141 committed). Three independent lenses ran — correctness against
the guide, locked invariants plus the sanitization floor, and tests/simplicity/drift
— and **all three returned `approved`**; two filed zero findings and the third filed
two at severity `minor`. Nothing was accepted on report: every promoted and every
dropped claim below was re-derived first-hand in this worktree. The main checkout
`/workspaces/daedalus` was never read, edited or run against, and this round fixed no
code.

**What landed.** `git status --porcelain` → exactly one line,
` M source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx`;
nothing staged, nothing untracked. `git diff --stat` → `1 file changed, 51 insertions(+)`,
**zero deletions**, so all thirteen pre-existing cases in the file are provably
untouched. `git diff --stat -- .../VotingPowerDelegationConfirmationDialog.tsx` prints
nothing — exactly what Step 2 (`:3785`) and AC-3 demand of this verification row. The
change is one import at spec `:14`
(`import { messages } from './VotingPowerDelegationConfirmationDialog.messages';`,
placed beside the component import at `:13` as instructed) plus one appended
`describe('VotingPowerDelegationConfirmationDialog — fee, hardware and passphrase sections')`
at spec `:172-220` carrying the guide's four `it` cases. It reproduces the Step 1
snippet with **one deviation**: the 85- and 83-column
`'Confirm the transaction using the "HW Test Wallet" device'` arguments (guide `:3739`,
`:3750`) are hand-wrapped onto their own lines at spec `:188-190` and `:201-203`. That
deviation is required rather than stylistic — see note 8.

### Blockers

**None.** No survivor at `blocker` or `major` severity. One `minor` survives, below.

### Minor (non-blocking; may be absorbed before the task-142 commit)

1. **The software-wallet case's negative assertion cannot fail for the right reason —
   spec `:187-191`**, inside `renders the labelled passphrase input for a software
   wallet`. Re-derived here end to end, and it is vacuous twice over. That `it` calls
   `renderDialog()` with no overrides, so `hwDeviceStatus` is `HwDeviceStatuses.READY`
   (spec `:44`) and `selectedWallet` is `softwareWallet` with `name: 'Test Wallet'`
   (spec `:21`, `:50`). Under the one regression this line is meant to catch — the
   `selectedWallet.isHardwareWallet ?` branch at
   `VotingPowerDelegationConfirmationDialog.tsx:179-185` leaking into its HW arm —
   `HardwareWalletStatus` would render with `walletName='Test Wallet'` and status
   `READY`, which falls through the `walletName &&` status list at
   `HardwareWalletStatus.tsx:297-303` to the `else` at `:317-319` and emits
   `messages.ready` → `Device ready` (`en-US.json:1145`). The queried string is
   produced only by `verifying_transaction`
   (`HardwareWalletStatus.tsx:72-78`), and it interpolates `{walletName}`, which here
   would be `Test Wallet`, never `HW Test Wallet`. `queryByText` therefore returns
   `null` under both the correct and the regressed implementation. **The pin itself is
   not weakened**: the two assertions immediately above at `:185-186`
   (`getByText('Spending password')` and
   `document.querySelector('input[type="password"]')`) both fail correctly if the
   branch flips, and they carry the case's intent unaided. **Fix, if a fix pass runs**:
   either delete `:187-191` outright, or make it discriminating against the string the
   leak would actually emit —
   `expect(screen.queryByText('Device ready')).not.toBeInTheDocument();` — which is
   `null` today, non-null under the regression, and (unlike a `/device/i` regex)
   immune to task-175's growth, whose only new copy is `CIP-105 DRep ID` and
   `Signed payload` (guide `:3920`, `:3926`). Either edit is a **deliberate deviation
   from a verbatim guide snippet** and must be recorded as one, because the defect
   originates in the guide, not in the implementation.

### Independent re-checks of the code deliverable (nothing new found)

Re-derived here, not inherited. **The comparison-row assertions are live, not
vacuous**: `VotingPowerDelegationConfirmationDialog.messages.ts:3-45` exports exactly
`title`, `vote`, `drepId`, `fee`, `password`, `errorGeneric`, `buttonCancel`,
`buttonConfirm` — eight keys, no more — so `expect(messages).not.toHaveProperty('previousVote')`
and `…('newVote')` at spec `:217-218` assert against a real object with a real shape,
and they **survive task-175**, whose Files-touched trio mints only `drepIdCip105` and
`signedPayload` (guide `:3920`, `:3926`, PRD S-5). **Every asserted string is
byte-exact against the catalog the spec loads** (`en-US.json` via spec `:8`):
`Transaction fee` (`:950`), `Spending password` (`:951`), `Confirm Transaction`
(`:952`); `formattedWalletAmount(new BigNumber('0.174257'))` → `0.174257 ADA`, and the
fixture uses the lossless string constructor. **The locked invariant against snapshot
and element-count assertions holds**: the appended block contains no `toMatchSnapshot`,
no `getAllBy…().length` and no "exactly N" claim, so it survives the identity block
task-175 is chartered to grow — the identity block being explicitly out of scope here
per the guide `:3702-3704` and D-3. **The sanitization floor is untouched**:
`grep -nE "logger|Logger|analytics|electron-store|localStorage|console\.|filterLogData"`
over the spec exits 1; the added block introduces no logger, analytics or store call
and no new renderer-domain object near one, and the HW render path it exercises was
already exercised by the pre-existing row at spec `:123-124`. **No i18n surface is
touched**: no descriptor is minted, no catalog value is edited, no `!!!` marker is
added or stripped, and the `!!!`-marked `drepId` copy stays asserted with its marker
by the pre-existing case. **Comment convention clean**: the diff adds **zero** comment
lines, and none of the four `it` names carries a task id, CAT/CP label, plan name, PR
number or ALL-CAPS emphasis. **Harness reuse honoured**: no second render harness was
written — all four cases go through `renderDialog` (spec `:30-54`) — and `afterEach(cleanup)`
matches the file's precedent in both pre-existing describes.

### Merged and dropped

1. *Merged — the vacuity of spec `:187-191`.* Filed by the tests/simplicity lens as
   `QUALITY-1-1` (severity `minor`) and, independently, by the invariants lens as an
   explicit non-blocking observation ("the real exclusivity pin is the password-input
   assertion at `:186`"). Kept as the single minor above, carrying the first lens's
   regression analysis as the evidence and the second's point about where the real pin
   lives. Promoted rather than dropped because the assertion is genuinely dead code in
   a test; held at `minor` because it hides no regression and its neighbours fail
   correctly.
2. *Dropped — `QUALITY-1-2`, "the HW case at spec `:194-207` is two-thirds redundant".*
   The evidence is factually correct and was re-verified: the `it.each` row at spec
   `:123-124` already pairs `VERIFYING_TRANSACTION` with the same string under the same
   overrides (`:135`), and the case at `:148-155` already asserts
   `document.querySelector('input[type="password"]')` is null under identical overrides
   (`:149-152`, `:154`); only the `Spending password` absence at `:206` is new. Dropped
   for three reasons. (a) The finding's own remedy reads "Optional … leaving it as-is
   is defensible and needs no change", so it asserts no defect. (b) The case is
   prescribed **verbatim** by the guide's Step 1 (`:3743-3756`); deleting a
   contract-prescribed case to retire two duplicate assertions is a larger deviation
   than the redundancy it removes. (c) The case's value is co-location — one render
   proving the branch is exclusive (HW copy present, password control absent, password
   label absent) is a stronger and more legible pin than the same three facts scattered
   across two other describes that exist for other reasons. Redundant-but-correct
   coverage is not a defect.
3. *Merged — three separate statements that the production dialog is untouched.* All
   three lenses asserted it; the strongest form is kept and re-run here as
   `git diff --stat` on the `.tsx`, which prints nothing, alongside the zero-deletion
   whole-tree stat. Together those two are the conclusive proof of AC-3's "untouched"
   half.
4. *Merged — three separate statements that the forward pins survive task-175.* Kept
   once, re-derived against the guide's own descriptor table rather than against any
   lens's paraphrase of it.
5. *Not promoted — the correctness lens's "SATISFIED-IN-PART" framing of AC-3.* That is
   not a defect in the diff. AC-3's `~L118-L127` citation matches this file at no
   commit, the guide resolves it by re-anchoring to the semantic
   `selectedWallet.isHardwareWallet ?` branch (`:3699-3701`, acceptance `:3799-3803`),
   and the implementation pins exactly that. What remains is a **scribe-phase
   obligation**, recorded as a handoff below, not a change request against this diff.
6. *Not promoted — `yarn lint` at 5595 warnings against the "roughly 5591" baseline.*
   Attributed by the gate and consistent with the diff: task-142's one changed file
   reports `3 problems (0 errors, 3 warnings)`, all `@typescript-eslint/no-explicit-any`
   on the pre-existing `as any` fixtures at `:23`, `:29`, `:92` — the same three that
   sit at `:22`, `:28`, `:91` in the HEAD blob, shifted by exactly the one added import
   line. This task's delta is **+0 warnings, +0 errors**; the +4 belongs to task-141,
   already at HEAD. Warnings are not failures.
7. *Not promoted — the `[React Intl] Missing message: voting.governance.currentVote.*`
   warnings on stderr during the wider run.* Pre-existing baseline noise emitted from
   **passing** suites; catalog seeding is task-146's single responsibility. Same
   disposition as the task-139, task-140, task-173 and task-141 rounds. Likewise the
   permanent self-skip of `tests/jest/governance/GovernanceCliArgvSmoke.spec.ts`
   (no `cardano-cli` in this devcontainer).
8. *Note — guide defect, worth recording before the next section is authored.* The
   claim at guide `:3207-3208` that "the snippets below are already formatted to
   prettier 2.1.2's 80-column output" is **false** for the task-142 snippet: guide
   `:3739` is 85 columns and `:3750` is 83, both inside a `tsx` fence at an 80-column
   print width. The implementer's hand-wrapping was therefore mandatory, not optional,
   and it is correct — `prettier --check` on the changed spec returns "All matched
   files use Prettier code style!", exit 0. The file is **not** in the baseline-red set
   (`VotingPowerDelegation.tsx`, `VotingPowerDelegationConfirmationDialog.tsx`,
   `VotingGovernancePage.tsx`, `Governance.stories.tsx`), and no `--write` was run
   anywhere in this round.

**Gate result and its attribution.** The supplied gate reports **PASS with zero
failures across all six steps**, and the load-bearing measurements were separated here
into re-run and inherited rather than blanket-accepted. **Re-run in this worktree:**
`git status --porcelain` (one modified file, no untracked, nothing staged);
`git diff --stat` (`1 file changed, 51 insertions(+)`, zero deletions) and the empty
`git diff --stat` on the production `.tsx`;
`jest --testPathPattern=VotingPowerDelegationConfirmationDialog --no-coverage --runInBand`
→ **1 suite / 17 of 17 tests passed / 0 snapshots**, exit 0, with all four new cases
green and named per the guide — "renders the fee row with the formatted amount",
"renders the labelled passphrase input for a software wallet", "renders the device
status instead of the passphrase input for a hardware wallet", "keeps the dialog chrome
and introduces no comparison rows"; `prettier --check` **read-only** on the one changed
file → clean, exit 0; and the sanitization grep over the spec → exit 1. **Inherited
from the gate, not re-run here:** `tsc --noEmit` exit 0, `yarn lint` exit 0 at 5595
warnings / 0 errors, and the wider `"(governance|voting)"` pattern at 18 passed + 1
skipped of 19 suites / 299 passed + 12 skipped of 311 tests / 9 snapshots, zero
failures — which reconciles exactly against the task-141 close basis (293/305) as
+2 committed task-141 and +4 task-142. `typed-scss-modules` was correctly skipped (no
`.scss` in the change set, so a bare `tsc` suffices) and `yarn i18n:manage` correctly
never invoked (no i18n path in the change set), so no catalog needed restoring; the
tracker JSON, both locale catalogs and `translations/messages.json` are all confirmed
untouched. No `git stash` anywhere, and `yarn check:all` / `yarn storybook:build` were
deliberately not run — both are red at HEAD for the unrelated manager-webpack JSX
loader reason and neither is a valid gate. `nix` is absent, so `nix fmt` remains an
owed pre-merge obligation with prettier-on-explicit-paths as the recorded substitute
(F-12). This round moved neither HEAD nor the change set; no file was edited except
this log.

**Decision: approved** — zero blockers, zero majors, one minor. task-142 may close as
it stands; absorbing the minor at spec `:187-191` is optional and, if done, needs no
new review round but does need the guide-deviation note. task-142 closes with one
subject-only commit,
`test(gov): task-142 pin the confirmation dialog fee, hardware and passphrase sections`
(guide `:3200`), carrying the single modified spec file, this log and the tracker row.
The scribe pass owes that row (`json :1369`, edited **by content** and never by the
PRD's pre-slice `json :1304-1320` citation, F-19, and never formatted with prettier)
three things: AC-1 and AC-2 recorded as satisfied in full — AC-1 by the fee, passphrase,
HW and chrome cases with the identity block deliberately excluded per D-3, AC-2 by DOM
absence plus the two descriptor-object assertions; **AC-3's re-anchoring recorded in
`statusReason`** — that the "lines ~L118-L127" citation corresponds to
`VotingPowerDelegationConfirmationDialog.tsx` at no commit in this repo's history, that
the HW-status section is instead pinned semantically via the
`selectedWallet.isHardwareWallet ?` branch at `:179-185`, and that the empty production
diff proves the file untouched; and the `nix fmt` unavailability with
prettier-on-explicit-paths substituted. One item travels forward as a note, not a debt:
these pins are authored to survive task-175's identity-block growth, so if task-175
turns any of the four cases red, that is the pin doing its job on an out-of-charter
change, not a stale test.

---

## Code Review: 2026-07-28 — task-175 round 1

**Scope reviewed.** The uncommitted working tree for task-175, against its guide
section `cv-2-implementation-guide.md:3805-4211` (Files-touched at `:3807-3814`,
Context at `:3816-3846`, the locked-invariant block at `:3848-3880`, Steps 1-6, and
the acceptance record at `:4199-4211`), the task-175 tracker row
(`governance-drep-discovery-plan-tasks.json:1393`, still `"status": "pending"`,
`"statusReason": null`), the PRD row charter at `cv-2-PRD.md:158` with its acceptance
restatement at `:254-258`, the descriptor table at `:685-686`, and invariants 10 / 11
/ 13 at `:1506-1516`. HEAD is `218f853f7` (task-142 committed). Three independent
lenses ran — guide conformance and runtime correctness, locked invariants plus the
sanitization floor, and tests plus simplicity and drift. Two returned `approved` with
zero blockers; one returned `requires_changes` with a single major. Nothing was
accepted on report: every promoted and dropped claim below was re-derived first-hand
in this worktree, including two live probes. The main checkout `/workspaces/daedalus`
was never read, edited or run against, and this round fixed no code.

**What landed.** `git status --porcelain` → exactly 3 modified, 0 staged, 0 untracked,
matching the guide's Files-touched list:
`VotingPowerDelegationConfirmationDialog.messages.ts` (+12/-0),
`VotingPowerDelegationConfirmationDialog.tsx` (+41/-2),
`VotingPowerDelegationConfirmationDialog.spec.tsx` (+130/-0). The production hunk is
byte-identical to the guide's Step 4 template (`:3956-4008`) — the `!isSentinelVote ?`
predicate, `drepIdentity?.raw ?? chosenOption` in the primary `<code>`, the
`drepIdentity?.cip105` and `drepIdentity?.credentialHex` conditional blocks, and
`<DRepSourceLabel source="on-chain" />` gated on `drepIdentity` — and the sentinel
else-branch is copied through unchanged. The two new descriptors carry the `!!!`
markers the PRD table fixes (`messages.ts` diff: `drepIdCip105` → `!!!CIP-105 DRep ID`,
`signedPayload` → `!!!Signed payload`), and both catalogs plus
`translations/messages.json` and the tracker JSON are clean. Re-run here:
`jest --testPathPattern=VotingPowerDelegationConfirmationDialog --no-coverage
--runInBand` → **1 suite / 26 of 26 passed**, exit 0 — the 17 pre-existing cases
(including task-142's four pins) plus the 9 new identity-block cases, none weakened.

### Blockers

**One major survives.** It is not an implementer deviation — the diff follows the
guide's "replace with exactly" template character-for-character — it is a gap in the
template itself, surfaced by an input the guide never contemplated.

**CR175-1 (major) — a CIP-105 script id renders the same string twice under two
different labels, and the guide's template cannot suppress it.** For any
`drep_script1…` input, `normalizeDRepIdentity` returns `raw === cip105`
(`normalizeDRepIdentity.ts:53-59`: `return { raw, cip129: bech32.encode('drep', …),
cip105: raw, … }`). The dialog then prints that one string in the primary `<code>`
(`VotingPowerDelegationConfirmationDialog.tsx:166-168`, `drepIdentity?.raw ??
chosenOption`) and again in the unconditional secondary block
(`:170-181`, guarded only by `drepIdentity?.cip105 &&`). The rendered DOM is
"!!!DRep ID: drep_script1t39n…" immediately followed by "!!!CIP-105 DRep ID:
drep_script1t39n…", with no CIP-129 line anywhere.

*Reachability, measured in this worktree, not inferred.* `Cardano.DRepID.isValid(
'drep_script1t39n52gcwur0texnc2c6p8uw04k9kj3e9qtsda0y60ptzae75nh')` → **`true`**
(`drep_vkh1…` → `false`, so only the script form gets through). That predicate is the
sole input gate: `VotingPowerDelegation.tsx:221` is `const drepInputIsValid =
Cardano.DRepID.isValid(state.drepInputState.value);`, feeding `formIsValid` (`:223-225`)
and `chosenOption` (`:242-245`). The plans already record the acceptance
(`cv-2-PRD.md:562-563`, `cv-2-implementation-guide.md:2467-2468`). A throwaway jest
probe under `source/renderer/app/utils/governance/` (created and deleted in the same
command; `git status --porcelain` re-verified as the same 3-modified set afterwards)
returned `{"rawEqCip105":true,"cip129":"drep1ydwykw3frpmsda0y60ptrgyl3e7kck628y5pwph4unfu9vg6sn5zd"}`
— and that `cip129` is exactly the `SCRIPT_CIP129` vector the new spec already uses,
so the form the template calls primary is derivable and simply never shown. The case
is reachable twice over: by paste, and by task-138's re-seed, since
`VotingPowerDelegation.tsx:185-186` seeds the input from `currentVote.drep.raw`, which
`cv-2-PRD.md:543` states "is byte-untouched and may be CIP-105".

*Why it survived adjudication as a defect rather than an accepted consequence.* No
cv-2 or anchor-2 row owns it — the anchor-2 `DRepIdDisplay` dual-form row explicitly
records that "the confirmation dialog keeps its own §7 identity block from task-175",
and `cv-2-PRD.md:1374` repeats it. None of the nine new cases exercises a CIP-105
input, so nothing catches it. And the guide itself already reasons about exactly this
failure mode one screen earlier: its Step 6 gate note (`:4180-4183`) justifies the
container cases by "which are different strings, so no ambiguous match appears" — the
CIP-105-raw case is the one input where they are the *same* string, which will make a
plain `getByText(<the id>)` throw on multiple matches for any later suite (task-147's
CIP-105-raw flow case, `cv-2-PRD.md:1610`) that renders one through this dialog.

*Fix (one guard, no new key).* Change the guard at
`VotingPowerDelegationConfirmationDialog.tsx:170` from `{drepIdentity?.cip105 && (` to
`{drepIdentity?.cip105 && drepIdentity.cip105 !== drepIdentity.raw && (`. That keeps
the primary line as `drepIdentity?.raw ?? chosenOption`, so invariant 10 is untouched;
fabricates nothing; and leaves all 26 current cases green, because both CIP-129
vectors have `cip105 !== raw`. Add one case rendering the script CIP-105 id that
asserts `screen.getAllByText(SCRIPT_CIP105)` has length 1 and that
`'!!!CIP-105 DRep ID'` is absent, with the signed-payload line and the source label
still present. Because this deviates from a "replace with exactly" template, record
the deviation in the tracker `statusReason` alongside the AC-4 note.

### Minors — absorbable in the same pass, none blocking

**CR175-2 (minor) — the predicate flip makes `mapVoteToIntlMessage`'s `default` arm
unreachable and empties a pre-existing assertion.** `default: return
sharedGovernanceMessages.delegateToDRep;` (`VotingPowerDelegationConfirmationDialog.tsx:38-39`)
has one call site, `:206`, which now sits inside the `: (` branch entered only when
`isSentinelVote` is true (`:119-120`, `:157`), so `chosenOption` there is always
`abstain` or `no_confidence`. The committed pin
`expect(screen.queryByText('Delegate to DRep (default)')).not.toBeInTheDocument();`
(spec `:69-71`) therefore asserts the absence of copy the component can no longer
render under any input. The flip itself is correct and guide-mandated — `:3299-3302`
assigns "the predicate fix" to task-175 precisely so a decoder-rejected id stops
falling into the vote-label branch — so this is dead-code hygiene, not a regression.
D-3 forbids weakening the 141/142 pins, so do not touch the case in this round.
*Fix:* record it in `statusReason`; the clean follow-up is to narrow the parameter to
`'abstain' | 'no_confidence'`, delete the default arm, and re-point spec `:69-71` at
the live guard `expect(screen.queryByText('Vote')).not.toBeInTheDocument()`.

**CR175-3 (minor) — the shared default fixture is now an unproducible identity
shape.** `renderDialog` defaults to
`drepIdentity={{ credentialType: 'key', raw: VALID_DREP_ID }}` (spec `:45`) — no
`cip105`, no `credentialHex`. Both live producers go through `normalizeDRepIdentity`
(`VotingGovernancePage.tsx:87`, `Governance.stories.tsx:59-62`), which populates all
five fields on success (`normalizeDRepIdentity.ts:38-44`, `:53-59`). Before this diff
the component read only `.raw`, so the partial shape was inert; now it suppresses
three of the four template parts, so every case defaulting through it pins a DOM
production cannot reach. Harmless today — those cases assert fee / HW / passphrase /
chrome copy — but it edits task-141/142-owned pinned rows, which is out of task-175's
charter. *Fix:* record the gap in `statusReason`; if a later row opens those cases,
switch `:45` to `drepIdentity={normalizeDRepIdentity(VALID_DREP_ID)}`.

**CR175-4 (minor) — AC-1's "in template order" clause is discharged by the template,
not by jest.** Every assertion in the new describe is presence-or-absence
(`screen.getByText` / `queryByText` / `.textContent`); none is positional, so swapping
the `cip105` block (`tsx:170-181`) with the `credentialHex` block (`:182-193`) leaves
all nine new cases green. This is knowingly allocated, not missed — the guide's own
acceptance line assigns AC-1 to "Step 4 + the first spec case" (`:4199-4201`). *Fix:*
either add one positional assertion to "renders all four parts for a key DRep" over
the `.paragraphTitle` texts, or state in `statusReason` that the order clause is
carried by the Step 4 template.

### Dropped findings, and why

1. *Not promoted — "the block never shows the CIP-129 form, so AC-1 is breached."*
   Dropped as framed. For a CIP-105 input, **invariant 10 forces `raw` into the
   primary slot** — `cv-2-PRD.md:1506-1509` and the guide's inline invariant block
   (`:3849-3861`) both require the primary line to be byte-equal to `chosenOption` and
   to the `delegateVotes` `dRepId`. Rendering `cip129` there would violate a locked
   invariant to satisfy a template sentence. What survives is only the *redundant
   duplicate*, promoted as CR175-1; the absent CIP-129 line is a consequence of the
   invariant and is not to be "fixed".
2. *Not promoted — the alternative fix of rendering `cip129` as a secondary line under
   a third descriptor.* Dropped as a cross-task contract break. task-175's descriptor
   count is fixed at two by `cv-2-PRD.md:685-686` and the guide's mint table
   (`:4801-4802`), and task-146 is chartered to mint exactly "the seven remaining
   enrichment keys". A third key authored here would silently invalidate task-146's
   fixed list. CR175-1's guard change mints nothing.
3. *Not promoted — the `prettier --check` warn on
   `VotingPowerDelegationConfirmationDialog.tsx`.* Pre-existing at HEAD, proven by the
   gate's in-repo probe (an out-of-repo `/tmp` copy reports a false clean because
   `--find-config-path` resolves nothing there). The two offending hunks —
   `(typeof messages)[keyof typeof messages]` at `:23-26` and the `useState<…>`
   wrapping at `:83-89` — are identical in the HEAD blob and the working tree, and
   task-175's added regions produce zero prettier diff. The file is in the documented
   baseline-red set, and the guide forbids running prettier on it (`:3204-3209`). Note,
   not blocker.
4. *Not promoted — the `[React Intl] Missing message` lines for
   `…confirmationDialog.drepIdCip105` and `…signedPayload` (18x each).* The expected
   interim state under D-9 (`:188-200`): descriptor present, catalog key absent, so
   react-intl falls back to the `!!!`-prefixed `defaultMessage`, which is what the
   assertions match. Both keys are seeded by task-146 and the assertions stay valid
   because both catalog values keep the `!!!` prefix. Same disposition as the 136 /
   140 / 173 rounds. The noise carries message ids only — never a DRep id — so it is
   not a sanitization leak either.
5. *Not promoted — `yarn i18n:manage` not run.* Not a skipped gate. D-9 names
   task-136, task-140 and task-175 as the three copy-minting rows that deliberately do
   not run it, and closes "a verifier seeing no i18n run on 136 / 140 / 175 is looking
   at this deviation, not a skipped gate." Running it here would have written a
   competing catalog diff on files only task-146 may own.
6. *Not promoted — the untouched tracker row and the missing commit.* Both are
   closing-phase obligations, not defects in the diff; neither implementer was
   permitted to do them. Carried as handoffs below.

**Independent re-checks that found nothing new.** The sanitization floor holds: the
diff adds only JSX plus one presentational import (`tsx:13`, `DRepSourceLabel`), and
`grep -nE "logger|console\.|analytics"` over the dialog is empty — the Step 6 pass
condition. `governance-sanitization` is green at 24 tests, exactly the count the guide
demands (`:4180`). Byte-equality is intact end-to-end: `normalizeDRepIdentity.ts:39`
sets `raw` to the untouched input, `VotingGovernancePage.tsx:84-87` derives the
identity from `chosenOption` itself, and the payload hex matches what the hardware
mappers send, now pinned by the two `Cardano.DRepID.toCredential` cases. Sentinels stay
form-only: `isSentinelVote` (`tsx:119-120`) uses the same two literals as the container
guard, and the two sentinel cases assert all four parts absent. Nothing is fabricated —
the legacy-id case proves a `null` identity renders the primary line verbatim with no
cip105, no payload and no source label, which also discharges the rendering half of
task-173 AC-2. Comment convention is clean: three comments added (`tsx:117-118`,
spec `:241-242`, `:310-312`), each 2-3 plain sentence-case lines stating a constraint,
none in a test name, no process ids, no ALL-CAPS. No local `IntlProvider` was
introduced; both Storybook call sites already derive identity through
`normalizeDRepIdentity` (`Governance.stories.tsx:59-62`), so no story drifts, and no
snapshot covers this dialog. `credentialHexOf` (spec `:243-249`) re-implements the
decode deliberately as an independent oracle rather than calling the code under test.

**Gate result and its attribution.** The supplied gate reports **PASS**, with one
`prettier --check` warn it attributes `pre-existing` / `CONFIRMED` — see dropped
finding 3; that attribution is accepted and is not a blocker for this task. **Re-run
here:** `git status --porcelain` (3 modified, nothing staged or untracked, re-verified
after both probes) and
`jest --testPathPattern=VotingPowerDelegationConfirmationDialog --no-coverage
--runInBand` → 1 suite / 26 of 26 passed, exit 0. **Probed here:**
`Cardano.DRepID.isValid` on both CIP-105 forms, and `normalizeDRepIdentity` on the
script CIP-105 vector — both throwaway files deleted in the same command that created
them. **Inherited from the gate, not re-run:** `tsc --noEmit` exit 0; `yarn lint`
exit 0 at 5595 warnings / 0 errors with +0 new errors and +0 new warning classes on
the three changed files; `VotingGovernancePage` 18/18; `governance-sanitization`
24/24; the wave `"(governance|voting)"` pattern at 18 passed + 1 skipped of 19 suites
and 308 passed + 12 skipped of 320 tests, 9 snapshots, zero failures — reconciling
against the wave baseline as +9 task-175 and +6 committed 141/142. The lone skipped
suite is the documented `GovernanceCliArgvSmoke` self-skip (no `cardano-cli` in this
devcontainer). No `.scss` module is in the change set, so `typed-scss-modules` was
correctly not required; `yarn check:all` and `yarn storybook:build` were deliberately
not run, both being red at HEAD for the unrelated manager-webpack JSX-loader reason.
No `--write`, no `git stash`, no commit, and no file edited except this log. `nix` is
absent, so `nix fmt` remains the owed pre-merge obligation with prettier-on-explicit-
paths as the recorded substitute (F-12).

**Handoffs for the closing pass (not review findings).** (a) The tracker row
(`json :1393`, edited **by content**, never formatted with prettier) still needs
`status`, `statusReason` and `evidence`: AC-4's catalog half recorded as carried by
task-146 (guide `:4205-4211`), plus CR175-1's template deviation, CR175-2's now-dead
default arm, CR175-3's fixture gap and CR175-4's order-clause allocation. (b) The
prescribed commit subject is
`feat(gov): task-175 render the pre-anchor confirmation identity block`
(guide `:3202`). (c) One guide inaccuracy worth not carrying forward: the Context
paragraph claims `DRepSourceLabel`'s on-chain copy is `!!!On-chain` in both catalogs
at `en-US.json:354` / `ja-JP.json:354`; verified here, `ja-JP.json:354` is
`"governance.drepDirectory.source.onChain": "!!!オンチェーン"`. No code or test
impact — the spec's `IntlProvider` loads `en-US.json` only.

**Decision: requires_changes** — one major (CR175-1) and three minors. The major is a
one-guard production change plus one spec case, both inside files task-175 already
owns; the three minors are `statusReason` records with optional follow-ups, and none
of them justifies a round on its own. A round-2 review needs to re-verify only the
changed guard, the new case, and that the dialog suite is green at 27.

---

## Code Review: 2026-07-28 — task-175 round 2

**Scope reviewed.** The round-2 working tree for task-175 — the CR175-1 guard change and
the CR175-4 order pin added on top of the round-1 tree — against the same contract:
`cv-2-implementation-guide.md:3805-4213` (Files-touched `:3807-3814`, Step 4's
"replace with exactly" template `:3960-4014`, Step 5's spec snippet `:4033-4158`, Step 6's
gates `:4164-4193`, acceptance `:4195-4213`), the PRD descriptor table
(`cv-2-PRD.md:685-686`) and invariants 10 / 11 / 13 (`:1506-1515`), and the round-1 entry
above (`cv-2-code-review.md:3664-3896`). HEAD is `218f853f7`. Three independent lenses ran
— guide conformance and runtime correctness, locked invariants plus the sanitization
floor, and tests plus simplicity and drift — and **all three returned `approved` with
empty blocker lists**. Nothing was accepted on report: every claim below was re-derived in
this worktree. The main checkout `/workspaces/daedalus` was never read, edited or run
against, and this round fixed no code.

**What landed since round 1.** `git diff --numstat -- source/renderer/app` →
`messages.ts` 12/0, `spec.tsx` 157/0, `VotingPowerDelegationConfirmationDialog.tsx` 39/2 —
the same three files, still zero deletions in the spec, so no pre-existing case was
touched. Round 2 moved exactly two things:

1. **The CR175-1 guard.** `VotingPowerDelegationConfirmationDialog.tsx:170` is now
   `{drepIdentity?.cip105 && drepIdentity.cip105 !== drepIdentity.raw && (`. Measured, not
   read: `diff -u` of the guide's Step 4 template (`:3961-4013`) against the live block
   (`tsx:157-209`) returns **exactly one hunk, that one line** — the primary `<code>`, the
   `credentialHex` block, the `<DRepSourceLabel source="on-chain" />` gate and the sentinel
   else-branch are byte-identical to the template. The guard is the round-1 fix text
   verbatim, and it is the only sanctioned deviation from a "replace with exactly" block.
2. **Two spec additions** (+27 lines): the CIP-105-raw case at `spec.tsx:296-309` and the
   positional `templateOrder` assertion at `spec.tsx:271-281`.

**Blockers: none.** No finding survived adjudication, and no lens proposed one.

**Round-1 findings, dispositioned.**

- **CR175-1 (major) — closed.** The duplicate-line defect is fixed at its root:
  `normalizeDRepIdentity.ts:53-59` returns `raw` and `cip105` as the same string for a
  CIP-105 input (`:54` `raw,`, `:56` `cip105: raw,`), so the added inequality half of the
  guard is the precise suppressor. The new case is not vacuous in either direction —
  `expect(screen.getAllByText(SCRIPT_CIP105)).toHaveLength(1)` (`spec:299`) would read 2
  and `queryByText('!!!CIP-105 DRep ID')` (`:300`) would be present if the guard were
  reverted, while `:301-308` still require the primary title, the signed-payload line and
  the on-chain label, so it does not over-assert absence. Invariant 10 is untouched: the
  primary slot is still `drepIdentity?.raw ?? chosenOption` (`tsx:166-168`); the change
  only decides whether a redundant second line renders, and fabricates nothing.
- **CR175-4 (minor) — closed, and the pin is real.** `spec:271-281` filters every `<p>`
  textContent in DOM order against
  `['!!!DRep ID', '!!!CIP-105 DRep ID', '!!!Signed payload', 'Transaction fee']` and
  compares with `toEqual`, so swapping the `cip105` block (`tsx:170-181`) with the
  `credentialHex` block (`:182-193`), or losing either, turns it red. `'Transaction fee'`
  is the live catalog value (`en-US.json:950`), so the pin also fixes the identity block
  ahead of the fee row. Forward-safe across task-146: the two new titles keep their `!!!`
  markers, which `cv-2-PRD.md:685-686` and invariant 11 (`:1510-1512`) fix as the values
  task-146 will seed.
- **CR175-2 and CR175-3 (minors) — deliberately untouched, correctly.** The now-unreachable
  `default:` arm of `mapVoteToIntlMessage` and the partial `drepIdentity` default fixture
  at `spec:45` were adjudicated in round 1 as `statusReason` records whose code remedies
  edit task-141/142-owned pinned rows; D-3 forbids weakening those pins in this row. They
  carry to the closing pass unchanged and are **not** grounds for a round 3.

**Dropped findings, and why.** No lens filed a blocker, so this round adjudicated the
observations attached to their summaries and the round-1 carries:

1. *Not promoted — the `<DRepSourceLabel>` paragraph's position is still unpinned* (raised
   as a non-blocking observation by two lenses). Correct as a fact — the order pin covers
   the three `.paragraphTitle` texts plus the fee sentinel, and the label paragraph
   (`tsx:194-198`) carries no title, so it is filtered out. But this is the allocation
   CR175-4's own fix text prescribed ("one positional assertion … over the
   `.paragraphTitle` texts"), and AC-1 assigns the order clause to "Step 4 + the first spec
   case" (`guide:4197-4199`). A `statusReason` scope sentence, not a defect.
2. *Not promoted — the absent CIP-129 line for a CIP-105 input.* Re-affirmed from round 1.
   Invariant 10 (`cv-2-PRD.md:1506-1509`) forces `raw` into the primary slot; rendering
   `cip129` there would break a locked invariant to satisfy a template sentence, and a
   third descriptor would invalidate task-146's fixed seven-key list. Not to be "fixed".
3. *Not promoted — the `prettier --check` warn on
   `VotingPowerDelegationConfirmationDialog.tsx`.* Re-proven here rather than inherited:
   piping the working-tree file through `prettier` 2.1.2 and diffing against itself yields
   exactly the two documented HEAD-drift hunks — `(typeof messages)[keyof typeof messages]`
   at `:23-26` and the `useState<…>` wrapping at `:83-89` — and **nothing else**. The new
   82-column guard line at `:170` is left untouched by prettier, so round 2 adds zero
   format debt. No `--write` was run on any file, per `guide:4193`. Note, not blocker.
4. *Not promoted — `yarn i18n:manage` not run, and the `[React Intl] Missing message` lines
   for `…drepIdCip105` / `…signedPayload`.* Both are the D-9 interim state
   (`guide:188-200`), identical to the 136 / 140 / 173 rounds. `git diff --stat --
   source/renderer/app/i18n/locales translations` prints nothing and a grep for both key
   names across `en-US.json`, `ja-JP.json` and `translations/messages.json` returns no
   hits, so task-146 still owns the catalog half whole.
5. *Not promoted — the untouched tracker row and the missing commit.* Closing-phase
   obligations, not diff defects. Carried below.
6. *Correction to a number in the round-1 entry, recorded rather than edited (this log is
   append-only).* That entry reports the production file at `+41/-2`; the true figure is
   `+39/-2`, and its own cited total of "181 insertions" only reconciles with 39
   (12 + 130 + 39). Nothing behavioural depends on it; round 2's spec growth takes the
   total to 208 insertions / 2 deletions.

**Independent re-checks that found nothing new.** Sanitization floor holds:
`grep -nE "logger|console\.|analytics|electron-store"` over the dialog, its messages file
and `DRepSourceLabel.tsx` exits 1 with no output — the Step 6 pass condition — and the
round-2 delta is JSX-only, adding no store write and no observable. Sentinels stay
form-only: `isSentinelVote` (`tsx:119-120`) uses exactly the two `VoteType` literals, and
both sentinel cases still assert all four parts absent. Comment convention is clean: round
2 added **no** comment; the three from round 1 (`tsx:117-118`, `spec:241-242`, `:337-339`)
are each 2-3 plain sentence-case lines stating a constraint, none in a test or describe
name, no process ids, no ALL-CAPS. No local `IntlProvider` was introduced — both new
additions run through the existing `renderDialog` harness. `credentialHexOf`
(`spec:243-248`) remains an independent oracle rather than a re-import of the code under
test.

**Gate result and its attribution.** The supplied gate reports **PASS** with one
`prettier --check` warn it attributes `pre-existing` / `CONFIRMED`; that attribution is
accepted (dropped finding 3) and is not a blocker for this task. **Re-run here, not
inherited:** `jest --testPathPattern=VotingPowerDelegationConfirmationDialog --no-coverage
--runInBand` → 1 suite, **27 of 27 passed**, exit 0 — the 17 pre-existing cases including
task-142's four, plus 10 identity-block cases; `jest
--testPathPattern="(VotingGovernancePage|governance-sanitization)"` → 2 suites, **42 of 42
passed**, exit 0, so AC-5's task-111 spy suite is green at its required 24 and the
container's byte-equality cases still resolve unambiguously; `node_modules/.bin/tsc
--noEmit` exit 0, zero diagnostics; `eslint` over the three changed files → **8 problems, 0
errors, 8 warnings**, all on pre-existing regions (`spec:26`, `:32`, `:95`; `tsx:63-69`),
so +0 errors and +0 new warning classes; the catalog and sanitization greps above; and the
prettier stdout probe. **Inherited from the gate, not re-run:** the full `yarn lint` total
(5595 warnings / 0 errors) and the wave `"(governance|voting)"` sweep at 18 passed + 1
skipped of 19 suites, 308 passed + 12 skipped of 320 tests, 9 snapshots — the lone skip
being the environment-gated `GovernanceCliArgvSmoke`. No `.scss` is in the change set, so
`typed-scss-modules` was correctly not required; `yarn check:all` and `yarn
storybook:build` were deliberately not run, both red at HEAD for the unrelated
manager-webpack JSX-loader reason. No `--write`, no `git stash`, no commit, and no file
edited except this log. `nix` is absent, so `nix fmt` remains the owed pre-merge obligation
with prettier-on-explicit-paths as the recorded substitute (F-12).

**Handoffs for the closing pass (not review findings).** (a) The tracker row
(`governance-drep-discovery-plan-tasks.json:1393`) is still `"status": "pending"` /
`"statusReason": null`; it needs, edited **by content** and never formatted with prettier:
AC-4's catalog half recorded as carried by task-146 (`guide:4206-4211`), the CR175-1
template deviation with its one-line diff-vs-template evidence, CR175-2's now-dead default
arm, CR175-3's fixture gap, and CR175-4's scope note that the order pin covers the three
`.paragraphTitle` texts and leaves the source-label paragraph's position to the template.
(b) The prescribed commit subject is `feat(gov): task-175 render the pre-anchor
confirmation identity block` (`guide:3202`). (c) The round-1 guide inaccuracy stands:
`ja-JP.json:354` is `"governance.drepDirectory.source.onChain": "!!!オンチェーン"`, not
`!!!On-chain`; no code or test impact.

**Decision: approved** — zero blockers at any severity. All three lenses returned
`approved` with empty lists; the round re-derived the load-bearing claims first-hand rather
than rubber-stamping — the identity block was diffed line-by-line against the Step 4
template, the order pin's falsifiability was traced through its `toEqual`, the CIP-105-raw
case was checked against `normalizeDRepIdentity`'s actual return shape, and the prettier
attribution was re-measured — and six observations were dropped with reasons above. Both
round-1 items that required a code change (CR175-1, CR175-4) are closed by executing
assertions; the two remaining minors are `statusReason` records the closing pass absorbs.
No round 3 is warranted.

---

## Code Review: 2026-07-28 — task-144 round 1

**Scope reviewed.** The uncommitted working tree for task-144, against its guide
section `cv-2-implementation-guide.md:4235-4362` (the shared task-144/task-145 preamble
at `:4217-4231`, Files-created at `:4237-4242`, Context at `:4244-4272`, the four locked
invariants at `:4274-4286`, the three resolved judgment calls at `:4288-4295`, Steps 1-2
at `:4297` and `:4332`, and the acceptance record at `:4348-4362`), the task-144 tracker
row (`governance-drep-discovery-plan-tasks.json`, still `"status": "pending"` with no
`statusReason`), and the PRD's acceptance restatement at `cv-2-PRD.md:277-278`. HEAD is
`b699d176c` (task-175 committed). Three independent lenses ran — correctness against the
guide, locked invariants and the sanitization floor, and tests plus simplicity and drift
— and **all three returned `approved` with zero blockers**. As in the task-141 and
task-175 rounds, that was not accepted on report: the byte comparison against the guide
snippet, the remount mechanism, the per-render regeneration hazard, and every gate were
re-derived first-hand here. The main checkout `/workspaces/daedalus` was never read,
edited or run against, and this round fixed no code.

**What landed.** `git status --porcelain` → exactly one line,
`?? storybook/stories/governance/_utils/GovernanceWrapper.tsx`; one new untracked file,
**zero modified tracked files**, nothing staged. `git log --oneline -3` is unchanged from
baseline, so nothing was committed. The file is **byte-identical to the guide's Step 1
block**: a programmatic compare of guide `:4302-4329` against the 28-line file returns
equal. `fixtures.ts` is untouched, honouring the Files-touched line at `:4241-4242`, and
none of the three prettier-baseline-red paths — `VotingPowerDelegation.tsx`,
`VotingPowerDelegationConfirmationDialog.tsx`, `storybook/stories/voting/Governance.stories.tsx`
— nor the currently-green `containers/voting/VotingGovernancePage.tsx` appears in the
change set.

### Blockers

**None.** No survivor at any severity. All three lenses filed zero, and adjudication
promoted nothing they left unfiled.

### Independent re-checks of the code deliverable (nothing new found)

Re-derived here, not inherited. **The remount mechanism is real at the installed React,
not assumed**: `react` and `react-dom` are both `16.14.0`, and
`react-dom/cjs/react-dom.development.js:14281` gates the top-level unwrap on
`newChild.key === null` — so a **keyed** top-level fragment is not unwrapped, falls to
`reconcileSingleElement`, and a key mismatch deletes the existing child and builds a
fresh fiber, taking the whole subtree with it. That is the entire mechanism the guide
claims at `:4291-4293`, and it holds. **The per-render regeneration hazard was hunted,
not waved past** — this is the one way the wrapper's design could have broken its
consumer. Every hook in `VotingPowerDelegation.tsx` was enumerated: exactly two
`useEffect`s, deps `[currentVoteKind, currentVoteDRepId]` (`:209`) and
`[initiateTransaction, intl, state]` (`:282`), and **no** `useMemo` or `useCallback` at
all. Neither dep array contains `wallets` or `drepIndex`, so the fresh object identities
the wrapper mints on every render cannot loop an effect or reset form state. Two lenses
reached that verdict by describing the second effect as depending "only on primitives" /
on "a `state.status` guard"; that description is wrong — its deps include the `state`
object and `intl` — but the conclusion survives on the stronger ground that neither
fixture flows into it at all. **Wallet-id lookup survives non-remount re-renders**:
`fixtures.ts` contains no `Math.random`, `Date.now`, `uuid` or `nanoid`, and the ids are
hard-coded `'governance-wallet-1'`/`-2`/`-3` (`:111`, `:120`, `:129`), so
`wallets.find((w) => w.id === state.selectedWalletId)` (`VotingPowerDelegation.tsx:181`)
still resolves after a fixture rebuild. **All four locked invariants hold**: the key is
the option id verbatim, `<React.Fragment key={option}>` at `:21` with no composite, index
or hash, and `option` typed as the five-value `CurrentVoteOption` (`fixtures.ts:14`); the
wrapper never reads the knob (`grep -c useCurrentVoteKnob` → 0, the hook staying at
`fixtures.ts:29`); no provider of any kind is added (`grep -c IntlProvider` → 0), leaving
`storybook/preview.tsx:8` the sole owner of the single `IntlProvider` and the EN/JA
toggle, so convention 5 is honoured; and the factories are called inside the render
return (`:23-24`) with no module variable, ref or state anywhere in the 28 lines.
**Sanitization floor untouched**: the file contains no sink at all — no logger,
analytics, `electron-store`, `localStorage` or `console` — and the CIP-129 fixtures reach
only the render-prop callback, never a payload, so the wire-keyed `filterLogData` hazard
cannot arise. No second delegation backend: the sole `GovernanceStore` reference is a
type-only import (`:3`), erased at compile. **Comment convention clean**: the single
comment at `:17-18` is two plain sentence-case lines stating the invariant and its reason,
with no task id, CAT/CP label, plan name, PR number, ALL-CAPS emphasis or change history.

### Findings considered and not promoted

All three lenses filed empty blocker lists, so nothing was rejected; these are the
observations raised in the lens summaries or the gate that were examined and **declined**
as defects, each with its reason. (1) **Guide-vs-live line drift** — the Context block
anchors the lazy initializer at `VotingPowerDelegation.tsx:115`, live it is `:163`.
Declined: the guide itself instructs "re-anchor by the quoted content, not by the number"
(`:4230-4231`), and its two quoted lines match verbatim at `:163-164`; the drift is
task-139's `drepIndex` prop pushing the file down. Documentation-only, zero code impact.
(2) **AC-2 only partly satisfied** — declined as a defect because the guide pre-records
the split at `:4353-4362` and assigns the observed half to task-145 Step 8; demanding it
here would contradict the contract. (3) **The `no-unused-vars` warning** on the `fixtures`
parameter name — declined: `:4340-4344` predicts it and explicitly forbids both the rename
and the `eslint-disable`. The implementer correctly left it. (4) **No test for task-144** —
declined: Step 2 is format/typecheck/lint only and neither tracker acceptance criterion
names a spec, so there is no missing or vacuous assertion to report. (5) **Nothing imports
the wrapper yet** — `grep -rn "GovernanceWrapper"` over `.ts`/`.tsx` excluding
`node_modules` returns exactly one hit, its own declaration at `:19`. Declined as a defect,
kept as the carry-forward below. (6) **The tracker row is still `pending`/`statusReason`
null** — declined as a blocker after checking the slice's own convention: every completed
sibling's `statusReason` (task-141, task-142, task-143, task-175) describes "the appended
round-1 review-log entry and this row" as already existing, so the row is authored at
close-out *after* the review round, and its absence at review time is expected rather than
a gap. It travels as a close-out obligation below. (7) **`passwordUpdateDate: new Date()`
at `fixtures.ts:102`** mints a fresh object on every per-render factory call — declined:
it feeds no dependency array, `storybook/preview.tsx:9-11` configures `timemachine` to a
frozen date string so it is deterministic in Storybook regardless, and the file is
task-143's, already committed and outside this change set.

**Gate result and its attribution.** The supplied gate reports **PASS** with zero
failures and nothing to attribute; that is confirmed, and every count below was re-measured
here rather than inherited. `node_modules/.bin/prettier --check` on the one newly created
path → exit 0, "All matched files use Prettier code style!" — and prettier was run on **no
other path**, so none of the three baseline-red files was rewritten. `node_modules/.bin/tsc
--noEmit` → exit 0, zero diagnostics, matching the slice baseline. `yarn lint` → **exit 0,
5596 warnings, 0 errors**; `grep -cE "  error  "` over the full log → 0, and exactly one
warning is attributable to the new file — `GovernanceWrapper.tsx:14:14  fixtures is defined
but never used.  no-unused-vars` — the predicted-and-protected one from dropped finding 3.
`jest --testPathPattern="(governance|voting)" --no-coverage --runInBand` → exit 0, **18
passed + 1 skipped of 19 suites, 309 passed + 12 skipped of 321 tests, 9 snapshots**,
byte-identical to the wave baseline including the lone environment-gated
`GovernanceCliArgvSmoke` skip. No `.scss` is in the change set, so `typed-scss-modules` was
correctly not required; `yarn i18n:manage` was correctly **not** run — this task changes no
copy, and scoped status over `source/renderer/app/i18n` and `translations` returns 0 lines,
so there is nothing to revert. `yarn check:all` and `yarn storybook:build` were deliberately
not run, both red at HEAD for the unrelated manager-webpack JSX-loader reason. No
`--write`, no `git stash`, no commit, and no file edited except this log. `nix` is absent,
so `nix fmt` remains the owed pre-merge obligation with prettier-on-explicit-paths as the
recorded substitute (F-12).

**Handoffs for the closing pass (not review findings).** (a) The task-144 tracker row needs
authoring by content and never formatted with prettier, and it must carry the wording the
guide mandates at `:4358-4362`: **AC-2 recorded OWED in part** — the structural half proven
here by file shape plus `tsc --noEmit`, the observed half (type a DRep id, switch the knob,
confirm the field is blank) discharged by task-145's Step 8 visual pass, which this
container cannot run for want of a browser. **AC-2 must never be asserted green at this
commit.** (b) The prescribed commit subject is `feat(gov): task-144 add key-based
GovernanceWrapper remount for storybook` (`guide:4346`). (c) Nothing imports the wrapper
yet, so the green jest sweep proves **no regression, not new behaviour** — no test
exercises this file, and its first exercise is task-145's rewiring of
`storybook/stories/voting/Governance.stories.tsx`. (d) The guide's `:115` initializer
anchor is stale at `:163`; a future editor should re-anchor by content per `:4230-4231`.

**Decision: approved** — zero blockers at any severity. All three lenses returned
`approved` with empty lists; the round re-derived the load-bearing claims first-hand rather
than rubber-stamping — the file was byte-compared to the guide snippet, the keyed-fragment
remount was traced into the installed `react-dom` reconciler rather than taken from the
guide's assertion, the per-render regeneration hazard was tested against a full enumeration
of the consumer's hooks (correcting two lenses' reasoning while upholding their verdict),
fixture id stability was checked for nondeterminism, and all five gates were re-run — and
seven observations were declined with reasons above. The only open items are the AC-2 OWED
record and the commit, both close-out steps. No round 2 is warranted.

---

## Code Review: 2026-07-28 — task-145 round 1

**Scope reviewed.** The uncommitted working tree for task-145 — one modified file,
`storybook/stories/voting/Governance.stories.tsx` (72 insertions, 87 deletions) — against
its guide section `cv-2-implementation-guide.md:4366-4750`: Files-touched and Do-not-touch
at `:4368-4382`, the `drepIndex` precondition at `:4384-4388`, Context at `:4390-4448`, the
seven locked invariants at `:4450-4468`, the resolved judgment calls at `:4470-4496`,
Steps 1-7 at `:4498-4684`, Step 8 verification at `:4686-4736`, and the acceptance record
at `:4738-4750`. HEAD is `76cfabacc` (task-144 committed since the wave baseline
`b699d176c`, as expected). Three independent lenses ran — correctness against the guide,
locked invariants plus the sanitization floor, and tests plus simplicity and drift — and
**all three returned `approved` with zero blockers**. Consistent with the task-141,
task-175 and task-144 rounds, that was not accepted on report: every load-bearing claim
was re-derived first-hand, the gate's prettier attribution was independently reproduced,
and the one hazard analysis no lens completed was finished here.

**Surviving blockers: none.** No finding from any lens was promoted, and the adjudication
pass raised none of its own.

**What was re-derived rather than inherited.** (1) **Guide conformance, byte level.** All
seven prescribed edits match the guide snippets as written: the `WalletSyncStateStatuses`
removal and the `GovernanceWrapper` / `makeGovernanceWallets` / `useCurrentVoteKnob` /
`CurrentVoteOption` import block (`:37-56`, guide Step 1 at `:4500-4520`); the
`GOVERNANCE_WALLETS` deletion leaving **exactly one** blank line between
`toStoryDRepIdentity` and `const voteOptions` — verified with `cat -A`, which shows a
single bare `$` at the seam (guide Step 2 at `:4522-4527`); `renderGovernancePanel(option)`
wrapping `<GovernanceWrapper>` with `drepIndex` first in the prop list (`:173-208`, Step 3);
the knob read as the **first** statement of the `withState` callback at `:302`, threaded to
`:362` (Step 4); both delegation stories rewritten to the prescribed shape (`:375-409`,
Steps 5-6); and the two dialog stories migrated to
`makeGovernanceWallets('noDelegation')[0]` / `[1]` at `:443` / `:478` (Step 7). (2) **Index
mapping.** `[0]` is `isHardwareWallet: false` (`fixtures.ts:110-118`) and `[1]` is
`isHardwareWallet: true` (`:119-127`), so the software and hardware dialog stories keep
their respective confirmation paths — the substitution is not merely type-correct but
semantically correct. (3) **AC-2 / AC-3.** `grep -n
"GOVERNANCE_WALLETS\|generateWallet\|WalletSyncStateStatuses"` over the file returns
nothing, **exit 1**; all four HEAD reuse sites are migrated and the module-level binding is
gone. (4) **The retained `Wallet` import is still load-bearing** — used as a type at
`:252` (`selectedWallet: Wallet`), so Step 1's partial import trim orphaned nothing.
(5) **`useCurrentVoteKnob` is not a React hook** — `fixtures.ts:29-31` is a bare
`return select('Current vote (mock)', currentVoteOptions, 'noDelegation');`. Calling it
from two non-component arrow bodies and from the `withState` callback therefore raises no
hook-ordering question, and `grep -rn "react-hooks" .eslintrc*` returns **nothing**, so the
`rules-of-hooks` plugin is not even configured in this repo — there is no latent lint trap
being deferred.

**The per-render identity hazard, closed completely.** The single real behavioural change
is that wallets are now minted fresh on every render instead of being read from a stable
module-level array. Two lenses cleared this by checking **one** consumer effect each; that
enumeration was incomplete, so all four state-holding sites were checked here. `VotingPowerDelegation`'s form state is a **mount-only lazy initializer** (`:163`), so a new
`wallets` array identity cannot re-seed it. Its re-seed effect keys on **primitives** —
`:209` is `}, [currentVoteKind, currentVoteDRepId]);`. Its **second** effect, which **no
lens examined**, is `:282` `}, [initiateTransaction, intl, state]);` — `selectedWallet` is
read in the body but is **not** a dependency, and the body early-returns unless
`state.status === 'form-submitted'`, so the fresh object cannot drive it. (`initiateTransaction` is an unstable inline arrow in the story, but it was equally
unstable at HEAD — pre-existing, not introduced here.) `VotingPowerDelegationConfirmationDialog`'s effect is `:115`
`}, [intl, onSubmit, redirectToWallet, state]);` — again `selectedWallet.id` is read in the
body only. No render loop is reachable from this change on any path.

**Dropped findings.** No lens filed a blocker, so nothing was rejected; these are the
observations raised in the lens summaries or the gate that were examined and **declined**
as defects. (1) **AC-1 satisfied only in part** — declined: the guide pre-records the
scoping at `:4740-4748`, assigning the `Current Vote Summary` knob to task-136
(`CurrentVoteSummary.stories.tsx:64`) and placing the two dialog stories, `Unavailable
while syncing` and the directory/detail/badge stories out of scope because they render no
current-vote surface. Treating the partial coverage as a defect would contradict the
contract. (2) **AC-4 unverifiable** — declined: it is the guide's own **OWED** entry at
`:4728-4736`, which forbids asserting it green without a browser; an environment limit is
not a code defect. (3) **`prettier --check` red** — declined as pre-existing, proven below.
(4) **The dialog stories build three wallets to consume one** (`:443`, `:478`) — declined:
this is Step 7's verbatim prescription at `:4676-4684`, the cost is two discarded objects
in a story, and hoisting a shared constant would reintroduce exactly the module-level
mutable wallet state invariant `:4456-4458` forbids. (5) **`Voting power delegation -
prefilled from directory` demonstrates the directory hand-off only at the `noDelegation`
default** — declined as designed, not defective: `deriveFormSeed` ranks the wallet's own
`currentVote` ahead of the inherited `initialFormState.selectedDRepId`, and
`governance-wallet-1` is the one wallet `makeGovernanceWallets` gives a knob-derived
`votingTarget` (`fixtures.ts:117`), so at the other four values the story demonstrates
current-vote precedence instead. The guide states this outcome and explicitly instructs
recording it rather than "fixing" the seed order or re-pointing `selectedWalletId` at
`governance-wallet-2` — which is `isHardwareWallet: true` and would swap the confirmation
path (`:4650-4674`). (6) **No test added** — declined: `jest.config.js:129` sets
`roots: ['<rootDir>/tests', '<rootDir>/source']`, so a spec under `storybook/` could never
execute; Step 8 lists only grep/tsc/lint/prettier, and Jest coverage is task-147/148's row.
(7) **Minor lens anchor slips**, corrected for the record, no code impact: lens 1 cited
`fixtures.ts:58-60` for `useCurrentVoteKnob` (actually `:29-31`; `:58-64` is
`UNVERIFIED_DREP`) and `:150,161` for the `drepIndex` keys (`:161` is `votingPower`; the
keyed sets are `:149-150` and `:159-160`), and lens 3 cited the re-seed deps at `:209`
while lens 1 cited `:208` — `:209` is correct.

**Gate result and its attribution.** The supplied gate reports **PASS**; that verdict is
upheld, and its single red sub-gate is confirmed **pre-existing**. `git status --porcelain`
→ exactly one line, ` M storybook/stories/voting/Governance.stories.tsx`; no tracker JSON,
no locale catalog, no `translations/messages.json`, no commit, and `storybook/stories/index.ts` untouched as `:4374-4378` requires. `node_modules/.bin/tsc
--noEmit` → **exit 0**, zero diagnostics, matching the slice baseline; this also discharges
the `drepIndex` precondition at `:4384-4388`, and the prop is real and optional —
`VotingPowerDelegation.tsx:56` declares `drepIndex?: ReadonlyMap<string,
AppDRepDirectoryEntry>`, defaulted at `:153` and consumed at `:217`, so the value is used,
not merely accepted. `node_modules/.bin/eslint` on the file → **exit 0 with zero output**,
confirming the two dropped imports stranded nothing; the full `yarn lint` 5596-warning
count is not this task's — the one new warning belongs to task-144's committed
`GovernanceWrapper.tsx:14`. `prettier --check` on the file → **exit 1, RED, and it must
stay red** (`:4728-4736`): running `node_modules/.bin/prettier` to stdout and diffing
yields **exactly two hunks and no third** — `initializeTxErrorOptions` reflowed at `:71`
and `STAKE_POOLS_LIST`'s double assertion at `:91` — neither inside any block this task
wrote, and `grep -c '^@@'` on that diff returns **2**. `prettier --write` was **not** run
on this file or any other, so none of the three baseline-red paths was rewritten. No
`.scss` is in the change set, so `typed-scss-modules` was correctly not required;
`yarn i18n:manage` was correctly **not** run — this task adds and removes no message id,
so none of the four tool-managed files needed writing or reverting. `yarn check:all` and
`yarn storybook:build` were deliberately not run, both red at HEAD for the unrelated
manager-webpack JSX-loader reason. No `git stash`, no commit, and no file edited except
this log. `nix` is absent, so `nix fmt` remains the owed pre-merge obligation with
prettier-on-explicit-paths as the recorded substitute (F-12).

**Handoffs for the closing pass (not review findings).** (a) The task-145 tracker row must
carry the **AC-1 scoping note** verbatim from `:4740-4748` — the knob lands on `Connected
flow`, `Voting power delegation` and `Voting power delegation - prefilled from directory`
only. (b) **AC-4 is OWED and must never be asserted green at this commit**: no browser
exists in this container, so still unobserved are the five knob labels with the `Not
delegated (warning)` default, the per-value badge and caption rendering, the remount proof
(type a DRep id, switch the knob, confirm the field clears), the "no current-vote knob on
the two dialog stories" check, the EN→JA toggle re-check, and the console-error/overflow
pass. (c) This same visual pass is the **observed half of task-144's AC-2**, which task-144
deferred here; whoever writes `statusReason` must record the OWED entry on **both** rows.
(d) The prescribed commit subject is `feat(gov): task-145 wire the current-vote knob into
the governance stories` (`guide:4738`). (e) Carry forward the per-knob-value behaviour of
the prefilled-from-directory story (dropped finding 5) so a later reader does not re-file
it.

**Decision: approved** — zero blockers at any severity. All three lenses returned
`approved` with empty lists, and the round re-derived the load-bearing claims first-hand
rather than rubber-stamping: the file was compared against the guide snippets edit by edit,
the deleted-block blank-line detail was checked with `cat -A`, the fixture index-to-hardware
mapping was verified so the dialog substitution is semantically and not just type-correct,
the `use*`-named helper was confirmed to be a plain `select()` with no `rules-of-hooks`
plugin configured, the per-render identity hazard was closed against a **complete**
enumeration of all four consumer state sites — finishing an analysis both lenses left
partial while upholding their verdict — and the gate's prettier red was independently
reproduced as exactly two pre-existing hunks. Seven observations were declined with reasons
above. The only open items are the AC-1 scoping note, the AC-4 / task-144 AC-2 OWED record
and the commit, all close-out steps. No round 2 is warranted.

---

## Code Review: 2026-07-28 — task-146 round 1

**Scope reviewed.** The uncommitted working tree for task-146 — five modified files,
all insert-only — against its guide section `cv-2-implementation-guide.md:4775-5045`
(files-touched and do-not-touch at `:4777-4787`, the descriptor table and pipeline
context at `:4789-4851`, the five locked invariants at `:4853-4866`, the four
resolved judgment calls at `:4868-4883`, Steps 1-7 at `:4884-5029`, the acceptance
record at `:5031-5045`). HEAD is `b34a96848` (task-144 `76cfabacc` and task-145
`b34a96848` committed since the wave baseline `b699d176c`, as expected); task-146
itself is uncommitted, as instructed. Three independent lenses ran — correctness
against the guide, locked invariants plus the sanitization floor, and tests plus
simplicity and drift. Two returned `approved` with empty lists; the third returned
`requires_changes` with two **minor** findings, both in the one hand-written comment
block and its neighbouring constant. Both survived adjudication against the files.
The main checkout `/workspaces/daedalus` was never read, edited or run against, and
this round fixed no code.

**What landed.** `git status --porcelain` → exactly 5 ` M` lines, zero untracked,
zero staged, every path guide-named. `git diff --numstat` is **insert-only
everywhere**: `defaultMessages.json` `35/0`, `en-US.json` `7/0`, `ja-JP.json` `7/0`,
`tests/jest/i18n/preliminaryCopyMarkers.spec.ts` `34/0`, `translations/messages.json`
`35/0`. Both locale catalogs keep mode `100755` (the diff headers carry a single
unchanged `100755` and no mode-change line). No `*.messages.ts`, no whitelist, no
production source file, no tracker JSON.

### Blockers

**None at blocker or major severity.** Two minors survive; both are one-line fixes
inside `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` and both are absorbable by
the closing pass without a round 2.

**CR146-1 (minor) — the hand-written comment above `PRELIMINARY_CONFIRMATION_KEYS`
states something the catalogs contradict, and it points a future maintainer at a
locked invariant.** `tests/jest/i18n/preliminaryCopyMarkers.spec.ts:14-15` reads
"`// Only these two confirmation-dialog keys are preliminary; the rest of that`
`// namespace predates the feature and is legitimately unmarked.`" That is false as
written. Enumerated first-hand over the post-task catalogs, the
`voting.governance.confirmationDialog.` namespace holds **10** keys of which **three**
carry a leading `!!!` — `drepId`, `drepIdCip105`, `signedPayload` — so `drepId` is a
third preliminary key, not part of "the rest … legitimately unmarked". It is present
and byte-identical in both locales at `en-US.json:948` and `ja-JP.json:948`, both
`"voting.governance.confirmationDialog.drepId": "!!!DRep ID"`. The guide states the
same truth in the opposite direction at `cv-2-implementation-guide.md:4822-4824`:
"`voting.governance.confirmationDialog.` has **8** keys (`en-US.json:946-953`); only
`:948` … is marked — the other seven legitimately predate the feature", and the
resolved judgment call at `:4872-4875` scopes the namespace assertion out for exactly
that reason. Why it matters rather than being cosmetic: **no assertion in this file
guards `drepId`.** The committed case at `:25-34` only flags en-marked/ja-unmarked, so
it is silent if the marker is stripped from both locales; the namespace case at
`:45-52` covers `voting.governance.currentVote.` only; and `drepId` is deliberately
absent from `PRELIMINARY_CONFIRMATION_KEYS` (`:16-19`). The comment is therefore the
only thing standing between a maintainer and the locked invariant at
`cv-2-implementation-guide.md:4860-4862` ("`voting.governance.confirmationDialog.drepId`
keeps its existing value"), and it points the wrong way. Secondarily, the clause
"predates the feature" narrates change history rather than stating a constraint, and
the guide's Step 5 snippet at `:4959-4964` ships this constant with **no comment at
all**. *Fix:* delete the two comment lines, matching the guide's snippet; or restate
the constraint without the false claim and without the history clause, e.g. "This
namespace is pinned key by key rather than by prefix: most of its copy is already
reviewed and carries no marker." Filed by the tests/simplicity lens as QUALITY-1-1;
confirmed here by direct enumeration.

**CR146-2 (minor) — the current-vote namespace assertion is vacuous if the prefix
ever stops matching.** `tests/jest/i18n/preliminaryCopyMarkers.spec.ts:45-52` filters
`Object.keys(en)` by `CURRENT_VOTE_NAMESPACE` (`:12`) and asserts the unmarked
remainder is `[]`; nothing asserts the filtered set is non-empty. Reproduced
first-hand rather than reasoned about: running the exact predicate against the real
catalogs with the constant mutated to `'voting.governance.currentvote.'` matches **0**
keys and the assertion evaluates to `[]` — green while protecting nothing. Today the
real prefix matches **17** keys (measured), so a future namespace rename — precisely
the change this guard exists to survive — silently disarms it, and the committed case
above it reaches only the en-marked/ja-unmarked half. Recorded honestly: this is the
guide's **verbatim** Step 5 snippet (`:4979-4987`), so applying the fix is a
deliberate, semantics-preserving hardening beyond the spec, not the correction of a
deviation; it must be labelled as such in the tracker `evidence` so a later reader
does not file it as drift from the guide. *Fix:* bind the filtered list, assert
`expect(currentVoteKeys.length).toBeGreaterThan(0);`, then run the existing marker
filter over that binding. Leave the assertion text and the rest of the case unchanged.
Filed by the tests/simplicity lens as QUALITY-1-2.

### Independent re-checks (nothing new found)

The two `approved` lenses were not accepted on report; their load-bearing claims were
re-derived here. **Catalog content**: `git diff` on both locale files shows exactly
7 added lines each, at the correct ASCII-sorted insertion points, with every value
byte-matching the guide's quoted blocks at `:4905-4912` (en-US) and `:4923-4930`
(ja-JP) — em dashes, the apostrophe in "This DRep's", the `{target, select, drep …
abstain … no_confidence … other …}` branch keys and the trailing full-width `。` all
intact. **Marker invariant**: all 7 new en-US and all 7 new ja-JP values carry the
leading `!!!`, and a full-catalog sweep leaves exactly one en-marked/ja-unmarked key,
the pre-existing allow-listed
`wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement` (spec `:8-10`).
**Parity**: 1618 keys per locale, zero one-sided. **`drepId` untouched**: confirmed
at `en-US.json:948` / `ja-JP.json:948` above; the insert-only `numstat` independently
proves no pre-existing value was altered in either catalog. **Counts**: 17
`currentVote` and 10 `confirmationDialog` keys per locale, matching Step 6 exactly.
**Suite**: `node_modules/.bin/jest --testPathPattern=preliminaryCopyMarkers
--no-coverage --runInBand` re-run here → **1 suite / 4 tests passed, 0 snapshots,
exit 0**, all four names green. **Non-vacuity of the two other new cases** was checked
by the tests lens by mutation and is consistent with the file as read: stripping a
marker from an en-US current-vote value surfaces
`voting.governance.currentVote.status.inactive`, from a ja-JP value surfaces
`voting.governance.currentVote.sameVoteHint`, and deleting a pinned confirmation key
surfaces `voting.governance.confirmationDialog.signedPayload`. **Sanitization floor**:
zero files under `stores/`, `api/` or `ipc/` are in the change set, the added lines
contain no DRep id, no bech32/CIP-129/CIP-105 string and no long hex, and the
`abstain` / `no_confidence` tokens appear only as ICU select branch keys in renderer
display copy — never as a logged payload. `status.expiring` / `status.expiringBadge`
live in the display-only `currentVote.status.*` namespace and add no canonical status
beyond `active | inactive`.

**Dropped findings.** No lens finding was rejected — the two `approved` lenses filed
nothing, and both findings from the third survived. These are the observations raised
in the lens summaries or the gate that were examined and **declined** as defects.
(1) **AC-3 satisfied only in part** — declined: the guide pre-records it at
`:5041-5043` as **NOT satisfied** and OWED, because the ja-JP length/overflow pass
needs a running Storybook with the global Japanese toggle and there is no browser in
this container. An environment limit the contract already books is not a code defect.
(2) **The gate's non-failing observation that the comment block is
"convention-compliant"** — declined as an incomplete check, and **overridden** by
CR146-1: the gate tested the comment for task ids, plan names, ALL-CAPS and change
history but did not test its factual claim against the catalogs, which contradict it.
The verdict PASS is unaffected; this was never a gate condition. (3) **`yarn lint`
reporting 5596 warnings against a ~5591 baseline** — declined as not this task's: the
lint script globs `source storybook utils --ext .ts,.tsx`, so `tests/` is never linted
and `.json` is outside `--ext`; linting the changed spec directly returns "File ignored
by default", exit 0. The +5 traces to the two commits that landed after the baseline,
3 of them from task-144's `storybook/stories/governance/_utils/GovernanceWrapper.tsx`.
(4) **`yarn i18n:manage` emitting formatjs "Duplicate message id" warnings and a long
untranslated-keys listing** — declined as pre-existing: this task modified no
`*.messages.ts` (the change set is 5 files, none of them a descriptor), so no
descriptor it owns can be a source, and the flagged namespaces — `assets.assetToken.*`,
`staking.stakePools.tooltip.*`, `wallet.*`, `governance.drepDirectory.*` — are outside
the two files this task's ids come from. (5) **`prettier --check` red on the three
baseline files** — not applicable and correctly untouched: all five changed files are
either tool-managed JSON or the pre-existing spec, and Step 6 (`:5020-5023`) forbids
running prettier on any of them. No `prettier --write` was run by any party.

**Gate result and its attribution.** The supplied gate reports **PASS with zero
failures**, and that verdict is upheld. `tsc --noEmit` → exit 0. `yarn lint` → exit 0,
zero errors, warning delta attributed above to task-144/145. Jest, using the two
patterns Step 6 names verbatim: `preliminaryCopyMarkers` → 4/4 green (independently
re-run here, same result); `voting-governance|VotingGovernancePage` → 3 suites / 56
tests / 7 snapshots passed with **zero snapshots written**, which is the snapshot-
stability check the guide asks for and which holds because every new en-US catalog
value equals its descriptor's `defaultMessage` (`CurrentVoteSummary.messages.ts:74,80,87,93,99`;
`VotingPowerDelegationConfirmationDialog.messages.ts:21,27`). The broad
`(governance|voting)` pattern → 18 of 19 suites passed, 309 passed / 12 skipped,
byte-identical to the wave baseline including the deliberate `GovernanceCliArgvSmoke`
self-skip. `yarn i18n:manage` — kept here, because **task-146 is the one task in this
slice that owns those writes** — ran clean and **fully idempotent**: all four
tool-managed JSONs byte-identical by md5 after a second run, no "Added keys" /
"Deleted keys" sections, `git diff --numstat` unchanged. That discharges AC-4
(`:5044`). `typed-scss-modules` was correctly not required (no `.scss` in the
change set). `yarn check:all` and `yarn storybook:build` were deliberately not run,
both red at HEAD for the unrelated manager-webpack JSX-loader reason. No `git stash`,
no commit, and no file edited except this log.

**Handoffs for the closing pass (not review findings).** (a) Absorb **CR146-1** and
**CR146-2** before the commit; both are single-line edits in
`tests/jest/i18n/preliminaryCopyMarkers.spec.ts`, and the file is prettier-clean today
(`prettier --check` on it → exit 0), so keep it that way by hand rather than running
`--write` on anything. (b) Record **CR146-2's fix as a deliberate hardening beyond the
guide's verbatim Step 5 snippet**, so it is not later mistaken for drift. (c) **AC-3's
second half is OWED** and must never be asserted green at this commit — the ja-JP
length / layout overflow pass needs a browser this container does not have; the
`statusReason` must carry that reason. (d) `nix` is absent, so `nix fmt` remains the
owed pre-merge obligation with prettier-on-explicit-paths as the recorded substitute.
(e) The prescribed commit subject is `feat(gov): task-146 carry the current-vote
enrichment copy into both catalogs` (`guide:5028`). (f) Neither the tracker row nor
this log's AC evidence had been updated at review time; both are close-out steps.

**Decision: approved** — no survivor reaches blocker or major severity. Two lenses
returned `approved` with empty lists and the third returned `requires_changes` on two
minors; adjudication confirmed both minors first-hand (the false comment by
enumerating the `confirmationDialog` namespace and finding three marked keys, not two;
the vacuity by running the mutated predicate and watching it pass green over zero
keys) and promoted them as **minor and absorbable** rather than as a fix round, because
neither affects runtime, catalog content or any gate, and each is a one-line edit
inside a file the closing pass must open anyway. Five observations were declined with
reasons above, including an override of the gate's "convention-compliant" reading of
the comment block. The deliverable itself — 14 catalog values byte-matching the spec,
insert-only, markers intact, 1618/1618 parity, `drepId` untouched, `i18n:manage`
idempotent — is correct as written. No round 2 is warranted.
