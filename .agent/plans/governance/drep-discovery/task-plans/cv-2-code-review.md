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
