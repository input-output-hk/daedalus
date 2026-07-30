# anchor-2 — Research findings

> Durable findings from the anchor-2 slice. Facts only; every `path:line` below was
> opened in the anchor-2 worktree at base `55e8985bf` (branch `feat/drep-discovery`)
> and verified there. Each finding carries **Resolution.** (what is true and what the
> slice does about it), **Disposition.** (fixed now / rides with task-NNN / raised to
> the user / record-only) and **Owner.** (who discharges it).

---

## F-1 (task-157) — The implementation guide defines the payment-address caption but never renders it, while its own spec asserts that string is in the document

Step 6's JSX adds `paymentAddressCaption` to `defineMessages` and Step 8 requires its key in both
locale catalogs, but nothing in the Step 6 markup references the descriptor. The same guide's Step 11
spec (`anchor-2-implementation-guide.md:1913-1918`) asserts the rendered text
`!!!This address is the DRep's own claim. Delegating your voting power requires no payment to any address.`
is in the document. Following Step 6 literally therefore ships a dead catalog key **and** a
guaranteed-red test in a later step of the same task.

**Resolution.** Reconciled toward the asserted behaviour, not the literal markup. The caption renders
as `<p className={styles.mutedValue}>` between the payment-address heading and the value row,
mirroring the caption-before-content ordering the guide mandates for the identity-references
sub-section. Independently corroborated outside the guide by `drep-discovery-design.md:226`: *the
accompanying copy states that the address is the DRep's own claim and that delegating voting power
requires no payment to any address*. The AC for `paymentAddress` requires the same copy.

**Disposition.** Fixed now, and the resolution is the one the design doc and the AC both demand — the
literal Step 6 reading would have failed AC-6. **Owner.** Discharged in task-157.

---

## F-2 (task-157) — The payment-address copy handler intentionally has no logger on either failure path, breaking symmetry with the DRepIdDisplay precedent

`DRepIdDisplay.tsx:52` and `:62` call `logger.warn` on the missing-clipboard and rejected-write
paths. The new payment-address copy handler in `DRepDetailAnchorContent.tsx` deliberately calls no
logger on either path.

**Resolution.** This is the sanitization floor, not an oversight. A CIP-119 `paymentAddress` is a
bech32-shaped value tied to a real DRep, and the slice's floor is that neither the value nor any
derived quantity — including a length — reaches a logger. The guide's
`reaches no logger on either payment-address copy path` test in `DRepDetailPage.spec.tsx` pins it,
and `tests/jest/security/governance-sanitization.spec.ts` (35 → 37) pins the `filterLogData` half.

**Disposition.** Record-only, and a standing trap: a future reader restoring the `logger.warn` "for
symmetry with `DRepIdDisplay`" turns two passing tests red and reopens the sanitization floor.
**Owner.** Record-only; the two tests are the enforcement.

---

## F-3 (task-157) — The guide's verbatim parser code uses `continue`, which the repo's ESLint config rejects as an error, while the same guide's Verify block expects zero errors

The Step 2 verbatim block for `readCip119References` in
`source/main/governance/AnchorVerificationService.ts` is written with `continue` statements. The repo
enables `no-continue` as an **error**, so the transcribed code produced three lint errors at `:82:25`,
`:96:7` and `:100:64` — and they were the *only* errors in the whole `yarn lint` run; everything else
in that run is a warning. Step 15b expects exit 0 with 0 errors. `git show HEAD:source/main/governance/AnchorVerificationService.ts | grep -n continue`
exits 1, so the rule had never been exercised on this file before.

**Resolution.** The loops were rewritten without `continue`. `eslint` on that file now exits 0 with a
single `prefer-destructuring` warning, and `AnchorVerificationService.spec.ts` re-runs at 21/21 after
the rewrite.

**Disposition.** Fixed now. Carries forward as a check on every remaining anchor-2 task: verbatim
guide code is not lint-clean by construction, and a guide block that predicts `0 errors` is a
prediction, not a guarantee. **Owner.** Discharged in task-157; the remaining five task guides in
this slice have not been audited for the same pattern.

---

## F-4 (task-157) — Sharding the build across implementers leaves the generated message catalogs un-regenerated, because every shard is individually forbidden to run the gate that writes them

`yarn i18n:manage` mutates files, so each implementer shard's brief forbids it — correctly, since an
uncontrolled run has to be reverted surgically. The consequence is that no shard owns the
regeneration, and `source/renderer/app/i18n/locales/defaultMessages.json` and
`translations/messages.json` were still **clean** when the verifier ran, i.e. never regenerated,
even though the guide's file table lists `defaultMessages.json` as produced by that gate and both
`351467833` (task-151) and `74bf92cdd` (task-172) ship the pair.

**Resolution.** Both files are now in the diff at **+65 lines each** — exactly the 13 new keys, no
unrelated churn, and no whitelist JSON touched by the run. The verifier's own `git restore` of both
files was the right call at the time (they were clean beforehand, so restoring returned the tree to
the state the implementer left it in); the gap was that the task was not yet finished, not that the
restore was wrong.

**Disposition.** Fixed now. Rides forward as a slice-level rule: whichever agent runs last on a task
that mints i18n keys owns the `yarn i18n:manage` run and the surgical revert of anything it touched
that was clean beforehand. **Owner.** Applies to task-153, task-174, task-154, task-155 and
task-156.

---

## F-5 (task-157) — A HEAD-versus-worktree prettier comparison run out of `/tmp` reports a false clean, because `.prettierrc` resolution is path-relative

Establishing that prettier drift is pre-existing requires formatting the HEAD version of the file and
comparing. Writing that HEAD copy to `/tmp` and checking it there reports **clean** — prettier
resolves its config by walking up from the file's own path, and from `/tmp` it never reaches the
repo's `.prettierrc`, so a different (default) config is applied and the drift disappears.

**Resolution.** The HEAD copy must be written to a **sibling path inside the same directory** as the
original. Done that way, the HEAD copies of `source/common/utils/logging.ts`,
`tests/jest/governance/GovernanceStore.spec.ts` and
`tests/jest/security/governance-sanitization.spec.ts` reproduce the *same three hunks* the working
tree shows — the `.reduce(…)` hug, the long-URL `url:` break, and the `((x as unknown) as jest.Mock)`
double-paren — proving the drift is inherited. `git diff` confirms task-157 touched none of those
regions; its `logging.ts` change is six array entries. Sibling files were deleted afterwards.

**Disposition.** Record-only, and directly reusable — this is the method any future baseline-versus-
worktree formatter comparison in this repo has to use. The three files stay prettier-red; `nix fmt`
before merge is a user-owned obligation that will settle them. **Owner.** User, at merge.

---

## F-6 (task-157) — The acceptance criterion's design-doc line citations have drifted by one and by three

AC-1 cites `drep-discovery-design.md:215` for the render list and requires the image deferral to be
recorded in that design doc. Both facts hold; both anchors are stale. The render list is at `:216`,
and the image deferral is at `:218`. The `references` `@type` rules sit at `:220-224` and the
`paymentAddress` rule at `:226`.

**Resolution.** Verified by grep in this worktree: the deferral sentence returns exactly one hit, at
`:218`. **No design-doc edit was required or made** — AC-1's substantive requirement (that the
deferral be recorded) was already satisfied before this slice began.

**Disposition.** Record-only; the tracker `statusReason` carries the correction so nobody re-opens
the design doc looking for a missing deferral at `:215`. **Owner.** Record-only.

---

## F-7 (task-153) — `doNotList` cohort exclusion is best-effort, because the flag only exists for DReps whose anchor was fetched in the current session

`doNotList` reaches `AppDRepDirectoryEntry` through the same lazy, per-detail-visit anchor fetch that
populates `verifiedName` (`GovernanceStore.fetchAnchorContent`, triggered from `DRepDetailPage`), and
`_rehydrateDReps` seeds it to `false` for every entry. Nothing fetches anchors in bulk, so at the
moment the default cohort is computed the store knows the real flag only for DReps the user has
already opened this session.

**User-visible consequence.** A DRep that declares `doNotList: true` still appears in the default
cohort until something in the session has fetched its anchor, and a favorited `doNotList: true` DRep
that has never been opened renders no `governance.drepFavorites.staleCaption`. Open its detail view
once and both correct themselves for the rest of the session. Exclusion is therefore a best-effort
courtesy to the DRep's stated preference, **not a security or privacy control**, and nothing else in
the app depends on it being complete.

**Why bulk prefetch was rejected.** Closing the gap means an outbound HTTPS fetch to a third-party
host for every registered DRep on every refresh, which is exactly the bulk cohort anchor-prefetch
phase both design docs already defer beyond v1 (`shared-design-tokens.md:250`,
`drep-discovery-design.md:247`) — the same deferral that keeps verified-`givenName` search out of
v1. The alternative, gating cohort membership on an unresolved fetch, empties the cohort on a cold
start. Both are worse than an incomplete flag.

**Resolution.** Shipped as stated: the filter reads whatever the store knows, tests assert the
behaviour through the real fetch path rather than by injecting store state to simulate global
knowledge, and the limitation is written into task-153's tracker `statusReason` instead of being
engineered around.

**Disposition.** Record-only and a standing trap — a future reader "fixing" the partial exclusion by
adding bulk anchor fetching reopens a deferred design decision and multiplies outbound requests per
refresh. **Owner.** Closes only when a bulk cohort anchor-prefetch phase is planned on its own
merits.

---

## F-8 (task-153) — The design doc promised stale-favorite status badges that the closed `DRepStatus` union cannot express

`drep-discovery-design.md:112` stated that a stale favorite keeps its `Retired` or
`Excluded from default cohort` status badge. `DRepStatus` is the closed union `'active' | 'inactive'`,
and task-153's own acceptance list forbids adding a member to it, so neither badge can render. There
is also no unregistration signal anywhere in the pipeline from which a `Retired` state could be
derived.

**Resolution.** `:112` was struck and rewritten rather than left as drift: the stale favorite keeps
its real `active`/`inactive` badge, the `staleCaption` alone carries the not-in-cohort signal, and
`Retired` is recorded as deferred until a distinct unregistration signal exists. The design-doc diff
is exactly one insertion and one deletion. task-122's AC-5 badge clause is satisfied by that real
badge, and its row now records the `doNotList` half as exercised with the `Retired` half still
deferred under invariant #14.

**Disposition.** Fixed now. Rides forward as the reason no `Retired` or `Excluded from default cohort`
badge exists in this release; any future task that wants one owns the union widening and the
unregistration signal together. **Owner.** Discharged in task-153; the `Retired` half stays with
whichever phase delivers unregistration.

---

## F-9 (task-174) — The guide's Step 9 test count contradicts the spec block it supplies, and its Verify total contradicts its own per-suite deltas

Step 9 says the new `DRepIdDisplay.spec.tsx` adds **+10 tests**. The verbatim spec block the same step
supplies (`anchor-2-implementation-guide.md:4355-4503`) contains exactly **9** `it` blocks. The Verify
block is separately inconsistent with itself: it states `125 -> 140` (+15) while its per-suite deltas
sum to `+17`.

**Resolution.** The supplied code is the authority; the prose count is not. Using the block verbatim
gives 9 new tests, and the correct five-suite total is `125 -> 141` (9 + 3 + 2 + 2 + 0), which is what
both the implementer and the verifier measured. Neither number in the guide is reachable from the
guide's own artifacts.

**Disposition.** Record-only, and a trap for anyone re-running the gate: a verifier trusting the prose
would read the green `141` as a `+1` overshoot and the green 9-test suite as a missing test, and could
manufacture a tenth. The measured counts in the task-174 tracker row are the reference.
**Owner.** Record-only.

---

## F-10 (task-174) — AC-6's design-doc correction was already shipped, at a different line, before the task started

task-174 AC-6 asks that `drep-discovery-design.md:240-241` be corrected because it calls the card
identity *the dual-ID display*, contradicting shared tokens §4. That correction already exists at
`:249-259`, under the heading `## Directory Identity: ID-Only in v1`, which states that card identity
is the CIP-129-primary truncated ID with a single copy button and that the full dual CIP-129 + CIP-105
rendering belongs to the detail view and the deduped search row. The AC's own `:240-241` citation is
stale — the same citation drift already recorded for other ACs in F-6.

**Resolution.** No design-doc edit was made and none was needed. The `git diff` on
`designs/drep-discovery-design.md` is legitimately empty for this task, and an empty diff here is
evidence of a discharged AC rather than of a skipped step.

**Disposition.** Record-only. A reviewer auditing AC-6 by diff alone will read the empty diff as a
gap; the check is `grep` for the `Directory Identity: ID-Only in v1` section, not `git diff`.
**Owner.** Record-only.

---

## F-11 (task-154) — The guide's dialog test count counts declarations, while its own case body expands one of them into two runtime tests

Step 7 predicts `27 → 32` for `VotingPowerDelegationConfirmationDialog.spec.tsx` and describes the
addition as "+5 cases". Its skeleton lists case 5 as a single `it`, but the body text for that case
mandates `it.each(['abstain', 'no_confidence'])`. Following the body — which is the operative
instruction — produces 4 `it` plus 1 `it.each` of two entries, so declarations grow by exactly the
promised 5 (19 → 24) while the runtime count grows by 6 (`27 → 33`).

**Resolution.** 33 is correct and 32 is unreachable from the guide's own artifacts. The 27 baseline
was re-confirmed against `git show HEAD:<path>`, the spec file being byte-identical at `55e8985bf`
and at `589e95272`. The same split explains the full-run delta: 1299 → 1310 is 6+3+1+1 across the
four edited specs, not 5+3+1+1.

**Disposition.** Record-only, and the second instance of the pattern already recorded as F-9: this
plan's per-step test arithmetic counts `it` declarations, not executed tests, wherever `it.each` is
involved. A verifier comparing a green run against the prose will read the extra test as an overshoot.
**Owner.** Record-only.

---

## F-12 (task-154) — The container spec harness had no channel for seeding `drepIndex`, and its confirmation helper cannot reach the sentinel vote types

Step 8 instructs the implementer to seed `drepIndex` "through the governance store stub the harness
builds". In the live `VotingGovernancePage.spec.tsx` no such channel existed: `drepIndex` was
hard-coded inside `buildStores`, `StoreOverrides` had no field for it, and `openConfirmation` accepted
only a drepId. `openConfirmation` additionally hard-codes `voteType: 'drep'`, so it cannot drive the
abstain and no-confidence paths at all.

**Resolution.** The harness was extended rather than duplicated: `StoreOverrides` gained
`drepIndex?: Map<string, any>`, `buildStores` gained a `drepIndex` parameter defaulted to the Map it
previously hard-coded, and `openConfirmation` gained an optional `storeOverrides` second argument
defaulting to `{}` — so every pre-existing call site is unchanged and the 27 inherited tests keep
their fixtures. The abstain case bypasses `openConfirmation` and calls `renderFlow` directly with
`voteType: 'abstain'`, seeding `drepIndex` with a named entry under both `'abstain'` and the valid id
so the null assertion is a real guard rather than an empty-map tautology.

**Disposition.** Record-only, and load-bearing for anyone extending this spec: seeding the governance
store now goes through `StoreOverrides`, and any sentinel-vote assertion must use `renderFlow`, not
`openConfirmation`. **Owner.** Record-only; the harness change is in the task-154 diff.

---

## F-13 (task-154) — Hand-editing the generated catalogs to extractor-equivalent output is achievable, and the extractor run proves it

F-4 recorded that sharding a build across implementers leaves `defaultMessages.json` and
`translations/messages.json` un-regenerated, because every shard is individually barred from running
the gate that writes them. task-154 took the other route: all four tool-managed files — both locale
catalogs, `defaultMessages.json` and `translations/messages.json` — were hand-written to the output
the extractor would produce, in source order for the generated pair and alphabetical order for the
catalogs.

**Resolution.** The hand-edit was exact. `yarn i18n:manage` was run afterwards, exited 0, and wrote
nothing: md5 of all four files identical before and after, and `git status --porcelain` identical
before and after. Nothing needed restoring, and the diff already carries what the extractor would
have produced.

**Disposition.** Record-only. The check that settles it is the md5-before/md5-after pair around a real
extractor run, not an eyeball of the diff — a hand-edit that is merely plausible will be silently
rewritten by the next person who runs the gate. **Owner.** Record-only.

---
