# Slice-2 Findings: Software-Wallet Delegate

> Durable findings from the slice-2 build (task-112/113/114), recorded per the
> slice-2 implementation guide Step 7 and PRD Final Outcome requirements.
> Companion docs: [slice-2-PRD.md](../task-plans/slice-2-PRD.md) ·
> [slice-2-implementation-guide.md](../task-plans/slice-2-implementation-guide.md)

---

## D1 as implemented — two-hop AC without a Detail route

- Production ships only the row-level "Select for delegation" CTA on `DRepCard`;
  `routes-config.ts` and `Routes.tsx` are byte-identical to base. No `DREP_DETAIL`
  literal and no "View details" CTA exist anywhere in production code.
- The Directory-side forwarding contract is production code:
  `source/renderer/app/containers/governance/delegationFormState.ts` exports
  `pickDelegationFormNavigationState` (form-side restore) and
  `pickDelegationFormReturnState` (directory-side forwarding of
  `{ from, selectedWalletId, voteType }` only).
- The two-hop Form → Directory → Detail → Form sequence is covered in
  `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` via a
  `DetailRouteStub` registered only in that Jest harness (path literal
  `/governance/dreps/:drepId` defined inside the spec). The stub forwards inherited
  state plus `selectedDRepId` using the production `pickDelegationFormReturnState`
  picker.
- **Binding on slice-4 (task-116/117):** the production Detail view must honor this
  exact contract — receive `{ from, selectedWalletId, voteType }` from the Directory's
  detail push and return it plus `selectedDRepId` to the form through the same
  `pickDelegationFormReturnState` helper. The harness stub is the executable
  specification; adopting the shared picker keeps the contract code, not convention,
  and the existing two-hop test then covers the real route unchanged.

## D2 as implemented — GovTool link removed, label unified

- The external gov.tools link on the DRep-ID input label was replaced with the
  in-app "Browse DReps" affordance (`voting.governance.browseDRepsLink`), which
  navigates to `/governance/dreps` carrying
  `{ from: '/voting/governance', selectedWalletId, voteType }` in `location.state`.
- Keys removed as dead from `VotingPowerDelegation.messages.ts` and BOTH locales:
  `voting.governance.drepInputLabelLinkUrl`, `…LinkUrlPreview`, `…LinkText`, and
  `…Preprod` (planner extension P-1). The preprod-only label variant existed only
  because gov.tools had no preprod directory; the in-app directory is
  network-agnostic, so the `environment.isPreprod` / `isMainnet` branches were
  removed with the keys and the label is now identical on every network.
- Changed/new copy (`drepInputLabel`, `browseDRepsLink`,
  `governance.drepDirectory.card.select`,
  `voting.governance.confirmationDialog.drepId`) is `!!!`-prefixed in both locales
  (invariant #11); locale delta vs base is +4 `!!!` strings per locale.

## D3 as implemented — raw-ID-only confirmation

- `VotingPowerDelegationConfirmationDialog` accepts
  `drepIdentity: DRepIdentity | null` and renders only `drepIdentity.raw` — full,
  monospaced, `word-break: break-all`, deliberately NOT the truncating
  `DRepIdDisplay` (confirmation is a security surface).
- The container (`VotingGovernancePage`) builds the identity from `chosenOption`
  verbatim; `credentialType` is a syntactic `drep_script` prefix classification
  that never touches the rendered or submitted bytes. Sentinels map to `null` and
  keep their label rendering.
- CIP-105 dual display, signed-payload line, source label, and the name slot are
  deferred (cv-1 / anchor-2). Byte-equality is pinned end-to-end by the task-114
  payload test: row select → rendered confirmation ID → `delegateVotes` called
  with the identical `chosenOption` string.

## D4 deviation — prettier substituted for nix fmt

- `nix` is not installed in the build container, so the mandated pre-commit
  `nix fmt` could not run. Substitute used throughout the slice:
  `node_modules/.bin/prettier --write` on changed `.ts/.tsx/.scss/.md` files only
  (never tracker JSON, locale JSONs, or `translations/messages.json`).
- **Action before merge: run `nix fmt` from a nix-capable environment.**
- The task-112 round observed that prettier 2.1.2 could not parse inline
  `import { withRouter, type RouteComponentProps }` syntax; this was resolved
  during task-113 by switching to separate `import type` statements. At slice
  close `prettier --check` passes on all touched `.ts/.tsx/.scss` files.

## P-5 deviation — walkthrough copies live in gitignored `.vscode/`

- `.vscode/` is gitignored (`.gitignore` entry `.vscode`), so the governance
  walkthroughs exist only in the main checkout and cannot travel with the branch.
- Task-114 copied `/workspaces/daedalus/.vscode/docs/walkthroughs/governance/`
  into the worktree (`.vscode/docs/walkthroughs/governance/`) and edited the
  copies: `02-voting-power-delegation.md` (label, in-app link, tip, and the
  now-false "DRep ID is not displayed in the dialog" claim),
  `04-troubleshooting.md` (:17, :57, :140), `05-improvements-vs-light-wallets.md`
  (:23, :204, :252 — flow-routing rows only; the Lace note at :33 is untouched).
- After the edits, `grep -rn -i "gov\.tools\|govtool"` over the walkthrough copies
  hits only the sanctioned 05 lines (Lace release-notes fact and the GovTool name
  in the ecosystem table row).
- **Action at slice close: manually sync the edited worktree copies back to the
  main checkout's `.vscode/docs/walkthroughs/governance/`** (worktree isolation
  forbade editing the main checkout directly; gitignored files do not commit).

## Durable notes from the slice-2 code-review log

All three tasks passed code review in one round each, approved with zero blockers
([slice-2-code-review.md](../task-plans/slice-2-code-review.md)). Items worth keeping:

- **Task-ID comments to strip:** guide-prescribed comments in
  `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` embed task IDs
  (e.g. the `DetailRouteStub` and payload-test comments), which the repo comment
  convention forbids. Strip them to plain "why" comments on the file's next touch.
- **Storybook not executed here:** the container has no display, so the new/updated
  stories (prefilled-from-directory, confirmation with `drepIdentity`) were verified at
  tsc/eslint/fixture-id level only. Eyeball them in both locales via the global
  English/Japanese toggle before merge.
- **`translations/messages.json` regeneration side effect:** `yarn i18n:manage`
  re-added unrelated `daedalus.diagnostics.dialog.*` descriptor hunks; this is
  tool-managed output and rides with whichever commit triggers regeneration — do not
  hand-edit it back.

## Behavior notes handed to `ux-refinement`

- **Un-synced return hop:** `VotingGovernancePage` renders `VotingUnavailable`
  whenever `!networkStatus.isSynced`, while `/governance/dreps` renders
  regardless. If the node drops out of sync while the user browses, the return
  hop lands on `VotingUnavailable` with the pre-fill parked in `location.state`
  (it survives and applies once the node is synced again, as long as the entry is
  not re-navigated). Tests pin the synced path only; this is expected behavior —
  sync UX is `ux-refinement`'s remit.
- **Directory-first entry edge (Critiquer N-4):** selecting a DRep from the
  directory without inherited form state pre-fills the ID, but the form's
  `WalletsDropdown` onChange resets to `initialState`, wiping the pre-filled ID
  when the user then picks a wallet. Outside slice-2's ACs (the round trip starts
  at the form); flagged for `ux-refinement`.
