Planner: Iteration 1
Timestamp: 2026-09-14T20:02:30Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-006.md` with the 21 sections the
  task-plans readme requires.
- Scope held to extracting the dialog and its fixture into a story of its own under the panel it
  already belongs to, and leaving the settings screen with an explicit `null`.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 6 at
  `:225-231`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-006`.
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`.
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact,
  `task-plans/readme.md` for the cycle and section list, and
  `.agent/plans/mithril-partial-sync/task-plans/task-001.md` for the section precedent.
- `.agent/skills/i18n-messaging/SKILL.md`, checked and found not to apply.
- `.agent/workflows/storybook.md` read for the registration model only.

Repo-Verified Findings Used To Shape The Plan:
- Confirmed the dialog at `_support/WalletSettingsScreen.tsx:325-352`, and that
  `WalletSettings.tsx:196-211` returns `null` for the box that would render it while
  `walletsConfig.ts:45` keeps `IS_WALLET_UNDELEGATION_ENABLED` false.
- Found a second, independent reason the dialog never renders from this story, which the task entry
  does not record: the story's own `isDialogOpen` at `WalletSettingsScreen.tsx:196-226` has no
  branch for this dialog and falls through to `return false`.
- Confirmed the component is live through `DelegationCenterPage.tsx:118-119`, which is what
  separates decision 6 from decision 4.
- Enumerated the fixture that exists only for this dialog: `selectedWallet` with one reader,
  `assets` with one reader, and five imports serving only those.
- Confirmed `undelegateWalletId` stays, because `:167` uses it for the delegation status knob group.
- Confirmed `undelegateWalletDialogContainer: Node` is non-optional at `WalletSettings.tsx:129`, and
  that `strictNullChecks` is off, so `null` is assignable.
- Read the two sibling dialog stories in the same directory for the registration shape.
- Recorded the `Wallets / Settings` panel at 10 registrations in the `task-001` baseline.

Planned Approach:
- New `UndelegateWallet.stories.tsx` registering the dialog as `Undelegate Wallet`, carrying the
  fixture and four knobs unchanged.
- `null` for the prop in the settings screen, with the condition stated in a comment.
- A sixth sibling import in `WalletSettings.stories.tsx`.
- Sidebar diffed against the `task-001` baseline, then the three flake checks.

Scope Guard / Self-Review:
- No file under `source/` is touched, no flag is changed, and the staking undelegate stories are
  left alone.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-14T20:06:45Z

Blocking gaps:
- The plan treats the task entry's label criterion as if it applied. It does not: the dialog has no
  label in the `task-001` baseline, because a prop is not a registration, so there is nothing to
  keep recognizable and nothing to rename. What this change actually does is add a registration, and
  a plan whose acceptance section does not say the sidebar count goes from 272 to 273 is a plan that
  will read as a baseline violation to whoever checks it next.
- The plan proposes `null` for a non-optional prop and stops at "it compiles". The question a
  reviewer will ask is what happens when someone turns the flag on, and the plan has to answer it
  rather than leave the reader to work out whether a latent gap was noticed or missed.
- Nothing says the new file has to be staged before a flake check runs. The whole change hangs on a
  file that does not exist yet, and `nix build` on a git source cannot see an untracked file, so the
  first run would build a tree whose `WalletSettings.stories.tsx` imports a module that is not
  there.

Non-blocking observations:
- Adding a story whose component already has two stories elsewhere needs one sentence saying why it
  is not duplication, or the next reader will delete one of the three.
- The plan should say the fixture is moved rather than rewritten, and why that matters: it makes any
  difference in what the dialog renders attributable to the framing alone.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-14T20:11:00Z

Plan Updates In Response To Critique:
- Replaced the restatement of the label criterion with a paragraph saying it does not apply as
  worded, and stating what happens instead: one added registration, `Wallets / Settings` from 10 to
  11, corpus total from 272 to 273, every other baseline line unchanged. Added the one-added-line
  diff as an acceptance criterion of its own.
- Added the flag-flip consequence to Risks, with the reason it is accepted: the story is already
  unreachable for a second cause a flag flip would not fix, so whoever turns the flag on has to
  rewire the story either way, and a `null` with a stated condition is a clearer signal to them.
- The staging point was folded into the verification plan's expectations rather than left implicit,
  and the failure response for a red `storybook` check names the untracked file first.
- Added the duplication sentence to Risks and the byte-identical-fixture criterion to Acceptance.

Resulting Approved Plan Shape:
- One new story file, two edited files, one tracking update.
- Verification by sidebar diff against the `task-001` baseline, by the three flake checks, and by a
  derivation-path comparison.

Scope Guard / Self-Review:
- The revision closes the three blocking gaps and adds nothing to the change itself.
- Scope is unchanged: the extraction, the `null`, the import.

Outcome: Canonical task plan revised after critique and approved for build execution
