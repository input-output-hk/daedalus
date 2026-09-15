Planner: Iteration 1
Timestamp: 2026-09-16T12:05:00Z

Drafted from the task's entry in the graph, read against `StakePoolsSettings.tsx`
and its container, `InlineEditingInput`, the routes and the settings menu.

Critique of Iteration 1:

1. *The first draft copied all three of the SMASH component's conditional
   branches.* Each exists for a reason this page does not have: the sync gate
   because SMASH feeds the stake pool list, the saved-label derivation because
   its update is a round trip to cardano-wallet, and the selfnode omission of the
   default preset because a preset with no URL would otherwise be selectable.
   The third is worth stating rather than just dropping: `task-030` made the
   absence a type, and a preset whose URL is empty submits nothing.

2. *Nothing said how the `direct` option is rendered as unavailable.* A branch
   in this component would have been the obvious guess and the wrong one.
   Finding 2 records that `react-polymorph` already ignores a click on a
   disabled option and skips it in keyboard navigation.

3. *The criteria for the two error paths were one criterion.* The validator's
   message and the probe's refusal are different things arriving through
   different props, and criterion 3 now asserts the submit did not happen, not
   only that a message appeared.

4. *The descriptions were left as "a description per type".* They are the
   disclosure the PRD's whole privacy argument rests on. The approach says what
   they have to say and the risks name them as the part to review hardest.

5. *The menu position was unstated.* Between Wallets and Stake Pools, with the
   reason, rather than appended where it would read as an afterthought.

Changes made in response: the three non-goals with their reasons, finding 2,
criterion 3 in two halves, the description paragraph, and the menu position.

Scope guard: no new widget, no sync gate, no change to the SMASH page, and no
change to anything behind the store.

Outcome: approved
