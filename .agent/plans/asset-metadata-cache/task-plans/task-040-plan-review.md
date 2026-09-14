Planner: Iteration 1
Timestamp: 2026-09-15T10:25:00Z

Drafted from the task's entry in the graph and from the PRD section the task
quotes, re-checked against the tree after `task-039`.

Critique of Iteration 1:

1. *The first draft compared denominations rather than values, so a move from
   unknown to zero would not have cleared.* The reasoning was sound on its own
   terms: unknown and zero both put the field in raw units, so the typed digits
   mean the same thing either way. It was still the wrong call. The rule this
   phase was approved under says a change in `decimals` clears, and a deviation
   that clears less is the one direction that cannot be taken on the
   implementer's judgement. Implemented as written, with the cost named in the
   plan: one unnecessary clear in a rare case.

2. *The snapshot did not move for a cleared row.* Caught by asking what the user
   does next: they re-type. Into a field still denominated in the value the
   application no longer believes, which is the original defect wearing a
   different hat. The snapshot now moves in both branches and the difference
   between them is only whether the field is cleared and the notice shown.

3. *The Cucumber steps were going to re-implement the decision.* A step that
   decides for itself whether to clear and then asserts it cleared tests the step
   file. The module now clears the field itself through a two-member interface
   that `mobx-react-form`'s `Field` satisfies, so the steps call exactly what the
   form calls.

4. *Only unknown-to-known was covered.* The rule says "and when a cached verified
   value changes on re-read". A comparison written against `null` on one side
   would pass every unknown-to-known case and fail that one silently. A third
   scenario drives six to two.

5. *Rule 1 had no case of its own.* Every draft case exercised rule 2, which
   would leave the snapshot's existence inferred from the clearing. `AssetInput`
   is now rendered with a snapshot that disagrees with the asset it is handed, and
   the field is asserted to obey the snapshot.

6. *"Blocking notice" was read as a new submit gate.* It is not needed: the
   field's validator already refuses an empty value, so a cleared field blocks
   the form. The notice says why. Recorded as finding 5 so a reviewer does not
   look for a gate that is not there.

Changes made in response: the literal comparison and its cost, the snapshot
moving in both branches, the module owning the clear, the re-read scenario,
the rule-1 case, and finding 5.

Scope guard: no confirmation-dialog change, no change to the resolution order,
no change to what the field accepts for a given denomination.

Outcome: approved
