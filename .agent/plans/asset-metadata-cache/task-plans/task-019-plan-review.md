Planner: Iteration 1
Timestamp: 2026-09-15T08:20:00Z

Drafted from the task's entry in the graph, re-checked against the tree after
`task-018`.

Critique of Iteration 1:

1. *The first draft put `resolveAssetDecimals` in `utils/assets.ts` because the
   task graph names that file.* The graph's target paths are a guess made before
   `task-001` existed. `utils/assets.ts` imports `Wallet`, `BigNumber` and the
   transaction domain, and a rule the send path depends on should not be reached
   through that. The deviation is recorded in the plan with its reason rather
   than taken silently, and `task-001`'s `utils/assetName.ts` is the precedent.

2. *The verification test was written `if (verified)`.* Under `strict: false` the
   field is optional on a merged row and can arrive `undefined`, and the rule for
   this branch is that a discriminated value is tested with `===`. Rewritten as
   `registryDecimalsVerified === true`, which states the one value that unlocks
   formatting rather than the set of values that do not.

3. *Nothing covered a user setting of zero.* It is the case that separates a
   correct implementation from one built on truthiness: a user who has
   deliberately chosen zero decimal places for a token whose issuer publishes a
   verified six must keep their zero. Added to the verification plan.

4. *The plan asserted only on the pure function.* A pure function passing says
   nothing about what a component reads. Store-level cases added, so the property
   under test is the value on the merged row.

5. *`utils/formatters.ts` is in the target paths and the plan did not say why it
   is untouched.* Finding 7 now says: both functions there take `decimals` as a
   parameter, and resolution happens before either is called. A target path left
   alone without explanation reads as an oversight.

Changes made in response: the new-file deviation and its reason, the `=== true`
comparison, the zero-setting case, the store-level cases, and finding 7.

Scope guard: no copy anywhere, no storage change, no snapshot, no label.

Outcome: approved
