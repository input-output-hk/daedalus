Planner: Iteration 1
Timestamp: 2026-09-15T09:20:00Z

Drafted from the task's entry in the graph after `task-019` landed.

Critique of Iteration 1:

1. *The first draft used `resolveAssetName` to pick the unit, so a decoded asset
   name could become the label.* That is the impersonation case `task-001` spent
   a whole task separating out: an asset whose name bytes spell `USDC` is free to
   exist, and a unit label is the most expensive place to render one unmarked.
   The unit is now the published ticker or the fingerprint, never the decoded
   name, and a spec case pins it.

2. *Three messages were drafted, with a separate one for zero decimal places.*
   Zero reads correctly in the known-decimals wording, and a third message is a
   third thing to translate and a third branch to get wrong. Two messages.

3. *The plan asserted the label and not the field.* A label that says six decimal
   places over a field that still refuses a separator is worse than no label,
   because it invites the user to type something the field will drop. Criterion 2
   is now driven by changing decimals under an open component and asserting on
   the label and on what the input accepts in the same case.

4. *The screenshot criterion was going to be quietly marked met.* It cannot be:
   there is no display here. It is recorded as a deviation with the operator
   procedure and the machine-checkable substitute, which is what `task-018` did
   with a criterion that belonged to manual QA.

Changes made in response: the unit source, two messages instead of three, the
paired assertion in criterion 2, and the screenshot deviation.

Scope guard: no snapshot, no clearing, no banner, no change to what the field
accepts.

Outcome: approved
