Planner: Iteration 1
Timestamp: 2026-09-15T07:00:00Z

Drafted from the task's entry in the graph and re-checked against the tree after
`task-017`.

Critique of Iteration 1:

1. *The plan proposed deleting layer by layer in separate steps.* Each layer
   holds the next one's only caller, so an intermediate state is either broken or
   full of unreachable exports the compiler will not complain about. One commit,
   deleting from the consumer end inwards, makes the compiler the check.

2. *"No HTTP request to /assets is issued during a session with several wallets
   open" cannot be asserted by a unit test and was written as though it could.*
   The criterion is restated as what is checkable here: the two references to
   that path are both deleted, and a grep says so. The session-level statement
   belongs to `task-027`'s manual QA.

3. *Nothing said what proves the poll is gone.* Reading the source is not a test.
   The store spec gained a case that spies on the repeating timer and advances a
   simulated ten minutes, so a poll reintroduced later fails rather than passing
   review.

4. *`_onAssetSettingsSubmit` still called the refresh.* Removing the function
   without noticing its third caller would have failed compilation, but the
   reason it can go needed stating: the call existed to make the endpoint re-read
   the browser storage the dialog had just written, and the store now holds that
   value itself.

Changes made in response: the approach became one commit with the compiler as
the check, criterion 1 was restated as something checkable, the timer case was
added, and the settings-dialog call was written into the findings.

Scope guard: no wallet-side change, no spinner, no decimals order, no logo.

Outcome: approved
