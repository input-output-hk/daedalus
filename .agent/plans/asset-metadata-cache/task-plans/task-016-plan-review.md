Planner: Iteration 1
Timestamp: 2026-09-15T05:40:00Z

Drafted from the task's entry in the graph, the PRD's cold-cache section, and the
findings `task-002`'s review carried forward.

Critique of Iteration 1:

1. *The deletions the task note lists cannot all happen here.* The note says to
   remove `assetsRequests`, `_createWalletTokensRequest` and
   `_retrieveAssetsRequest`, and separately that `all` goes only once `task-017`
   has moved its readers. `all` reads `_retrieveAssetsRequest` and
   `_refreshAssetsData` calls it, and neither goes until `task-017` and
   `task-018`. Removing the machinery here would not compile. The plan now says
   the endpoint stays and names the two commits that take it away, which is what
   "sequence the two commits accordingly" has to mean.

2. *The graph's test case and the graph's implementation note contradict each
   other.* "getAsset returns undefined for an unresolved subject" against "attach
   the locally computed fingerprint when building a merged row". A `getAsset`
   that returns nothing cannot attach anything, and `task-002`'s review records
   the `searchAssets` hole as closed here by filling the field. The plan takes
   the note, states the departure in its own words, and keeps the property the
   test case is about by asserting that a caller renders from a row with no
   metadata.

3. *The per-token decimal setting had no owner in the new design.* Today the poll
   reads it from browser storage and stamps it onto the asset. With the poll
   gone nothing would read it, and the settings dialog would silently stop
   working. The store reads it at startup and writes it on submit, with no change
   to which value formats an amount, because that is `task-019`.

4. *Nothing said what happens to a subject that fails to resolve.* The retry
   window is the main process's and the renderer does not re-ask, which is the
   whole point of the design and needed saying out loud rather than being
   discovered as an absence.

Changes made in response: Non-Goals gained the endpoint, Implementation Approach
gained the departure and its argument, the decimal setting became a scope item,
and Risks gained the unresolved-subject behaviour.

Scope guard: no decimals resolution order, no logo, no spinner, no component.

Outcome: approved
