Planner: Iteration 1
Timestamp: 2026-09-16T17:20:00Z

Drafted from the task's entry in the graph, read against the resolver, the
schema, the channel and the name resolver rather than against the PRD.

Critique of Iteration 1:

1. *The first draft wrote chain rows and let the resolver's precedence rule sort
   it out.* `_supersedes` handles a registry row replacing a chain one and has no
   code for the other direction, because nothing wrote a chain row before. The
   simpler answer is not to build one: excluding subjects the registry answered
   before the request means the question never arises, and it is also the filter
   the privacy argument depends on. Finding 2, and criterion 2 asserts the
   request was never made rather than only that the row survived.

2. *The selected source was going to reach the main process on its own channel.*
   That is a second thing to keep in step and a startup order to get right, for a
   value every read already has an argument list for. A field on the existing
   request, and the resolver takes it as a plain assignment.

3. *Nothing said how the renderer distinguishes a chain name from a registry
   name.* Both arrive as `metadata.name`, because the cache has one name column.
   The row's `source` is the discriminator and it stops at the store today, which
   is the same four-edit gap `hasImage` had. Findings 4 and 5.

4. *A chain name was going to be marked minter-chosen, by analogy.* It should not
   be. A CIP-25 record is in the transaction that minted the asset, which had to
   satisfy the minting policy, so it is bound to that policy. The marker exists
   for a name bound to nothing.

5. *Criterion 3 asserted the retry equals a constant.* A criterion that compares
   a constant to itself passes after a change that moves it outside the window it
   has to be inside. It now asserts both.

6. *The plan did not say where the chain path comes from.* The channel is
   registered before the backend starts and the path is a local in an async
   function that runs later. Finding 7, and the handler resolves it itself.

Changes made in response: precedence by exclusion, the request field, findings 4
to 7, the provenance argument, and the two-part criterion 3.

Scope guard: no decimals from this channel, no image fetch, no schema change, no
freshness rule.

Outcome: approved
