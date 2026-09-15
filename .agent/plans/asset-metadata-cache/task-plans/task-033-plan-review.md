Planner: Iteration 1
Timestamp: 2026-09-16T14:00:00Z

Drafted from the task's entry in the graph and the research note, read against
`assetRegistryClient.ts` and against the live endpoints rather than against the
note's description of them.

Critique of Iteration 1:

1. *The first draft reused `httpRegistryTransport` by importing it.* That works
   until the response cap matters, and it matters here: the registry cap is 1 MiB,
   sized from a 103 KiB answer, and a batch of raw transactions is an order
   larger. The cap has to be a parameter, so the transport moves to its own
   module and `assetRegistryClient` re-exports the names its four callers and its
   47 spec cases use.

2. *The select list was copied from the research note, `minting_tx_metadata`
   included.* That single field would have undone the channel: the CIP-25 record
   would have come from the index rather than from bytes the user's own node
   confirms, and nothing downstream could tell the difference. It is dropped, and
   the reason is in the module rather than only here. `cip68_metadata` stays,
   because a datum at a spendable UTxO is not in the mint transaction at all.

3. *"A per-IP ceiling" was a phrase, not a mechanism.* Written as a number with
   an argument: a fifth of the published 100 per 10 seconds, checked before every
   request including the retry, in-process, cleared by a restart, and stated as a
   guard against a defect here rather than a budget to spend.

4. *The `429` behaviour was "back off rather than retry", which the registry
   client already does for every 4xx.* The distinction that matters is between
   `429` and the rest: `429` says come back later and the others say never. It
   gets the throttle path and its own criterion asserted per status code.

5. *Nothing said what happens to a pointer whose transaction does not come
   back.* Carrying it forward would hand `task-034` a pointer it cannot confirm.
   Dropped, with a case.

6. *The task graph lists `assetMetadataResolver.ts` as a target path.* There is
   nothing for the resolver to do until there is something confirmed to write.
   Moved to `task-035` and the graph corrected rather than a half-wiring landing
   here.

Changes made in response: the transport extraction with its findings, the select
list and its reason, the ceiling as a number, the `429` split, the dropped
pointer, and the corrected target paths.

Scope guard: no confirmation, no row, no resolver wiring, no `tx_info`, no
interval timer.

Outcome: approved
