Planner: Iteration 1
Timestamp: 2026-09-16T19:00:00Z

Drafted from the task's entry in the graph, then rewritten once the first
finding made the task as written unimplementable.

Critique of Iteration 1:

1. *The first draft read closure from the registry's `policy` field, as the task
   and the PRD both say.* A chain row exists precisely for a subject the registry
   does not answer, so that field is never present for the rows this rule is
   about. The rule would have applied to nothing. The script comes from the
   minting transaction's witness set instead, which is available exactly for
   these rows and is better evidence besides. It has its own section, because it
   is a correction to the PRD rather than a detail.

2. *The measurement was going to be restated as though it applied here.* 89.6
   percent of **registry entries** are under a closed policy. Chain rows are
   NFTs, which include long-running open collections, and nothing measures their
   share. Stated as the gap it is.

3. *The verdict was going to be recomputed at read time.* `_due` runs on every
   read and finding the immutable tip means listing a directory with three files
   per chunk. Decided at write time, where the tip is already in hand, and
   stored. Finding 5.

4. *The verdict was going to sit beside the minter's keys in the metadata
   column.* A CIP-25 payload whose author wrote a key of that name would then
   freeze its own row. Nested under a key of ours, with the channel unwrapping
   the record on the way to the renderer.

5. *The fold's uncertain cases were not decided.* `n of k`, an `any` with an
   unexpiring branch, and an empty list all have to resolve towards open, because
   a wrongly frozen row is never read again. Written into the approach and given
   three cases.

6. *Nothing said how a wrongly frozen row recovers.* The manual refresh already
   skips the window and the backoff; it skips this too, and that is criterion 5.

Changes made in response: the caveat section, the measurement gap, the
write-time verdict, the nested storage, the conservative fold and the forced
read.

Scope guard: no column, no timer, no change to the registry channel, no Plutus
analysis.

Outcome: approved
