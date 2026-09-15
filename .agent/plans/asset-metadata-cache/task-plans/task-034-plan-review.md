Planner: Iteration 1
Timestamp: 2026-09-16T15:30:00Z

Drafted from the task's entry in the graph and the PRD's two open questions,
then rewritten twice against a real immutable database rather than against a
description of one.

Critique of Iteration 1:

1. *The first draft measured the volatile window with k and a slot length.* That
   needs a per-network security parameter, a slot length that differs by era, and
   an estimate of where the tip is. It also answers the wrong question. The
   question is whether this database holds that slot, and the immutable
   database's own tip answers it exactly, with no constant and no arithmetic.

2. *The first draft hardcoded the chunk size at 21,600.* Chunk zero's primary
   index is `chunkSize + 2` offsets long, so the database says what its own chunk
   size is. Finding 2, and one fewer per-network constant to be wrong about.

3. *The decoder question was going to be answered in the implementation.* The
   task graph says explicitly to name it in the plan, and the PRD carries it as
   open question 2. It has its own section, with the reason the requirement
   cannot be dropped, the three arguments for writing a structural reader rather
   than adding a dependency, and what the alternative would have cost.

4. *The mint check matched the policy and the asset name and stopped.* Reading
   the recorded transaction found that it mints one asset and burns a sibling
   under the same policy in the same field. A check that did not read the
   quantity would have confirmed the burned one. Finding 10, and a criterion.

5. *The auxiliary-data check was one-directional.* Checking that a claimed hash
   matches the data present leaves a transaction that carries data and claims no
   hash, which would let a pointer bring metadata that is bound to nothing. Both
   directions, and a rejection for each.

6. *The plan had no closing note.* The task's own acceptance criteria require the
   user-visible consequence of the volatile window to be written down, and it is
   the consequence most likely to be read as a defect. It is a section of its own.

7. *"An unrecognised chunk shape fails closed" had no failure taxonomy.* Four
   outcomes, and only one of them says the pointer is wrong. Written out, because
   `task-035` has to write a different resolution row for each.

Changes made in response: the tip-based window, the derived chunk size, the
decoder section, the quantity check, the two-directional auxiliary check, the
closing note and the four-outcome result type.

Scope guard: no `volatile/`, no CIP-68 confirmation, no chain-sync, no row.

Outcome: approved
