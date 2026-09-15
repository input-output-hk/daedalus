Planner: Iteration 1
Timestamp: 2026-09-15T12:30:00Z

Drafted from the task's entry in the graph after `task-022`.

Critique of Iteration 1:

1. *The first draft cleared `updated_at` and deleted the resolution row before
   fetching, which is what the task's wording says.* It is wrong when the fetch
   fails, which for this control is the expected case for anyone offline: the row
   would then look never-updated and every render afterwards would re-schedule
   it. Bypassing the window achieves the same fetch and writes nothing first.
   Recorded as a deviation with its reason rather than taken quietly.

2. *Nothing said how the result reaches the dialog that asked for it.* It does
   not: `editedAsset` is the object the token row handed over when the dialog
   opened. A refresh control whose result is invisible on the screen it was
   pressed from is not a feature. `editedAsset` becomes a computed over the cache.

3. *Criterion 1 had no complement.* A resolver that ignored the window for
   everything would pass "a forced refresh inside the window fetches". The same
   subject without the flag, asserted not to fetch, is what makes the case about
   the flag.

4. *Criterion 3 was going to be driven with content that verifies.* That passes
   even if the verdict were copied from the stored row rather than recomputed.
   Driven instead with new content whose attestation fails, so the stored verdict
   has to move from true to false.

5. *The store handler took a subject list.* One control, one subject: a signature
   that cannot express a bulk refresh is the cheapest way to keep the acceptance
   criterion true later.

Changes made in response: bypass rather than clear, the computed `editedAsset`,
the complement to criterion 1, the failing-attestation case, and the single-
subject signature.

Scope guard: no fourth channel, no second fetch path, no bulk refresh, no
spinner, no error surface.

Outcome: approved
