Planner: Iteration 1
Timestamp: 2026-09-15T13:10:00Z

Drafted from the task's entry in the graph after `task-039` and `task-040`.

Critique of Iteration 1:

1. *The first draft held dismissal in component state.* It satisfies "shown once"
   for the length of a session and fails "survives a restart", which is the
   criterion that matters. The flag goes to browser storage through the store,
   and the component takes a callback.

2. *The banner started visible and hid itself once the read came back.* That
   flashes it at every start for a user who dismissed it a year ago. The
   observable starts acknowledged and only becomes unacknowledged when a read
   says so.

3. *The fresh-profile rule was a reaction over two requests.* It would have to
   decide what to do while one of them was still loading, and would give
   different answers depending on which resolved first. One await chain that
   reads both and then decides.

4. *Nothing said how a fresh profile is recognised.* Written into the findings:
   the terms of use are the only per-profile marker of age that is present for
   everyone, since the update-completed key is written only by the in-application
   updater.

5. *Criterion 4 was going to get a test that asserted `task-039`'s work again.*
   The label cannot depend on the banner, because the component has no access to
   it. Stated as construction rather than tested twice.

Changes made in response: storage-backed dismissal, the hidden-by-default
observable, the single chain, finding 4, and criterion 4 as an argument.

Scope guard: no modal, nothing per asset, no change to the field or the label.

Outcome: approved
