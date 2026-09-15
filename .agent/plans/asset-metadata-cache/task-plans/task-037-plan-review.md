Planner: Iteration 1
Timestamp: 2026-09-16T20:15:00Z

Drafted after a coverage run over the seven modules phase 7 added or changed,
rather than from the task's target paths.

Critique of Iteration 1:

1. *The first draft was going to write the specs the task graph names.* Three of
   the four do not exist under those names, because each phase 7 task tested its
   own module as it was built, which is the pattern `task-026`'s "Why Chosen Now"
   describes. The graph's paths are corrected and the work is the sweep, not the
   coverage.

2. *"Every negative case has a spec" was going to be the criterion.* It is not
   checkable: the set of negative cases is whatever someone thought of.
   `confirmChainPointer` returns eleven distinct rejection reasons and that set
   **is** enumerable, so the criterion is one case per reason, or a recorded
   argument for why one cannot be reached.

3. *The rejection cases were going to assert the shape.* Six of the reasons are
   returned from the same block of guards, so a case asserting only
   `status: 'rejected'` would pass for five of them after the sixth broke. Each
   asserts its own reason.

4. *Nothing said what to do with a line that cannot be reached.* Two of them are
   catches around `Buffer.from(hex)`, which does not throw. Chasing those would
   mean changing the module to make a test pass. Recorded instead, with the
   argument, which is what `task-026` did for the paths it left.

5. *The transport's cap was not on the list.* `task-033` moved the transport out
   specifically so the cap could be a parameter, and nothing asserted it as one.

Changes made in response: the corrected target paths, criterion 3, the recorded
unreachables, and the cap.

Scope guard: no source change to reach a line, no manual QA, no network.

Outcome: approved
