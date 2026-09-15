Planner: Iteration 1
Timestamp: 2026-09-15T04:55:00Z

Drafted from the task's entry in the graph, the PRD's correlation paragraph, and
the finding in `.agent/findings/ipc-channel-response-correlation.md`.

Critique of Iteration 1:

1. *The first draft put the correlation check inside each request.* It does not
   work, and working out why changed the design. `IpcChannel.request` registers
   `receiver.once`, so the first response to arrive removes the first listener
   whatever it answers. A call that inspected the id on its own promise and kept
   waiting would wait forever, because the message it wanted was consumed by
   another call's listener. The waiters share one registry instead, and whichever
   promise settles routes the payload by the id it carries.

2. *The task's own note says this file is "a declaration, not a layer", which
   the acceptance criteria contradict.* A declaration of three channels cannot
   discard anything or keep waiting. The note is right about what does not
   belong here, which is caching, retries and anything that holds state about
   assets; the correlation is the channel's own wire discipline and is the
   smallest thing that can satisfy the criteria at all.

3. *"The out-of-order test fails if the correlation check is removed" was going
   to be asserted by describing it.* The plan gained two things: a control case
   in the same spec that drives the raw channel and asserts the payloads swap,
   and a requirement to physically remove the check once and record the failure.

4. *The graph's three `targetPaths` disagree with the PRD and with `task-024`.*
   Rather than write files at paths one later task will not look for, the plan
   writes the path the PRD's Components Affected names and corrects the graph in
   the same commit.

Changes made in response: the shared registry replaced the per-call check, the
control case and the removal run were added to the Verification Plan, and the
path decision was written into Risks with its evidence.

Scope guard: no store, no timeout, no change to `IpcChannel` and no move to
`IpcConversation`.

Outcome: approved
