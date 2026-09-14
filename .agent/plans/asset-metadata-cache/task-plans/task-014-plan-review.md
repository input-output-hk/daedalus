Planner: Iteration 1
Timestamp: 2026-09-15T04:10:00Z

Drafted from the task's entry in the graph, from the governance channel pair the
graph names as the shape to copy, and from the closing note of
`task-010-impl-review.md` about pushing from `onResolved` rather than awaiting
`pending()`.

Critique of Iteration 1:

1. *The plan said the handler would answer without the network and proposed no
   way to tell.* A transport stub that resolves quickly proves nothing: an
   implementation that awaited it would pass. The verification plan gained a
   transport whose promise never settles, so the case fails by hanging if the
   handler ever starts awaiting.

2. *`hasImage` had no answer.* The entry carries it and the database has no
   existence query, only `readImage`, which returns the blob. Filling the
   boolean with `readImage` would read every cached logo on every bulk read,
   which is the cost the separate image channel exists to avoid. The plan gained
   `readImageSubjects` and recorded the file as outside `targetPaths` in advance.

3. *Two database handles.* The resolver and the image store each default to
   opening their own. The plan now opens one and passes it to both.

4. *Registration was assumed to happen once.* It does today, through one call in
   `createMainWindow`, but a second registration would answer one request with
   two responses, and under the correlation scheme the spare response is taken by
   another request's one-shot listener, leaving that request unresolvable.
   Registration is idempotent and the spec drives it.

5. *The push needed the window and the module reached for `BrowserWindow`.*
   Importing the value would pull Electron into a module the spec has to load.
   The window is passed in at registration, as `handleFileDialogRequests(window)`
   and `downloadManagerChannel(window)` already do, and the type import is
   erased.

Changes made in response: the four points above, each written into Implementation
Approach or Verification Plan rather than left as an intention.

Scope guard: no renderer code, no change to the resolution policy, and no
correction of `IpcChannel`.

Outcome: approved
