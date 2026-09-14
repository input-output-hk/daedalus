Implementation: Iteration 1
Timestamp: 2026-09-15T05:15:00Z

Changes made:
- `source/renderer/app/ipc/assetMetadataChannel.ts`: new. The three channel
  declarations, one waiter registry per request channel, the two request
  functions and the update subscription.
- `source/renderer/app/ipc/assetMetadataChannel.spec.ts`: new. Eleven cases
  across four groups, against a fake `ipcRenderer` that fires one-shot listeners
  in registration order exactly as Electron does.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`:
  `task-015`'s `targetPath` and `targetPaths` corrected to the file written, for
  the reason recorded in the plan.

Files touched:
- `source/renderer/app/ipc/assetMetadataChannel.ts`
- `source/renderer/app/ipc/assetMetadataChannel.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-015.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-015-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-015-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

One detail decided during implementation:

The fake `ipcRenderer` does not match responses to listeners. It takes the
oldest one-shot listener on the response channel, removes it, and fires it with
whatever payload the case supplied. That is the behaviour under test: if the
fake matched by id, every case would pass against an uncorrelated client and the
suite would assert nothing.

Verification run:

- `jest source/renderer/app/ipc/assetMetadataChannel --coverage=false` — 11
  passed.
- Two overlapping metadata requests answered in reverse order each resolve with
  the payload carrying their own id and their own subjects.
- The control, in the same suite: the same two responses driven straight through
  `assetMetadataChannel.request` resolve each promise with the other's payload.
  So the defect is demonstrated in the same file that shows the client does not
  have it, rather than argued from the source of `IpcChannel`.
- The correlation was then removed and the suite re-run, which is the check the
  plan required. Replacing `deliver(metadataWaiters, response)` with
  `resolve(response)` fails exactly two cases, "gives each of two overlapping
  requests the answer it asked for" and "discards a response it did not issue and
  keeps waiting", and leaves the other nine passing, including the control, which
  is what it should do. The check was restored and all eleven pass again.
- A response carrying an id nobody issued resolves nothing: the waiter is still
  pending after the event loop drains, and the matching response then resolves
  it.
- A response on the metadata channel does not resolve a request on the image
  channel, which is what shows the registries are per channel rather than shared
  by accident.
- Each request sends exactly one message on its request channel, carrying the
  id it minted and the subject or subjects it was given, and two requests mint
  different ids.
- A request left unanswered is still pending after the loop drains twice, which
  states that nothing here expires a request.
- A pushed message reaches the subscribed handler with its payload intact.

Checks, all four through Nix with both new files staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `r9y1j53s67308x65bdvsszviz90ns0k3-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `zh494h224naiarj0gxi7yv9dy66ipnaz-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 83 suites passed, 1228
  tests with 1225 passed and 3 skipped, exit 0. The previous state of this branch
  was 82 suites and 1217 tests, so one suite and eleven tests were added and
  nothing else moved.

`nix fmt` was run and changed one file before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`. The one place the spec needs
an untyped global, assigning the fake to `global.ipcRenderer`, is written as a
cast rather than a suppression.
`git diff HEAD -- package.json yarn.lock` produces nothing; `uuid` was already a
dependency at 8.3.2.

Deviations from the approved plan:
- None beyond the path correction, which the plan recorded in advance and which
  this commit applies to the graph as well as to the tree.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T05:22:00Z

Acceptance criteria, each against the evidence:

1. *A client discards a response it did not issue and keeps waiting.* Met, and
   the case is honest about the mechanism: the stray response consumes a
   listener, so the case supplies another request before the real answer arrives,
   which is what happens in practice when two reads overlap.

2. *The out-of-order test fails if the correlation check is removed.* Met by
   removing it and recording which cases failed. Two failed and the control
   passed, which is the pattern that shows the suite is testing the client rather
   than the fake.

3. *No client blocks a render waiting on the network.* Met at this layer: a
   request is a promise the caller is free not to await, and the response it
   waits for is answered from disk by the handler in `task-014`, which has its
   own case for that.

4. *The push delivers.* Met.

5-6. *Compile, lint, Jest, no new dependency and no new suppressions.* All met.

The decision worth naming is the shared registry. A reviewer's first instinct is
that each request should check its own response, and the reason that cannot work
is one sentence long and now sits above the registry in the source, with the
control case behind it.

Summary: The renderer can ask the cache about a subject list twice before the
first answer arrives and get both answers back, each to the caller that asked.
The correlation is local to these three channels; the defect in the primitive
remains open for every other channel and is recorded where a reader will find it.

Decision: approved
