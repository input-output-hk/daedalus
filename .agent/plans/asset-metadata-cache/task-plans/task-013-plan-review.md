Planner: Iteration 1
Timestamp: 2026-09-15T03:20:00Z

Drafted from the task's entry in the graph and from the PRD's declarations at
`asset-metadata-cache-prd.md:554-604`, re-checked against the repository as it
stands at `f01ecf61e` rather than as the PRD described it.

Critique of Iteration 1:

1. *"Every request type carries a requestId and every response echoes it,
   enforced by the types rather than by convention" is not met by writing the
   field into five object literals.* The PRD's own sample does exactly that, and
   the image response is a two-member union, so the declaration as written would
   let a third member be added later without the field. The criterion asks for
   the types to do the enforcing.

2. *Two declarations of `AssetResolutionState` would exist.* `task-006` already
   declared it at `assetMetadataDb.ts:136-140` and the schema constrains
   `asset_resolution.state` to the same four values with a `CHECK`. A second copy
   in `source/common` is free to drift from the one the engine enforces.

3. *The verification plan asserted nothing.* A types-only change has no runtime
   behaviour, and "yarn compile passes" says only that the file parses. It does
   not show that the correlation field is required.

Changes made in response:

- The correlation wrapper `AssetIpcCorrelated<TBody>` was introduced and every
  request and response, including each member of the image response union, is
  built from it. A member that omits the field cannot be written.
- The common module declares `AssetResolutionState` and `AssetMetadataSource`,
  and `assetMetadataDb.ts` imports and re-exports them under the names its
  callers already use. The file is outside the task's declared `targetPaths`, so
  the deviation is recorded in the plan under Risks before the work rather than
  after it.
- The verification plan gained a negative compile: a scratch file declaring an
  image response without a `requestId` is compiled, observed to fail with the
  error naming the missing property, and discarded.

Scope guard: no handler, no client, no consumer, and no change to `IpcChannel`.
The correlation defect is repository-wide and this task narrows only the three
channels it declares.

Outcome: approved
