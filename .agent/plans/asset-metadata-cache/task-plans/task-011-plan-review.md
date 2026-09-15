Planner: Iteration 1
Timestamp: 2026-09-15T01:20:00Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-011.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`. A stubbed transport and a temporary database cover every criterion.
- One new module, one new spec, and two methods added to the module that owns the table.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the image bounds at `:474-486`, the schema at `:363-372` and the chain-image note at `:488-491`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-011` and `task-012` entries.
- `.agent/plans/asset-metadata-cache/task-plans/task-009.md` for the logo value encoding.

Repo-Verified Findings Used To Shape The Plan:
- 400 registry subjects queried for the `logo` property alone: 374 carry one, all 374 are PNG by magic bytes, sizes run 806 bytes to 65,249 with a median of 23,251, and none exceeds the 256 KiB per-entry cap.
- The registry declares no content type beside the logo, so the type has to be detected from the decoded bytes.
- `asset_image` and its foreign key already exist, and `task-006`'s spec asserts that an image for an unknown subject raises, so the pragma is live.
- `node:sqlite` round-trips a `Uint8Array` through a `BLOB` column unchanged.
- `ASSET_REGISTRY_PROPERTIES` is a module constant of five names and is the only input to the bulk request body, so no call site can pass `logo` into a bulk query.

Planned Approach:
- Two accessors on the database module, a magic-byte media-type check, a single-subject single-property fetch through the shared transport, and an in-memory record of subjects with no logo.

Scope Guard / Self-Review:
- No eviction, no bulk mirror, no logo in the bulk query, no IPC and no image transformation.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-15T01:28:00Z

Three gaps, in descending severity.

1. Nothing checks that the response is about the subject that was asked for. The bulk client refuses an entry whose subject is not in the request's own list, and the reason it does is that a server answering a question nobody asked must not be able to create a row. A single-subject fetch has exactly the same exposure and would currently take the first entry in the response whatever subject it names, which is how one asset ends up wearing another's logo. The check is one comparison and its absence is the only place in this phase where response data reaches a row unchecked.

2. Two renders of the same row fetch the same logo twice. The metadata resolver claims its subjects before scheduling so that a token list rendered twice in a tick produces one query; the image store as planned has no equivalent, and the rendering case is worse, not better, because a list of thirty tokens is thirty independent components each deciding to show a picture. An in-flight map keyed by subject, returning the same promise to the second caller, costs a few lines.

3. The order of checks is stated but not justified, and the justification is the interesting part. `Buffer.from(x, 'base64')` is lenient: it skips characters it cannot read rather than refusing, so a malformed value decodes to something rather than failing. That is safe here only because the decode happens behind the transport's 1 MiB response cap, which bounds the allocation before this module sees it, and because the media-type check then refuses whatever the garbage turned out to be. Say that, or the next reader moves the cap check before the decode to "save work" and removes the bound that made the decode safe.

Nothing else in the plan is wrong. Measuring 400 real logos before choosing a media-type policy is what turns "refuse SVG" from a preference into a decision with a measured cost of zero.

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-15T01:35:00Z

Plan Updates In Response To Critique:
- The fetch now refuses an entry whose subject is not the one requested, with a case.
- Added an in-flight map keyed by subject so concurrent fetches for one logo share a request, with a case.
- Stated why the decode precedes the cap check: the transport's response cap bounds the input first, and base64 decoding is lenient rather than refusing.

Resulting Approved Plan Shape:
- One new module and two accessors on the database module. Detect the type from the bytes, refuse anything that is not raster, cap each entry, store decoded bytes, share concurrent fetches and remember which subjects have no logo.

Scope Guard / Self-Review:
- The revision adds one comparison, one map and one paragraph of reasoning. It changes nothing about what is stored.

Outcome: Canonical task plan revised after critique and approved for build execution
