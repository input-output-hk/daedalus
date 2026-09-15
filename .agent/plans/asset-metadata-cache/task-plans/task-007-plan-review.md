Planner: Iteration 1
Timestamp: 2026-09-14T21:10:00Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-007.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`, with the one criterion that cannot be settled here, the selfnode endpoint end to end, handed to `task-027` with its procedure and its expected evidence.
- Two new files and no change to any existing one.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the fetch path at `:653-730`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-007` entry plus `task-008`, `task-010` and `task-011`.
- `.agent/plans/asset-metadata-cache/task-plans/task-006.md` for the row types and the timestamp handoff.
- `CLAUDE.md` for the spec conventions and the `@ts-ignore` rule.

Repo-Verified Findings Used To Shape The Plan:
- `source/main/config.ts:71` declares `metadataUrl`; `nix/internal/launcher-config.nix:36-40` and `:76` populate it per network; `:448-450` omits it on selfnode; `source/main/index.ts:216-219` is the existing literal fallback.
- `source/main/config.ts:169-172` declares `MOCK_TOKEN_METADATA_SERVER_URL` and `MOCK_TOKEN_METADATA_SERVER_PORT`, and nothing reads them. `source/main/environment.ts:111` exports `isSelfnode`.
- Measured live today against `https://tokens.cardano.org/metadata/query`: 6,099 request bytes returned 200 with a 94,772-byte response, 8,190 returned 200, and 8,682 returned 413. The cap sits between 8,190 and 8,682, consistent with 8,192, and 90 subjects here is 8,682 bytes where the PRD's sample of 90 was 7,156.
- The largest response measured was 103,314 bytes, against `ANCHOR_MAX_BYTES` of 1 MiB at `source/main/governance/AnchorFetchService.ts:8`.
- `AnchorFetchService.ts:232-292` is the request shape to copy; `:48-157` is the address filtering not to copy.

Planned Approach:
- Resolve the endpoint from four candidates, size batches in request bytes, issue them sequentially through a stubbable transport, apply one rule per status class, and return the entries plus one resolution row per subject asked for.

Scope Guard / Self-Review:
- No database, no verification, no logo, no interval timer, no address filtering and no caching.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-14T21:18:00Z

Four gaps, in descending severity.

1. The 413 split has no base case. "Splits the batch in half and re-sends" is well defined for a batch of fifty and undefined for a batch of one: half of one subject is one subject and zero subjects, so the rule either re-sends the identical request or produces an empty one. Both are wrong, and the second would issue a request for nothing. A batch of one that returns 413 is a subject the endpoint will not accept at any size, and the honest outcome is to record it `failed` without splitting. The plan needs that stated and a case driving it.

2. Splitting turns one batch into two, and nothing says what the halves inherit. A half is an ordinary batch for every rule except that it is not split again, so a half that times out still gets its one retry, and a half that returns 200 resolves its own subjects while the other half fails its own. Without that said, an implementer can reasonably read "splits once and re-sends" as a single re-send of the whole batch, which is a different behaviour with the same words. The result also has to keep per-subject outcomes across the split rather than one verdict for the original batch, and the plan's wording of criterion 6 assumes a batch is atomic.

3. Duplicate subjects are not handled. Nothing upstream guarantees the caller deduplicates, and the very shape of this design, several wallets holding one token, is the case that produces duplicates. A duplicated subject would be asked for twice, occupy space in two batches, and produce two resolution rows for one primary key. Deduplicating on entry is one line and removes all three.

4. Normalisation is described against "the subjects asked for" without saying which request that means. With splitting there are more requests than there are batches, and an entry belongs to the request that asked for it. Checking against the whole call's subject list would let a server answer a question asked in a different batch; checking against the request's own list is the property worth having, and it is the one that makes case three of the verification plan meaningful.

Nothing else in the plan is wrong. Re-measuring the batch boundary live rather than carrying the PRD's table is what turned up the sharper version of the finding: the same subject count produced a different byte count and a different status, which is stronger evidence for sizing in bytes than the original table was.

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-14T21:26:00Z

Plan Updates In Response To Critique:
- Gave the 413 rule a base case: a batch of one is recorded `failed` rather than split, and the split is by subject count with any remainder in the second half.
- Stated what a half inherits: every rule except splitting, its own retry, its own status handling and its own subjects' outcomes. Rewrote criterion 6 so a resolution belongs to a subject rather than to a batch.
- Added deduplication of the incoming subject list, preserving first-seen order, with a case.
- Scoped normalisation to the subject list of the request that produced the response rather than to the call's, with a case that answers a batch with an entry belonging to another batch.

Resulting Approved Plan Shape:
- Two new files. Endpoint resolution with four candidates, byte-sized batching, sequential requests through a stubbable transport, one rule per status class with a terminating split, and one resolution row per distinct subject asked for.

Scope Guard / Self-Review:
- The revision adds a base case, a deduplication and a narrower scope for one check. It changes nothing about what is requested or what is returned on the ordinary path.

Outcome: Canonical task plan revised after critique and approved for build execution
