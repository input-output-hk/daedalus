# Task 705 Local Publish/Fetch Helper Research

- Date: 2026-03-30
- Task: `task-705`
- Evidence: `package.json`, `scripts/agentic-kb-publish.sh`, `scripts/agentic-kb-fetch.sh`, `.agent/workflows/agentic-kb.md`, `agentic/README.md`, `.agent/plans/agentic/task-plans/task-705.md`, `.agent/plans/agentic/task-plans/task-705-impl-review.md`

## Durable Findings

- The shipped helper surface is intentionally local-path based only. `yarn agentic:kb:publish` and `yarn agentic:kb:fetch` require `AGENTIC_KB_SHARED_DIR` to point at a locally accessible Dropbox-synced folder named `Daedalus_KB`; the repo still does not own Dropbox API integration, remote listing, OAuth/bootstrap, or a richer artifact registry.
- The publish helper wraps the existing `sync all` plus `snapshot export` flow and then copies the exported `.dump` plus sibling `.manifest.json` into the configured shared folder. The fetch helper copies one explicit sibling pair back into `agentic/snapshots/`.
- The helper contract intentionally keeps artifact selection explicit. Fetch requires a snapshot basename or sibling filename and does not claim zero-argument latest discovery.
- The sibling-pair contract is enforced on both directions. Publish fails if export does not leave both the dump and sibling manifest together before copy, and fetch fails if either side of the selected pair is missing in the shared folder.
- Manual validation succeeded against a real Dropbox-synced `Daedalus_KB` path on 2026-03-30 using basename `agentic-kb-20260330`: publish copied both sibling files into the shared folder, fetch restored both files locally, fresh-stack `snapshot import` succeeded, and `status --json` reported `ok: true` with `embedding_compatibility.state = compatible`.

## Gotchas

- Real Dropbox desktop sync and shared-folder accessibility remain external prerequisites. Repo-local tests can validate wrapper behavior, but they cannot prove actual Dropbox sync without a real local shared-folder path.
- The inherited BM25 validation query still returned `.agent/workflows/agentic-kb.md` in hits after import, but not as the first hit during this task's manual validation run. That did not block task-705 because helper-command acceptance was otherwise satisfied, but it is durable evidence that the stronger first-hit expectation currently documented elsewhere is not stable enough to assume blindly.

## No New Research Beyond Implementation Capture

- No new storage backend, snapshot format, or embedding-policy decision was introduced here. The durable value from this task is the shipped helper-command contract and the real-path validation evidence for it.
