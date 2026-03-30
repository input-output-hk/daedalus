# Task 605 Dropbox Shared Snapshot Backend Research

- Date: 2026-03-30
- Task: `task-605`
- Decision: Dropbox shared-folder storage is the selected private shared snapshot backend for v1 team handoff.
- Evidence: `.agent/plans/agentic/task-plans/task-605.md`, `.agent/plans/agentic/task-plans/task-605-impl-review.md`, `.agent/workflows/agentic-kb.md`, `agentic/README.md`, `.agent/plans/agentic/knowledge-base-platform-prd.md`

## Durable Findings

- The selected backend is Dropbox, using one shared folder named `Daedalus_KB` as the canonical location for the exported snapshot pair.
- The access contract is shared-folder based, not token- or helper-command based. Developer 1 creates `Daedalus_KB` in an existing Dropbox account and shares it with Developer 2 with write access. The folder must be writable by both developers.
- Developer 2 bootstrap is only documented to the truthful minimum currently available from the decision record: Developer 2 needs a Dropbox access path that can accept the shared folder and confirm write access. No repo-specific bootstrap helper, team-wide Dropbox SOP, or deeper account-provisioning detail is recorded yet.
- Artifact discovery is intentionally manual for this task. The current contract is to open `Daedalus_KB` and identify the intended canonical `.dump` plus sibling `.manifest.json` pair with the same basename. Task-605 does not define helper commands, automation, or broader publication workflow mechanics.
- Naming and location expectations stay aligned with the shipped snapshot-export contract from task-602: publish the exported `.dump` and sibling `.manifest.json` directly into `Daedalus_KB`, keep the pair together, and do not rename only one file from the pair.
- Retention is manual in v1. The current minimum documented expectation is to keep the current canonical pair in `Daedalus_KB` until a newer compatible pair replaces it, and keep the previous known-good pair available as a short-term fallback when possible.
- Integrity after download remains the shipped import-side validation from tasks `602` and `612`, not a new Dropbox-specific mechanism. Operators download both sibling files together and rely on `snapshot import` to validate manifest schema plus dump size/hash before any destructive restore into a disposable KB target.
- Recovery for Dropbox outage or latest-artifact-unavailable cases remains intentionally narrow: use a last known-good compatible local pair or rebuild locally from `develop` on a trusted GPU-capable machine. Do not switch to GitHub Actions artifacts, GitHub Releases, or other ad hoc publication channels.
- User validation ultimately succeeded end to end for task-605 scope once the KB snapshot workflow was healthy again: `sync docs` persisted `1822` documents, `snapshot export` produced `agentic-kb-20260330T154354Z.dump` plus manifest, a fresh DB reset/import succeeded, post-import document count returned `1822`, `status --json` reported `"ok": true`, and BM25 search returned hits including `.agent/workflows/agentic-kb.md`.

## Gotchas

- Dropbox access and shared-folder writability cannot be proven from repo state alone. This task now has manual validation evidence recorded in the implementation review log, but future operators should still treat Dropbox access as an external prerequisite rather than something the repo can self-verify.
- This task records only the backend decision and minimum operator contract. It does not define helper commands, upload/download wrappers, broader fallback ownership, or a richer multi-developer SOP.

## Validation Chronology Note

- Early manual validation attempts produced empty exported snapshots because of a separate docs-sync transaction bug that was fixed elsewhere in the repo by enabling autocommit in `PostgresDocsStore`.
- The durable task-605 implication is only that the Dropbox shared-folder workflow validated successfully once the KB snapshot workflow was healthy again; task-605 itself does not claim that unrelated bug fix as part of its implementation scope.

## No New Research Beyond Decision Capture

- No broader vendor evaluation or new runtime behavior was added here. The durable value from this task is the explicit Dropbox backend decision and the minimum truthful operator contract it requires.
