# Task 611 Canonical Embedding Contract Policy Research

- Date: 2026-03-29
- Task: `task-611`
- Evidence: `.agent/workflows/agentic-kb.md`, `agentic/config/snapshot-manifest.schema.json`, `agentic/config/snapshot-manifest.example.json`, `.agent/plans/agentic/research/task-608-snapshot-embedding-contract-compatibility.md`

## Durable Findings

- The shipped compatibility contract is the exact manifest `embedding_contract` tuple: `contract_id`, `embedding_model`, and `embedding_dimension`. Documentation for shared-baseline policy should use those field names directly instead of paraphrasing them.
- The repo documents the contract shape in the checked-in schema and example manifest, but it does not expose a separate dedicated canonical tuple registry for operators. Truthful v1 discovery therefore depends on the workflow policy plus the published manifest for a specific snapshot artifact.
- `status --json` is the correct post-import compatibility confirmation surface for a specific imported baseline, but it is not a source of truth for which tuple should be considered canonical before artifact selection.

## Verification Notes

- Verified the workflow wording against the approved task-611 plan and the task-608 compatibility research.
- Verified the contract field names and strict manifest shape against `agentic/config/snapshot-manifest.schema.json` and `agentic/config/snapshot-manifest.example.json`.
