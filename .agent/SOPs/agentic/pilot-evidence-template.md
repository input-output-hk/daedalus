# SOP: Pilot Evidence Template

**SOP ID**: `pilot-evidence-template`
**Related Task**: `task-903`
**Version**: 1.0

---

## Purpose

This template provides a structured format for capturing pilot results, command outputs, and validation evidence. Fill in all sections during pilot execution.

---

## Pilot Metadata

| Field | Value |
|-------|-------|
| **Pilot Date** | |
| **Developer 1 (Publisher)** | |
| **Developer 2 (Importer)** | |
| **Communication Channel** | (Slack, email, etc.) |

---

## Developer 1 Machine Details

| Field | Value |
|-------|-------|
| **OS** | |
| **GPU Available** | Yes / No |
| **Docker Version** | |
| **Branch** | `develop` |
| **Commit Hash** | |

---

## Developer 2 Machine Details

| Field | Value |
|-------|-------|
| **OS** | |
| **GPU Available** | Yes / No |
| **Docker Version** | |
| **Branch** | `develop` |
| **Commit Hash** | |

---

## Snapshot Information

| Field | Value |
|-------|-------|
| **Snapshot Basename** | (e.g., `agentic-kb-20260331T120000Z`) |
| **Manifest Hash** | |
| **Embedding Contract (from manifest)** | `contract_id`, `embedding_model`, `embedding_dimension` |

---

## Timestamps

| Stage | Timestamp (UTC) |
|-------|-----------------|
| Publish started | |
| Publish completed | |
| Dropbox sync confirmed | |
| Fetch started | |
| Fetch completed | |
| Import started | |
| Import completed | |
| Status validation | |
| BM25 proof | |
| Sync changed started | |
| Sync changed completed | |

---

## Pre-Flight Checks

### Developer 2 Dropbox Pre-Flight

- [ ] Confirmed ability to accept shared folder before pilot date
- [ ] Test: (describe test performed)

### Docker Volume Hygiene

```bash
# Output of: docker volume ls | grep agentic
```

### GITHUB_TOKEN Scope Validation

```bash
# Output of: gh auth status
```

- [ ] `read:project` scope confirmed: Yes / No

---

## Command Outputs

### Developer 1: Publish

```bash
# Command: AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:publish
# Exit code:
# Output (truncated to relevant lines):
```

### Developer 1: Snapshot Files Confirmed

```bash
# Command: ls -la "/path/to/Daedalus_KB"/agentic-kb-*
# Output:
```

### Developer 2: Fetch

```bash
# Command: AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:fetch -- <snapshot-basename>
# Exit code:
# Output (truncated to relevant lines):
```

### Developer 2: Import

```bash
# Command: docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/<snapshot>.dump --yes
# Exit code:
# Output (truncated to relevant lines):
```

---

## Structured JSON Artifacts

Save the following raw JSON outputs as files for downstream task-905 validation.

### `status--json.json`

Full output of `docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json`:

```json
{
  "ok": true,
  "embedding_compatibility": {
    "state": "compatible"
  }
}
```

**Task-905 field mapping:**
- `ok: true` → import health validation
- `embedding_compatibility.state: "compatible"` → embedding contract compatibility
- Row counts → data completeness validation

### `bm25-proof-result.json`

Full output of BM25 proof search:

```bash
# Command: docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"
# Save stdout to bm25-proof-result.json
```

**Task-905 field mapping:**
- Hit count > 0 → search functionality on imported baseline
- First hit `entity_type: "documents"` → correct entity indexing
- First hit `fields.source_path: ".agent/workflows/agentic-kb.md"` → expected content present

### `sync-changed-result.json`

Full output of `sync changed`:

```bash
# Command: docker compose -f docker-compose.agentic.yml run --rm kb-tools sync changed
# Save stdout to sync-changed-result.json
```

**Task-905 field mapping:**
- Exit code 0 → incremental refresh works post-import
- Per-source results → individual source sync health
- No errors → baseline compatibility confirmed

---

## Validation Results

### Import Validation (HUMAN CHECKPOINT 2)

- [ ] `status --json` reports `ok: true`: Yes / No
- [ ] `embedding_compatibility.state: "compatible"`: Yes / No

### BM25 Proof Validation (HUMAN CHECKPOINT 3)

- [ ] At least one hit returned: Yes / No
- [ ] First hit has `entity_type: "documents"`: Yes / No

### Sync Changed Validation

- [ ] `sync changed` completed successfully: Yes / No
- [ ] Or: reported no changes (baselines current): Yes / No

---

## Errors and Resolution Log

| Step | Error | Resolution | Resolved? |
|------|-------|------------|-----------|
| | | | |
| | | | |

---

## Notes

### Workflow Clarity

_(Notes on whether the SOP instructions were clear, any ambiguities encountered, and suggestions for improvement)_

### Documentation Accuracy

_(Notes on whether the documented commands matched actual behavior, any discrepancies found)_

### Additional Observations

_(Any other findings relevant to downstream tasks task-904, task-905, task-906)_
