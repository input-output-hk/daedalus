# SOP: Pilot Rollback Procedures

**SOP ID**: `pilot-rollback`
**Related Task**: `task-903`
**Version**: 1.0

---

## Purpose

This SOP documents rollback procedures for each stage of the two-developer pilot. Use these procedures when a pilot step fails and needs to be undone before retrying.

---

## Rollback by Stage

### After Failed Publish

**When**: Developer 1's `yarn agentic:kb:publish` fails or produces incomplete output.

**Impact**: No impact on Developer 2. No cleanup needed on Developer 2 side.

**Procedure**:
1. Developer 1 reviews error output
2. Consult `pilot-failure-modes.md` for matching failure mode
3. Fix the root cause (e.g., set `AGENTIC_KB_SHARED_DIR`, start stack)
4. Retry publish
5. If partial files were copied to `Daedalus_KB`, remove them before retry:
   ```bash
   rm "/path/to/Daedalus_KB"/agentic-kb-*
   ```

---

### After Failed Fetch

**When**: Developer 2's `yarn agentic:kb:fetch` fails or produces incomplete files.

**Impact**: Partial files may exist in `agentic/snapshots/`.

**Procedure**:
1. Delete partial files from `agentic/snapshots/`:
   ```bash
   rm -f agentic/snapshots/<snapshot-basename>.*
   ```
2. Verify both `.dump` and `.manifest.json` are present and complete in `Daedalus_KB`
3. Confirm Dropbox sync is fully complete (no sync-in-progress indicator)
4. Retry fetch

---

### After Failed Import

**When**: `snapshot import` fails (contract mismatch, non-disposable target, or artifact mismatch).

**Impact**: KB volume may be in an inconsistent state.

**Procedure**:
1. Tear down the KB volume to ensure a clean disposable target:
   ```bash
   docker compose -f docker-compose.agentic.yml down -v
   ```
2. Recreate a fresh stack:
   ```bash
   docker compose -f docker-compose.agentic.yml up -d
   ```
3. Verify clean state:
   ```bash
   docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json
   ```
4. Retry import from the fetched snapshot

---

### After Failed `sync changed`

**When**: `sync changed` fails after successful import and BM25 proof.

**Impact**: No data loss risk — `sync changed` is additive only.

**Procedure**:
1. Identify the failing source from the error output
2. If baseline is missing for a source, run the individual sync command:
   ```bash
   # For docs baseline
   docker compose -f docker-compose.agentic.yml run --rm kb-tools sync docs
   # For code baseline
   docker compose -f docker-compose.agentic.yml run --rm kb-tools sync code
   # For GitHub baseline
   docker compose -f docker-compose.agentic.yml run --rm kb-tools sync github
   # For Project baseline
   docker compose -f docker-compose.agentic.yml run --rm kb-tools sync project
   ```
3. If `GITHUB_TOKEN` scope issue (HTTP 403), refresh token:
   ```bash
   gh auth refresh -s read:project
   ```
4. Retry `sync changed`

---

## Full Rollback

**When**: Multiple stages have failed, or the pilot needs to be restarted from scratch.

**Procedure for Both Developers**:

1. Tear down stacks and destroy all KB volumes:
   ```bash
   docker compose -f docker-compose.agentic.yml down -v
   ```

2. Clear local snapshot artifacts:
   ```bash
   rm -rf agentic/snapshots/agentic-kb-*
   ```

3. (Developer 2 only) Remove any partial files from `Daedalus_KB` that may have been fetched but not imported:
   ```bash
   # Do NOT remove Developer 1's published files — only remove fetched copies if stored separately
   ```

4. Verify clean state:
   ```bash
   docker volume ls | grep agentic
   # Should return no results
   ```

5. Restart pilot from the beginning per `pilot-two-developer.md`

---

## Safety Notes

- `snapshot import` only runs against disposable targets; rollback via `down -v` is safe
- `sync changed` is additive; rollback does not require data restoration
- Published files in `Daedalus_KB` are not modified by import or sync operations
- Always consult `pilot-failure-modes.md` before rolling back to understand the root cause
