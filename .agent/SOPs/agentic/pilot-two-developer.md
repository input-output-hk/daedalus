# SOP: Two-Developer Pilot Workflow

**SOP ID**: `pilot-two-developer`
**Related Task**: `task-903`
**Version**: 1.0

---

## Purpose

This SOP documents the complete end-to-end pilot workflow for sharing a KB snapshot between two developers using Dropbox as the shared storage backend.

---

## Pre-Pilot Checklist

Complete all items before starting the pilot:

- [ ] **Dropbox installed**: Both developers have Dropbox desktop installed and running
- [ ] **Shared folder created**: Developer 1 created `Daedalus_KB` shared folder and shared with Developer 2 (write access)
- [ ] **Shared folder accepted**: Developer 2 accepted the shared folder invitation and confirmed local sync
- [ ] **Developer 2 Dropbox pre-flight**: Developer 2 confirms they can create/accept a shared folder before the pilot date (test by accepting a dummy shared folder or confirming Dropbox account has no sharing restrictions)
- [ ] **Docker volume hygiene check**: Developer 2 runs `docker volume ls | grep agentic` to check for stale volumes from prior KB work; if any exist, run `docker compose -f docker-compose.agentic.yml down -v` to clean up before pilot
- [ ] **GITHUB_TOKEN scope validation**: Developer 2 runs `gh auth status` or checks token scopes to confirm `GITHUB_TOKEN` has `read:project` scope (required for `sync changed`); a repo-only token will cause HTTP 403 failures
- [ ] **Current branch**: Both developers have current `develop` branch checked out
- [ ] **GPU availability**: Developer 1 has GPU-capable machine (or accepts longer CPU embedding time)
- [ ] **Developer 2 CPU-only acceptable**: Import and `sync changed` work on CPU

---

## Developer 1 (Publisher) Steps

### Step 1: Start Stack

```bash
docker compose -f docker-compose.agentic.yml up -d
```

### Step 2: Publish Baseline

```bash
AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:publish
```

### Step 3: Confirm Snapshot Files

Verify both `.dump` and `.manifest.json` appear in the local `Daedalus_KB` folder:

```bash
ls -la "/path/to/Daedalus_KB"/agentic-kb-*
```

Expected: one `.dump` file and one sibling `.manifest.json` file with matching basename.

### Step 4: Wait for Dropbox Sync

Wait for Dropbox to sync the files to Developer 2's machine. Notify Developer 2 with:
- Snapshot basename (e.g., `agentic-kb-20260331T120000Z`)
- Manifest hash (from `.manifest.json`)

---

## Developer 2 (Importer) Steps

### Step 1: Confirm Snapshot Pair Visible

Check that both files appear in the local `Daedalus_KB` folder:

```bash
ls -la "/path/to/Daedalus_KB"/agentic-kb-*
```

### HUMAN CHECKPOINT 1: Dropbox Sync Confirmation

> **STOP**: Developer 2 must visually confirm Dropbox sync completed before proceeding.
> - Both `.dump` and `.manifest.json` are present
> - Dropbox icon shows "Up to date" (no sync-in-progress indicator)
> - File sizes match what Developer 1 reported
>
> Do NOT run fetch until sync is fully complete.

### Step 2: Clean Stack Start

```bash
docker compose -f docker-compose.agentic.yml down -v
docker compose -f docker-compose.agentic.yml up -d
```

The `down -v` ensures a clean import target per the disposable safety contract.

### Step 3: Fetch Snapshot

```bash
AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:fetch -- <snapshot-basename>
```

### Step 4: Import Snapshot

```bash
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/<snapshot>.dump --yes
```

### Step 5: Validate Import

```bash
docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json
```

### HUMAN CHECKPOINT 2: Import Validation

> **STOP**: Developer 2 must visually confirm the following before proceeding:
> - `status --json` reports `"ok": true`
> - `status --json` reports `"embedding_compatibility": { "state": "compatible", ... }`
>
> If either check fails, do NOT proceed to BM25 proof. Follow `pilot-failure-modes.md` instead.

### Step 6: Run Deterministic BM25 Proof

```bash
docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"
```

### HUMAN CHECKPOINT 3: BM25 Proof Results

> **STOP**: Developer 2 must visually confirm the following before proceeding:
> - BM25 proof query returns at least one hit
> - The first hit has `"entity_type": "documents"`
>
> If zero hits are returned, do NOT proceed to sync. Follow `pilot-failure-modes.md` instead.

### Step 7: Run Incremental Refresh (Conditional)

**Only if** import validation (Step 5) and BM25 proof (Step 6) passed:

```bash
docker compose -f docker-compose.agentic.yml run --rm kb-tools sync changed
```

**If validation failed**, follow `pilot-failure-modes.md` before attempting sync.

---

## Success Criteria

The pilot is considered successful when all of the following are met:

- **Developer 1**: `yarn agentic:kb:publish` exits 0, both sibling files (`.dump` + `.manifest.json`) present in `Daedalus_KB`
- **Developer 2**: `status --json` reports `"ok": true` and `"embedding_compatibility": { "state": "compatible" }`
- **Developer 2**: BM25 proof query returns at least one hit with `"entity_type": "documents"`
- **Developer 2**: `sync changed` completes successfully (or reports no changes if baselines are current)

---

## Post-Pilot

1. Developer 2 captures all evidence using `pilot-evidence-template.md`
2. Both developers review results together per `pilot-coordination.md`
3. If any step failed, consult `pilot-failure-modes.md` for recovery before retry
4. If rollback is needed, follow `pilot-rollback.md`
