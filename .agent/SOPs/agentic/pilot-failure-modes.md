# SOP: Pilot Failure Modes

**SOP ID**: `pilot-failure-modes`
**Related Task**: `task-903`
**Version**: 1.0

---

## Purpose

This SOP documents common failure modes encountered during the two-developer pilot, with symptoms and recovery procedures.

---

## Failure Mode Reference

| Failure Mode | Symptom | Recovery |
|-------------|---------|----------|
| **Dropbox sync delay** | Developer 2 cannot see `.dump` or `.manifest.json` files in `Daedalus_KB` folder | Force Dropbox sync (right-click folder → "Sync now"), verify sharing permissions, confirm Developer 1 granted write access to Developer 2's Dropbox account. If files still missing, Developer 1 re-confirms upload and waits for sync completion. |
| **Incompatible embedding contract** | `snapshot import` rejects manifest with contract mismatch error | Developer 1 republishes from current `develop` branch. Developer 2 recreates KB volume (`docker compose -f docker-compose.agentic.yml down -v && up -d`) and re-imports the new snapshot. |
| **Non-disposable import target** | `snapshot import` refuses target with existing data ("target is not disposable") | Run `docker compose -f docker-compose.agentic.yml down -v` to destroy existing KB volume, then `docker compose -f docker-compose.agentic.yml up -d` to create fresh stack. Retry import. |
| **`sync changed` fails after import — missing baseline** | `sync changed` returns error about missing baseline for one or more sources | Run individual sync commands to establish baselines first: `sync docs`, `sync code`, `sync github`, `sync project` (or `sync all`). Then retry `sync changed`. |
| **`GITHUB_TOKEN` scope failure (HTTP 403)** | `sync changed` or `sync project` returns HTTP 403 with error detail `"project scope missing"` | Developer 2 must update `GITHUB_TOKEN` to include `read:project` scope. Run `gh auth refresh -s read:project` to add the missing scope. A repo-only token is insufficient for project-level sync. |
| **`yarn agentic:kb:publish` fails** | Command exits non-zero; error about missing `AGENTIC_KB_SHARED_DIR` or stack not running | Ensure `AGENTIC_KB_SHARED_DIR` environment variable is set to the local `Daedalus_KB` path. Ensure stack is running (`docker compose -f docker-compose.agentic.yml ps`). Retry publish. |
| **`yarn agentic:kb:fetch` fails** | Command exits non-zero; error about missing snapshot basename or incomplete pair | Verify both `.dump` and `.manifest.json` files exist together in `Daedalus_KB` with matching basenames. Use the correct basename (without extension) in the fetch command. |
| **`status --json` reports `ok: false`** | Status command returns `"ok": false` with error details | Check `docker compose -f docker-compose.agentic.yml ps` for container health. Inspect container logs (`docker compose -f docker-compose.agentic.yml logs kb-tools`). Restart stack if needed. |
| **BM25 proof query returns zero hits** | Search query returns empty results array | Re-run import with `--yes` flag to ensure acknowledgement. Check `status --json` row counts to confirm data was loaded. If row counts are zero, the import may have silently failed — recreate volume and retry. |

---

## Escalation Path

If a failure mode is not covered above or recovery does not resolve the issue:

1. Developer 2 shares the full error output with Developer 1 immediately
2. Both developers consult this failure mode table
3. If no matching failure mode is found, consult `pilot-coordination.md` for escalation procedures
4. As a last resort, follow `pilot-rollback.md` for full rollback and restart from scratch

---

## Notes

- Always capture error output verbatim for the evidence template (`pilot-evidence-template.md`)
- Do not retry a failed step more than twice without consulting the failure modes table
- If `sync changed` fails after successful import and BM25 proof, the issue is likely a baseline gap or token scope problem — check those first
