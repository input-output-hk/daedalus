# SOP: Pilot Coordination and Communication

**SOP ID**: `pilot-coordination`
**Related Task**: `task-903`
**Version**: 1.0

---

## Purpose

This SOP documents how the two developers coordinate during the two-developer pilot, including communication protocols, checkpoint acknowledgments, and failure escalation.

---

## Pre-Pilot Sync

Before starting the pilot, both developers must confirm:

1. **Availability**: Both developers are available for the duration of the pilot (estimated 30-60 minutes)
2. **Pre-flight checklist complete**: All items in the pre-pilot checklist from `pilot-two-developer.md` are checked off
3. **Communication channel agreed**: Both developers agree on the primary communication channel (Slack, email, video call, etc.)
4. **Timezone alignment**: Both developers confirm their local time and agree on a start time
5. **Fallback contact**: Both developers share a secondary contact method in case the primary channel fails

---

## Publish Notification

When Developer 1 completes the publish step:

1. Developer 1 sends a notification to Developer 2 via the agreed channel containing:
   - Snapshot basename (e.g., `agentic-kb-20260331T120000Z`)
   - Manifest hash (from `.manifest.json`)
   - Approximate file sizes
   - Timestamp of publish completion

   **Example notification**:
   ```
   Snapshot published: agentic-kb-20260331T120000Z
   Manifest hash: <hash>
   Dump size: <size>
   Manifest size: <size>
   Published at: <UTC timestamp>
   ```

2. Developer 2 acknowledges receipt of the notification

---

## Sync Confirmation

After receiving the publish notification:

1. Developer 2 checks their local `Daedalus_KB` folder for the snapshot files
2. Developer 2 confirms Dropbox sync is complete (Dropbox icon shows "Up to date")
3. Developer 2 notifies Developer 1 that files are visible and sync is complete
4. Developer 2 proceeds with the import steps per `pilot-two-developer.md`

---

## Checkpoint Acknowledgments

Developer 2 reports back to Developer 1 after each HUMAN CHECKPOINT step:

### Checkpoint 1: Dropbox Sync Confirmation
- **Message**: "Dropbox sync confirmed. Both files present and up to date."
- **Or**: "Dropbox sync not complete. Files missing or still syncing."

### Checkpoint 2: Import Validation
- **Message**: "Import validated. ok: true, embedding_compatibility: compatible."
- **Or**: "Import validation FAILED. Error: <error details>"

### Checkpoint 3: BM25 Proof Results
- **Message**: "BM25 proof passed. <N> hits returned, first hit entity_type: documents."
- **Or**: "BM25 proof FAILED. Zero hits returned."

### Checkpoint 4: Sync Changed
- **Message**: "sync changed completed successfully."
- **Or**: "sync changed FAILED. Error: <error details>"

---

## Failure Escalation Path

When any step fails:

1. **Immediate notification**: Developer 2 shares the full error output with Developer 1 immediately
2. **Joint diagnosis**: Both developers review the error output together
3. **Consult failure modes**: Both developers consult `pilot-failure-modes.md` to identify the matching failure mode
4. **Apply recovery**: Follow the recovery procedure from the failure modes table
5. **Retry**: After recovery, retry the failed step
6. **Escalate if unresolved**: If the failure mode is not covered or recovery does not work:
   - Document the error in the evidence template
   - Consider full rollback per `pilot-rollback.md`
   - Restart the pilot from the appropriate stage

---

## Evidence Handoff

After the pilot completes (success or failure):

1. Developer 2 compiles all evidence using `pilot-evidence-template.md`
2. Developer 2 shares the completed evidence template with Developer 1
3. Developer 2 shares all structured JSON artifacts:
   - `status--json.json`
   - `bm25-proof-result.json`
   - `sync-changed-result.json`
4. Developer 1 reviews the evidence and confirms completeness
5. Both developers archive the evidence for downstream task-905 validation

---

## Post-Pilot Debrief

After evidence handoff, both developers conduct a brief debrief:

1. **Review evidence**: Both developers review the completed evidence template together
2. **Assess success criteria**: Confirm whether all success criteria from `pilot-two-developer.md` were met
3. **Document SOP improvements**: Note any ambiguities, missing steps, or inaccuracies in the SOPs
4. **Confirm readiness**: Confirm whether the pilot results support readiness for task-905 validation
5. **Action items**: Document any follow-up actions needed before task-904, task-905, or task-906

---

## Communication Log Template

Use this template to track communications during the pilot:

| Timestamp (UTC) | From | To | Message | Status |
|-----------------|------|-----|---------|--------|
| | Dev 1 | Dev 2 | Publish notification sent | Acknowledged |
| | Dev 2 | Dev 1 | Dropbox sync confirmed | |
| | Dev 2 | Dev 1 | Checkpoint 2: Import validation | |
| | Dev 2 | Dev 1 | Checkpoint 3: BM25 proof | |
| | Dev 2 | Dev 1 | Checkpoint 4: Sync changed | |
| | Dev 2 | Dev 1 | Evidence handoff complete | |
