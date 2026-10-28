# Feature Specification: Replay ledger after Mithril conversion failure

**Feature Branch**: `mithril-converter-ledger-replay`
**Created**: 2026-10-28
**Status**: Approved for implementation by the feature request

## User Scenarios & Testing

### User Story 1 — Reuse verified blocks (P1)
When snapshot conversion fails, the user can keep the downloaded immutable blocks and replay the ledger locally, avoiding another Mithril download or a full network sync from genesis.

Acceptance scenarios:
1. Given a verified Mithril download whose conversion or LSM import failed, when the user chooses replay, then the downloaded blocks are installed and the node starts without downloaded or partially converted ledger/LSM state.
2. Given a partial sync, when replay is selected, then existing earlier immutable blocks and downloaded newer blocks are retained together; stale blocks after the downloaded tip are removed.
3. Given a rebuild or first bootstrap, when replay is selected, then the verified blocks are installed without another download.

### User Story 2 — Other recovery choices remain usable (P2)
1. Given a conversion failure, when Retry is chosen, then the retained download is cleaned and normal Mithril retry proceeds.
2. Given a conversion failure, when existing-chain continuation or genesis sync is chosen, then retained staging is cleaned and the explicit choice starts the node.
3. Given a failure before verification or after cutover, then replay is not offered.

## Requirements
- FR-001: Retain verified staging until a recovery choice or shutdown in the current watchdog session.
- FR-002: Offer replay only for conversion failures, including LSM import failures.
- FR-003: Remove staged ledger, LSM, volatile state and clean sentinel before replay installation; remove stale local equivalents on partial installation.
- FR-004: Preserve existing network, path, cutover and interrupted-install protections.
- FR-005: Explain that ledger replay takes time and reuses downloaded blocks, in English and Japanese.
- FR-006: An install or cleanup failure must not start the node on a mixed database.

## Edge Cases and Assumptions
- Replay availability lasts for the current watchdog session. Shutdown removes staging; after an abrupt crash the existing staging and rebuild safeguards apply.
- Cancel and stop remain effective while awaiting recovery.
- Genesis means replaying blocks from the start locally when blocks are retained; a full genesis sync remains the separate existing user choice.

## Success Criteria
- Integration tests prove replay starts the node with preserved immutable bytes and no snapshot ledger state, without another Mithril invocation.
- Renderer tests prove the replay action is restricted to conversion failure and dispatches the recovery command.
- Relevant Rust, Jest and TypeScript checks pass.
