# Mithril bootstrap UI model

## Sources
- Mithril root docs: `docs/website/root/manual/getting-started/bootstrap-cardano-node.md`
- Mithril root docs: `docs/website/root/manual/develop/run-mithril-devnet.md`
- Mithril CLI source: `mithril-client-cli/src/commands/cardano_db/download/v2.rs`
- Mithril CLI source: `mithril-client-cli/src/commands/cardano_db/shared_steps.rs`
- Mithril CLI source: `mithril-client-cli/src/utils/progress_reporter.rs`
- Mithril CLI source: `mithril-client-cli/src/utils/feedback_receiver.rs`
- Daedalus source: `source/main/mithril/MithrilBootstrapService.ts`
- Daedalus source: `source/main/mithril/mithrilProgress.ts`

## Confirmed Mithril behavior
- Current `cardano-db download` defaults to backend `v2` and runs a 7-step internal flow: disk check, certificate-chain validation, download/unpack, digest verification, database verification, message computation, signature verification.
- Mithril JSON mode emits structured step and progress data, but UI copy should not depend on raw Mithril message strings.
- The progress Daedalus already consumes is file-oriented and sufficient to drive the visible download phase.
- Daedalus byte progress is still an estimate derived from `filesDownloaded / filesTotal * snapshot.size`; keep it framed as estimated transfer metadata rather than exact Mithril byte telemetry.

## Recommended visible step model
- `Preparing`
  - Covers local preflight work and the early Mithril checks before file transfer is underway.
  - No progress bar.
- `Downloading`
  - Keep the current name `Downloading`.
  - Show the only determinate progress bar here.
  - Drive the bar from the exposed file download progress for this step and scale it from 0 to 100 inside the step.
  - Keep estimated bytes, rate, elapsed, and remaining metadata here only.
- `Verifying` (implemented — maps to the visible `Downloading` step)
  - `verifying` status is emitted by the service when the CLI enters step 4 (digest verification).
  - The service synthesizes both progress bars to 100% and drops late download-phase events.
  - Both progress bars are hidden during verification; download sub-items collapse to a green checkmark.
  - Verification-phase errors carry a distinct `verify` error stage.
  - The 3-step visible waterfall (Preparing / Downloading / Finalizing) is preserved — `verifying` is not a 4th visible step.
- `Finalizing`
  - Covers Daedalus local handoff after Mithril exits.
  - Reword Daedalus `unpacking` to `installing` for local DB placement work.
  - If `_installSnapshot()` can expose honest measured progress, surface it.
  - If `fs.move()` progress cannot be exposed truthfully across same-device moves, renames, or symlink targets, keep this step minimal and indeterminate like `Verifying`.

## Service and UI guardrails
- Do not use Mithril JSON `message` strings as UI text.
- If service-side phase boundaries need Mithril JSON, rely on machine-readable structure and keep renderer copy fully localized.
- Reserve the determinate progress bar for `Downloading` only.
- Do not keep showing transfer stats once the app has moved into `Verifying` or `Finalizing`.
- Confirm the existing extracted i18n copy remains appropriate for `Preparing -> Downloading -> Verifying -> Finalizing`; update translations only if gaps are found.

## Implications for Daedalus
- Add an explicit visible `verifying` state after file download completion.
- Rename local post-download `unpacking` wording to `installing` where it describes Daedalus-side file placement.
- Treat `_installSnapshot()` progress exposure as opportunistic: instrument it if the result is honest, otherwise keep the finalizing UX activity-only.
- Keep Mithril restore completion and Cardano startup handoff as separate statuses: `completed` for restore done, `starting-node` for the live post-restore Cardano startup phase.
- Generic Cardano crash recovery must not restart the backend while Mithril is active; Mithril owns the first post-restore startup attempt and any immediate retry/error handling.
- If Cardano exits during the `starting-node` handoff, surface that as Mithril `node-start` failure rather than leaving the UI in a terminal-looking completed state.