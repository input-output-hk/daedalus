import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

const messages: Record<string, ReactIntlMessage> = defineMessages({
  title: {
    id: 'loading.mithrilBootstrap.title',
    defaultMessage: '!!!Fast sync with Mithril',
    description: 'Headline for the Mithril bootstrap prompt.',
  },
  progressSubtitle: {
    id: 'loading.mithrilBootstrap.progress.subtitle',
    defaultMessage:
      '!!!Snapshot download and verification time can vary based on your network connection and storage performance.',
    description: 'Supporting copy shown under the Mithril progress view title.',
  },
  description: {
    id: 'loading.mithrilBootstrap.description',
    defaultMessage:
      '!!!Mithril can download a verified snapshot to sync your blockchain data faster. Choose a snapshot and continue, or do a Blockchain Sync from Genesis.',
    description: 'Description for the Mithril bootstrap prompt.',
  },
  accept: {
    id: 'loading.mithrilBootstrap.accept',
    defaultMessage: '!!!Use Mithril fast sync',
    description: 'Button label to accept Mithril bootstrap.',
  },
  decline: {
    id: 'loading.mithrilBootstrap.decline',
    defaultMessage: '!!!Blockchain Sync from Genesis',
    description: 'Button label to decline Mithril bootstrap.',
  },
  selectLabel: {
    id: 'loading.mithrilBootstrap.selectLabel',
    defaultMessage: '!!!Snapshot',
    description: 'Label for the snapshot selector.',
  },
  snapshotLatest: {
    id: 'loading.mithrilBootstrap.snapshotLatest',
    defaultMessage: '!!!Latest snapshot',
    description: 'Label for the latest snapshot option.',
  },
  snapshotDetailsTitle: {
    id: 'loading.mithrilBootstrap.snapshotDetailsTitle',
    defaultMessage: '!!!Snapshot details',
    description: 'Title for the snapshot metadata section.',
  },
  snapshotDetailsUnavailable: {
    id: 'loading.mithrilBootstrap.snapshotDetailsUnavailable',
    defaultMessage: '!!!Snapshot details unavailable',
    description: 'Fallback text when snapshot metadata is missing.',
  },
  snapshotSizeLabel: {
    id: 'loading.mithrilBootstrap.snapshotSizeLabel',
    defaultMessage: '!!!Size',
    description: 'Label for snapshot size metadata.',
  },
  snapshotCreatedLabel: {
    id: 'loading.mithrilBootstrap.snapshotCreatedLabel',
    defaultMessage: '!!!Created',
    description: 'Label for snapshot creation date metadata.',
  },
  snapshotNodeVersionLabel: {
    id: 'loading.mithrilBootstrap.snapshotNodeVersionLabel',
    defaultMessage: '!!!Node version',
    description: 'Label for snapshot node version metadata.',
  },
  snapshotDigestLabel: {
    id: 'loading.mithrilBootstrap.snapshotDigestLabel',
    defaultMessage: '!!!Digest',
    description: 'Label for snapshot digest metadata.',
  },
  progressElapsedLabel: {
    id: 'loading.mithrilBootstrap.progress.elapsedLabel',
    defaultMessage: '!!!Elapsed time:',
    description: 'Label for Mithril elapsed time metadata.',
  },
  cancel: {
    id: 'loading.mithrilBootstrap.cancel',
    defaultMessage: '!!!Cancel',
    description: 'Button label to cancel Mithril bootstrap.',
  },
  wipeAndRetry: {
    id: 'loading.mithrilBootstrap.wipeAndRetry',
    defaultMessage: '!!!Wipe chain & retry',
    description: 'Button label to wipe chain and retry Mithril bootstrap.',
  },
  errorTitle: {
    id: 'loading.mithrilBootstrap.errorTitle',
    defaultMessage: '!!!Mithril bootstrap failed',
    description: 'Generic title for the Mithril bootstrap error state.',
  },
  startFailureHint: {
    id: 'loading.mithrilBootstrap.startFailureHint',
    defaultMessage:
      '!!!The node could not start with the restored chain data. Wipe the chain and try Mithril Sync again, or do a Blockchain Sync from Genesis.',
    description: 'Hint for node-start failures after Mithril bootstrap.',
  },
  stepPreparing: {
    id: 'loading.mithrilBootstrap.step.preparing',
    defaultMessage: '!!!Preparing',
    description: 'Label for the preparing step in the Mithril step indicator.',
  },
  stepDownloading: {
    id: 'loading.mithrilBootstrap.step.downloading',
    defaultMessage: '!!!Downloading',
    description:
      'Label for the downloading step in the Mithril step indicator.',
  },
  stepFinalizing: {
    id: 'loading.mithrilBootstrap.step.finalizing',
    defaultMessage: '!!!Finalizing',
    description: 'Label for the finalizing step in the Mithril step indicator.',
  },
  stepIndicatorLabel: {
    id: 'loading.mithrilBootstrap.stepIndicatorLabel',
    defaultMessage: '!!!Mithril Sync progress',
    description: 'Accessible label for the Mithril step indicator list.',
  },
  stepIndicatorDetailsLabel: {
    id: 'loading.mithrilBootstrap.stepIndicatorDetailsLabel',
    defaultMessage: '!!!{stepName} details',
    description:
      'Accessible label for nested Mithril step indicator details lists.',
  },
  snapshotSelectorGroupLabel: {
    id: 'loading.mithrilBootstrap.snapshotSelectorGroupLabel',
    defaultMessage: '!!!Snapshot selection',
    description: 'Accessible label for the Mithril snapshot selector group.',
  },
  errorDownloadTitle: {
    id: 'loading.mithrilBootstrap.error.download.title',
    defaultMessage: '!!!Snapshot download failed',
    description: 'Title for Mithril download-stage failures.',
  },
  errorDownloadHint: {
    id: 'loading.mithrilBootstrap.error.download.hint',
    defaultMessage:
      '!!!Check your network connection and try the download again.',
    description: 'Hint shown for Mithril download-stage failures.',
  },
  errorVerifyTitle: {
    id: 'loading.mithrilBootstrap.error.verify.title',
    defaultMessage: '!!!Snapshot verification failed',
    description: 'Title for Mithril verification-stage failures.',
  },
  errorVerifyHint: {
    id: 'loading.mithrilBootstrap.error.verify.hint',
    defaultMessage:
      '!!!Daedalus could not verify the snapshot integrity. Try another snapshot or do a Blockchain Sync from Genesis.',
    description: 'Hint shown for Mithril verification-stage failures.',
  },
  errorConvertTitle: {
    id: 'loading.mithrilBootstrap.error.convert.title',
    defaultMessage: '!!!Ledger conversion failed',
    description: 'Title for Mithril conversion-stage failures.',
  },
  errorConvertHint: {
    id: 'loading.mithrilBootstrap.error.convert.hint',
    defaultMessage:
      '!!!Daedalus could not prepare the downloaded snapshot for use. Try again or do a Blockchain Sync from Genesis.',
    description: 'Hint shown for Mithril conversion-stage failures.',
  },
  errorNodeStartTitle: {
    id: 'loading.mithrilBootstrap.error.nodeStart.title',
    defaultMessage: '!!!Node failed to start',
    description: 'Title for node-start failures after Mithril bootstrap.',
  },
  errorDetailsHeader: {
    id: 'loading.mithrilBootstrap.errorDetailsHeader',
    defaultMessage: '!!!Technical details',
    description:
      'Collapsed technical-details section header on the Mithril error view (bootstrap and Mithril Sync overlays).',
  },
  progressDiskCheck: {
    id: 'loading.mithrilBootstrap.progress.diskCheck',
    defaultMessage: '!!!Checking local disk info',
    description:
      'Label for Mithril progress step 1 (mithril-client disk-check step).',
  },
  progressCertificateChain: {
    id: 'loading.mithrilBootstrap.progress.certificateChain',
    defaultMessage: '!!!Fetching certificate chain',
    description:
      'Label for Mithril progress step 2 (mithril-client certificate-chain step).',
  },
  progressDownloadingSnapshot: {
    id: 'loading.mithrilBootstrap.progress.downloadingSnapshot',
    defaultMessage: '!!!Downloading snapshot data',
    description:
      'Label for Mithril progress step 3 (mithril-client downloading-snapshot step).',
  },
  progressVerifyingDigests: {
    id: 'loading.mithrilBootstrap.progress.verifyingDigests',
    defaultMessage: '!!!Verifying snapshot digests',
    description:
      'Label for Mithril progress step 4 (mithril-client verifying-digests step).',
  },
  progressVerifyingDatabase: {
    id: 'loading.mithrilBootstrap.progress.verifyingDatabase',
    defaultMessage: '!!!Verifying database integrity',
    description:
      'Label for Mithril progress step 5 (mithril-client verifying-database step).',
  },
  progressComputingMessage: {
    id: 'loading.mithrilBootstrap.progress.computingMessage',
    defaultMessage: '!!!Computing verification message',
    description:
      'Label for Mithril progress step 6 (mithril-client computing-message step).',
  },
  progressVerifyingSignature: {
    id: 'loading.mithrilBootstrap.progress.verifyingSignature',
    defaultMessage: '!!!Verifying snapshot signature',
    description:
      'Label for Mithril progress step 7 (mithril-client verifying-signature step).',
  },
  progressInstallSnapshot: {
    id: 'loading.mithrilBootstrap.progress.installSnapshot',
    defaultMessage: '!!!Moving snapshot to storage',
    description:
      'Label for Mithril progress install-snapshot finalizing step (moving/restoring snapshot to blockchain storage).',
  },
  progressMoveCaution: {
    id: 'loading.mithrilBootstrap.progress.moveCaution',
    defaultMessage:
      "!!!To preserve data integrity, please don't close Daedalus until this step is complete.",
    description:
      'Caution shown while the snapshot is being moved to blockchain storage (install-snapshot step), on both the bootstrap and Mithril Sync overlays.',
  },
  progressCleanup: {
    id: 'loading.mithrilBootstrap.progress.cleanup',
    defaultMessage: '!!!Cleaning up bootstrap files',
    description:
      'Label for Mithril progress cleanup finalizing step (removing bootstrap temporary files).',
  },
  progressConversion: {
    id: 'loading.mithrilBootstrap.progress.conversion',
    defaultMessage: '!!!Converting snapshot format',
    description:
      'Label for Mithril progress conversion finalizing step (only shown if format conversion is needed). Conversion-phase label only.',
  },
  progressCombinedLabel: {
    id: 'loading.mithrilBootstrap.progress.combinedLabel',
    defaultMessage: '!!!Snapshot Files and Ledger State',
    description:
      'Label for the combined Mithril download progress bar that merges snapshot files and ledger state progress.',
  },
  progressCombinedDetail: {
    id: 'loading.mithrilBootstrap.progress.combinedDetail',
    defaultMessage:
      '!!!Snapshot files: {snapshotDownloaded} / {snapshotTotal} files | Ledger state: {fastSyncDownloaded} / {fastSyncTotal}',
    description:
      'Detail line for the combined Mithril download progress bar showing snapshot and ledger state transfer totals.',
  },
  progressSnapshotSizeContext: {
    id: 'loading.mithrilBootstrap.progress.snapshotSizeContext',
    defaultMessage: '!!!≈ {totalSize} total',
    description:
      'Static, real-size context appended to the Mithril download readout (derived from snapshot.size); not a moving bar.',
  },
  progressLongPhaseReassurance: {
    id: 'loading.mithrilBootstrap.progress.longPhaseReassurance',
    defaultMessage:
      '!!!This can take several minutes — Daedalus is still working.',
    description:
      'Reassurance line shown during long non-download Mithril phases (verifying/unpacking/converting/installing/finalizing) so the dialogue never looks frozen.',
  },
  nodeStoppingTitle: {
    id: 'loading.mithrilBootstrap.progress.nodeStoppingTitle',
    defaultMessage: '!!!Stopping Cardano node...',
    description:
      'Title shown for the in-dialogue node-stop frame (default; bootstrap fallback).',
  },
  nodeStoppingDetail: {
    id: 'loading.mithrilBootstrap.progress.nodeStoppingDetail',
    defaultMessage:
      '!!!Daedalus is stopping the Cardano node so it can restore verified chain data. This can take a couple of minutes.',
    description:
      'Detail copy shown for the in-dialogue node-stop frame (default; bootstrap fallback).',
  },
  nodeStartingTitle: {
    id: 'loading.mithrilBootstrap.progress.nodeStartingTitle',
    defaultMessage: '!!!Starting Cardano node...',
    description:
      'Title shown for the dedicated starting-node handoff state after Mithril snapshot restore.',
  },
  nodeStartingDetail: {
    id: 'loading.mithrilBootstrap.progress.nodeStartingDetail',
    defaultMessage:
      '!!!The Mithril snapshot has been restored. Cardano node is starting so Daedalus can continue syncing.',
    description:
      'Detail copy shown for the dedicated starting-node handoff state after Mithril restore.',
  },
  partialSyncTitle: {
    id: 'loading.mithrilPartialSync.title',
    defaultMessage: '!!!Mithril Sync',
    description:
      'Headline for the diagnostics-launched Mithril partial sync overlay.',
  },
  partialSyncProgressSubtitle: {
    id: 'loading.mithrilPartialSync.progress.subtitle',
    defaultMessage:
      '!!!Daedalus is restoring verified Mithril chain data. Download and verification time can vary based on your network connection and storage performance.',
    description:
      'Supporting copy shown under the Mithril partial sync progress title.',
  },
  partialSyncNodeStartingTitle: {
    id: 'loading.mithrilPartialSync.progress.nodeStartingTitle',
    defaultMessage: '!!!Starting Cardano node...',
    description:
      'Title shown while Cardano node starts after Mithril partial sync.',
  },
  partialSyncNodeStartingDetail: {
    id: 'loading.mithrilPartialSync.progress.nodeStartingDetail',
    defaultMessage:
      '!!!Mithril Sync has finished restoring chain data. Cardano node is starting so Daedalus can resume Blockchain Sync.',
    description:
      'Detail copy shown while Cardano node starts after Mithril partial sync.',
  },
  partialSyncNodeStoppingTitle: {
    id: 'loading.mithrilPartialSync.progress.nodeStoppingTitle',
    defaultMessage: '!!!Stopping Cardano node...',
    description:
      'Title shown while the Cardano node is being stopped before Mithril partial sync restores chain data.',
  },
  partialSyncNodeStoppingDetail: {
    id: 'loading.mithrilPartialSync.progress.nodeStoppingDetail',
    defaultMessage:
      '!!!Daedalus is stopping the Cardano node before restoring verified Mithril chain data. This can take a couple of minutes.',
    description:
      'Detail copy shown while the Cardano node is being stopped before Mithril partial sync restores chain data.',
  },
  partialSyncCancellingTitle: {
    id: 'loading.mithrilPartialSync.progress.cancellingTitle',
    defaultMessage: '!!!Cleaning up...',
    description:
      'Title shown while Daedalus is cleaning up after a cancelled Mithril partial sync.',
  },
  partialSyncCancellingDetail: {
    id: 'loading.mithrilPartialSync.progress.cancellingDetail',
    defaultMessage:
      '!!!Daedalus is cleaning up Mithril Sync before you continue.',
    description:
      'Detail copy shown while Daedalus is cleaning up after a cancelled Mithril partial sync.',
  },
  partialSyncCompletedSubtitle: {
    id: 'loading.mithrilPartialSync.completed.subtitle',
    defaultMessage: '!!!Mithril Sync completed successfully.',
    description:
      'Supporting copy shown after Mithril partial sync reaches completed status.',
  },
  partialSyncCompletedTransition: {
    id: 'loading.mithrilPartialSync.completed.transition',
    defaultMessage: '!!!Returning to Daedalus...',
    description:
      'Spinner caption shown on the completed Mithril partial sync overlay while it auto-hands-off back to the normal Daedalus loading flow.',
  },
  partialSyncFailedTitle: {
    id: 'loading.mithrilPartialSync.error.failed.title',
    defaultMessage: '!!!Mithril Sync failed',
    description: 'Title for Mithril partial sync failed terminal state.',
  },
  partialSyncFailedHint: {
    id: 'loading.mithrilPartialSync.error.failed.hint',
    defaultMessage:
      '!!!Use one of the available recovery actions to retry Mithril Sync, restart normally, or wipe chain data and do a full Mithril Sync.',
    description: 'Hint shown for Mithril partial sync failed terminal state.',
  },
  partialSyncFinalizeFailedTitle: {
    id: 'loading.mithrilPartialSync.error.finalizeFailed.title',
    defaultMessage: '!!!Finishing Mithril Sync failed',
    description:
      'Title shown when the automatic finalize hand-off after a completed Mithril partial sync fails twice.',
  },
  partialSyncFinalizeFailedHint: {
    id: 'loading.mithrilPartialSync.error.finalizeFailed.hint',
    defaultMessage:
      '!!!Mithril Sync completed, but Daedalus could not finish the final cleanup step. Try again to continue to Daedalus.',
    description:
      'Hint shown when the automatic finalize hand-off after a completed Mithril partial sync fails twice.',
  },
  partialSyncFinalizeFailedRetry: {
    id: 'loading.mithrilPartialSync.error.finalizeFailed.retry',
    defaultMessage: '!!!Try again',
    description:
      'Retry action label on the finalize-failed error view; re-invokes the finalize hand-off.',
  },
  partialSyncCancelledTitle: {
    id: 'loading.mithrilPartialSync.error.cancelled.title',
    defaultMessage: '!!!Mithril Sync was cancelled',
    description: 'Title for Mithril partial sync cancelled terminal state.',
  },
  partialSyncCancelledHint: {
    id: 'loading.mithrilPartialSync.error.cancelled.hint',
    defaultMessage:
      '!!!Mithril Sync was stopped before it finished. Your existing chain data is unchanged — choose how to continue below.',
    description:
      'Calmer hint shown when the user cancelled Mithril partial sync (distinct from the failed hint).',
  },
  partialSyncErrorLatestDriftTitle: {
    id: 'loading.mithrilPartialSync.error.latestDrift.title',
    defaultMessage: '!!!The verified Mithril snapshot moved on',
    description:
      'Title shown when PARTIAL_SYNC_LATEST_DRIFT is emitted (a newer verified snapshot appeared while preparing).',
  },
  partialSyncErrorLatestDriftHint: {
    id: 'loading.mithrilPartialSync.error.latestDrift.hint',
    defaultMessage:
      '!!!A newer verified snapshot became available while Daedalus was preparing. Retry Mithril Sync to use the refreshed snapshot — your chain data was not changed.',
    description:
      'Retriable hint shown for PARTIAL_SYNC_LATEST_DRIFT (pre-cutover; not a silent reset).',
  },
  partialSyncErrorStagedDbInvalidTitle: {
    id: 'loading.mithrilPartialSync.error.stagedDbInvalid.title',
    defaultMessage: '!!!The Mithril snapshot could not be verified',
    description:
      'Title shown when PARTIAL_SYNC_STAGED_DB_INVALID is emitted (staged output incomplete or invalid).',
  },
  partialSyncErrorStagedDbInvalidHint: {
    id: 'loading.mithrilPartialSync.error.stagedDbInvalid.hint',
    defaultMessage:
      '!!!The downloaded snapshot was incomplete or did not match the expected verified chain data. Choose how to continue below.',
    description: 'Hint shown for PARTIAL_SYNC_STAGED_DB_INVALID.',
  },
  partialSyncErrorDownloadFailedTitle: {
    id: 'loading.mithrilPartialSync.error.downloadFailed.title',
    defaultMessage: '!!!Downloading the Mithril snapshot failed',
    description:
      'Title shown when PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED is emitted.',
  },
  partialSyncErrorDownloadFailedHint: {
    id: 'loading.mithrilPartialSync.error.downloadFailed.hint',
    defaultMessage:
      '!!!Daedalus could not finish downloading and verifying the Mithril snapshot. Check your internet connection, then choose how to continue below.',
    description: 'Hint shown for PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED.',
  },
  partialSyncErrorConversionFailedTitle: {
    id: 'loading.mithrilPartialSync.error.conversionFailed.title',
    defaultMessage: '!!!Preparing the Mithril snapshot failed',
    description: 'Title shown when PARTIAL_SYNC_CONVERSION_FAILED is emitted.',
  },
  partialSyncErrorConversionFailedHint: {
    id: 'loading.mithrilPartialSync.error.conversionFailed.hint',
    defaultMessage:
      '!!!Daedalus downloaded the verified snapshot but could not prepare it for use. Choose how to continue below.',
    description: 'Hint shown for PARTIAL_SYNC_CONVERSION_FAILED.',
  },
  partialSyncErrorInsufficientDiskSpaceTitle: {
    id: 'loading.mithrilPartialSync.error.insufficientDiskSpace.title',
    defaultMessage: '!!!Not enough disk space for Mithril Sync',
    description:
      'Title shown when PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE is emitted (disk-space preflight failed).',
  },
  partialSyncErrorInsufficientDiskSpaceHint: {
    id: 'loading.mithrilPartialSync.error.insufficientDiskSpace.hint',
    defaultMessage:
      '!!!Daedalus does not have enough free disk space to prepare the verified snapshot. The required and available amounts are shown in the error details. Free up space on the disk that stores your chain data, then retry Mithril Sync.',
    description:
      'Hint shown for PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE. The needed/available GB figures come from the backend error message rendered by MithrilErrorView.',
  },
  partialSyncErrorMetadataUnavailableTitle: {
    id: 'loading.mithrilPartialSync.error.metadataUnavailable.title',
    defaultMessage: '!!!Checking the latest Mithril snapshot failed',
    description:
      'Title shown when Daedalus cannot resolve the latest verified Mithril snapshot metadata before or during Mithril Sync.',
  },
  partialSyncErrorMetadataUnavailableHint: {
    id: 'loading.mithrilPartialSync.error.metadataUnavailable.hint',
    defaultMessage:
      '!!!Daedalus could not read the latest verified snapshot details. Check your internet connection, then choose how to continue below.',
    description:
      'Hint shown when Daedalus cannot resolve the latest verified Mithril snapshot metadata before or during Mithril Sync.',
  },
  partialSyncRetry: {
    id: 'loading.mithrilPartialSync.error.retry',
    defaultMessage: '!!!Retry Mithril Sync (fast)',
    description: 'Recovery action label for retrying Mithril partial sync.',
  },
  partialSyncRestartNormally: {
    id: 'loading.mithrilPartialSync.error.restartNormally',
    defaultMessage: '!!!Restart Blockchain Sync (slow)',
    description:
      'Recovery action label for restarting Daedalus normally after partial sync failure.',
  },
  partialSyncWipeAndFullSync: {
    id: 'loading.mithrilPartialSync.error.wipeAndFullSync',
    defaultMessage: '!!!Wipe chain data and do full Mithril Sync',
    description:
      'Recovery action label for wiping chain data and doing a full Mithril Sync.',
  },
  partialSyncQuit: {
    id: 'loading.mithrilPartialSync.error.quit',
    defaultMessage: '!!!Quit Daedalus',
    description:
      'Defensive Quit fallback button shown on a partial-sync failure only when no recovery actions are available (so the overlay is never an unclosable dead-end).',
  },
  partialSyncCancelStoppingTooltip: {
    id: 'loading.mithrilPartialSync.progress.cancelStoppingTooltip',
    defaultMessage: '!!!Cancellation available once the node has stopped',
    description:
      'Tooltip explaining why the Cancel button is disabled while the Cardano node is still stopping (stopping-node phase).',
  },
  partialSyncStageVerifying: {
    id: 'loading.mithrilPartialSync.progress.stageVerifying',
    defaultMessage: '!!!Verifying snapshot...',
    description:
      'Step-indicator label for the verifying stage emitted during Mithril Sync.',
  },
  partialSyncStageConverting: {
    id: 'loading.mithrilPartialSync.progress.stageConverting',
    defaultMessage: '!!!Converting snapshot format...',
    description:
      'Step-indicator label for the converting stage emitted during Mithril Sync.',
  },
  partialSyncStageInstalling: {
    id: 'loading.mithrilPartialSync.progress.stageInstalling',
    defaultMessage: '!!!Installing snapshot...',
    description:
      'Step-indicator label for the installing stage emitted during Mithril Sync.',
  },
  partialSyncStartFailure: {
    id: 'loading.mithrilPartialSync.error.startFailure',
    defaultMessage: '!!!Unable to start Mithril Sync.',
    description:
      'Fallback message shown when a Mithril Sync start request is rejected.',
  },
});

export default messages;
