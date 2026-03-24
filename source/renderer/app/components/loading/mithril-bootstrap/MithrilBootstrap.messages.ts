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
      '!!!Mithril can download a verified snapshot to sync your blockchain data faster. Choose a snapshot and continue, or sync from genesis.',
    description: 'Description for the Mithril bootstrap prompt.',
  },
  accept: {
    id: 'loading.mithrilBootstrap.accept',
    defaultMessage: '!!!Use Mithril fast sync',
    description: 'Button label to accept Mithril bootstrap.',
  },
  decline: {
    id: 'loading.mithrilBootstrap.decline',
    defaultMessage: '!!!Sync from genesis',
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
    defaultMessage: '!!!Elapsed',
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
      '!!!The node could not start with the restored chain data. Wipe the chain and try Mithril again, or sync from genesis.',
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
    defaultMessage: '!!!Mithril sync progress',
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
      '!!!Daedalus could not verify the snapshot integrity. Try another snapshot or sync from genesis.',
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
      '!!!Daedalus could not prepare the downloaded snapshot for use. Try again or sync from genesis.',
    description: 'Hint shown for Mithril conversion-stage failures.',
  },
  errorNodeStartTitle: {
    id: 'loading.mithrilBootstrap.error.nodeStart.title',
    defaultMessage: '!!!Node failed to start',
    description: 'Title for node-start failures after Mithril bootstrap.',
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
    defaultMessage: '!!!Snapshot Files and Fast Sync',
    description:
      'Label for the combined Mithril download progress bar that merges snapshot files and fast sync progress.',
  },
  progressCombinedDetail: {
    id: 'loading.mithrilBootstrap.progress.combinedDetail',
    defaultMessage:
      '!!!Snapshot files: {snapshotDownloaded} / {snapshotTotal} | Fast sync: {fastSyncDownloaded} / {fastSyncTotal}',
    description:
      'Detail line for the combined Mithril download progress bar showing snapshot and fast sync transfer totals.',
  },
  nodeStartingTitle: {
    id: 'loading.mithrilBootstrap.progress.nodeStartingTitle',
    defaultMessage: '!!!Starting cardano-node',
    description:
      'Title shown during the completion delay after Mithril snapshot restore, while cardano-node is starting to complete remaining sync.',
  },
  nodeStartingDetail: {
    id: 'loading.mithrilBootstrap.progress.nodeStartingDetail',
    defaultMessage:
      '!!!The Mithril snapshot has been restored. Cardano-node is starting up to complete the remaining sync.',
    description:
      'Detail copy shown during the completion delay while cardano-node is starting after Mithril restore.',
  },
});

export default messages;
