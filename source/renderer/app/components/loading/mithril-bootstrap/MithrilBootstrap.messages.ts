import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

const messages: Record<string, ReactIntlMessage> = defineMessages({
  title: {
    id: 'loading.mithrilBootstrap.title',
    defaultMessage: '!!!Fast sync with Mithril',
    description: 'Headline for the Mithril bootstrap prompt.',
  },
  description: {
    id: 'loading.mithrilBootstrap.description',
    defaultMessage:
      '!!!Mithril can download a verified snapshot to sync your blockchain data faster. Choose a snapshot and continue, or sync from genesis.',
    description: 'Description for the Mithril bootstrap prompt.',
  },
  storageTitle: {
    id: 'loading.mithrilBootstrap.storage.title',
    defaultMessage: '!!!Select blockchain data location',
    description: 'Headline for the preliminary chain storage picker screen.',
  },
  storageDescriptionWithSnapshotEstimate: {
    id: 'loading.mithrilBootstrap.storage.descriptionWithSnapshotEstimate',
    defaultMessage:
      '!!!Choose where Daedalus should keep blockchain data before you continue. The latest available snapshot is about {requiredSpace}, so make sure this location has plenty of free space.',
    description:
      'Description shown on the preliminary chain storage picker screen when the latest snapshot size is available.',
  },
  storageDescriptionLargeRequirement: {
    id: 'loading.mithrilBootstrap.storage.descriptionLargeRequirement',
    defaultMessage:
      '!!!Choose where Daedalus should keep blockchain data before you continue. Blockchain data can require a large amount of free space, so choose a location with plenty of capacity.',
    description:
      'Description shown on the preliminary chain storage picker screen when an estimated snapshot size is not available yet.',
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
  storageDirectoryLabel: {
    id: 'loading.mithrilBootstrap.storage.directoryLabel',
    defaultMessage: '!!!Blockchain data location',
    description: 'Label for the selected chain storage directory.',
  },
  storageChooseDirectory: {
    id: 'loading.mithrilBootstrap.storage.chooseDirectory',
    defaultMessage: '!!!Choose directory',
    description: 'Button label to open the chain storage directory picker.',
  },
  storageResetToDefault: {
    id: 'loading.mithrilBootstrap.storage.resetToDefault',
    defaultMessage: '!!!Reset to default',
    description:
      'Button label to reset chain storage back to the default path.',
  },
  storageContinue: {
    id: 'loading.mithrilBootstrap.storage.continue',
    defaultMessage: '!!!Continue',
    description: 'Button label to continue after confirming chain storage.',
  },
  storageChangeLocation: {
    id: 'loading.mithrilBootstrap.storage.changeLocation',
    defaultMessage: '!!!Change location',
    description:
      'Button label to return from the snapshot decision screen to the blockchain data location picker.',
  },
  storageUpdating: {
    id: 'loading.mithrilBootstrap.storage.updating',
    defaultMessage: '!!!Updating blockchain data location...',
    description:
      'Status message shown while Daedalus is applying a storage location change.',
  },
  storageAvailableSpaceSubtext: {
    id: 'loading.mithrilBootstrap.storage.availableSpaceSubtext',
    defaultMessage: '!!!Available disk space: {availableSpace}',
    description: 'Subtext shown below the storage location input.',
  },
  storageAvailableSpaceUnknown: {
    id: 'loading.mithrilBootstrap.storage.availableSpaceUnknown',
    defaultMessage: '!!!Unavailable',
    description:
      'Fallback value when available disk space is unavailable in the storage picker.',
  },
  storageDefaultLocationLabel: {
    id: 'loading.mithrilBootstrap.storage.defaultLocationLabel',
    defaultMessage: '!!!Default location',
    description: 'Fallback label for the default chain storage location.',
  },
  storageValidationPathNotFound: {
    id: 'loading.mithrilBootstrap.storage.validation.pathNotFound',
    defaultMessage: '!!!The selected directory does not exist.',
    description:
      'Validation message when the selected chain storage directory cannot be found.',
  },
  storageValidationNotWritable: {
    id: 'loading.mithrilBootstrap.storage.validation.notWritable',
    defaultMessage: '!!!The selected path must be a writable directory.',
    description:
      'Validation message when the selected chain storage path cannot be written to.',
  },
  storageValidationInsideStateDir: {
    id: 'loading.mithrilBootstrap.storage.validation.insideStateDir',
    defaultMessage:
      '!!!Choose a directory outside the Daedalus state directory.',
    description:
      'Validation message when the selected chain storage path is inside the state directory.',
  },
  storageValidationInsufficientSpace: {
    id: 'loading.mithrilBootstrap.storage.validation.insufficientSpace',
    defaultMessage:
      '!!!The selected directory does not have enough free space for blockchain data.',
    description:
      'Validation message when the selected chain storage path does not have enough free space.',
  },
  storageValidationUnknown: {
    id: 'loading.mithrilBootstrap.storage.validation.unknown',
    defaultMessage: '!!!Daedalus could not validate the selected directory.',
    description:
      'Fallback validation message when chain storage validation fails for an unknown reason.',
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
  progressSnapshotFilesLabel: {
    id: 'loading.mithrilBootstrap.progress.snapshotFilesLabel',
    defaultMessage: '!!!Snapshot files',
    description:
      'Label for snapshot files progress bar during main snapshot download.',
  },
  progressFastSyncLabel: {
    id: 'loading.mithrilBootstrap.progress.fastSyncLabel',
    defaultMessage: '!!!Fast State Sync',
    description:
      'Label for fast sync data progress bar during concurrent fast sync downloads.',
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
