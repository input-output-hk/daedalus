import { defineMessages } from 'react-intl';

export default defineMessages({
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
  storageDescription: {
    id: 'loading.mithrilBootstrap.storage.description',
    defaultMessage:
      '!!!Choose where Daedalus should keep blockchain data before you continue.',
    description:
      'Description shown on the preliminary chain storage picker screen.',
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
  progressLabel: {
    id: 'loading.mithrilBootstrap.progressLabel',
    defaultMessage: '!!!Bootstrap progress',
    description: 'Label for the Mithril progress bar.',
  },
  progressStatusLabel: {
    id: 'loading.mithrilBootstrap.progress.statusLabel',
    defaultMessage: '!!!Current status',
    description: 'Label for the current Mithril progress status summary.',
  },
  progressPercentLabel: {
    id: 'loading.mithrilBootstrap.progress.percentLabel',
    defaultMessage: '!!!Progress',
    description: 'Label for Mithril overall progress metadata.',
  },
  progressActivityLabel: {
    id: 'loading.mithrilBootstrap.progress.activityLabel',
    defaultMessage: '!!!Current activity',
    description: 'Label for the active Mithril operation metadata row.',
  },
  progressElapsedLabel: {
    id: 'loading.mithrilBootstrap.progress.elapsedLabel',
    defaultMessage: '!!!Elapsed',
    description: 'Label for Mithril elapsed time metadata.',
  },
  progressTimeRemainingLabel: {
    id: 'loading.mithrilBootstrap.progress.timeRemainingLabel',
    defaultMessage: '!!!Time remaining',
    description: 'Label for Mithril remaining time metadata.',
  },
  progressPreparingTitle: {
    id: 'loading.mithrilBootstrap.progress.preparingTitle',
    defaultMessage: '!!!Preparing Mithril fast sync',
    description: 'Title shown while Mithril bootstrap is preparing to start.',
  },
  progressPreparingDetail: {
    id: 'loading.mithrilBootstrap.progress.preparingDetail',
    defaultMessage:
      '!!!Checking the selected snapshot and preparing your blockchain data location before the download starts.',
    description: 'Detail copy shown while Mithril bootstrap is preparing.',
  },
  progressDownloadingTitle: {
    id: 'loading.mithrilBootstrap.progress.downloadingTitle',
    defaultMessage: '!!!Downloading the selected snapshot',
    description: 'Title shown while Mithril bootstrap is actively downloading.',
  },
  progressDownloadingDetail: {
    id: 'loading.mithrilBootstrap.progress.downloadingDetail',
    defaultMessage:
      '!!!Fetching a verified Mithril snapshot now. Transfer details update below as data arrives.',
    description: 'Detail copy shown while the Mithril snapshot is downloading.',
  },
  progressDownloadVerifyingTitle: {
    id: 'loading.mithrilBootstrap.progress.downloadVerifyingTitle',
    defaultMessage: '!!!Verifying the downloaded snapshot',
    description:
      'Title shown when Mithril is verifying the downloaded snapshot before local restore begins.',
  },
  progressDownloadVerifyingDetail: {
    id: 'loading.mithrilBootstrap.progress.downloadVerifyingDetail',
    defaultMessage:
      '!!!Checking the downloaded snapshot before Daedalus restores it into your blockchain data location.',
    description:
      'Detail copy shown when Mithril is still in download status while verification completes before local restore.',
  },
  progressUnpackingTitle: {
    id: 'loading.mithrilBootstrap.progress.unpackingTitle',
    defaultMessage: '!!!Unpacking snapshot data',
    description:
      'Title shown while Mithril is unpacking the restored snapshot.',
  },
  progressConvertingTitle: {
    id: 'loading.mithrilBootstrap.progress.convertingTitle',
    defaultMessage: '!!!Preparing snapshot for Daedalus',
    description:
      'Title shown while Mithril is converting the restored snapshot.',
  },
  progressConvertingDetail: {
    id: 'loading.mithrilBootstrap.progress.convertingDetail',
    defaultMessage:
      '!!!Converting the restored data into the format Daedalus needs before the node starts again.',
    description:
      'Detail copy shown while Mithril is converting the restored snapshot.',
  },
  progressFinalizingTitle: {
    id: 'loading.mithrilBootstrap.progress.finalizingTitle',
    defaultMessage: '!!!Finalizing Mithril fast sync',
    description: 'Title shown while Mithril is performing final cleanup.',
  },
  progressCompletedTitle: {
    id: 'loading.mithrilBootstrap.progress.completedTitle',
    defaultMessage: '!!!Starting Daedalus',
    description:
      'Title shown after Mithril restore completes while the node is starting.',
  },
  progressCompletedDetail: {
    id: 'loading.mithrilBootstrap.progress.completedDetail',
    defaultMessage:
      '!!!The snapshot has been restored. Starting Daedalus with your blockchain data now.',
    description:
      'Detail copy shown after Mithril restore completes while the node is starting.',
  },
  progressTiming: {
    id: 'loading.mithrilBootstrap.progressTiming',
    defaultMessage: '!!!Elapsed {elapsed} • Remaining {remaining}',
    description: 'Label for the Mithril elapsed and remaining time row.',
  },
  progressElapsed: {
    id: 'loading.mithrilBootstrap.progressElapsed',
    defaultMessage: '!!!Elapsed {elapsed}',
    description: 'Label for the Mithril elapsed time row.',
  },
  progressRemaining: {
    id: 'loading.mithrilBootstrap.progressRemaining',
    defaultMessage: '!!!Remaining {remaining}',
    description: 'Label for the Mithril remaining time row.',
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
  stepUnpacking: {
    id: 'loading.mithrilBootstrap.step.unpacking',
    defaultMessage: '!!!Unpacking',
    description:
      'Label for the unpacking sub-phase in the Mithril progress flow.',
  },
  stepVerifying: {
    id: 'loading.mithrilBootstrap.step.verifying',
    defaultMessage: '!!!Verifying',
    description: 'Label for the verifying step in the Mithril step indicator.',
  },
  stepFinalizing: {
    id: 'loading.mithrilBootstrap.step.finalizing',
    defaultMessage: '!!!Finalizing',
    description: 'Label for the finalizing step in the Mithril step indicator.',
  },
  downloadBytesLabel: {
    id: 'loading.mithrilBootstrap.download.bytesLabel',
    defaultMessage: '!!!Downloaded',
    description: 'Label for bytes transferred during Mithril download.',
  },
  downloadRateLabel: {
    id: 'loading.mithrilBootstrap.download.rateLabel',
    defaultMessage: '!!!Transfer rate',
    description: 'Label for transfer speed during Mithril download.',
  },
  progressStageLabel: {
    id: 'loading.mithrilBootstrap.progress.stageLabel',
    defaultMessage: '!!!Current step',
    description: 'Label for non-download progress stage details.',
  },
  progressTimingLabel: {
    id: 'loading.mithrilBootstrap.progress.timingLabel',
    defaultMessage: '!!!Timing',
    description: 'Label for Mithril timing metadata.',
  },
  progressUnknownDurationValue: {
    id: 'loading.mithrilBootstrap.progress.unknownDurationValue',
    defaultMessage: '!!!Not available yet',
    description:
      'Fallback metadata value while duration estimates are unavailable.',
  },
  progressWaitingValue: {
    id: 'loading.mithrilBootstrap.progress.waitingValue',
    defaultMessage: '!!!Waiting for updates',
    description:
      'Fallback metadata value while Mithril has not emitted data yet.',
  },
  progressRatePendingValue: {
    id: 'loading.mithrilBootstrap.progress.ratePendingValue',
    defaultMessage: '!!!Available during download',
    description:
      'Fallback transfer-rate value shown before download throughput is known.',
  },
  progressTimingPendingValue: {
    id: 'loading.mithrilBootstrap.progress.timingPendingValue',
    defaultMessage: '!!!Estimate available after transfer starts',
    description:
      'Fallback timing value shown before timing estimates are available.',
  },
  progressLocalProcessingValue: {
    id: 'loading.mithrilBootstrap.progress.localProcessingValue',
    defaultMessage: '!!!Local processing',
    description:
      'Metadata value shown for fields that do not apply during local post-download work.',
  },
  progressFinalizingRemainingValue: {
    id: 'loading.mithrilBootstrap.progress.finalizingRemainingValue',
    defaultMessage:
      '!!!Completing the final local restore. Exact time depends on disk speed.',
    description:
      'Fallback time-remaining value shown during local post-download processing.',
  },
  progressPreparingActivityValue: {
    id: 'loading.mithrilBootstrap.progress.preparingActivityValue',
    defaultMessage: '!!!Checking snapshot details and storage location',
    description: 'Short activity label shown while Mithril is preparing.',
  },
  progressDownloadingActivityValue: {
    id: 'loading.mithrilBootstrap.progress.downloadingActivityValue',
    defaultMessage: '!!!Downloading snapshot data',
    description: 'Short activity label shown while Mithril is downloading.',
  },
  progressDownloadVerifyingActivityValue: {
    id: 'loading.mithrilBootstrap.progress.downloadVerifyingActivityValue',
    defaultMessage: '!!!Verifying the downloaded snapshot',
    description:
      'Short activity label shown while Mithril verifies the downloaded snapshot before local restore.',
  },
  progressUnpackingActivityValue: {
    id: 'loading.mithrilBootstrap.progress.unpackingActivityValue',
    defaultMessage: '!!!Moving snapshot data into blockchain storage',
    description:
      'Short activity label shown while Mithril is unpacking locally.',
  },
  progressConvertingActivityValue: {
    id: 'loading.mithrilBootstrap.progress.convertingActivityValue',
    defaultMessage: '!!!Preparing restored data for Daedalus',
    description:
      'Short activity label shown while Mithril is converting locally.',
  },
  progressFinalizingActivityValue: {
    id: 'loading.mithrilBootstrap.progress.finalizingActivityValue',
    defaultMessage: '!!!Cleaning up bootstrap data and handing off to the node',
    description:
      'Short activity label shown while Mithril is in final cleanup.',
  },
  progressPercentValue: {
    id: 'loading.mithrilBootstrap.progress.percentValue',
    defaultMessage: '!!!{progress}% complete',
    description:
      'Formatted overall Mithril progress percentage metadata value.',
  },
  progressSnapshotSizeValue: {
    id: 'loading.mithrilBootstrap.progress.snapshotSizeValue',
    defaultMessage: '!!!Snapshot size {size}',
    description:
      'Fallback value for the downloaded amount field when only total snapshot size is known.',
  },
  progressUnpackingDetail: {
    id: 'loading.mithrilBootstrap.progress.unpackingDetail',
    defaultMessage:
      '!!!Moving the restored snapshot into your blockchain data location and preparing it for use.',
    description:
      'Detail copy shown while the restored Mithril snapshot is being unpacked locally.',
  },
  progressFinalizingDetail: {
    id: 'loading.mithrilBootstrap.progress.finalizingDetail',
    defaultMessage:
      '!!!Cleaning up bootstrap files and preparing to start the node with the restored blockchain data.',
    description:
      'Detail copy shown while Mithril bootstrap performs final local cleanup.',
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
  progressFastSyncDownloaded: {
    id: 'loading.mithrilBootstrap.progress.fastSyncDownloaded',
    defaultMessage: '!!!Fast State Sync downloaded',
    description:
      'Label for the amount of fast sync data downloaded metadata value.',
  },
  nodeStartingTitle: {
    id: 'loading.mithrilBootstrap.progress.nodeStartingTitle',
    defaultMessage: '!!!Starting cardano-node',
    description:
      'Title shown during the 3-second completion delay after Mithril snapshot restore, while cardano-node is starting to complete remaining sync.',
  },
  nodeStartingDetail: {
    id: 'loading.mithrilBootstrap.progress.nodeStartingDetail',
    defaultMessage:
      '!!!The Mithril snapshot has been restored. Cardano-node is starting up to complete the remaining sync.',
    description:
      'Detail copy shown during the 3-second completion delay while cardano-node is starting after Mithril restore.',
  },
});
