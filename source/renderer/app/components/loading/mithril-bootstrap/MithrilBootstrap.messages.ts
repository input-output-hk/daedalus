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
      '!!!Mithril can download a verified snapshot to sync your wallet faster. Choose a snapshot and continue, or sync from genesis.',
    description: 'Description for the Mithril bootstrap prompt.',
  },
  storageTitle: {
    id: 'loading.mithrilBootstrap.storage.title',
    defaultMessage: '!!!Choose blockchain data location',
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
  stepInstalling: {
    id: 'loading.mithrilBootstrap.step.installing',
    defaultMessage: '!!!Installing',
    description: 'Label for the installing step in the Mithril step indicator.',
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
  progressInstallingDetail: {
    id: 'loading.mithrilBootstrap.progress.installingDetail',
    defaultMessage:
      '!!!Download complete. Installing the restored snapshot into chain storage.',
    description:
      'Detail copy shown while the restored Mithril snapshot is being installed locally.',
  },
  progressFinalizingDetail: {
    id: 'loading.mithrilBootstrap.progress.finalizingDetail',
    defaultMessage:
      '!!!Wrapping up Mithril bootstrap before the node starts again.',
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
    defaultMessage: '!!!Back to directory location',
    description:
      'Button label to return from the snapshot decision screen to the blockchain data location picker.',
  },
  storageUpdating: {
    id: 'loading.mithrilBootstrap.storage.updating',
    defaultMessage: '!!!Updating blockchain data location...',
    description:
      'Status message shown while Daedalus is applying a storage location change.',
  },
  storageAvailableSpaceLabel: {
    id: 'loading.mithrilBootstrap.storage.availableSpaceLabel',
    defaultMessage: '!!!Available space',
    description: 'Label for available disk space in the storage picker.',
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
});
