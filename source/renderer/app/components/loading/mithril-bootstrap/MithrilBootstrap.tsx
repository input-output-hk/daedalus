import React from 'react';
import { observer } from 'mobx-react';
import type {
  MithrilBootstrapStatus,
  MithrilSnapshotItem,
  MithrilBootstrapError,
  ChainStorageValidation,
  MithrilProgressItem,
} from '../../../../../common/types/mithril-bootstrap.types';
import MithrilDecisionView from './MithrilDecisionView';
import MithrilErrorView from './MithrilErrorView';
import MithrilProgressView from './MithrilProgressView';
import BlockDataStorageLocationPicker from './BlockDataStorageLocationPicker';
import styles from './MithrilBootstrap.scss';

interface Props {
  status: MithrilBootstrapStatus;
  progress: number;
  filesDownloaded?: number;
  filesTotal?: number;
  bytesDownloaded?: number;
  snapshotSize?: number;
  throughputBps?: number;
  remainingSeconds?: number;
  customChainPath?: string | null;
  defaultChainPath?: string | null;
  defaultChainStorageValidation?: ChainStorageValidation;
  chainStorageValidation?: ChainStorageValidation;
  latestSnapshotSize?: number;
  isChainStorageLoading?: boolean;
  storageLocationConfirmed?: boolean;
  snapshots: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  selectedSnapshot?: MithrilSnapshotItem | null;
  error?: MithrilBootstrapError | null;
  isFetchingSnapshots: boolean;
  onOpenExternalLink?: (...args: [string]) => void;
  onSetChainStorageDirectory?: (...args: [string | null]) => Promise<unknown>;
  onResetChainStorageDirectory?(): Promise<unknown>;
  onValidateChainStorageDirectory?: (
    ...args: [string]
  ) => Promise<ChainStorageValidation>;
  onConfirmStorageLocation?(): void;
  onReturnToStorageLocation?(): void;
  onLoadChainStorageConfig?(): Promise<void>;
  onSelectSnapshot: (...args: [string | null]) => void;
  onAccept(): void;
  onDecline(): void;
  onWipeRetry(): void;
  onCancel(): void;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryRemainingSeconds?: number;
  ancillaryProgress?: number;
  progressItems?: MithrilProgressItem[];
  overallElapsedSeconds?: number;
}

const DECISION_STATUSES: Array<MithrilBootstrapStatus> = [
  'decision',
  'idle',
  'cancelled',
];

const WORKING_STATUSES: Array<MithrilBootstrapStatus> = [
  'preparing',
  'downloading',
  'unpacking',
  'finalizing',
  'converting',
  'completed',
];

function MithrilBootstrap(props: Props) {
  const {
    status,
    progress,
    progressItems,
    filesDownloaded,
    filesTotal,
    bytesDownloaded,
    snapshotSize,
    throughputBps,
    remainingSeconds,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
    ancillaryRemainingSeconds,
    overallElapsedSeconds,
    customChainPath,
    defaultChainPath,
    defaultChainStorageValidation,
    chainStorageValidation,
    latestSnapshotSize,
    isChainStorageLoading,
    storageLocationConfirmed,
    snapshots,
    selectedDigest,
    selectedSnapshot,
    error,
    isFetchingSnapshots,
    onOpenExternalLink,
    onSetChainStorageDirectory,
    onResetChainStorageDirectory,
    onValidateChainStorageDirectory,
    onConfirmStorageLocation,
    onReturnToStorageLocation,
    onSelectSnapshot,
    onAccept,
    onDecline,
    onWipeRetry,
    onCancel,
  } = props;

  let content = null;

  if (DECISION_STATUSES.includes(status) && !storageLocationConfirmed) {
    content = (
      <BlockDataStorageLocationPicker
        customChainPath={customChainPath}
        defaultChainPath={defaultChainPath}
        defaultChainStorageValidation={defaultChainStorageValidation}
        chainStorageValidation={chainStorageValidation}
        latestSnapshotSize={latestSnapshotSize}
        isChainStorageLoading={isChainStorageLoading}
        onSetChainStorageDirectory={onSetChainStorageDirectory}
        onResetChainStorageDirectory={onResetChainStorageDirectory}
        onValidateChainStorageDirectory={onValidateChainStorageDirectory}
        onConfirmStorageLocation={onConfirmStorageLocation}
      />
    );
  } else if (DECISION_STATUSES.includes(status)) {
    content = (
      <MithrilDecisionView
        snapshots={snapshots}
        selectedDigest={selectedDigest}
        selectedSnapshot={selectedSnapshot}
        isFetchingSnapshots={isFetchingSnapshots}
        customChainPath={customChainPath}
        defaultChainPath={defaultChainPath}
        onSelectSnapshot={onSelectSnapshot}
        onReturnToStorageLocation={onReturnToStorageLocation}
        onAccept={onAccept}
        onDecline={onDecline}
      />
    );
  } else if (WORKING_STATUSES.includes(status)) {
    content = (
      <MithrilProgressView
        status={status}
        progress={progress}
        progressItems={progressItems}
        filesDownloaded={filesDownloaded}
        filesTotal={filesTotal}
        bytesDownloaded={bytesDownloaded}
        snapshotSize={snapshotSize}
        throughputBps={throughputBps}
        remainingSeconds={remainingSeconds}
        ancillaryBytesDownloaded={ancillaryBytesDownloaded}
        ancillaryBytesTotal={ancillaryBytesTotal}
        ancillaryProgress={ancillaryProgress}
        ancillaryRemainingSeconds={ancillaryRemainingSeconds}
        overallElapsedSeconds={overallElapsedSeconds}
        onCancel={onCancel}
      />
    );
  } else if (status === 'failed') {
    content = (
      <MithrilErrorView
        error={error}
        onOpenExternalLink={onOpenExternalLink}
        onWipeRetry={onWipeRetry}
        onDecline={onDecline}
      />
    );
  }

  return (
    <div className={styles.component}>
      <div className={styles.backdrop} />
      <div className={styles.content}>
        {content && <div className={styles.card}>{content}</div>}
      </div>
    </div>
  );
}

export default observer(MithrilBootstrap);
