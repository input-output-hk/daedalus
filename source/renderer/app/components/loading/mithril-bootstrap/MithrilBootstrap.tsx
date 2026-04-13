import React from 'react';
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
import ChainStorageLocationPicker from '../../chain-storage/ChainStorageLocationPicker';
import {
  MITHRIL_CHAIN_STORAGE_HEADING_ID,
  MITHRIL_DECISION_HEADING_ID,
  MITHRIL_ERROR_HEADING_ID,
  MITHRIL_PROGRESS_HEADING_ID,
} from './accessibilityIds';
import styles from './MithrilBootstrap.scss';

interface Props {
  status: MithrilBootstrapStatus;
  bytesDownloaded?: number;
  snapshotSize?: number;
  customChainPath?: string | null;
  defaultChainPath?: string | null;
  defaultChainStorageValidation?: ChainStorageValidation;
  chainStorageValidation?: ChainStorageValidation;
  pendingChainPath?: string | null;
  isRecoveryFallback?: boolean;
  latestSnapshotSize?: number;
  isChainStorageLoading?: boolean;
  isApplyingStorageLocation?: boolean;
  storageLocationConfirmed?: boolean;
  snapshots: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  selectedSnapshot?: MithrilSnapshotItem | null;
  error?: MithrilBootstrapError | null;
  isFetchingSnapshots: boolean;
  onOpenExternalLink?: (arg: string) => void;
  onSetChainStorageDirectory?: (arg: string | null) => Promise<unknown>;
  onResetChainStorageDirectory?(): Promise<unknown>;
  onValidateChainStorageDirectory?: (
    arg: string
  ) => Promise<ChainStorageValidation>;
  onConfirmStorageLocation?(): void;
  onReturnToStorageLocation?(): void;
  onSelectSnapshot: (arg: string | null) => void;
  onAccept(): void;
  onDecline(): void;
  onWipeRetry(): void;
  onCancel(): void;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryProgress?: number;
  progressItems?: MithrilProgressItem[];
  bootstrapStartedAt?: number | null;
}

const DECISION_STATUSES: Array<MithrilBootstrapStatus> = [
  'decision',
  'idle',
  'cancelled',
];

const WORKING_STATUSES: Array<MithrilBootstrapStatus> = [
  'preparing',
  'downloading',
  'verifying',
  'unpacking',
  'finalizing',
  'converting',
  'completed',
  'starting-node',
];

const getActiveHeadingId = (
  status: MithrilBootstrapStatus,
  storageLocationConfirmed?: boolean,
  isApplyingStorageLocation?: boolean
) => {
  if (
    DECISION_STATUSES.includes(status) &&
    !storageLocationConfirmed &&
    !isApplyingStorageLocation
  ) {
    return MITHRIL_CHAIN_STORAGE_HEADING_ID;
  }

  if (DECISION_STATUSES.includes(status)) {
    return MITHRIL_DECISION_HEADING_ID;
  }

  if (WORKING_STATUSES.includes(status)) {
    return MITHRIL_PROGRESS_HEADING_ID;
  }

  if (status === 'failed') {
    return MITHRIL_ERROR_HEADING_ID;
  }

  return undefined;
};

function MithrilBootstrap(props: Props) {
  const {
    status,
    progressItems,
    bytesDownloaded,
    snapshotSize,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
    bootstrapStartedAt,
    customChainPath,
    defaultChainPath,
    defaultChainStorageValidation,
    chainStorageValidation,
    pendingChainPath,
    isRecoveryFallback,
    latestSnapshotSize,
    isChainStorageLoading,
    isApplyingStorageLocation,
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
  const activeHeadingId = getActiveHeadingId(
    status,
    storageLocationConfirmed,
    isApplyingStorageLocation
  );

  if (
    DECISION_STATUSES.includes(status) &&
    !storageLocationConfirmed &&
    !isApplyingStorageLocation
  ) {
    content = (
      <ChainStorageLocationPicker
        customChainPath={customChainPath}
        defaultChainPath={defaultChainPath}
        defaultChainStorageValidation={defaultChainStorageValidation}
        chainStorageValidation={chainStorageValidation}
        pendingChainPath={pendingChainPath}
        isRecoveryFallback={isRecoveryFallback}
        estimatedRequiredSpaceBytes={latestSnapshotSize}
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
        isStorageLocationApplying={isApplyingStorageLocation}
        pendingChainPath={pendingChainPath}
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
        progressItems={progressItems}
        bytesDownloaded={bytesDownloaded}
        snapshotSize={snapshotSize}
        ancillaryBytesDownloaded={ancillaryBytesDownloaded}
        ancillaryBytesTotal={ancillaryBytesTotal}
        ancillaryProgress={ancillaryProgress}
        bootstrapStartedAt={bootstrapStartedAt}
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
        {content && (
          <div
            className={styles.card}
            role="dialog"
            aria-modal="true"
            aria-labelledby={activeHeadingId}
          >
            {content}
          </div>
        )}
      </div>
    </div>
  );
}

export default MithrilBootstrap;
