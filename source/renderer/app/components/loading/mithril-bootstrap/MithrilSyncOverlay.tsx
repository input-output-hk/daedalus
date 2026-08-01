import React, { useEffect, useRef, useState } from 'react';
import { intlShape } from 'react-intl';
import {
  isMithrilSyncWorkingStatus,
  type MithrilSyncStatus,
  type MithrilSyncFlowType,
  type MithrilSyncError,
} from '../../../../../common/types/mithril-sync.types';
import type {
  MithrilProgressItem,
  MithrilSnapshotItem,
  ChainStorageValidation,
} from '../../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../../types/i18nTypes';
import {
  MITHRIL_ERROR_HEADING_ID,
  MITHRIL_PROGRESS_HEADING_ID,
  MITHRIL_DECISION_HEADING_ID,
  MITHRIL_CHAIN_STORAGE_HEADING_ID,
} from './accessibilityIds';
import MithrilBootstrapMessages from './MithrilBootstrap.messages';
import MithrilDecisionView from './MithrilDecisionView';
import MithrilErrorView from './MithrilErrorView';
import MithrilProgressView from './MithrilProgressView';
import ChainStorageLocationPicker from '../../chain-storage/ChainStorageLocationPicker';
import { resolvePartialSyncErrorCopy } from './partialSyncErrorCopy';
import styles from './MithrilBootstrap.scss';

// Linger the success frame long enough to read before the finalize IPC fires automatically.
const COMPLETED_AUTO_DISMISS_DELAY_MS = 4000;
// Delay before the single silent retry of a rejected finalize.
const FINALIZE_RETRY_DELAY_MS = 2000;

type Props = {
  status: MithrilSyncStatus;
  flowType: MithrilSyncFlowType | null;
  progressItems?: MithrilProgressItem[];
  startedAt?: number | null;
  bootstrapStartedAt?: number | null;
  filesDownloaded?: number;
  filesTotal?: number;
  snapshotBytesDownloaded?: number;
  snapshotBytesTotal?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryProgress?: number;
  error?: MithrilSyncError | null;
  // Bootstrap-specific decision/storage props
  snapshots?: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  selectedSnapshot?: MithrilSnapshotItem | null;
  isFetchingSnapshots?: boolean;
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
  // Partial-sync recovery
  canRetry: boolean;
  canRestartNormally: boolean;
  canWipeAndFullSync: boolean;
  // Callbacks
  onCancel(): void;
  onRetry(): void;
  onRestartNormally(): void;
  onWipeAndFullSync(): void;
  onDismissCompleted(): void | Promise<void>;
  onQuit(): void;
  onOpenExternalLink?: (arg: string) => void;
  // Bootstrap-specific callbacks
  onSetChainStorageDirectory?: (arg: string | null) => Promise<unknown>;
  onResetChainStorageDirectory?(): Promise<unknown>;
  onValidateChainStorageDirectory?: (
    arg: string
  ) => Promise<ChainStorageValidation>;
  onConfirmStorageLocation?(): void;
  onReturnToStorageLocation?(): void;
  onSelectSnapshot?: (arg: string | null) => void;
  onAccept?(): void;
  onDecline?(): void;
  onWipeRetry?(): void;
};

interface Context {
  intl: Intl;
}

function MithrilSyncOverlay(props: Props, { intl }: Context) {
  const {
    status,
    flowType,
    progressItems,
    startedAt,
    bootstrapStartedAt,
    filesDownloaded,
    filesTotal,
    snapshotBytesDownloaded,
    snapshotBytesTotal,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
    error,
    // Bootstrap decision/storage
    snapshots = [],
    selectedDigest,
    selectedSnapshot,
    isFetchingSnapshots = false,
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
    // Partial-sync recovery
    canRetry,
    canRestartNormally,
    canWipeAndFullSync,
    // Callbacks
    onCancel,
    onRetry,
    onRestartNormally,
    onWipeAndFullSync,
    onDismissCompleted,
    onQuit,
    onOpenExternalLink,
    // Bootstrap callbacks
    onSetChainStorageDirectory,
    onResetChainStorageDirectory,
    onValidateChainStorageDirectory,
    onConfirmStorageLocation,
    onReturnToStorageLocation,
    onSelectSnapshot,
    onAccept,
    onDecline,
    onWipeRetry,
  } = props;

  const isBootstrap = flowType === 'bootstrap';
  const isPartialSync = flowType === 'partial-sync';
  // True only when the full bootstrap decision callbacks are wired (i.e., MithrilBootstrapPage).
  // App.tsx mounts this overlay without them for the recovery-bootstrap case.
  const hasBootstrapCallbacks =
    onAccept != null || onConfirmStorageLocation != null;

  // Finalize failure state — component-local, mirrors MithrilPartialSyncOverlay logic
  const [finalizeFailed, setFinalizeFailed] = useState(false);
  const isUnmountedRef = useRef(false);
  useEffect(
    () => () => {
      isUnmountedRef.current = true;
    },
    []
  );

  // Auto-dismiss on 'completed' only for partial-sync flow
  useEffect(() => {
    if (status !== 'completed' || !isPartialSync) return undefined;
    let disposed = false;
    let retryTimer: ReturnType<typeof setTimeout> | undefined;
    const timer = setTimeout(() => {
      Promise.resolve(onDismissCompleted()).catch(() => {
        if (disposed || isUnmountedRef.current) return;
        retryTimer = setTimeout(() => {
          Promise.resolve(onDismissCompleted()).catch(() => {
            if (disposed || isUnmountedRef.current) return;
            setFinalizeFailed(true);
          });
        }, FINALIZE_RETRY_DELAY_MS);
      });
    }, COMPLETED_AUTO_DISMISS_DELAY_MS);
    return () => {
      disposed = true;
      clearTimeout(timer);
      if (retryTimer !== undefined) clearTimeout(retryTimer);
    };
  }, [status, isPartialSync, onDismissCompleted]);

  const handleFinalizeRetry = () => {
    Promise.resolve(onDismissCompleted())
      .then(() => {
        if (isUnmountedRef.current) return;
        setFinalizeFailed(false);
      })
      .catch(() => {
        if (isUnmountedRef.current) return;
        setFinalizeFailed(true);
      });
  };

  // Decision view only applies to bootstrap flow when the full decision callbacks are present.
  // Without them (App.tsx recovery context) 'cancelled'/'failed' fall through to the error branch.
  const isDecisionStatus =
    isBootstrap &&
    hasBootstrapCallbacks &&
    (status === 'decision' || status === 'idle' || status === 'cancelled');

  // Progress view: all working statuses; completed is shown as progress for partial-sync hand-off
  const isProgressStatus =
    isMithrilSyncWorkingStatus(status) || status === 'completed';
  const isFinalizeFailureShown =
    finalizeFailed && status === 'completed' && isPartialSync;
  const showProgressView = isProgressStatus && !isFinalizeFailureShown;

  // Active heading
  let activeHeadingId: string | undefined;
  if (
    isDecisionStatus &&
    !storageLocationConfirmed &&
    !isApplyingStorageLocation
  ) {
    activeHeadingId = MITHRIL_CHAIN_STORAGE_HEADING_ID;
  } else if (isDecisionStatus) {
    activeHeadingId = MITHRIL_DECISION_HEADING_ID;
  } else if (showProgressView) {
    activeHeadingId = MITHRIL_PROGRESS_HEADING_ID;
  } else if (
    status === 'failed' ||
    status === 'cancelled' ||
    isFinalizeFailureShown
  ) {
    activeHeadingId = MITHRIL_ERROR_HEADING_ID;
  }

  // Build recovery actions for partial-sync error view
  const recoveryActions = [
    ...(canRetry
      ? [
          {
            label: intl.formatMessage(
              MithrilBootstrapMessages.partialSyncRetry
            ),
            onClick: onRetry,
            variant: 'primary' as const,
          },
        ]
      : []),
    ...(canRestartNormally
      ? [
          {
            label: intl.formatMessage(
              MithrilBootstrapMessages.partialSyncRestartNormally
            ),
            onClick: onRestartNormally,
            variant: canRetry ? ('secondary' as const) : ('primary' as const),
          },
        ]
      : []),
    ...(canWipeAndFullSync
      ? [
          {
            label: intl.formatMessage(
              MithrilBootstrapMessages.partialSyncWipeAndFullSync
            ),
            onClick: onWipeAndFullSync,
            variant:
              canRetry || canRestartNormally
                ? ('secondary' as const)
                : ('primary' as const),
          },
        ]
      : []),
  ];

  const errorActions =
    recoveryActions.length > 0
      ? recoveryActions
      : [
          {
            label: intl.formatMessage(MithrilBootstrapMessages.partialSyncQuit),
            onClick: onQuit,
            variant: 'primary' as const,
          },
        ];
  const orderedErrorActions = [
    ...errorActions.filter((a) => a.variant !== 'primary'),
    ...errorActions.filter((a) => a.variant === 'primary'),
  ];

  const finalizeFailureActions = [
    {
      label: intl.formatMessage(
        MithrilBootstrapMessages.partialSyncFinalizeFailedRetry
      ),
      onClick: handleFinalizeRetry,
      variant: 'primary' as const,
    },
  ];

  // Cancel button visibility for progress view
  // Hidden for: cancelling, installing, finalizing, starting-node, completed
  const hideAction = [
    'cancelling',
    'installing',
    'finalizing',
    'starting-node',
    'completed',
  ].includes(status);

  // For bootstrap, show the cancel in all working statuses except starting-node
  const bootstrapHideAction = status === 'starting-node';

  const effectiveHideAction = isPartialSync ? hideAction : bootstrapHideAction;

  // Stopping-node: cancel button is present but disabled (partial-sync only)
  const actionDisabled = isPartialSync && status === 'stopping-node';

  // Resolve partial-sync error copy
  const partialSyncErrorCopy = isFinalizeFailureShown
    ? {
        title: MithrilBootstrapMessages.partialSyncFinalizeFailedTitle,
        hint: MithrilBootstrapMessages.partialSyncFinalizeFailedHint,
      }
    : resolvePartialSyncErrorCopy(
        // resolvePartialSyncErrorCopy accepts MithrilPartialSyncStatus/Error;
        // MithrilSyncStatus is a superset so the cast is safe
        status as any,
        error as any
      );

  let content: React.ReactNode = null;

  if (
    isDecisionStatus &&
    !storageLocationConfirmed &&
    !isApplyingStorageLocation
  ) {
    // Bootstrap chain-storage picker
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
  } else if (isDecisionStatus) {
    // Bootstrap decision / snapshot selector
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
        onSelectSnapshot={onSelectSnapshot ?? (() => {})}
        onReturnToStorageLocation={onReturnToStorageLocation}
        onAccept={onAccept ?? (() => {})}
        onDecline={onDecline ?? (() => {})}
      />
    );
  } else if (showProgressView) {
    content = (
      <MithrilProgressView
        status={status}
        variant={isPartialSync ? 'partial-sync' : 'bootstrap'}
        progressItems={progressItems}
        filesDownloaded={filesDownloaded}
        filesTotal={filesTotal}
        snapshotBytesDownloaded={snapshotBytesDownloaded}
        snapshotBytesTotal={snapshotBytesTotal}
        ancillaryBytesDownloaded={ancillaryBytesDownloaded}
        ancillaryBytesTotal={ancillaryBytesTotal}
        ancillaryProgress={ancillaryProgress}
        bootstrapStartedAt={isBootstrap ? bootstrapStartedAt : startedAt}
        hideAction={effectiveHideAction}
        actionDisabled={actionDisabled}
        showDownloadProgressBar
        onAction={onCancel}
      />
    );
  } else if (
    status === 'failed' ||
    status === 'cancelled' ||
    isFinalizeFailureShown
  ) {
    const isCancelled = status === 'cancelled';
    if (isPartialSync || (isBootstrap && !hasBootstrapCallbacks)) {
      // Partial-sync error view, or bootstrap in recovery context (App.tsx, no decision callbacks).
      content = (
        <MithrilErrorView
          error={isFinalizeFailureShown ? null : (error as any)}
          onOpenExternalLink={onOpenExternalLink}
          title={intl.formatMessage(partialSyncErrorCopy.title)}
          hint={intl.formatMessage(partialSyncErrorCopy.hint)}
          hintAsBody={isCancelled}
          actions={
            isFinalizeFailureShown
              ? finalizeFailureActions
              : orderedErrorActions
          }
          rightAlignActions
        />
      );
    } else {
      // Bootstrap error view (MithrilBootstrapPage context, full callbacks present).
      content = (
        <MithrilErrorView
          error={error as any}
          onOpenExternalLink={onOpenExternalLink}
          onWipeRetry={onWipeRetry}
          onDecline={onDecline}
        />
      );
    }
  }

  if (!content) return null;

  return (
    <div className={styles.component}>
      <div className={styles.backdrop} />
      <div className={styles.content}>
        <div
          className={styles.card}
          role="dialog"
          aria-modal="true"
          aria-labelledby={activeHeadingId}
        >
          {content}
        </div>
      </div>
    </div>
  );
}

MithrilSyncOverlay.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilSyncOverlay;
