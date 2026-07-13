import React, { useEffect, useRef, useState } from 'react';
import { intlShape } from 'react-intl';
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncStatus,
  MithrilPartialSyncTransferProgress,
} from '../../../../../common/types/mithril-partial-sync.types';
import { isMithrilPartialSyncWorkingStatus } from '../../../../../common/types/mithril-partial-sync.types';
import type { MithrilProgressItem } from '../../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../../types/i18nTypes';
import {
  MITHRIL_ERROR_HEADING_ID,
  MITHRIL_PROGRESS_HEADING_ID,
} from './accessibilityIds';
import MithrilBootstrapMessages from './MithrilBootstrap.messages';
import MithrilErrorView from './MithrilErrorView';
import MithrilProgressView from './MithrilProgressView';
import { resolvePartialSyncErrorCopy } from './partialSyncErrorCopy';
import styles from './MithrilBootstrap.scss';

type Props = {
  status: MithrilPartialSyncStatus;
  progressItems?: MithrilProgressItem[];
  startedAt?: number | null;
  transferProgress?: MithrilPartialSyncTransferProgress;
  error?: MithrilPartialSyncError | null;
  canRetry: boolean;
  canRestartNormally: boolean;
  canWipeAndFullSync: boolean;
  onCancel(): void;
  onRetry(): void;
  onRestartNormally(): void;
  onWipeAndFullSync(): void;
  onDismissCompleted(): void | Promise<void>;
  onQuit(): void;
  onOpenExternalLink?: (arg: string) => void;
};

interface Context {
  intl: Intl;
}

// Linger the success frame long enough to read before the finalize IPC fires automatically.
const COMPLETED_AUTO_DISMISS_DELAY_MS = 4000;
// Delay before the single silent retry of a rejected finalize: long enough for a transient hiccup to clear, short enough not to linger.
const FINALIZE_RETRY_DELAY_MS = 2000;

function MithrilPartialSyncOverlay(props: Props, { intl }: Context) {
  const {
    status,
    progressItems,
    startedAt,
    transferProgress,
    error,
    canRetry,
    canRestartNormally,
    canWipeAndFullSync,
    onCancel,
    onRetry,
    onRestartNormally,
    onWipeAndFullSync,
    onDismissCompleted,
    onQuit,
    onOpenExternalLink,
  } = props;

  // Switches to the error view after the finalize attempt and its retry both fail; component-local because 'completed' otherwise renders through the progress view.
  const [finalizeFailed, setFinalizeFailed] = useState(false);
  // Guards state updates so a late finalize outcome never sets state after the overlay has unmounted.
  const isUnmountedRef = useRef(false);
  useEffect(
    () => () => {
      isUnmountedRef.current = true;
    },
    []
  );

  // On 'completed', auto-fire the finalize IPC via onDismissCompleted once the success frame has lingered.
  useEffect(() => {
    if (status !== 'completed') return undefined;
    let disposed = false;
    let retryTimer: ReturnType<typeof setTimeout> | undefined;
    // Retry a rejected finalize once, then surface the error view — never swallow it, so a failure neither strands the success frame nor becomes an unhandled rejection.
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
  }, [status, onDismissCompleted]);

  // Manual finalize retry from the error view; renderer-local, not a backend recovery action.
  const handleFinalizeRetry = () => {
    Promise.resolve(onDismissCompleted())
      .then(() => {
        if (isUnmountedRef.current) return;
        // Success hands control back to the store-driven dismissal; clear the
        // failure flag so the hand-off frame shows until the overlay hides.
        setFinalizeFailed(false);
      })
      .catch(() => {
        if (isUnmountedRef.current) return;
        setFinalizeFailed(true);
      });
  };

  const isProgressStatus =
    isMithrilPartialSyncWorkingStatus(status) || status === 'completed';
  // 'completed' is a progress status, so this flag forces the error view; gating on 'completed' keeps a stale flag from leaking into other statuses.
  const isFinalizeFailureShown = finalizeFailed && status === 'completed';
  const showProgressView = isProgressStatus && !isFinalizeFailureShown;
  const activeHeadingId = showProgressView
    ? MITHRIL_PROGRESS_HEADING_ID
    : MITHRIL_ERROR_HEADING_ID;
  const errorCopy = isFinalizeFailureShown
    ? {
        title: MithrilBootstrapMessages.partialSyncFinalizeFailedTitle,
        hint: MithrilBootstrapMessages.partialSyncFinalizeFailedHint,
      }
    : resolvePartialSyncErrorCopy(status, error);

  // Recovery actions render strictly from allowedRecoveryActions (here
  // via the canRetry/canRestartNormally/canWipeAndFullSync booleans).
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
  // Quit renders only when no recovery actions exist, so a failure is never an unclosable dead-end; renderer-only, not a backend recovery action.
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
  // Reorders this overlay's actions to secondary-first, primary-last (a stable reorder that never adds or drops actions; membership still comes from allowedRecoveryActions).
  const orderedErrorActions = [
    ...errorActions.filter((action) => action.variant !== 'primary'),
    ...errorActions.filter((action) => action.variant === 'primary'),
  ];

  // The finalize-failed frame offers exactly one action: retry the finalize
  // hand-off. It deliberately bypasses the recovery-action membership above.
  const finalizeFailureActions = [
    {
      label: intl.formatMessage(
        MithrilBootstrapMessages.partialSyncFinalizeFailedRetry
      ),
      onClick: handleFinalizeRetry,
      variant: 'primary' as const,
    },
  ];

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
          {showProgressView ? (
            <MithrilProgressView
              status={status}
              variant="partial-sync"
              progressItems={progressItems}
              filesDownloaded={transferProgress?.filesDownloaded}
              filesTotal={transferProgress?.filesTotal}
              ancillaryBytesDownloaded={
                transferProgress?.ancillaryBytesDownloaded
              }
              ancillaryBytesTotal={transferProgress?.ancillaryBytesTotal}
              bootstrapStartedAt={startedAt}
              hideAction={[
                'cancelling',
                'installing',
                'finalizing',
                'starting-node',
                'completed',
              ].includes(status)}
              actionDisabled={status === 'stopping-node'}
              showDownloadProgressBar
              onAction={onCancel}
            />
          ) : (
            <MithrilErrorView
              error={isFinalizeFailureShown ? null : error}
              onOpenExternalLink={onOpenExternalLink}
              title={intl.formatMessage(errorCopy.title)}
              hint={intl.formatMessage(errorCopy.hint)}
              hintAsBody={status === 'cancelled'}
              actions={
                isFinalizeFailureShown
                  ? finalizeFailureActions
                  : orderedErrorActions
              }
              rightAlignActions
            />
          )}
        </div>
      </div>
    </div>
  );
}

MithrilPartialSyncOverlay.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilPartialSyncOverlay;
