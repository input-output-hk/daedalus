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

// The completed overlay is a success acknowledgment that
// auto-hands-off to the normal loading/Wallet-Summary screen — no "Continue"
// click. This linger keeps the success frame visible long enough to read before
// the finalize IPC fires automatically (see the effect below).
const COMPLETED_AUTO_DISMISS_DELAY_MS = 4000;
// Delay before the single, silent retry of a rejected automatic finalize; long
// enough for a transient hiccup to clear, short enough that the success frame
// does not linger noticeably beyond its normal hand-off.
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

  // A finalize failure must not strand the success frame: this local flag
  // switches the overlay to the error view once the automatic finalize attempt
  // and its single silent retry have both failed. It has to be component-local
  // state because 'completed' renders through the progress view.
  const [finalizeFailed, setFinalizeFailed] = useState(false);
  // Finalize outcomes resolve asynchronously and can land after the store has
  // already hidden (unmounted) the overlay; guard state updates so a late
  // outcome never sets state on an unmounted component.
  const isUnmountedRef = useRef(false);
  useEffect(
    () => () => {
      isUnmountedRef.current = true;
    },
    []
  );

  // On 'completed', auto-fire the finalize IPC (reset-to-idle +
  // remove staging dir + clear marker + folder deletion) via the stable
  // `onDismissCompleted` MobX action once the success frame has lingered — the
  // explicit "Continue to Daedalus" click is gone. All cleanup semantics are
  // preserved; only the trigger changes from a click to this timeout.
  useEffect(() => {
    if (status !== 'completed') return undefined;
    let disposed = false;
    let retryTimer: ReturnType<typeof setTimeout> | undefined;
    // Not fire-and-forget — `onDismissCompleted` awaits the async finalize
    // IPC. A rejection is retried once, silently, after a short delay; a second
    // rejection surfaces the finalize-failed error view below instead of being
    // swallowed, so a failure can neither strand the success frame nor become
    // an unhandled promise rejection.
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

  // Manual finalize retry from the error view. It re-invokes the same finalize
  // hand-off and is renderer-local (same footing as the defensive Quit below) —
  // it is NOT a backend recovery action and never joins the
  // allowedRecoveryActions-driven membership.
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
        // Remain on the failure view; the retry action stays available.
        setFinalizeFailed(true);
      });
  };

  const isProgressStatus =
    isMithrilPartialSyncWorkingStatus(status) || status === 'completed';
  // 'completed' is a progress status, so the finalize-failed frame needs this
  // explicit override to leave the progress view. Gating on 'completed' keeps
  // a stale flag from ever leaking into any other status.
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
  // Defensive Quit: rendered ONLY when no recovery actions are
  // available, so a failure can never become an unclosable dead-end. `quit` is
  // renderer-only — it is NOT a backend allowedRecoveryActions value.
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
  // This caller (not the shared, render-order-agnostic MithrilErrorView) owns the
  // partial-sync overlay's display order: secondary actions first (left), the
  // primary action last (right), opting into right-alignment below. This is a
  // STABLE reorder of the SAME action set — it never
  // filters, drops, or adds an action; allowedRecoveryActions still owns
  // membership. Non-primary actions keep their relative order.
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
              error={isFinalizeFailureShown ? null : (error as any)}
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

export { isMithrilPartialSyncOverlayStatus } from '../../../../../common/types/mithril-partial-sync.types';

MithrilPartialSyncOverlay.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilPartialSyncOverlay;
