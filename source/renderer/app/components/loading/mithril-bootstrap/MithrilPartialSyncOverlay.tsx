import React, { useEffect } from 'react';
import { intlShape } from 'react-intl';
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncStatus,
  MithrilPartialSyncTransferProgress,
} from '../../../../../common/types/mithril-partial-sync.types';
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

const PROGRESS_STATUSES: MithrilPartialSyncStatus[] = [
  'stopping-node',
  'cancelling',
  'preparing',
  'downloading',
  'verifying',
  'converting',
  'installing',
  'finalizing',
  'starting-node',
  'completed',
];

// The completed overlay is a success acknowledgment that
// auto-hands-off to the normal loading/Wallet-Summary screen — no "Continue"
// click. This linger keeps the success frame visible long enough to read before
// the finalize IPC fires automatically (see the effect below).
const COMPLETED_AUTO_DISMISS_DELAY_MS = 4000;

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

  // On 'completed', auto-fire the finalize IPC (reset-to-idle +
  // remove staging dir + clear marker + folder deletion) via the stable
  // `onDismissCompleted` MobX action once the success frame has lingered — the
  // explicit "Continue to Daedalus" click is gone. All cleanup semantics are
  // preserved; only the trigger changes from a click to this timeout.
  useEffect(() => {
    if (status !== 'completed') return undefined;
    // Not fire-and-forget — `onDismissCompleted` awaits the async
    // finalize IPC, so wrap it in `Promise.resolve(...).catch(...)` to ensure a
    // finalize rejection can never surface as an unhandled promise rejection.
    const timer = setTimeout(() => {
      Promise.resolve(onDismissCompleted()).catch(() => {});
    }, COMPLETED_AUTO_DISMISS_DELAY_MS);
    return () => clearTimeout(timer);
  }, [status, onDismissCompleted]);

  const isProgressStatus = PROGRESS_STATUSES.includes(status);
  const activeHeadingId = isProgressStatus
    ? MITHRIL_PROGRESS_HEADING_ID
    : MITHRIL_ERROR_HEADING_ID;
  const errorCopy = resolvePartialSyncErrorCopy(status, error);

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
          {isProgressStatus ? (
            <MithrilProgressView
              status={status}
              progressItems={progressItems}
              filesDownloaded={transferProgress?.filesDownloaded}
              filesTotal={transferProgress?.filesTotal}
              ancillaryBytesDownloaded={
                transferProgress?.ancillaryBytesDownloaded
              }
              ancillaryBytesTotal={transferProgress?.ancillaryBytesTotal}
              bootstrapStartedAt={startedAt}
              title={intl.formatMessage(
                MithrilBootstrapMessages.partialSyncTitle
              )}
              subtitle={intl.formatMessage(
                status === 'completed'
                  ? MithrilBootstrapMessages.partialSyncCompletedSubtitle
                  : MithrilBootstrapMessages.partialSyncProgressSubtitle
              )}
              completedTransitionLabel={intl.formatMessage(
                MithrilBootstrapMessages.partialSyncCompletedTransition
              )}
              actionLabel={intl.formatMessage(MithrilBootstrapMessages.cancel)}
              startingNodeTitle={intl.formatMessage(
                MithrilBootstrapMessages.partialSyncNodeStartingTitle
              )}
              startingNodeDetail={intl.formatMessage(
                MithrilBootstrapMessages.partialSyncNodeStartingDetail
              )}
              stoppingNodeTitle={intl.formatMessage(
                MithrilBootstrapMessages.partialSyncNodeStoppingTitle
              )}
              stoppingNodeDetail={intl.formatMessage(
                MithrilBootstrapMessages.partialSyncNodeStoppingDetail
              )}
              cancellingTitle={intl.formatMessage(
                MithrilBootstrapMessages.partialSyncCancellingTitle
              )}
              cancellingDetail={intl.formatMessage(
                MithrilBootstrapMessages.partialSyncCancellingDetail
              )}
              hideAction={[
                'cancelling',
                'installing',
                'finalizing',
                'starting-node',
                'completed',
              ].includes(status)}
              actionDisabled={status === 'stopping-node'}
              actionDisabledTooltip={
                status === 'stopping-node'
                  ? intl.formatMessage(
                      MithrilBootstrapMessages.partialSyncCancelStoppingTooltip
                    )
                  : undefined
              }
              showDownloadProgressBar
              onAction={onCancel}
            />
          ) : (
            <MithrilErrorView
              error={error as any}
              onOpenExternalLink={onOpenExternalLink}
              title={intl.formatMessage(errorCopy.title)}
              hint={intl.formatMessage(errorCopy.hint)}
              hintAsBody={status === 'cancelled'}
              actions={orderedErrorActions}
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
