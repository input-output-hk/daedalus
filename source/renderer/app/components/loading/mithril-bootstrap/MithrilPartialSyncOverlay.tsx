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
  onDismissCompleted(): void;
  onQuit(): void;
  onOpenExternalLink?: (arg: string) => void;
};

interface Context {
  intl: Intl;
}

const PROGRESS_STATUSES: MithrilPartialSyncStatus[] = [
  'stopping-node',
  'preparing',
  'downloading',
  'verifying',
  'converting',
  'installing',
  'finalizing',
  'starting-node',
  'completed',
];

// ADR D-702a-1: the completed overlay is a success acknowledgment that
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

  // ADR D-702a-1: on 'completed', auto-fire the finalize IPC (reset-to-idle +
  // remove staging dir + clear marker + folder deletion) via the stable
  // `onDismissCompleted` MobX action once the success frame has lingered — the
  // explicit "Continue to Daedalus" click is gone. All cleanup semantics are
  // preserved; only the trigger changes from a click to this timeout.
  useEffect(() => {
    if (status !== 'completed') return undefined;
    const timer = setTimeout(
      () => onDismissCompleted(),
      COMPLETED_AUTO_DISMISS_DELAY_MS
    );
    return () => clearTimeout(timer);
  }, [status, onDismissCompleted]);

  const isProgressStatus = PROGRESS_STATUSES.includes(status);
  const activeHeadingId = isProgressStatus
    ? MITHRIL_PROGRESS_HEADING_ID
    : MITHRIL_ERROR_HEADING_ID;
  const errorCopy = resolvePartialSyncErrorCopy(status, error);

  // lock #5: recovery actions render strictly from allowedRecoveryActions (here
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
  // Defensive Quit (D5d, gaps #8/#31): rendered ONLY when no recovery actions are
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
              hideAction={[
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
              actions={errorActions}
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
