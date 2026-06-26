import React from 'react';
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
              actionLabel={intl.formatMessage(
                status === 'completed'
                  ? MithrilBootstrapMessages.partialSyncContinue
                  : MithrilBootstrapMessages.cancel
              )}
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
              onAction={status === 'completed' ? onDismissCompleted : onCancel}
            />
          ) : (
            <MithrilErrorView
              error={error as any}
              onOpenExternalLink={onOpenExternalLink}
              title={intl.formatMessage(errorCopy.title)}
              hint={intl.formatMessage(errorCopy.hint)}
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
