import React from 'react';
import { intlShape } from 'react-intl';
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncStatus,
} from '../../../../../common/types/mithril-partial-sync.types';
import type { MithrilProgressItem } from '../../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../../types/i18nTypes';
import { MITHRIL_ERROR_HEADING_ID, MITHRIL_PROGRESS_HEADING_ID } from './accessibilityIds';
import MithrilBootstrapMessages from './MithrilBootstrap.messages';
import MithrilErrorView from './MithrilErrorView';
import MithrilProgressView from './MithrilProgressView';
import styles from './MithrilBootstrap.scss';

type Props = {
  status: MithrilPartialSyncStatus;
  progressItems?: MithrilProgressItem[];
  filesDownloaded?: number;
  filesTotal?: number;
  elapsedSeconds?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  error?: MithrilPartialSyncError | null;
  canRetry: boolean;
  canRestartNormally: boolean;
  canWipeAndFullSync: boolean;
  onCancel(): void;
  onRetry(): void;
  onRestartNormally(): void;
  onWipeAndFullSync(): void;
  onDismissCompleted(): void;
  onOpenExternalLink?: (arg: string) => void;
};

interface Context {
  intl: Intl;
}

const PROGRESS_STATUSES: MithrilPartialSyncStatus[] = [
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
    filesDownloaded,
    filesTotal,
    elapsedSeconds,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    error,
    canRetry,
    canRestartNormally,
    canWipeAndFullSync,
    onCancel,
    onRetry,
    onRestartNormally,
    onWipeAndFullSync,
    onDismissCompleted,
    onOpenExternalLink,
  } = props;

  const isProgressStatus = PROGRESS_STATUSES.includes(status);
  const activeHeadingId = isProgressStatus
    ? MITHRIL_PROGRESS_HEADING_ID
    : MITHRIL_ERROR_HEADING_ID;

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
              bytesDownloaded={filesDownloaded}
              snapshotSize={filesTotal}
              ancillaryBytesDownloaded={ancillaryBytesDownloaded}
              ancillaryBytesTotal={ancillaryBytesTotal}
              elapsedSeconds={elapsedSeconds}
              title={intl.formatMessage(MithrilBootstrapMessages.partialSyncTitle)}
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
              hideAction={status === 'starting-node'}
              showDownloadProgressBar={false}
              onAction={
                status === 'completed' ? onDismissCompleted : onCancel
              }
            />
          ) : (
            <MithrilErrorView
              error={error as any}
              onOpenExternalLink={onOpenExternalLink}
              title={intl.formatMessage(
                status === 'cancelled'
                  ? MithrilBootstrapMessages.partialSyncCancelledTitle
                  : MithrilBootstrapMessages.partialSyncFailedTitle
              )}
              hint={intl.formatMessage(
                status === 'cancelled'
                  ? MithrilBootstrapMessages.partialSyncCancelledHint
                  : MithrilBootstrapMessages.partialSyncFailedHint
              )}
              actions={[
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
              ]}
              onWipeRetry={onRetry}
              onDecline={onRestartNormally}
            />
          )}
        </div>
      </div>
    </div>
  );
}

export const isMithrilPartialSyncOverlayStatus = (
  status: MithrilPartialSyncStatus
): boolean =>
  PROGRESS_STATUSES.includes(status) || ['failed', 'cancelled'].includes(status);

MithrilPartialSyncOverlay.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilPartialSyncOverlay;
