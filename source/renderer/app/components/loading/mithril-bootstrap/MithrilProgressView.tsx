import React, { useEffect, useState } from 'react';
import { intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import type {
  MithrilBootstrapStatus,
  MithrilProgressItem,
} from '../../../../../common/types/mithril-bootstrap.types';
import type { MithrilPartialSyncStatus } from '../../../../../common/types/mithril-partial-sync.types';
import spinnerIcon from '../../../assets/images/spinner-universal.inline.svg';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';
import { MITHRIL_PROGRESS_HEADING_ID } from './accessibilityIds';
import MithrilStepIndicator from './MithrilStepIndicator';
import styles from './MithrilProgressView.scss';

interface Props {
  status: MithrilBootstrapStatus | MithrilPartialSyncStatus;
  variant?: 'bootstrap' | 'partial-sync';
  progressItems?: MithrilProgressItem[];
  filesDownloaded?: number;
  filesTotal?: number;
  snapshotSizeBytes?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryProgress?: number;
  bootstrapStartedAt?: number | null;
  elapsedSeconds?: number;
  hideAction?: boolean;
  actionDisabled?: boolean;
  showDownloadProgressBar?: boolean;
  onAction(): void;
}

interface Context {
  intl: Intl;
}

const TERMINAL_STATUSES = new Set<
  MithrilBootstrapStatus | MithrilPartialSyncStatus
>(['failed', 'cancelled', 'completed']);

const LONG_RUNNING_STATUSES = new Set<
  MithrilBootstrapStatus | MithrilPartialSyncStatus
>(['verifying', 'unpacking', 'converting', 'installing', 'finalizing']);

const formatDuration = (value?: number) => {
  if (value == null || Number.isNaN(value)) return null;

  const totalSeconds = Math.max(0, Math.floor(value));
  const hours = Math.floor(totalSeconds / 3600);
  const minutes = Math.floor((totalSeconds % 3600) / 60);
  const seconds = totalSeconds % 60;

  if (hours > 0) {
    return `${hours}:${String(minutes).padStart(2, '0')}:${String(
      seconds
    ).padStart(2, '0')}`;
  }

  return `${minutes}:${String(seconds).padStart(2, '0')}`;
};

// Shared completion frame for the in-dialogue node stop/start hand-offs and the
// partial-sync completed-transition frame. The three blocks differ only in
// spinner position (top for the completed transition) and whether a detail line
// is present, so they collapse into one helper while keeping
// role=status/aria-live=polite/aria-atomic=true and the spinner verbatim.
function CompletionBlock({
  title,
  detail,
  spinnerPosition = 'bottom',
}: {
  title: string;
  detail?: string;
  spinnerPosition?: 'top' | 'bottom';
}) {
  const spinner = (
    <SVGInline
      svg={spinnerIcon}
      className={styles.completionSpinner}
      aria-hidden="true"
    />
  );

  return (
    <div
      className={styles.completionBlock}
      role="status"
      aria-live="polite"
      aria-atomic="true"
    >
      {spinnerPosition === 'top' && spinner}
      <h2 className={styles.completionTitle}>{title}</h2>
      {detail != null && <p className={styles.completionDetail}>{detail}</p>}
      {spinnerPosition === 'bottom' && spinner}
    </div>
  );
}

function MithrilProgressView(props: Props, { intl }: Context) {
  const {
    status,
    variant = 'bootstrap',
    progressItems,
    filesDownloaded,
    filesTotal,
    snapshotSizeBytes,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
    bootstrapStartedAt,
    elapsedSeconds: elapsedSecondsProp,
    hideAction,
    actionDisabled,
    showDownloadProgressBar,
    onAction,
  } = props;

  const isStartingNode = status === 'starting-node';
  const isStoppingNode = status === 'stopping-node';
  const isCancelling = status === 'cancelling';
  const isLongRunningPhase = LONG_RUNNING_STATUSES.has(status);
  const isPartialSync = variant === 'partial-sync';
  // The partial-sync variant turns the 'completed' frame into a loading-style
  // hand-off (spinner + "Returning to Daedalus...") while the finalize
  // auto-timeout runs. The default bootstrap variant keeps its own 'completed'
  // frame byte-for-byte unchanged.
  const isCompletedTransition = status === 'completed' && isPartialSync;

  // Local elapsed-seconds timer — only this component re-renders each second
  const [elapsedSeconds, setElapsedSeconds] = useState<number | undefined>(
    undefined
  );

  useEffect(() => {
    if (elapsedSecondsProp != null) {
      setElapsedSeconds(elapsedSecondsProp);
      return undefined;
    }

    if (bootstrapStartedAt == null || TERMINAL_STATUSES.has(status)) {
      // Freeze value on terminal status; clear on null start
      if (bootstrapStartedAt == null) setElapsedSeconds(undefined);
      return undefined;
    }
    const tick = () =>
      setElapsedSeconds(
        Math.max(0, Math.floor((Date.now() - bootstrapStartedAt) / 1000))
      );
    tick();
    const id = setInterval(tick, 1000);
    return () => clearInterval(id);
  }, [bootstrapStartedAt, elapsedSecondsProp, status]);

  const elapsedLabel = formatDuration(elapsedSeconds) ?? '0:00';

  let subtitleMessage = messages.progressSubtitle;
  if (isPartialSync) {
    subtitleMessage =
      status === 'completed'
        ? messages.partialSyncCompletedSubtitle
        : messages.partialSyncProgressSubtitle;
  }
  // A disabled Cancel needs an explanation; only the partial-sync flow
  // disables it (while the node is still stopping).
  const actionDisabledTooltip =
    isPartialSync && isStoppingNode
      ? intl.formatMessage(messages.partialSyncCancelStoppingTooltip)
      : undefined;

  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1 id={MITHRIL_PROGRESS_HEADING_ID}>
          {intl.formatMessage(
            isPartialSync ? messages.partialSyncTitle : messages.title
          )}
        </h1>
        <p>{intl.formatMessage(subtitleMessage)}</p>
      </div>

      {!isCancelling && (
        <div className={styles.timerDisplay}>
          <span className={styles.timerLabel}>
            {intl.formatMessage(messages.progressElapsedLabel)}
          </span>
          <span className={styles.timerValue}>{elapsedLabel}</span>
        </div>
      )}

      {isLongRunningPhase && (
        <p className={styles.reassurance} aria-live="polite">
          {intl.formatMessage(messages.progressLongPhaseReassurance)}
        </p>
      )}

      {!isCancelling && (
        <div className={styles.waterfallContainer}>
          <MithrilStepIndicator
            status={status}
            progressItems={progressItems}
            filesDownloaded={filesDownloaded}
            filesTotal={filesTotal}
            snapshotSizeBytes={snapshotSizeBytes}
            ancillaryBytesDownloaded={ancillaryBytesDownloaded}
            ancillaryBytesTotal={ancillaryBytesTotal}
            ancillaryProgress={ancillaryProgress}
            showDownloadProgressBar={showDownloadProgressBar}
          />
        </div>
      )}

      {isStoppingNode && (
        <CompletionBlock
          title={intl.formatMessage(
            isPartialSync
              ? messages.partialSyncNodeStoppingTitle
              : messages.nodeStoppingTitle
          )}
          detail={intl.formatMessage(
            isPartialSync
              ? messages.partialSyncNodeStoppingDetail
              : messages.nodeStoppingDetail
          )}
        />
      )}

      {isCancelling && (
        <CompletionBlock
          title={intl.formatMessage(messages.partialSyncCancellingTitle)}
          detail={intl.formatMessage(messages.partialSyncCancellingDetail)}
        />
      )}

      {isStartingNode && (
        <CompletionBlock
          title={intl.formatMessage(
            isPartialSync
              ? messages.partialSyncNodeStartingTitle
              : messages.nodeStartingTitle
          )}
          detail={intl.formatMessage(
            isPartialSync
              ? messages.partialSyncNodeStartingDetail
              : messages.nodeStartingDetail
          )}
        />
      )}

      {isCompletedTransition && (
        <CompletionBlock
          title={intl.formatMessage(messages.partialSyncCompletedTransition)}
          spinnerPosition="top"
        />
      )}

      {!hideAction && (
        <div className={styles.actions}>
          {(() => {
            const actionButton = (
              <Button
                className={styles.secondaryAction}
                skin={ButtonSkin}
                label={intl.formatMessage(messages.cancel)}
                onClick={onAction}
                disabled={isStartingNode || actionDisabled}
              />
            );
            // A disabled <button> swallows hover events, so the tooltip is hosted
            // on a wrapping <span> that still receives them (matches the
            // SidebarCategory PopOver pattern).
            return actionDisabledTooltip ? (
              <PopOver content={actionDisabledTooltip}>
                <span>{actionButton}</span>
              </PopOver>
            ) : (
              actionButton
            );
          })()}
        </div>
      )}
    </div>
  );
}

MithrilProgressView.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilProgressView;
