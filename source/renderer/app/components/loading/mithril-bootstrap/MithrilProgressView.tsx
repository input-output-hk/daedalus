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
  progressItems?: MithrilProgressItem[];
  filesDownloaded?: number;
  filesTotal?: number;
  snapshotSizeBytes?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryProgress?: number;
  bootstrapStartedAt?: number | null;
  elapsedSeconds?: number;
  title?: string;
  subtitle?: string;
  actionLabel?: string;
  startingNodeTitle?: string;
  startingNodeDetail?: string;
  stoppingNodeTitle?: string;
  stoppingNodeDetail?: string;
  completedTransitionLabel?: string;
  hideAction?: boolean;
  actionDisabled?: boolean;
  actionDisabledTooltip?: string;
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

function MithrilProgressView(props: Props, { intl }: Context) {
  const {
    status,
    progressItems,
    filesDownloaded,
    filesTotal,
    snapshotSizeBytes,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
    bootstrapStartedAt,
    elapsedSeconds: elapsedSecondsProp,
    title,
    subtitle,
    actionLabel,
    startingNodeTitle,
    startingNodeDetail,
    stoppingNodeTitle,
    stoppingNodeDetail,
    completedTransitionLabel,
    hideAction,
    actionDisabled,
    actionDisabledTooltip,
    showDownloadProgressBar,
    onAction,
  } = props;

  const isStartingNode = status === 'starting-node';
  const isStoppingNode = status === 'stopping-node';
  const isLongRunningPhase = LONG_RUNNING_STATUSES.has(status);
  // ADR D-702a-1: the partial-sync overlay passes `completedTransitionLabel` to
  // turn the 'completed' frame into a loading-style hand-off (spinner +
  // "Returning to Daedalus...") while the finalize auto-timeout runs. Bootstrap
  // never passes the prop, so its 'completed' frame is byte-for-byte unchanged.
  const isCompletedTransition =
    status === 'completed' && !!completedTransitionLabel;

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

  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1 id={MITHRIL_PROGRESS_HEADING_ID}>
          {title || intl.formatMessage(messages.title)}
        </h1>
        <p>{subtitle || intl.formatMessage(messages.progressSubtitle)}</p>
      </div>

      <div className={styles.timerDisplay}>
        <span className={styles.timerLabel}>
          {intl.formatMessage(messages.progressElapsedLabel)}
        </span>
        <span className={styles.timerValue}>{elapsedLabel}</span>
      </div>

      {isLongRunningPhase && (
        <p className={styles.reassurance} aria-live="polite">
          {intl.formatMessage(messages.progressLongPhaseReassurance)}
        </p>
      )}

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

      {isStoppingNode && (
        <div
          className={styles.completionBlock}
          role="status"
          aria-live="polite"
          aria-atomic="true"
        >
          <h2 className={styles.completionTitle}>
            {stoppingNodeTitle ||
              intl.formatMessage(messages.nodeStoppingTitle)}
          </h2>
          <p className={styles.completionDetail}>
            {stoppingNodeDetail ||
              intl.formatMessage(messages.nodeStoppingDetail)}
          </p>
          <SVGInline
            svg={spinnerIcon}
            className={styles.completionSpinner}
            aria-hidden="true"
          />
        </div>
      )}

      {isStartingNode && (
        <div
          className={styles.completionBlock}
          role="status"
          aria-live="polite"
          aria-atomic="true"
        >
          <h2 className={styles.completionTitle}>
            {startingNodeTitle ||
              intl.formatMessage(messages.nodeStartingTitle)}
          </h2>
          <p className={styles.completionDetail}>
            {startingNodeDetail ||
              intl.formatMessage(messages.nodeStartingDetail)}
          </p>
          <SVGInline
            svg={spinnerIcon}
            className={styles.completionSpinner}
            aria-hidden="true"
          />
        </div>
      )}

      {isCompletedTransition && (
        <div
          className={styles.completionBlock}
          role="status"
          aria-live="polite"
          aria-atomic="true"
        >
          <SVGInline
            svg={spinnerIcon}
            className={styles.completionSpinner}
            aria-hidden="true"
          />
          <h2 className={styles.completionTitle}>{completedTransitionLabel}</h2>
        </div>
      )}

      {!hideAction && (
        <div className={styles.actions}>
          {(() => {
            const actionButton = (
              <Button
                className={styles.secondaryAction}
                skin={ButtonSkin}
                label={actionLabel || intl.formatMessage(messages.cancel)}
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
