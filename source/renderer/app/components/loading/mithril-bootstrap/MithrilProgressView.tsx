import React, { useEffect, useRef } from 'react';
import { intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import type {
  MithrilBootstrapStatus,
  MithrilProgressItem,
} from '../../../../../common/types/mithril-bootstrap.types';
import spinnerIcon from '../../../assets/images/spinner-universal.inline.svg';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';
import MithrilStepIndicator from './MithrilStepIndicator';
import styles from './MithrilProgressView.scss';

interface Props {
  status: MithrilBootstrapStatus;
  progress: number;
  progressItems?: MithrilProgressItem[];
  filesDownloaded?: number;
  filesTotal?: number;
  bytesDownloaded?: number;
  snapshotSize?: number;
  throughputBps?: number;
  remainingSeconds?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryProgress?: number;
  ancillaryRemainingSeconds?: number;
  overallElapsedSeconds?: number;
  onCancel(): void;
}

interface Context {
  intl: Intl;
}

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
    progress,
    progressItems,
    filesDownloaded,
    filesTotal,
    bytesDownloaded,
    snapshotSize,
    throughputBps,
    remainingSeconds,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
    ancillaryRemainingSeconds,
    overallElapsedSeconds,
    onCancel,
  } = props;

  const isCompleted = status === 'completed';
  const completionRef = useRef<HTMLHeadingElement>(null);

  useEffect(() => {
    if (isCompleted && completionRef.current) {
      completionRef.current.focus();
    }
  }, [isCompleted]);

  const elapsedLabel = formatDuration(overallElapsedSeconds) ?? '0:00';

  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1>{intl.formatMessage(messages.title)}</h1>
      </div>

      <div className={styles.timerDisplay}>
        <span className={styles.timerLabel}>
          {intl.formatMessage(messages.progressElapsedLabel)}
        </span>
        <span className={styles.timerValue}>{elapsedLabel}</span>
      </div>

      <div className={styles.waterfallContainer}>
        <MithrilStepIndicator
          status={status}
          progress={progress}
          progressItems={progressItems}
          filesDownloaded={filesDownloaded}
          filesTotal={filesTotal}
          bytesDownloaded={bytesDownloaded}
          snapshotSize={snapshotSize}
          throughputBps={throughputBps}
          remainingSeconds={remainingSeconds}
          ancillaryBytesDownloaded={ancillaryBytesDownloaded}
          ancillaryBytesTotal={ancillaryBytesTotal}
          ancillaryProgress={ancillaryProgress}
          ancillaryRemainingSeconds={ancillaryRemainingSeconds}
          overallElapsedSeconds={overallElapsedSeconds}
        />
      </div>

      {isCompleted && (
        <div
          className={styles.completionBlock}
          role="status"
          aria-live="polite"
          aria-atomic="true"
        >
          <SVGInline svg={spinnerIcon} className={styles.completionSpinner} />
          <h2
            className={styles.completionTitle}
            ref={completionRef}
            tabIndex={-1}
          >
            {intl.formatMessage(messages.nodeStartingTitle)}
          </h2>
          <p className={styles.completionDetail}>
            {intl.formatMessage(messages.nodeStartingDetail)}
          </p>
        </div>
      )}

      <div className={styles.actions}>
        <Button
          className={styles.secondaryAction}
          skin={ButtonSkin}
          label={intl.formatMessage(messages.cancel)}
          onClick={onCancel}
          disabled={isCompleted}
        />
      </div>
    </div>
  );
}

MithrilProgressView.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilProgressView;
