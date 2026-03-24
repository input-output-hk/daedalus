import React, { useEffect, useRef, useState } from 'react';
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
import { MITHRIL_PROGRESS_HEADING_ID } from './accessibilityIds';
import MithrilStepIndicator from './MithrilStepIndicator';
import styles from './MithrilProgressView.scss';

interface Props {
  status: MithrilBootstrapStatus;
  progressItems?: MithrilProgressItem[];
  bytesDownloaded?: number;
  snapshotSize?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryProgress?: number;
  bootstrapStartedAt?: number | null;
  onCancel(): void;
}

interface Context {
  intl: Intl;
}

const TERMINAL_STATUSES: ReadonlySet<MithrilBootstrapStatus> = new Set([
  'failed',
  'cancelled',
]);

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
    bytesDownloaded,
    snapshotSize,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
    bootstrapStartedAt,
    onCancel,
  } = props;

  const isStartingNode = status === 'starting-node';
  const headingRef = useRef<HTMLHeadingElement>(null);
  const completionRef = useRef<HTMLHeadingElement>(null);

  // Local elapsed-seconds timer — only this component re-renders each second
  const [elapsedSeconds, setElapsedSeconds] = useState<number | undefined>(
    undefined
  );

  useEffect(() => {
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
  }, [bootstrapStartedAt, status]);

  useEffect(() => {
    headingRef.current?.focus();
  }, []);

  useEffect(() => {
    if (isStartingNode && completionRef.current) {
      completionRef.current.focus();
    }
  }, [isStartingNode]);

  const elapsedLabel = formatDuration(elapsedSeconds) ?? '0:00';

  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1 id={MITHRIL_PROGRESS_HEADING_ID} ref={headingRef} tabIndex={-1}>
          {intl.formatMessage(messages.title)}
        </h1>
        <p>{intl.formatMessage(messages.progressSubtitle)}</p>
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
          progressItems={progressItems}
          bytesDownloaded={bytesDownloaded}
          snapshotSize={snapshotSize}
          ancillaryBytesDownloaded={ancillaryBytesDownloaded}
          ancillaryBytesTotal={ancillaryBytesTotal}
          ancillaryProgress={ancillaryProgress}
        />
      </div>

      {isStartingNode && (
        <div
          className={styles.completionBlock}
          role="status"
          aria-live="polite"
          aria-atomic="true"
        >
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
          <SVGInline
            svg={spinnerIcon}
            className={styles.completionSpinner}
            aria-hidden="true"
          />
        </div>
      )}

      <div className={styles.actions}>
        <Button
          className={styles.secondaryAction}
          skin={ButtonSkin}
          label={intl.formatMessage(messages.cancel)}
          onClick={onCancel}
          disabled={isStartingNode}
        />
      </div>
    </div>
  );
}

MithrilProgressView.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilProgressView;
