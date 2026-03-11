import React from 'react';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import type { MithrilBootstrapStatus } from '../../../../../common/types/mithril-bootstrap.types';
import ProgressBarLarge from '../../widgets/ProgressBarLarge';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';
import MithrilStepIndicator from './MithrilStepIndicator';
import styles from './MithrilProgressView.scss';
import { formatTransferSize } from './snapshotFormatting';

interface Props {
  status: MithrilBootstrapStatus;
  progress: number;
  bytesDownloaded?: number;
  snapshotSize?: number;
  throughputBps?: number;
  elapsedSeconds?: number;
  remainingSeconds?: number;
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

const formatTimingMeta = (
  intl: Intl,
  elapsedLabel: string | null,
  remainingLabel: string | null
) => {
  if (elapsedLabel && remainingLabel) {
    return intl.formatMessage(messages.progressTiming, {
      elapsed: elapsedLabel,
      remaining: remainingLabel,
    });
  }

  if (elapsedLabel) {
    return intl.formatMessage(messages.progressElapsed, {
      elapsed: elapsedLabel,
    });
  }

  if (remainingLabel) {
    return intl.formatMessage(messages.progressRemaining, {
      remaining: remainingLabel,
    });
  }

  return null;
};

function MithrilProgressView(props: Props, { intl }: Context) {
  const {
    status,
    progress,
    bytesDownloaded,
    snapshotSize,
    throughputBps,
    elapsedSeconds,
    remainingSeconds,
    onCancel,
  } = props;

  const normalizedProgress = Math.min(Math.max(progress, 0), 100);
  const progressLabel = normalizedProgress.toFixed(1);
  const isDownloadStage = status === 'downloading';
  const totalSizeLabel =
    snapshotSize != null && snapshotSize > 0
      ? formatTransferSize(snapshotSize)
      : null;
  const throughputLabel =
    throughputBps != null ? formatTransferSize(throughputBps) : null;
  const downloadedLabel =
    bytesDownloaded != null
      ? [formatTransferSize(bytesDownloaded), totalSizeLabel]
          .filter(Boolean)
          .join(' / ')
      : null;
  const transferRateLabel = throughputLabel ? `${throughputLabel}/s` : null;
  let stageDetail = null;
  if (status === 'installing') {
    stageDetail = intl.formatMessage(messages.progressInstallingDetail);
  } else if (status === 'finalizing' || status === 'converting') {
    stageDetail = intl.formatMessage(messages.progressFinalizingDetail);
  }
  const timingLabel = formatTimingMeta(
    intl,
    formatDuration(elapsedSeconds),
    formatDuration(remainingSeconds)
  );

  return (
    <div className={styles.root}>
      <div className={styles.stepIndicator}>
        <MithrilStepIndicator
          status={status}
          progress={normalizedProgress}
          bytesDownloaded={bytesDownloaded}
          snapshotSize={snapshotSize}
          throughputBps={throughputBps}
        />
      </div>

      <div className={styles.progressBar}>
        <ProgressBarLarge
          progress={normalizedProgress}
          leftLabel={intl.formatMessage(messages.progressLabel)}
          rightLabel1={`${progressLabel}%`}
          isDarkMode
        />
      </div>

      {isDownloadStage && (downloadedLabel || transferRateLabel) && (
        <div className={styles.metadataGrid}>
          {downloadedLabel && (
            <div className={styles.metadataItem}>
              <span className={styles.metadataLabel}>
                {intl.formatMessage(messages.downloadBytesLabel)}
              </span>
              <span className={styles.metadataValue}>{downloadedLabel}</span>
            </div>
          )}
          {transferRateLabel && (
            <div className={styles.metadataItem}>
              <span className={styles.metadataLabel}>
                {intl.formatMessage(messages.downloadRateLabel)}
              </span>
              <span className={styles.metadataValue}>{transferRateLabel}</span>
            </div>
          )}
        </div>
      )}

      {!isDownloadStage && stageDetail && (
        <div className={styles.metadataGrid}>
          <div className={styles.metadataItem}>
            <span className={styles.metadataLabel}>
              {intl.formatMessage(messages.progressStageLabel)}
            </span>
            <span className={styles.metadataValue}>{stageDetail}</span>
          </div>
        </div>
      )}

      {isDownloadStage && timingLabel && (
        <div className={styles.timingMeta}>{timingLabel}</div>
      )}

      <div className={styles.actions}>
        <Button
          className={styles.secondaryAction}
          skin={ButtonSkin}
          label={intl.formatMessage(messages.cancel)}
          onClick={onCancel}
        />
      </div>
    </div>
  );
}

MithrilProgressView.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilProgressView;
