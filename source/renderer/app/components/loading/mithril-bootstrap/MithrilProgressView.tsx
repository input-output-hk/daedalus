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

type StageCopy = {
  title: string;
  detail: string;
};

type MetadataItem = {
  label: string;
  value: string;
};

const DOWNLOAD_VERIFICATION_THRESHOLD = 89.5;
const LOCAL_PROCESSING_STATUSES: Array<MithrilBootstrapStatus> = [
  'unpacking',
  'converting',
  'finalizing',
  'completed',
];

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

const getStageDetail = (intl: Intl, status: MithrilBootstrapStatus) => {
  switch (status) {
    case 'preparing':
      return {
        title: intl.formatMessage(messages.progressPreparingTitle),
        detail: intl.formatMessage(messages.progressPreparingDetail),
      };
    case 'downloading':
      return null;
    case 'unpacking':
      return {
        title: intl.formatMessage(messages.progressUnpackingTitle),
        detail: intl.formatMessage(messages.progressUnpackingDetail),
      };
    case 'converting':
      return {
        title: intl.formatMessage(messages.progressConvertingTitle),
        detail: intl.formatMessage(messages.progressConvertingDetail),
      };
    case 'finalizing':
      return {
        title: intl.formatMessage(messages.progressFinalizingTitle),
        detail: intl.formatMessage(messages.progressFinalizingDetail),
      };
    case 'completed':
      return {
        title: intl.formatMessage(messages.progressCompletedTitle),
        detail: intl.formatMessage(messages.progressCompletedDetail),
      };
    default:
      return null;
  }
};

const getFallbackDownloadedValue = (
  intl: Intl,
  status: MithrilBootstrapStatus,
  snapshotSize?: number
) => {
  const snapshotSizeLabel =
    snapshotSize != null && snapshotSize > 0
      ? formatTransferSize(snapshotSize)
      : null;

  if (snapshotSizeLabel && LOCAL_PROCESSING_STATUSES.includes(status)) {
    return `${snapshotSizeLabel} / ${snapshotSizeLabel}`;
  }

  return snapshotSizeLabel
    ? intl.formatMessage(messages.progressSnapshotSizeValue, {
        size: snapshotSizeLabel,
      })
    : intl.formatMessage(messages.progressWaitingValue);
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
  const isLocalProcessingStage = LOCAL_PROCESSING_STATUSES.includes(status);
  const isNearDownloadPlateau =
    isDownloadStage && normalizedProgress >= DOWNLOAD_VERIFICATION_THRESHOLD;
  const stageDetail = getStageDetail(intl, status);
  const totalSizeLabel =
    snapshotSize != null && snapshotSize > 0
      ? formatTransferSize(snapshotSize)
      : null;
  const downloadedLabel =
    bytesDownloaded != null
      ? [formatTransferSize(bytesDownloaded), totalSizeLabel]
          .filter(Boolean)
          .join(' / ')
      : getFallbackDownloadedValue(intl, status, snapshotSize);
  const transferRateLabel =
    isDownloadStage && throughputBps != null
      ? `${formatTransferSize(throughputBps)}/s`
      : intl.formatMessage(
          isDownloadStage
            ? messages.progressRatePendingValue
            : messages.progressLocalProcessingValue
        );
  const elapsedLabel =
    formatDuration(elapsedSeconds) ||
    intl.formatMessage(
      isDownloadStage
        ? messages.progressTimingPendingValue
        : messages.progressUnknownDurationValue
    );
  let remainingFallbackMessage = messages.progressWaitingValue;
  if (isDownloadStage) {
    remainingFallbackMessage = messages.progressTimingPendingValue;
  } else if (isLocalProcessingStage) {
    remainingFallbackMessage = messages.progressFinalizingRemainingValue;
  }
  const remainingLabel =
    formatDuration(remainingSeconds) ||
    (status === 'completed'
      ? formatDuration(0)
      : intl.formatMessage(remainingFallbackMessage));
  const statusCopy: StageCopy = isDownloadStage
    ? {
        title: intl.formatMessage(
          isNearDownloadPlateau
            ? messages.progressDownloadVerifyingTitle
            : messages.progressDownloadingTitle
        ),
        detail: intl.formatMessage(
          isNearDownloadPlateau
            ? messages.progressDownloadVerifyingDetail
            : messages.progressDownloadingDetail
        ),
      }
    : stageDetail || {
        title: intl.formatMessage(messages.progressPreparingTitle),
        detail: intl.formatMessage(messages.progressPreparingDetail),
      };
  const metadataItems: Array<MetadataItem> = [
    {
      label: intl.formatMessage(messages.downloadBytesLabel),
      value: downloadedLabel,
    },
    {
      label: intl.formatMessage(messages.downloadRateLabel),
      value: transferRateLabel,
    },
    {
      label: intl.formatMessage(messages.progressElapsedLabel),
      value: elapsedLabel,
    },
    {
      label: intl.formatMessage(messages.progressTimeRemainingLabel),
      value:
        remainingLabel || intl.formatMessage(messages.progressWaitingValue),
    },
  ];

  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1>{intl.formatMessage(messages.title)}</h1>
      </div>

      <div className={styles.stepIndicator}>
        <MithrilStepIndicator status={status} />
      </div>

      <div className={styles.progressBar}>
        <ProgressBarLarge
          progress={normalizedProgress}
          leftLabel={intl.formatMessage(messages.progressLabel)}
          rightLabel1={`${progressLabel}%`}
          isDarkMode
        />
      </div>

      <div
        className={styles.statusPanel}
        role="status"
        aria-live="polite"
        aria-atomic="true"
      >
        <div className={styles.statusCopy}>
          <span className={styles.statusLabel}>
            {intl.formatMessage(messages.progressStatusLabel)}
          </span>
          <h2 className={styles.statusTitle}>{statusCopy.title}</h2>
          <p className={styles.statusDetail}>{statusCopy.detail}</p>
        </div>
      </div>

      <div className={styles.metadataGrid}>
        {metadataItems.map(({ label, value }) => (
          <div key={label} className={styles.metadataItem}>
            <span className={styles.metadataLabel}>{label}</span>
            <span className={styles.metadataValue}>{value}</span>
          </div>
        ))}
      </div>

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
