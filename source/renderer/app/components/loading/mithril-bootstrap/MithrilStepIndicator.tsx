import classNames from 'classnames';
import React from 'react';
import { intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import spinnerIcon from '../../../assets/images/spinner-universal.inline.svg';
import checkMarkIcon from '../../../assets/images/check-mark-universal.inline.svg';
import type { MithrilBootstrapStatus } from '../../../../../common/types/mithril-bootstrap.types';
import messages from './MithrilBootstrap.messages';
import styles from './MithrilStepIndicator.scss';
import type { Intl } from '../../../types/i18nTypes';
import { formatTransferSize } from './snapshotFormatting';

type StepId = 'preparing' | 'downloading' | 'installing' | 'finalizing';

type StepState = 'completed' | 'active' | 'pending';

type Props = {
  status: MithrilBootstrapStatus;
  progress: number;
  bytesDownloaded?: number;
  snapshotSize?: number;
  throughputBps?: number;
};

interface Context {
  intl: Intl;
}

const STEPS: ReadonlyArray<StepId> = [
  'preparing',
  'downloading',
  'installing',
  'finalizing',
];

const STATUS_TO_STEP: Partial<Record<MithrilBootstrapStatus, StepId>> = {
  preparing: 'preparing',
  downloading: 'downloading',
  verifying: 'downloading',
  installing: 'installing',
  converting: 'finalizing',
  finalizing: 'finalizing',
};

const STEP_MESSAGES: Record<StepId, keyof typeof messages> = {
  preparing: 'stepPreparing',
  downloading: 'stepDownloading',
  installing: 'stepInstalling',
  finalizing: 'stepFinalizing',
};

function getStepState(
  stepId: StepId,
  activeStepIndex: number,
  stepIndex: number
): StepState {
  if (stepIndex < activeStepIndex) return 'completed';
  if (stepIndex === activeStepIndex) return 'active';
  return 'pending';
}

function MithrilStepIndicator(props: Props, { intl }: Context) {
  const {
    status,
    progress,
    bytesDownloaded,
    snapshotSize,
    throughputBps,
  } = props;

  const activeStep = STATUS_TO_STEP[status];
  const activeStepIndex = activeStep ? STEPS.indexOf(activeStep) : -1;

  return (
    <div className={styles.root}>
      {STEPS.map((stepId, index) => {
        const state = getStepState(stepId, activeStepIndex, index);
        const messageKey = STEP_MESSAGES[stepId];
        const label = intl.formatMessage(messages[messageKey]);

        const isDownloadActive = stepId === 'downloading' && state === 'active';
        const bytesDownloadedLabel =
          bytesDownloaded != null ? formatTransferSize(bytesDownloaded) : null;
        const snapshotSizeLabel =
          snapshotSize != null && snapshotSize > 0
            ? formatTransferSize(snapshotSize)
            : null;
        const throughputLabel =
          throughputBps != null ? formatTransferSize(throughputBps) : null;

        const isLast = index === STEPS.length - 1;

        return (
          <div
            key={stepId}
            className={classNames(styles.step, {
              [styles.stepCompleted]: state === 'completed',
              [styles.stepActive]: state === 'active',
              [styles.stepPending]: state === 'pending',
            })}
          >
            {!isLast && (
              <div
                className={classNames(styles.connector, {
                  [styles.connectorCompleted]: state === 'completed',
                })}
              />
            )}
            <div className={styles.iconContainer}>
              {state === 'completed' && (
                <SVGInline
                  svg={checkMarkIcon}
                  className={classNames(styles.icon, styles.iconCheck)}
                />
              )}
              {state === 'active' && (
                <SVGInline
                  svg={spinnerIcon}
                  className={classNames(styles.icon, styles.iconSpinner)}
                />
              )}
              {state === 'pending' && <div className={styles.pendingCircle} />}
            </div>
            <div className={styles.labelContainer}>
              <span className={styles.label}>
                {label}
                {state === 'active' && (
                  <span className={styles.percentage}>
                    {' '}
                    {progress.toFixed(1)}%
                  </span>
                )}
              </span>
              {isDownloadActive &&
                (bytesDownloaded != null || throughputBps != null) && (
                  <span className={styles.meta}>
                    {bytesDownloadedLabel && (
                      <>
                        {bytesDownloadedLabel}
                        {snapshotSizeLabel && <> / {snapshotSizeLabel}</>}
                      </>
                    )}
                    {bytesDownloadedLabel && throughputLabel && ' • '}
                    {throughputLabel && `${throughputLabel}/s`}
                  </span>
                )}
            </div>
          </div>
        );
      })}
    </div>
  );
}

MithrilStepIndicator.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilStepIndicator;
