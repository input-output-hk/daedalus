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

type StepId = 'preparing' | 'downloading' | 'finalizing';

type StepState = 'completed' | 'active' | 'pending';

type Props = {
  status: MithrilBootstrapStatus;
  progress?: number;
};

interface Context {
  intl: Intl;
}

const STEPS: ReadonlyArray<StepId> = ['preparing', 'downloading', 'finalizing'];
const DOWNLOAD_VERIFICATION_THRESHOLD = 89.5;

const STATUS_TO_STEP: Partial<Record<MithrilBootstrapStatus, StepId>> = {
  preparing: 'preparing',
  downloading: 'downloading',
  unpacking: 'finalizing',
  converting: 'finalizing',
  finalizing: 'finalizing',
};

const STEP_MESSAGES: Record<StepId, keyof typeof messages> = {
  preparing: 'stepPreparing',
  downloading: 'stepDownloading',
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

function getActiveStepIndex(
  status: MithrilBootstrapStatus,
  progress?: number
): number {
  if (status === 'completed') {
    return STEPS.length;
  }

  if (
    status === 'downloading' &&
    typeof progress === 'number' &&
    progress >= DOWNLOAD_VERIFICATION_THRESHOLD
  ) {
    return STEPS.indexOf('finalizing');
  }

  const activeStep = STATUS_TO_STEP[status];
  return activeStep ? STEPS.indexOf(activeStep) : -1;
}

function MithrilStepIndicator(props: Props, { intl }: Context) {
  const { status, progress } = props;
  const activeStepIndex = getActiveStepIndex(status, progress);

  return (
    <div className={styles.root} role="list">
      {STEPS.map((stepId, index) => {
        const state = getStepState(stepId, activeStepIndex, index);
        const messageKey = STEP_MESSAGES[stepId];
        const label = intl.formatMessage(messages[messageKey]);
        const isLast = index === STEPS.length - 1;

        return (
          <div
            key={stepId}
            role="listitem"
            aria-current={state === 'active' ? 'step' : undefined}
            className={classNames(styles.step, {
              [styles.stepCompleted]: state === 'completed',
              [styles.stepActive]: state === 'active',
              [styles.stepPending]: state === 'pending',
            })}
          >
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
              <span className={styles.label}>{label}</span>
            </div>
            {!isLast && (
              <div
                className={classNames(styles.connector, {
                  [styles.connectorCompleted]: state === 'completed',
                })}
              />
            )}
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
