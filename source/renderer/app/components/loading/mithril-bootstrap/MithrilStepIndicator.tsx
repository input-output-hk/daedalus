import classNames from 'classnames';
import React, { useEffect, useRef } from 'react';
import { intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import spinnerIcon from '../../../assets/images/spinner-universal.inline.svg';
import checkMarkIcon from '../../../assets/images/check-mark-universal.inline.svg';
import closeCrossIcon from '../../../assets/images/close-cross.inline.svg';
import type {
  MithrilBootstrapStatus,
  MithrilProgressItem,
} from '../../../../../common/types/mithril-bootstrap.types';
import { formatTransferSize } from './snapshotFormatting';
import messages from './MithrilBootstrap.messages';
import styles from './MithrilStepIndicator.scss';
import type { Intl } from '../../../types/i18nTypes';

/* eslint-disable react/no-unused-prop-types -- Staged props for task-024i */

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type StepId = 'preparing' | 'downloading' | 'finalizing';
type StepState = 'completed' | 'active' | 'pending' | 'error';
type SubItemState = 'completed' | 'active' | 'pending' | 'error';

type Props = {
  status: MithrilBootstrapStatus;
  progress?: number;
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
};

interface Context {
  intl: Intl;
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const STEPS: ReadonlyArray<StepId> = ['preparing', 'downloading', 'finalizing'];

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

const STRUCTURAL_IDS = new Set<string>([
  'preparing',
  'downloading',
  'finalizing',
]);

const DOWNLOAD_SUB_IDS = new Set<string>([
  'step-1',
  'step-2',
  'step-3',
  'step-4',
  'step-5',
  'step-6',
  'step-7',
]);

const FINALIZE_SUB_IDS = new Set<string>([
  'install-snapshot',
  'cleanup',
  'conversion',
]);

const ITEM_ID_TO_MESSAGE: Record<string, keyof typeof messages> = {
  'step-1': 'progressDiskCheck',
  'step-2': 'progressCertificateChain',
  'step-3': 'progressDownloadingSnapshot',
  'step-4': 'progressVerifyingDigests',
  'step-5': 'progressVerifyingDatabase',
  'step-6': 'progressComputingMessage',
  'step-7': 'progressVerifyingSignature',
  'install-snapshot': 'progressInstallSnapshot',
  cleanup: 'progressCleanup',
  conversion: 'progressConversion',
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function getActiveStepIndex(status: MithrilBootstrapStatus): number {
  if (status === 'completed') return STEPS.length;
  if (status === 'failed') return -1;
  const activeStep = STATUS_TO_STEP[status];
  return activeStep ? STEPS.indexOf(activeStep) : -1;
}

function hasPhaseError(items: MithrilProgressItem[], stepId: StepId): boolean {
  return items.some((item) => {
    if (item.state !== 'error') return false;
    if (item.id === stepId) return true;
    if (stepId === 'downloading' && DOWNLOAD_SUB_IDS.has(item.id)) return true;
    if (stepId === 'finalizing' && FINALIZE_SUB_IDS.has(item.id)) return true;
    return false;
  });
}

function deriveTopLevelState(
  stepIndex: number,
  activeStepIndex: number,
  status: MithrilBootstrapStatus
): StepState {
  if (status === 'completed') return 'completed';
  if (activeStepIndex < 0) return 'pending';
  if (stepIndex < activeStepIndex) return 'completed';
  if (stepIndex === activeStepIndex) return 'active';
  return 'pending';
}

function deriveFailedStepState(
  stepId: StepId,
  items: MithrilProgressItem[],
  hasError: boolean
): StepState {
  if (hasError) return 'error';
  const hasCompleted = items.some(
    (item) =>
      item.state === 'completed' &&
      (item.id === stepId ||
        (stepId === 'downloading' && DOWNLOAD_SUB_IDS.has(item.id)) ||
        (stepId === 'finalizing' && FINALIZE_SUB_IDS.has(item.id)))
  );
  if (hasCompleted) return 'completed';
  return 'pending';
}

function groupSubItems(
  items: MithrilProgressItem[],
  stepId: StepId,
  activeStepId?: StepId
): MithrilProgressItem[] {
  return items.filter((item) => {
    if (STRUCTURAL_IDS.has(item.id)) return false;
    if (stepId === 'downloading' && DOWNLOAD_SUB_IDS.has(item.id)) return true;
    if (stepId === 'finalizing' && FINALIZE_SUB_IDS.has(item.id)) return true;
    if (
      !DOWNLOAD_SUB_IDS.has(item.id) &&
      !FINALIZE_SUB_IDS.has(item.id) &&
      stepId === activeStepId
    ) {
      return true;
    }
    return false;
  });
}

function formatRemainingTime(seconds?: number): string | null {
  if (typeof seconds !== 'number' || seconds < 0) return null;
  const mins = Math.floor(seconds / 60);
  const secs = Math.floor(seconds % 60);
  if (mins > 0) return `${mins}:${secs.toString().padStart(2, '0')}`;
  return `${secs}s`;
}

// ---------------------------------------------------------------------------
// Sub-components
// ---------------------------------------------------------------------------

function TopLevelIcon({ state }: { state: StepState }) {
  if (state === 'completed') {
    return (
      <SVGInline
        svg={checkMarkIcon}
        className={classNames(styles.icon, styles.iconCheck)}
      />
    );
  }
  if (state === 'active') {
    return (
      <SVGInline
        svg={spinnerIcon}
        className={classNames(styles.icon, styles.iconSpinner)}
      />
    );
  }
  if (state === 'error') {
    return (
      <SVGInline
        svg={closeCrossIcon}
        className={classNames(styles.icon, styles.iconError)}
      />
    );
  }
  return <div className={styles.pendingCircle} />;
}

function SubItemIcon({ state }: { state: SubItemState }) {
  if (state === 'completed') {
    return (
      <SVGInline
        svg={checkMarkIcon}
        className={classNames(styles.subItemIcon, styles.subItemIconCheck)}
      />
    );
  }
  if (state === 'active') {
    return (
      <SVGInline
        svg={spinnerIcon}
        className={classNames(styles.subItemIcon, styles.subItemIconSpinner)}
      />
    );
  }
  if (state === 'error') {
    return (
      <SVGInline
        svg={closeCrossIcon}
        className={classNames(styles.subItemIcon, styles.subItemIconError)}
      />
    );
  }
  return <div className={styles.subItemPendingCircle} />;
}

function InlineProgressBar({
  label,
  percent,
  downloaded,
  total,
  remaining,
}: {
  label: string;
  percent: number;
  downloaded?: number;
  total?: number;
  remaining?: string | null;
}) {
  const clamped = Math.min(100, Math.max(0, percent));
  const downloadedStr = formatTransferSize(downloaded) ?? '\u2014';
  const totalStr = formatTransferSize(total) ?? '\u2014';

  return (
    <div className={styles.inlineBar}>
      <div
        role="progressbar"
        aria-valuenow={Math.round(clamped)}
        aria-valuemin={0}
        aria-valuemax={100}
        aria-label={`${label}: ${Math.round(clamped)}%`}
      >
        <div className={styles.inlineBarMeta}>
          <span>{label}</span>
          <span>{Math.round(clamped)}%</span>
        </div>
        <div className={styles.inlineBarTrack}>
          <div
            className={classNames(styles.inlineBarFill, {
              [styles.inlineBarFillActive]: clamped < 100,
              [styles.inlineBarFillComplete]: clamped >= 100,
            })}
            style={{ width: `${clamped}%` }}
          />
        </div>
      </div>
      <div className={styles.inlineBarMeta}>
        <span>
          {downloadedStr} / {totalStr}
        </span>
        {remaining && <span>{remaining}</span>}
      </div>
    </div>
  );
}

// ---------------------------------------------------------------------------
// Main component
// ---------------------------------------------------------------------------

function MithrilStepIndicator(props: Props, { intl }: Context) {
  const {
    status,
    progressItems = [],
    bytesDownloaded,
    snapshotSize,
    remainingSeconds,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
    ancillaryRemainingSeconds,
  } = props;

  const activeStepIndex = getActiveStepIndex(status);
  const activeStepId: StepId | undefined =
    activeStepIndex >= 0 && activeStepIndex < STEPS.length
      ? STEPS[activeStepIndex]
      : undefined;

  // ---------- Auto-scroll ----------
  const activeRef = useRef<HTMLDivElement | null>(null);
  const prevActiveIdRef = useRef<string | null>(null);
  const activeSubItem = progressItems.find((i) => i.state === 'active');
  const activeSubItemId = activeSubItem?.id ?? null;

  useEffect(() => {
    if (activeSubItemId && activeSubItemId !== prevActiveIdRef.current) {
      prevActiveIdRef.current = activeSubItemId;
      if (activeRef.current) {
        const prefersReducedMotion = window.matchMedia(
          '(prefers-reduced-motion: reduce)'
        ).matches;
        activeRef.current.scrollIntoView({
          behavior: prefersReducedMotion ? 'auto' : 'smooth',
          block: 'nearest',
        });
      }
    }
  }, [activeSubItemId]);

  // ---------- Progress bar values ----------
  const snapshotPercent =
    typeof snapshotSize === 'number' && snapshotSize > 0
      ? ((bytesDownloaded ?? 0) / snapshotSize) * 100
      : 0;
  const ancPercent =
    typeof ancillaryProgress === 'number' ? ancillaryProgress : 0;

  return (
    <div className={styles.root} role="list" aria-label="Mithril sync progress">
      {STEPS.map((stepId, stepIndex) => {
        const hasError =
          status === 'failed' && hasPhaseError(progressItems, stepId);

        const state =
          status === 'failed'
            ? deriveFailedStepState(stepId, progressItems, hasError)
            : deriveTopLevelState(stepIndex, activeStepIndex, status);

        const label = intl.formatMessage(messages[STEP_MESSAGES[stepId]]);
        const isLast = stepIndex === STEPS.length - 1;

        const subItems =
          state !== 'pending'
            ? groupSubItems(progressItems, stepId, activeStepId)
            : [];
        const hasSubContent = subItems.length > 0;
        const showBars = stepId === 'downloading' && state !== 'pending';

        const connectorCls = classNames(styles.connector, {
          [styles.connectorCompleted]: state === 'completed',
          [styles.connectorError]: state === 'error',
        });

        return (
          <div
            key={stepId}
            role="listitem"
            aria-current={state === 'active' ? 'step' : undefined}
            className={classNames(styles.step, {
              [styles.stepCompleted]: state === 'completed',
              [styles.stepActive]: state === 'active',
              [styles.stepPending]: state === 'pending',
              [styles.stepError]: state === 'error',
            })}
          >
            {/* Step row: icon + label */}
            <div className={styles.stepRow}>
              <div className={styles.iconContainer}>
                <TopLevelIcon state={state} />
              </div>
              <div className={styles.labelContainer}>
                <span className={styles.label}>{label}</span>
              </div>
            </div>

            {/* Sub-content */}
            {hasSubContent && (
              <div
                className={styles.subContent}
                role="list"
                aria-label={`${label} details`}
              >
                {subItems.map((item) => {
                  const itemState = item.state as SubItemState;
                  const isActive = itemState === 'active';
                  const msgKey = ITEM_ID_TO_MESSAGE[item.id];
                  const itemLabel = msgKey
                    ? intl.formatMessage(messages[msgKey])
                    : item.label;

                  return (
                    <div
                      key={item.id}
                      ref={isActive ? activeRef : undefined}
                      role="listitem"
                      aria-current={isActive ? 'step' : undefined}
                      className={classNames(styles.subItem, {
                        [styles.subItemCompleted]: itemState === 'completed',
                        [styles.subItemActive]: itemState === 'active',
                        [styles.subItemPending]: itemState === 'pending',
                        [styles.subItemError]: itemState === 'error',
                        [styles.subItemNoAnimate]: itemState === 'completed',
                      })}
                    >
                      <div className={styles.subItemIconContainer}>
                        <SubItemIcon state={itemState} />
                      </div>
                      <span className={styles.subItemLabel}>{itemLabel}</span>
                    </div>
                  );
                })}

                {/* Progress bars inside Downloading */}
                {showBars && (
                  <div className={styles.progressBars} role="listitem">
                    <InlineProgressBar
                      label={intl.formatMessage(
                        messages.progressSnapshotFilesLabel
                      )}
                      percent={snapshotPercent}
                      downloaded={bytesDownloaded}
                      total={snapshotSize}
                      remaining={
                        formatRemainingTime(remainingSeconds)
                          ? intl.formatMessage(messages.progressRemaining, {
                              remaining: formatRemainingTime(remainingSeconds),
                            })
                          : null
                      }
                    />
                    <InlineProgressBar
                      label={intl.formatMessage(messages.progressFastSyncLabel)}
                      percent={ancPercent}
                      downloaded={ancillaryBytesDownloaded}
                      total={ancillaryBytesTotal}
                      remaining={
                        formatRemainingTime(ancillaryRemainingSeconds)
                          ? intl.formatMessage(messages.progressRemaining, {
                              remaining: formatRemainingTime(
                                ancillaryRemainingSeconds
                              ),
                            })
                          : null
                      }
                    />
                  </div>
                )}
              </div>
            )}

            {/* Connector line */}
            {!isLast && <div className={connectorCls} />}
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
