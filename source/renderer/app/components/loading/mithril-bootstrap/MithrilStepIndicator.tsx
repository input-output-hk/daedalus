import classNames from 'classnames';
import React, { useEffect, useRef, useState } from 'react';
import { intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import spinnerIcon from '../../../assets/images/spinner-universal.inline.svg';
import checkMarkIcon from '../../../assets/images/check-mark-universal.inline.svg';
import closeCrossIcon from '../../../assets/images/close-cross.inline.svg';
import type {
  MithrilBootstrapStatus,
  MithrilProgressItem,
} from '../../../../../common/types/mithril-bootstrap.types';
import { isMithrilBootstrapRestoreCompleteStatus as isRestoreCompleteStatus } from '../../../../../common/types/mithril-bootstrap.types';
import InlineProgressBar from './InlineProgressBar';
import messages from './MithrilBootstrap.messages';
import { formatTransferSize } from './snapshotFormatting';
import styles from './MithrilStepIndicator.scss';
import type { Intl } from '../../../types/i18nTypes';

type StepId = 'preparing' | 'downloading' | 'finalizing';
type StepState = 'completed' | 'active' | 'pending' | 'error';
type SubItemState = 'completed' | 'active' | 'pending' | 'error';

type Props = {
  status: MithrilBootstrapStatus;
  progressItems?: MithrilProgressItem[];
  bytesDownloaded?: number;
  snapshotSize?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryProgress?: number;
};

interface Context {
  intl: Intl;
}

const STEPS: ReadonlyArray<StepId> = ['preparing', 'downloading', 'finalizing'];

const STATUS_TO_STEP: Partial<Record<MithrilBootstrapStatus, StepId>> = {
  preparing: 'preparing',
  downloading: 'downloading',
  verifying: 'downloading',
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

const DOWNLOAD_PROGRESS_ANCHOR_ID = 'step-3';
const VERIFYING_DIGESTS_ID = 'step-4';
const SNAPSHOT_PROGRESS_WEIGHT = 85;
const FAST_SYNC_PROGRESS_WEIGHT = 15;
const VERIFYING_TRANSITION_DELAY_MS = 500;

const clampPercent = (value?: number) => {
  if (typeof value !== 'number' || Number.isNaN(value)) {
    return undefined;
  }

  return Math.min(100, Math.max(0, value));
};

const isTransferComplete = (downloaded?: number, total?: number) =>
  typeof downloaded === 'number' &&
  typeof total === 'number' &&
  total > 0 &&
  downloaded >= total;

const isVerificationOrLater = (status: MithrilBootstrapStatus) =>
  status === 'verifying' ||
  status === 'unpacking' ||
  status === 'converting' ||
  status === 'finalizing' ||
  isRestoreCompleteStatus(status);

function deriveCombinedDownloadPercent({
  status,
  snapshotPercent,
  ancillaryPercent,
  bytesDownloaded,
  snapshotSize,
  ancillaryBytesDownloaded,
  ancillaryBytesTotal,
}: {
  status: MithrilBootstrapStatus;
  snapshotPercent?: number;
  ancillaryPercent?: number;
  bytesDownloaded?: number;
  snapshotSize?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
}) {
  const normalizedSnapshotPercent = clampPercent(snapshotPercent) ?? 0;
  const normalizedAncillaryPercent = clampPercent(ancillaryPercent) ?? 0;

  if (
    isVerificationOrLater(status) ||
    isTransferComplete(bytesDownloaded, snapshotSize) ||
    isTransferComplete(ancillaryBytesDownloaded, ancillaryBytesTotal) ||
    normalizedAncillaryPercent >= 100
  ) {
    return 100;
  }

  return (
    (normalizedSnapshotPercent / 100) * SNAPSHOT_PROGRESS_WEIGHT +
    (normalizedAncillaryPercent / 100) * FAST_SYNC_PROGRESS_WEIGHT
  );
}

function formatCombinedProgressDetails({
  intl,
  bytesDownloaded,
  snapshotSize,
  ancillaryBytesDownloaded,
  ancillaryBytesTotal,
}: {
  intl: Intl;
  bytesDownloaded?: number;
  snapshotSize?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
}) {
  return intl.formatMessage(messages.progressCombinedDetail, {
    snapshotDownloaded: formatTransferSize(bytesDownloaded) ?? '\u2014',
    snapshotTotal: formatTransferSize(snapshotSize) ?? '\u2014',
    fastSyncDownloaded:
      formatTransferSize(ancillaryBytesDownloaded) ?? '\u2014',
    fastSyncTotal: formatTransferSize(ancillaryBytesTotal) ?? '\u2014',
  });
}

function synthesizeVerifyingDigestProgress(
  items: MithrilProgressItem[]
): MithrilProgressItem[] {
  let hasVerifyingDigest = false;

  const nextItems = items.map((item) => {
    if (item.id === DOWNLOAD_PROGRESS_ANCHOR_ID && item.state === 'active') {
      return { ...item, state: 'completed' as const };
    }

    if (item.id === VERIFYING_DIGESTS_ID) {
      hasVerifyingDigest = true;
      if (item.state === 'completed' || item.state === 'error') {
        return item;
      }
      return { ...item, state: 'active' as const };
    }

    return item;
  });

  if (hasVerifyingDigest) {
    return nextItems;
  }

  return [
    ...nextItems,
    {
      id: VERIFYING_DIGESTS_ID,
      label: 'verifying-digests',
      state: 'active' as const,
    },
  ];
}

function getActiveStepIndex(status: MithrilBootstrapStatus): number {
  if (isRestoreCompleteStatus(status)) return STEPS.length;
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
  if (isRestoreCompleteStatus(status)) return 'completed';
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

function splitSubItemsAroundAnchor(
  items: MithrilProgressItem[],
  anchorId: string
): {
  itemsBeforeAnchor: MithrilProgressItem[];
  itemsAfterAnchor: MithrilProgressItem[];
} {
  const anchorIndex = items.findIndex((item) => item.id === anchorId);

  if (anchorIndex === -1) {
    return {
      itemsBeforeAnchor: items,
      itemsAfterAnchor: [],
    };
  }

  return {
    itemsBeforeAnchor: items.slice(0, anchorIndex + 1),
    itemsAfterAnchor: items.slice(anchorIndex + 1),
  };
}

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
    return <div className={styles.activeCircle} />;
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

function MithrilStepIndicator(props: Props, { intl }: Context) {
  const {
    status,
    progressItems = [],
    bytesDownloaded,
    snapshotSize,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
  } = props;

  const activeStepIndex = getActiveStepIndex(status);
  const activeStepId: StepId | undefined =
    activeStepIndex >= 0 && activeStepIndex < STEPS.length
      ? STEPS[activeStepIndex]
      : undefined;

  const snapshotPercent =
    typeof snapshotSize === 'number' && snapshotSize > 0
      ? ((bytesDownloaded ?? 0) / snapshotSize) * 100
      : 0;
  const ancPercent =
    typeof ancillaryProgress === 'number' ? ancillaryProgress : 0;
  const combinedDownloadPercent = deriveCombinedDownloadPercent({
    status,
    snapshotPercent,
    ancillaryPercent: ancPercent,
    bytesDownloaded,
    snapshotSize,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
  });
  const combinedProgressDetails = formatCombinedProgressDetails({
    intl,
    bytesDownloaded,
    snapshotSize,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
  });

  const [showVerifyingTransition, setShowVerifyingTransition] = useState(false);

  const actualActiveSubItem = progressItems.find((i) => i.state === 'active');
  const shouldDelayVerifyingTransition =
    status === 'downloading' &&
    actualActiveSubItem?.id === DOWNLOAD_PROGRESS_ANCHOR_ID &&
    combinedDownloadPercent >= 100;

  useEffect(() => {
    if (!shouldDelayVerifyingTransition) {
      setShowVerifyingTransition(false);
      return undefined;
    }

    const timeoutId = window.setTimeout(() => {
      setShowVerifyingTransition(true);
    }, VERIFYING_TRANSITION_DELAY_MS);

    return () => {
      window.clearTimeout(timeoutId);
    };
  }, [shouldDelayVerifyingTransition]);

  const displayedProgressItems = showVerifyingTransition
    ? synthesizeVerifyingDigestProgress(progressItems)
    : progressItems;

  const activeRef = useRef<HTMLDivElement | null>(null);
  const prevActiveIdRef = useRef<string | null>(null);
  const activeSubItem = displayedProgressItems.find(
    (i) => i.state === 'active'
  );
  const activeSubItemId = activeSubItem?.id ?? null;

  useEffect(() => {
    if (activeSubItemId && activeSubItemId !== prevActiveIdRef.current) {
      prevActiveIdRef.current = activeSubItemId;
      if (
        activeRef.current &&
        typeof activeRef.current.scrollIntoView === 'function'
      ) {
        const prefersReducedMotion =
          typeof window.matchMedia === 'function' &&
          window.matchMedia('(prefers-reduced-motion: reduce)').matches;
        activeRef.current.scrollIntoView({
          behavior: prefersReducedMotion ? 'auto' : 'smooth',
          block: 'nearest',
        });
      }
    }
  }, [activeSubItemId]);

  const renderSubItem = (
    item: MithrilProgressItem,
    extraClassName?: string
  ) => {
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
        className={classNames(styles.subItem, extraClassName, {
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
  };

  return (
    <div className={styles.root} role="list" aria-label="Mithril sync progress">
      {STEPS.map((stepId, stepIndex) => {
        const hasError =
          status === 'failed' && hasPhaseError(displayedProgressItems, stepId);

        const state =
          status === 'failed'
            ? deriveFailedStepState(stepId, displayedProgressItems, hasError)
            : deriveTopLevelState(stepIndex, activeStepIndex, status);

        const label = intl.formatMessage(messages[STEP_MESSAGES[stepId]]);
        const isLast = stepIndex === STEPS.length - 1;

        const subItems =
          state === 'active'
            ? groupSubItems(displayedProgressItems, stepId, activeStepId)
            : [];
        const showBars =
          stepId === 'downloading' &&
          state === 'active' &&
          activeSubItemId === DOWNLOAD_PROGRESS_ANCHOR_ID;
        const {
          itemsBeforeAnchor: subItemsBeforeBars,
          itemsAfterAnchor: subItemsAfterBars,
        } =
          stepId === 'downloading'
            ? splitSubItemsAroundAnchor(subItems, DOWNLOAD_PROGRESS_ANCHOR_ID)
            : {
                itemsBeforeAnchor: subItems,
                itemsAfterAnchor: [],
              };
        const hasSubContent = subItems.length > 0 || showBars;

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
            <div className={styles.stepRow}>
              <div className={styles.iconContainer}>
                <TopLevelIcon state={state} />
              </div>
              <div className={styles.labelContainer}>
                <span className={styles.label}>{label}</span>
              </div>
            </div>

            {hasSubContent && (
              <div
                className={styles.subContent}
                role="list"
                aria-label={`${label} details`}
              >
                {subItemsBeforeBars.map((item) => renderSubItem(item))}

                {showBars && (
                  <div
                    className={classNames(styles.progressBars, {
                      [styles.progressBarsAfterSubItems]:
                        subItemsBeforeBars.length > 0,
                    })}
                    role="listitem"
                  >
                    <InlineProgressBar
                      label={intl.formatMessage(messages.progressCombinedLabel)}
                      percent={combinedDownloadPercent}
                      details={combinedProgressDetails}
                    />
                  </div>
                )}

                {subItemsAfterBars.map((item, index) =>
                  renderSubItem(
                    item,
                    index === 0 ? styles.subItemAfterBars : undefined
                  )
                )}
              </div>
            )}

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
