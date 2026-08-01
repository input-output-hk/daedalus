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
import type { MithrilPartialSyncStatus } from '../../../../../common/types/mithril-partial-sync.types';
import { isMithrilPartialSyncRestoreCompleteStatus } from '../../../../../common/types/mithril-partial-sync.types';
import { isMithrilBootstrapRestoreCompleteStatus as isRestoreCompleteStatus } from '../../../../../common/types/mithril-bootstrap.types';
import {
  isMithrilSyncRestoreCompleteStatus,
  type MithrilSyncStatus,
} from '../../../../../common/types/mithril-sync.types';
import InlineProgressBar from './InlineProgressBar';
import messages from './MithrilBootstrap.messages';
import { formatTransferSize } from './snapshotFormatting';
import styles from './MithrilStepIndicator.scss';
import type { Intl } from '../../../types/i18nTypes';

type StepId = 'preparing' | 'downloading' | 'finalizing';
type StepState = 'completed' | 'active' | 'pending' | 'error';
type SubItemState = 'completed' | 'active' | 'pending' | 'error';

type Props = {
  status: MithrilBootstrapStatus | MithrilPartialSyncStatus | MithrilSyncStatus;
  variant?: 'bootstrap' | 'partial-sync';
  progressItems?: MithrilProgressItem[];
  filesDownloaded?: number;
  filesTotal?: number;
  snapshotBytesDownloaded?: number;
  snapshotBytesTotal?: number;
  snapshotSizeBytes?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryProgress?: number;
  showDownloadProgressBar?: boolean;
};

interface Context {
  intl: Intl;
}

const STEPS: ReadonlyArray<StepId> = ['preparing', 'downloading', 'finalizing'];

const STATUS_TO_STEP: Partial<
  Record<
    MithrilBootstrapStatus | MithrilPartialSyncStatus | MithrilSyncStatus,
    StepId
  >
> = {
  'stopping-node': 'preparing',
  preparing: 'preparing',
  downloading: 'downloading',
  verifying: 'downloading',
  unpacking: 'finalizing',
  converting: 'finalizing',
  installing: 'finalizing',
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
  'verifying',
]);

const FINALIZE_SUB_IDS = new Set<string>([
  'install-snapshot',
  'cleanup',
  'conversion',
  'converting',
  'installing',
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
  verifying: 'partialSyncStageVerifying',
  converting: 'partialSyncStageConverting',
  installing: 'partialSyncStageInstalling',
};

export const DOWNLOAD_PROGRESS_ANCHOR_ID = 'step-3';
const VERIFYING_DIGESTS_ID = 'step-4';
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

// Each guard narrows within its own status family and only compares against
// members of that family, so funnelling the union through both is safe; the
// two casts live here once instead of at every call site.
const isAnyRestoreCompleteStatus = (
  status: MithrilBootstrapStatus | MithrilPartialSyncStatus | MithrilSyncStatus
): boolean =>
  isRestoreCompleteStatus(status as MithrilBootstrapStatus) ||
  isMithrilPartialSyncRestoreCompleteStatus(
    status as MithrilPartialSyncStatus
  ) ||
  isMithrilSyncRestoreCompleteStatus(status as MithrilSyncStatus);

const isVerificationOrLater = (
  status: MithrilBootstrapStatus | MithrilPartialSyncStatus | MithrilSyncStatus
) =>
  status === 'verifying' ||
  status === 'unpacking' ||
  status === 'converting' ||
  status === 'installing' ||
  status === 'finalizing' ||
  isAnyRestoreCompleteStatus(status);

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

function keepInstallingActiveDuringFinalizing(
  items: MithrilProgressItem[]
): MithrilProgressItem[] {
  let hasInstallingItem = false;

  const nextItems = items.map((item) => {
    if (item.id !== 'install-snapshot') {
      return item;
    }

    hasInstallingItem = true;
    if (item.state === 'error') {
      return item;
    }

    return { ...item, state: 'active' as const };
  });

  if (hasInstallingItem) {
    return nextItems;
  }

  return [
    ...nextItems,
    {
      id: 'install-snapshot',
      label: 'install-snapshot',
      state: 'active' as const,
    },
  ];
}

function getActiveStepIndex(
  status: MithrilBootstrapStatus | MithrilPartialSyncStatus | MithrilSyncStatus
): number {
  if (isAnyRestoreCompleteStatus(status)) {
    return STEPS.length;
  }
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
  status: MithrilBootstrapStatus | MithrilPartialSyncStatus | MithrilSyncStatus
): StepState {
  if (isAnyRestoreCompleteStatus(status)) {
    return 'completed';
  }
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
        aria-hidden="true"
        className={classNames(styles.icon, styles.iconCheck)}
      />
    );
  }
  if (state === 'active') {
    return (
      <SVGInline
        svg={spinnerIcon}
        aria-hidden="true"
        className={classNames(styles.icon, styles.iconSpinner)}
      />
    );
  }
  if (state === 'error') {
    return (
      <SVGInline
        svg={closeCrossIcon}
        aria-hidden="true"
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
        aria-hidden="true"
        className={classNames(styles.subItemIcon, styles.subItemIconCheck)}
      />
    );
  }
  if (state === 'active') {
    return (
      <SVGInline
        svg={spinnerIcon}
        aria-hidden="true"
        className={classNames(styles.subItemIcon, styles.subItemIconSpinner)}
      />
    );
  }
  if (state === 'error') {
    return (
      <SVGInline
        svg={closeCrossIcon}
        aria-hidden="true"
        className={classNames(styles.subItemIcon, styles.subItemIconError)}
      />
    );
  }
  return <div className={styles.subItemPendingCircle} />;
}

function MithrilStepIndicator(props: Props, { intl }: Context) {
  const {
    status,
    variant = 'bootstrap',
    progressItems = [],
    filesDownloaded,
    filesTotal,
    snapshotBytesDownloaded,
    snapshotBytesTotal,
    snapshotSizeBytes,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
    showDownloadProgressBar = true,
  } = props;

  const activeStepIndex = getActiveStepIndex(status);
  const activeStepId: StepId | undefined =
    activeStepIndex >= 0 && activeStepIndex < STEPS.length
      ? STEPS[activeStepIndex]
      : undefined;

  const snapshotPercent =
    typeof filesTotal === 'number' && filesTotal > 0
      ? ((filesDownloaded ?? 0) / filesTotal) * 100
      : 0;

  const [showVerifyingTransition, setShowVerifyingTransition] = useState(false);

  const actualActiveSubItem = progressItems.find((i) => i.state === 'active');
  const shouldDelayVerifyingTransition =
    status === 'downloading' &&
    actualActiveSubItem?.id === DOWNLOAD_PROGRESS_ANCHOR_ID &&
    snapshotPercent >= 100;

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

  const progressItemsWithTransitions = showVerifyingTransition
    ? synthesizeVerifyingDigestProgress(progressItems)
    : progressItems;

  // Bootstrap's finalizing status still covers cleanup after the snapshot
  // move, so the don't-close caution must stay on screen; partial sync
  // reports the move as its own 'installing' item and completes it before
  // finalizing, so fabricating the bootstrap 'install-snapshot' step there
  // would show a stage that never runs in that flow.
  const displayedProgressItems =
    status === 'finalizing' && variant === 'bootstrap'
      ? keepInstallingActiveDuringFinalizing(progressItemsWithTransitions)
      : progressItemsWithTransitions;

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
    // Interrupting the snapshot move can leave the chain store half-written,
    // so the active move step carries a "don't close Daedalus" caution.
    // Bootstrap reports the move as 'install-snapshot'; partial sync as 'installing'.
    const showMoveCaution =
      (item.id === 'install-snapshot' || item.id === 'installing') && isActive;

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
          [styles.subItemWithCaution]: showMoveCaution,
        })}
      >
        <div className={styles.subItemIconContainer}>
          <SubItemIcon state={itemState} />
        </div>
        {showMoveCaution ? (
          <div className={styles.subItemLabelGroup}>
            <span className={styles.subItemLabel}>{itemLabel}</span>
            <span className={styles.subItemCaution}>
              {intl.formatMessage(messages.progressMoveCaution)}
            </span>
          </div>
        ) : (
          <span className={styles.subItemLabel}>{itemLabel}</span>
        )}
      </div>
    );
  };

  return (
    <div
      className={styles.root}
      role="list"
      aria-label={intl.formatMessage(messages.stepIndicatorLabel)}
    >
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
          showDownloadProgressBar &&
          stepId === 'downloading' &&
          state === 'active' &&
          (activeSubItemId === DOWNLOAD_PROGRESS_ANCHOR_ID ||
            ((activeSubItemId === stepId || activeSubItemId == null) &&
              (typeof filesDownloaded === 'number' ||
                typeof filesTotal === 'number' ||
                typeof ancillaryBytesDownloaded === 'number' ||
                typeof ancillaryBytesTotal === 'number')));
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
                aria-label={intl.formatMessage(
                  messages.stepIndicatorDetailsLabel,
                  { stepName: label }
                )}
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
                    {(() => {
                      const inLedgerPhase =
                        typeof ancillaryBytesTotal === 'number' &&
                        ancillaryBytesTotal > 0;
                      const hasFiles =
                        typeof filesTotal === 'number' && filesTotal > 0;

                      if (inLedgerPhase) {
                        let combinedPercent: number;
                        if (
                          isTransferComplete(
                            ancillaryBytesDownloaded,
                            ancillaryBytesTotal
                          )
                        ) {
                          combinedPercent = 100;
                        } else if (
                          snapshotSizeBytes != null &&
                          typeof filesTotal === 'number' &&
                          filesTotal > 0
                        ) {
                          combinedPercent =
                            clampPercent(
                              ((((filesDownloaded ?? 0) / filesTotal) *
                                snapshotSizeBytes +
                                (ancillaryBytesDownloaded ?? 0)) /
                                (snapshotSizeBytes + ancillaryBytesTotal)) *
                                100
                            ) ?? 0;
                        } else {
                          combinedPercent =
                            clampPercent(
                              ((ancillaryBytesDownloaded ?? 0) /
                                ancillaryBytesTotal) *
                                100
                            ) ?? 0;
                        }
                        const fmt = new Intl.NumberFormat(intl.locale);
                        const combinedDetail = intl.formatMessage(
                          messages.progressCombinedDetail,
                          {
                            snapshotDownloaded: fmt.format(
                              filesDownloaded ?? 0
                            ),
                            snapshotTotal: fmt.format(filesTotal ?? 0),
                            fastSyncDownloaded:
                              formatTransferSize(ancillaryBytesDownloaded) ??
                              '—',
                            fastSyncTotal:
                              formatTransferSize(ancillaryBytesTotal) ?? '—',
                          }
                        );
                        return (
                          <InlineProgressBar
                            label={intl.formatMessage(
                              messages.progressCombinedLabel
                            )}
                            percent={combinedPercent}
                            details={combinedDetail}
                            emphasized
                          />
                        );
                      }

                      if (variant === 'partial-sync' && hasFiles) {
                        const fmt = new Intl.NumberFormat(intl.locale);
                        const combinedDetail = intl.formatMessage(
                          messages.progressCombinedDetail,
                          {
                            snapshotDownloaded: fmt.format(
                              filesDownloaded ?? 0
                            ),
                            snapshotTotal: fmt.format(filesTotal ?? 0),
                            fastSyncDownloaded: '—',
                            fastSyncTotal: '—',
                          }
                        );
                        return (
                          <InlineProgressBar
                            label={intl.formatMessage(
                              messages.progressCombinedLabel
                            )}
                            percent={
                              isVerificationOrLater(status) ||
                              isTransferComplete(filesDownloaded, filesTotal)
                                ? 100
                                : snapshotPercent
                            }
                            details={combinedDetail}
                            emphasized
                          />
                        );
                      }

                      const hasBytes =
                        snapshotBytesTotal != null && snapshotBytesTotal > 0;
                      const bytesStr = hasBytes
                        ? `${formatTransferSize(snapshotBytesDownloaded ?? 0) ?? '—'} / ${formatTransferSize(snapshotBytesTotal) ?? '—'}`
                        : null;
                      const filesDetail = hasFiles
                        ? intl.formatMessage(
                            messages.progressSnapshotFilesDetail,
                            {
                              filesDownloaded: filesDownloaded ?? 0,
                              filesTotal,
                            }
                          )
                        : null;
                      const details =
                        filesDetail && bytesStr
                          ? `${filesDetail} · ${bytesStr}`
                          : (filesDetail ?? bytesStr ?? undefined);
                      const sizeContextText =
                        snapshotSizeBytes != null
                          ? intl.formatMessage(
                              messages.progressSnapshotSizeContext,
                              {
                                totalSize:
                                  formatTransferSize(snapshotSizeBytes) ?? '—',
                              }
                            )
                          : null;
                      return (
                        <>
                          <InlineProgressBar
                            label={intl.formatMessage(
                              messages.progressSnapshotFilesLabel
                            )}
                            percent={
                              isVerificationOrLater(status) ||
                              isTransferComplete(filesDownloaded, filesTotal)
                                ? 100
                                : snapshotPercent
                            }
                            details={details}
                            emphasized
                          />
                          {sizeContextText && <span>{sizeContextText}</span>}
                        </>
                      );
                    })()}
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
