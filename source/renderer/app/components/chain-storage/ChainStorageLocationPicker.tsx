import classNames from 'classnames';
import React from 'react';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import SVGInline from 'react-svg-inline';
import type { ChainStorageValidation } from '../../../../common/types/mithril-bootstrap.types';
import type { FileDialogRequestParams } from '../../../../common/types/file-dialog.types';
import type { Intl } from '../../types/i18nTypes';
import penIcon from '../../assets/images/pen.inline.svg';
import spinnerIcon from '../../assets/images/spinner-universal.inline.svg';
import { showOpenDialogChannel } from '../../ipc/show-file-dialog-channels';
import messages from './ChainStorage.messages';
import styles from './ChainStorageLocationPicker.scss';
import {
  formatStorageSize,
  formatAvailableSpace,
  getValidationMessage,
  getStorageHelpText,
  pathsAreEqual,
  createDefaultValidation,
  getManagedChainDisplayPath,
} from './chainStorageUtils';
import { MITHRIL_CHAIN_STORAGE_HEADING_ID } from '../loading/mithril-bootstrap/accessibilityIds';

interface Props {
  customChainPath?: string | null;
  defaultChainPath?: string | null;
  defaultChainStorageValidation?: ChainStorageValidation;
  chainStorageValidation?: ChainStorageValidation;
  pendingChainPath?: string | null;
  isRecoveryFallback?: boolean;
  estimatedRequiredSpaceBytes?: number;
  isChainStorageLoading?: boolean;
  onSetChainStorageDirectory?: (arg: string | null) => Promise<unknown>;
  onResetChainStorageDirectory?(): Promise<unknown>;
  onValidateChainStorageDirectory?: (
    arg: string
  ) => Promise<ChainStorageValidation>;
  onConfirmStorageLocation?(): void;
}

interface Context {
  intl: Intl;
}

type StorageCandidate = {
  path: string | null;
  validation: ChainStorageValidation;
};

const STORAGE_LOCATION_INPUT_ID = 'chain-storage-location-input';
const STORAGE_LOCATION_HELP_TEXT_ID = 'chain-storage-help-text';
const STORAGE_LOCATION_RECOVERY_NOTICE_ID =
  'chain-storage-location-recovery-notice';
const STORAGE_LOCATION_DATA_FOUND_ID = 'chain-storage-location-data-found';
const STORAGE_LOCATION_VALIDATION_MESSAGE_ID =
  'chain-storage-location-validation-message';

function ChainStorageLocationPicker(props: Props, { intl }: Context) {
  const {
    customChainPath,
    defaultChainPath,
    defaultChainStorageValidation,
    chainStorageValidation,
    pendingChainPath,
    isRecoveryFallback,
    estimatedRequiredSpaceBytes,
    isChainStorageLoading,
    onSetChainStorageDirectory,
    onResetChainStorageDirectory,
    onValidateChainStorageDirectory,
    onConfirmStorageLocation,
  } = props;
  const [isSelectingDirectory, setIsSelectingDirectory] = React.useState(false);
  const [isApplyingStorageChange, setIsApplyingStorageChange] =
    React.useState(false);
  const [candidate, setCandidate] = React.useState<StorageCandidate | null>(
    null
  );
  const [selectionValidation, setSelectionValidation] =
    React.useState<ChainStorageValidation | null>(null);
  const isMountedRef = React.useRef(true);
  const recoveryNoticeRef = React.useRef<HTMLDivElement | null>(null);
  const [isRecoveryNoticeDismissed, setIsRecoveryNoticeDismissed] =
    React.useState(false);

  React.useEffect(
    () => () => {
      isMountedRef.current = false;
    },
    []
  );

  React.useEffect(() => {
    if (isRecoveryFallback) {
      setIsRecoveryNoticeDismissed(false);
    }
  }, [isRecoveryFallback]);

  const defaultValidation = React.useMemo(
    () =>
      createDefaultValidation(defaultChainPath, defaultChainStorageValidation),
    [defaultChainPath, defaultChainStorageValidation]
  );

  React.useEffect(() => {
    setCandidate(null);
    setSelectionValidation(null);
  }, [
    customChainPath,
    pendingChainPath,
    defaultChainPath,
    chainStorageValidation,
    defaultChainStorageValidation,
  ]);

  const shouldShowRecoveryNotice =
    Boolean(isRecoveryFallback) && !isRecoveryNoticeDismissed;

  React.useEffect(() => {
    if (shouldShowRecoveryNotice) {
      recoveryNoticeRef.current?.focus();
    }
  }, [shouldShowRecoveryNotice]);

  const committedPath =
    pendingChainPath !== undefined ? pendingChainPath : customChainPath;
  const committedValidation =
    committedPath == null
      ? defaultValidation
      : chainStorageValidation || {
          isValid: true,
          path: committedPath,
        };
  const effectiveSelection = candidate || {
    path: committedPath,
    validation: committedValidation,
  };

  const displayedValidation =
    selectionValidation != null && !selectionValidation.isValid
      ? selectionValidation
      : effectiveSelection.validation;
  const estimatedRequiredSpace =
    typeof estimatedRequiredSpaceBytes === 'number' &&
    estimatedRequiredSpaceBytes > 0
      ? formatStorageSize(estimatedRequiredSpaceBytes)
      : null;
  const storageDescription =
    estimatedRequiredSpace == null
      ? intl.formatMessage(messages.descriptionLargeRequirement)
      : intl.formatMessage(messages.descriptionWithRequiredSpaceEstimate, {
          requiredSpace: estimatedRequiredSpace,
        });
  const validationMessage = getValidationMessage(intl, displayedValidation);
  const storageHelpText = getStorageHelpText(intl, displayedValidation);
  const hasBlockchainData =
    displayedValidation.isValid &&
    displayedValidation.chainSubdirectoryStatus === 'existing-directory';
  const isCurrentStorageInvalid =
    committedPath != null &&
    chainStorageValidation != null &&
    !chainStorageValidation.isValid &&
    chainStorageValidation.path != null &&
    pathsAreEqual(chainStorageValidation.path, committedPath);
  const hasValidCandidateOverride =
    candidate != null && candidate.validation.isValid;
  const hasInvalidCandidate =
    candidate != null && !candidate.validation.isValid;
  const isBusy =
    Boolean(isSelectingDirectory) ||
    Boolean(isApplyingStorageChange) ||
    Boolean(isChainStorageLoading);
  const isContinueDisabled =
    isBusy ||
    hasInvalidCandidate ||
    (isCurrentStorageInvalid && !hasValidCandidateOverride);
  const canResetToDefault = effectiveSelection.path != null;
  const inputClasses = classNames(styles.storageInput, {
    [styles.error]: validationMessage != null,
  });
  const describedByIds = [
    storageHelpText && !hasBlockchainData
      ? STORAGE_LOCATION_HELP_TEXT_ID
      : undefined,
    validationMessage ? STORAGE_LOCATION_VALIDATION_MESSAGE_ID : undefined,
    shouldShowRecoveryNotice ? STORAGE_LOCATION_RECOVERY_NOTICE_ID : undefined,
    hasBlockchainData ? STORAGE_LOCATION_DATA_FOUND_ID : undefined,
  ].filter(Boolean);
  const describedById =
    describedByIds.length > 0 ? describedByIds.join(' ') : undefined;
  const isInvalidDraft =
    candidate != null &&
    !candidate.validation.isValid &&
    candidate.path != null;
  const displayedPath = isInvalidDraft
    ? candidate.path
    : getManagedChainDisplayPath(effectiveSelection.path, defaultChainPath) ||
      intl.formatMessage(messages.defaultLocationLabel);
  const availableSpace = formatAvailableSpace(
    intl,
    effectiveSelection.validation.availableSpaceBytes
  );

  const handleChooseDirectory = async () => {
    if (isBusy || !onValidateChainStorageDirectory) {
      return;
    }

    setIsSelectingDirectory(true);

    try {
      const params: FileDialogRequestParams = {
        buttonLabel: intl.formatMessage(messages.chooseDirectory),
        defaultPath: effectiveSelection.path || undefined,
        properties: ['openDirectory'],
        title: intl.formatMessage(messages.chooseDirectory),
      };
      const { canceled, filePaths } = await showOpenDialogChannel.send(params);

      if (!isMountedRef.current) {
        return;
      }

      if (canceled || !filePaths || filePaths.length === 0) {
        return;
      }

      const [selectedPath] = filePaths;
      if (!selectedPath) {
        return;
      }

      if (pathsAreEqual(selectedPath, defaultChainPath)) {
        setIsRecoveryNoticeDismissed(true);
        setCandidate({
          path: null,
          validation: defaultValidation,
        });
        setSelectionValidation(defaultValidation);
        return;
      }

      if (
        pathsAreEqual(selectedPath, customChainPath) &&
        !isCurrentStorageInvalid &&
        candidate == null
      ) {
        return;
      }

      if (
        candidate?.path != null &&
        pathsAreEqual(selectedPath, candidate.path)
      ) {
        return;
      }

      const validation = await onValidateChainStorageDirectory(selectedPath);

      if (!isMountedRef.current) {
        return;
      }

      setSelectionValidation(validation);

      if (validation.isValid) {
        setIsRecoveryNoticeDismissed(true);
        setCandidate({
          path: validation.path ?? selectedPath,
          validation,
        });
      } else {
        setCandidate({
          path: selectedPath,
          validation,
        });
      }
    } finally {
      if (isMountedRef.current) {
        setIsSelectingDirectory(false);
      }
    }
  };

  const handleResetToDefault = async () => {
    if (isBusy) {
      return;
    }

    setIsRecoveryNoticeDismissed(true);
    setCandidate({
      path: null,
      validation: defaultValidation,
    });
    setSelectionValidation(defaultValidation);
  };

  const handleContinue = async () => {
    if (isContinueDisabled || !onConfirmStorageLocation) {
      return;
    }

    setIsRecoveryNoticeDismissed(true);

    const nextPath = candidate ? candidate.path : committedPath;
    const shouldResetToDefault = nextPath == null && customChainPath != null;
    const shouldSetCustomPath =
      nextPath != null &&
      (!pathsAreEqual(nextPath, customChainPath) || isCurrentStorageInvalid);

    if (shouldResetToDefault && !onResetChainStorageDirectory) {
      return;
    }

    if (shouldSetCustomPath && !onSetChainStorageDirectory) {
      return;
    }

    setIsApplyingStorageChange(true);

    try {
      let appliedValidation: unknown = null;

      if (shouldResetToDefault) {
        appliedValidation = await onResetChainStorageDirectory?.();
      } else if (shouldSetCustomPath && nextPath) {
        appliedValidation = await onSetChainStorageDirectory?.(nextPath);
      }

      if (!isMountedRef.current) {
        return;
      }

      const nextValidation = appliedValidation as ChainStorageValidation | null;
      if (nextValidation != null && !nextValidation.isValid) {
        setSelectionValidation(nextValidation);
        return;
      }

      onConfirmStorageLocation();
    } catch {
      if (isMountedRef.current) {
        setSelectionValidation({
          isValid: false,
          path: nextPath ?? null,
          reason: 'unknown',
          message: 'An unexpected error occurred. Please try again.',
        });
      }
    } finally {
      if (isMountedRef.current) {
        setIsApplyingStorageChange(false);
      }
    }
  };

  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1 id={MITHRIL_CHAIN_STORAGE_HEADING_ID}>
          {intl.formatMessage(messages.title)}
        </h1>
        <p>{storageDescription}</p>
      </div>

      <div className={styles.storageField}>
        <label
          className={styles.screenReaderOnly}
          htmlFor={STORAGE_LOCATION_INPUT_ID}
        >
          {intl.formatMessage(messages.directoryLabel)}
        </label>
        <p className={styles.storageSubtext}>
          {intl.formatMessage(messages.availableSpaceSubtext, {
            availableSpace,
          })}
        </p>
        {shouldShowRecoveryNotice && (
          <div
            ref={recoveryNoticeRef}
            className={styles.storageSubtext}
            id={STORAGE_LOCATION_RECOVERY_NOTICE_ID}
            role="status"
            aria-live="polite"
            tabIndex={-1}
          >
            {intl.formatMessage(messages.recoveryNotice)}
          </div>
        )}
        <div className={styles.storageInputWrapper}>
          <input
            id={STORAGE_LOCATION_INPUT_ID}
            type="text"
            className={inputClasses}
            value={displayedPath}
            aria-describedby={describedById}
            aria-invalid={validationMessage != null}
            placeholder={
              defaultChainPath ||
              intl.formatMessage(messages.defaultLocationLabel)
            }
            readOnly
          />
          <button
            type="button"
            className={styles.selectDirectoryButton}
            disabled={isBusy}
            onClick={handleChooseDirectory}
            aria-label={intl.formatMessage(messages.chooseDirectory)}
            title={intl.formatMessage(messages.chooseDirectory)}
          >
            <SVGInline
              svg={penIcon}
              className={styles.penIcon}
              aria-hidden="true"
            />
          </button>
        </div>
        {storageHelpText && !hasBlockchainData && (
          <p
            className={classNames(
              styles.storageSubtext,
              styles.storageHelpText
            )}
            id={STORAGE_LOCATION_HELP_TEXT_ID}
          >
            {storageHelpText}
          </p>
        )}
        {hasBlockchainData && (
          <p
            className={classNames(
              styles.storageSubtext,
              styles.storageHelpText
            )}
            id={STORAGE_LOCATION_DATA_FOUND_ID}
          >
            {intl.formatMessage(messages.dataFoundNotice)}
          </p>
        )}
        <div className={styles.resetActionRow}>
          {canResetToDefault && (
            <button
              type="button"
              className={styles.inlineAction}
              disabled={isBusy}
              onClick={handleResetToDefault}
            >
              {intl.formatMessage(messages.resetToDefault)}
            </button>
          )}
        </div>
      </div>

      {(validationMessage || isApplyingStorageChange) && (
        <div
          className={styles.feedbackRegion}
          role="status"
          aria-live="polite"
          aria-atomic="true"
        >
          {validationMessage && (
            <div
              className={styles.validationMessage}
              id={STORAGE_LOCATION_VALIDATION_MESSAGE_ID}
            >
              {validationMessage}
            </div>
          )}

          {isApplyingStorageChange && (
            <div className={styles.statusMessage}>
              <SVGInline
                svg={spinnerIcon}
                className={classNames(styles.spinnerIcon, styles.spinnerActive)}
                aria-hidden="true"
              />
              <span>{intl.formatMessage(messages.updating)}</span>
            </div>
          )}
        </div>
      )}

      <div className={styles.actions}>
        <Button
          className={styles.primaryAction}
          skin={ButtonSkin}
          disabled={isContinueDisabled}
          label={intl.formatMessage(messages.continue)}
          onClick={handleContinue}
        />
      </div>
    </div>
  );
}

ChainStorageLocationPicker.contextTypes = {
  intl: intlShape.isRequired,
};

export default ChainStorageLocationPicker;
