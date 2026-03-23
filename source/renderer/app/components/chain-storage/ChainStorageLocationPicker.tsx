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
import messages from './ChainStorageMessages';
import styles from './ChainStorageLocationPicker.scss';
import {
  formatStorageSize,
  formatAvailableSpace,
  getValidationMessage,
  pathsAreEqual,
  createDefaultValidation,
} from './chainStorageUtils';

interface Props {
  customChainPath?: string | null;
  defaultChainPath?: string | null;
  defaultChainStorageValidation?: ChainStorageValidation;
  chainStorageValidation?: ChainStorageValidation;
  estimatedRequiredSpaceBytes?: number;
  isChainStorageLoading?: boolean;
  onSetChainStorageDirectory?: (...args: [string | null]) => Promise<unknown>;
  onResetChainStorageDirectory?(): Promise<unknown>;
  onValidateChainStorageDirectory?: (
    ...args: [string]
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

function ChainStorageLocationPicker(props: Props, { intl }: Context) {
  const {
    customChainPath,
    defaultChainPath,
    defaultChainStorageValidation,
    chainStorageValidation,
    estimatedRequiredSpaceBytes,
    isChainStorageLoading,
    onSetChainStorageDirectory,
    onResetChainStorageDirectory,
    onValidateChainStorageDirectory,
    onConfirmStorageLocation,
  } = props;
  const [isSelectingDirectory, setIsSelectingDirectory] = React.useState(false);
  const [isApplyingStorageChange, setIsApplyingStorageChange] = React.useState(
    false
  );
  const [candidate, setCandidate] = React.useState<StorageCandidate | null>(
    null
  );
  const [
    selectionValidation,
    setSelectionValidation,
  ] = React.useState<ChainStorageValidation | null>(null);
  const isMountedRef = React.useRef(true);

  React.useEffect(
    () => () => {
      isMountedRef.current = false;
    },
    []
  );

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
    defaultChainPath,
    chainStorageValidation,
    defaultChainStorageValidation,
  ]);

  const committedValidation =
    customChainPath == null
      ? defaultValidation
      : chainStorageValidation || {
          isValid: true,
          path: customChainPath,
        };
  const effectiveSelection = candidate || {
    path: customChainPath,
    validation: committedValidation,
  };

  const currentPath = effectiveSelection.path || defaultChainPath;
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
  const isCurrentStorageInvalid =
    customChainPath != null &&
    chainStorageValidation != null &&
    !chainStorageValidation.isValid &&
    chainStorageValidation.path != null &&
    pathsAreEqual(chainStorageValidation.path, customChainPath);
  const hasValidCandidateOverride =
    candidate != null && candidate.validation.isValid;
  const isBusy =
    Boolean(isSelectingDirectory) ||
    Boolean(isApplyingStorageChange) ||
    Boolean(isChainStorageLoading);
  const isContinueDisabled =
    isBusy || (isCurrentStorageInvalid && !hasValidCandidateOverride);
  const canResetToDefault = effectiveSelection.path != null;
  const inputClasses = classNames(styles.storageInput, {
    [styles.error]: validationMessage != null,
  });
  const displayedPath =
    currentPath || intl.formatMessage(messages.defaultLocationLabel);
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
        setCandidate({
          path: validation.path ?? selectedPath,
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

    const nextPath = candidate ? candidate.path : customChainPath;
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
    } finally {
      if (isMountedRef.current) {
        setIsApplyingStorageChange(false);
      }
    }
  };

  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1>{intl.formatMessage(messages.title)}</h1>
        <p>{storageDescription}</p>
      </div>

      <div className={styles.storageField}>
        <p className={styles.storageSubtext}>
          {intl.formatMessage(messages.availableSpaceSubtext, {
            availableSpace,
          })}
        </p>
        <div className={styles.storageInputWrapper}>
          <input
            type="text"
            className={inputClasses}
            value={displayedPath}
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
            <SVGInline svg={penIcon} className={styles.penIcon} />
          </button>
        </div>
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

      {validationMessage && (
        <div className={styles.validationMessage}>{validationMessage}</div>
      )}

      {isApplyingStorageChange && (
        <div className={styles.statusMessage}>
          <SVGInline
            svg={spinnerIcon}
            className={classNames(styles.spinnerIcon, styles.spinnerActive)}
          />
          <span>{intl.formatMessage(messages.updating)}</span>
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
