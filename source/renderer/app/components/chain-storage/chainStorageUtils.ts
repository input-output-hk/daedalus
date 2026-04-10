import path from 'path';
import prettysize from 'prettysize';
import type { ChainStorageValidation } from '../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../types/i18nTypes';
import messages from './ChainStorage.messages';

export const formatStorageSize = (sizeBytes?: number): string | null => {
  if (sizeBytes == null || Number.isNaN(sizeBytes)) {
    return null;
  }

  return prettysize(sizeBytes);
};

export const formatAvailableSpace = (
  intl: Intl,
  availableSpaceBytes?: number
): string => {
  const formattedSize = formatStorageSize(availableSpaceBytes);

  if (formattedSize == null) {
    return intl.formatMessage(messages.availableSpaceUnknown);
  }

  return formattedSize;
};

export const getValidationMessage = (
  intl: Intl,
  validation?: ChainStorageValidation
): string | null => {
  if (!validation || validation.isValid) {
    return null;
  }

  switch (validation.reason) {
    case 'path-not-found':
      return intl.formatMessage(messages.validationPathNotFound);
    case 'not-writable':
      return intl.formatMessage(messages.validationNotWritable);
    case 'inside-state-dir':
      return intl.formatMessage(messages.validationInsideStateDir);
    case 'is-managed-child':
      return intl.formatMessage(messages.validationIsManagedChild);
    case 'insufficient-space':
      return intl.formatMessage(messages.validationInsufficientSpace);
    case 'path-is-file':
      return intl.formatMessage(messages.subdirectoryErrorConflict);
    case 'unknown':
    default:
      return intl.formatMessage(messages.validationUnknown);
  }
};

export const getStorageHelpText = (
  intl: Intl,
  validation?: ChainStorageValidation
): string | null => {
  if (!validation || !validation.isValid) {
    return null;
  }

  switch (validation.chainSubdirectoryStatus) {
    case 'will-create':
      return intl.formatMessage(messages.subdirectoryCreationNotice);
    case 'existing-directory':
      return intl.formatMessage(messages.subdirectoryWarningExists);
    default:
      return null;
  }
};

export const getComparablePath = (value?: string | null): string | null => {
  if (typeof value !== 'string' || value.trim().length === 0) {
    return null;
  }

  const resolvedPath = path.resolve(value);
  return process.platform === 'win32'
    ? resolvedPath.toLowerCase()
    : resolvedPath;
};

export const pathsAreEqual = (
  firstPath?: string | null,
  secondPath?: string | null
): boolean => {
  const comparableFirstPath = getComparablePath(firstPath);
  const comparableSecondPath = getComparablePath(secondPath);

  return (
    comparableFirstPath != null &&
    comparableSecondPath != null &&
    comparableFirstPath === comparableSecondPath
  );
};

export const createDefaultValidation = (
  defaultChainPath?: string | null,
  defaultChainStorageValidation?: ChainStorageValidation
): ChainStorageValidation =>
  defaultChainStorageValidation || {
    isValid: true,
    path: null,
    resolvedPath: defaultChainPath || undefined,
  };
