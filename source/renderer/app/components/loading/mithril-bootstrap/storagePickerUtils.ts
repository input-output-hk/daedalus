import path from 'path';
import prettysize from 'prettysize';
import type { ChainStorageValidation } from '../../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';

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
    return intl.formatMessage(messages.storageAvailableSpaceUnknown);
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
      return intl.formatMessage(messages.storageValidationPathNotFound);
    case 'not-writable':
      return intl.formatMessage(messages.storageValidationNotWritable);
    case 'inside-state-dir':
      return intl.formatMessage(messages.storageValidationInsideStateDir);
    case 'insufficient-space':
      return intl.formatMessage(messages.storageValidationInsufficientSpace);
    case 'unknown':
    default:
      return intl.formatMessage(messages.storageValidationUnknown);
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
