import type { Intl } from '../types/i18nTypes';
import {
  partialSyncStartFailureMessage,
  resolvePartialSyncErrorCopyByCode,
} from '../components/loading/mithril-bootstrap/partialSyncErrorCopy';

// Single extraction point for messages carried by unknown thrown values, so
// the Mithril start-failure surfaces cannot drift apart again. Returns null
// when the value carries no usable message.
export const extractMithrilErrorMessage = (error: unknown): string | null => {
  if (error instanceof Error) {
    return error.message.trim() ? error.message : null;
  }
  if (
    error &&
    typeof error === 'object' &&
    typeof (error as { message?: unknown }).message === 'string' &&
    (error as { message: string }).message.trim()
  ) {
    return (error as { message: string }).message;
  }
  return null;
};

// Start rejections carry backend codes, not user copy: resolve a known code to localized copy, else the shared fallback, so the raw message never reaches the user.
export const getMithrilStartErrorMessage = (
  error: unknown,
  intl: Intl
): string => {
  const message = extractMithrilErrorMessage(error);
  const copy = message
    ? resolvePartialSyncErrorCopyByCode(message.trim())
    : undefined;
  return intl.formatMessage(copy ? copy.title : partialSyncStartFailureMessage);
};

// Store-side variant (no intl context): pass a real Error through, else wrap the extracted message; an empty message signals the component to use the shared fallback.
export const toMithrilStartError = (error: unknown): Error =>
  error instanceof Error
    ? error
    : new Error(extractMithrilErrorMessage(error) ?? '');
