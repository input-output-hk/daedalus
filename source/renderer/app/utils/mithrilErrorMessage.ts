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

// For components: the user-facing line for a rejected start request. Start
// rejections carry stable backend codes, not user copy — a known code resolves
// to its localized copy and anything else gets the shared fallback, so the raw
// rejection message never surfaces to the user.
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

// For the store, which throws instead of rendering (stores have no intl
// context): a real Error passes through unchanged; anything else is wrapped
// around the extracted message. An empty message means "no usable message" —
// the catching component resolves it to the shared fallback.
export const toMithrilStartError = (error: unknown): Error =>
  error instanceof Error
    ? error
    : new Error(extractMithrilErrorMessage(error) ?? '');
