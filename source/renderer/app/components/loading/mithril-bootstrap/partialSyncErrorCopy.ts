import type { MessageDescriptor } from 'react-intl';
import messages from './MithrilBootstrap.messages';
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncErrorCode,
  MithrilPartialSyncErrorStage,
  MithrilPartialSyncStatus,
} from '../../../../../common/types/mithril-partial-sync.types';

export type PartialSyncErrorCopy = {
  title: MessageDescriptor;
  hint: MessageDescriptor;
};

const NO_CERTIFIED_RANGE: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorNoCertifiedRangeTitle,
  hint: messages.partialSyncErrorNoCertifiedRangeHint,
};
const LATEST_DRIFT: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorLatestDriftTitle,
  hint: messages.partialSyncErrorLatestDriftHint,
};
const STAGED_DB_INVALID: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorStagedDbInvalidTitle,
  hint: messages.partialSyncErrorStagedDbInvalidHint,
};
const DOWNLOAD_FAILED: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorDownloadFailedTitle,
  hint: messages.partialSyncErrorDownloadFailedHint,
};
const CONVERSION_FAILED: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorConversionFailedTitle,
  hint: messages.partialSyncErrorConversionFailedHint,
};
const METADATA_UNAVAILABLE: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorMetadataUnavailableTitle,
  hint: messages.partialSyncErrorMetadataUnavailableHint,
};
const INSUFFICIENT_DISK_SPACE: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorInsufficientDiskSpaceTitle,
  hint: messages.partialSyncErrorInsufficientDiskSpaceHint,
};

const FAILED: PartialSyncErrorCopy = {
  title: messages.partialSyncFailedTitle,
  hint: messages.partialSyncFailedHint,
};
const CANCELLED: PartialSyncErrorCopy = {
  title: messages.partialSyncCancelledTitle,
  hint: messages.partialSyncCancelledHint,
};

// 1st tier — exact backend code (wins; code disambiguates a code shared across stages).
const COPY_BY_CODE: Record<MithrilPartialSyncErrorCode, PartialSyncErrorCopy> =
  {
    PARTIAL_SYNC_NO_CERTIFIED_RANGE: NO_CERTIFIED_RANGE,
    PARTIAL_SYNC_LATEST_DRIFT: LATEST_DRIFT,
    PARTIAL_SYNC_STAGED_DB_INVALID: STAGED_DB_INVALID,
    PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED: DOWNLOAD_FAILED,
    PARTIAL_SYNC_CONVERSION_FAILED: CONVERSION_FAILED,
    PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE: INSUFFICIENT_DISK_SPACE,
    PARTIAL_SYNC_METADATA_UNAVAILABLE: METADATA_UNAVAILABLE,
    PARTIAL_SYNC_DISABLED: FAILED,
    PARTIAL_SYNC_ALREADY_RUNNING: FAILED,
    PARTIAL_SYNC_START_NOT_ALLOWED: FAILED,
    PARTIAL_SYNC_LAYOUT_UNSUPPORTED: FAILED,
    PARTIAL_SYNC_CANCEL_NOT_ALLOWED: FAILED,
    PARTIAL_SYNC_RECOVERY_NOT_ALLOWED: FAILED,
  };

// 2nd tier — stage, for a code-less failure at a meaningful phase (reuses the same descriptors,
// no extra i18n keys). preparing/finalizing/starting-node/stopping-node intentionally omitted
// (ambiguous or no bespoke copy) → fall through to generic.
const COPY_BY_STAGE: Partial<
  Record<MithrilPartialSyncErrorStage, PartialSyncErrorCopy>
> = {
  downloading: DOWNLOAD_FAILED,
  verifying: STAGED_DB_INVALID,
  converting: CONVERSION_FAILED,
  installing: STAGED_DB_INVALID,
};

export function resolvePartialSyncErrorCopy(
  status: MithrilPartialSyncStatus,
  error?: MithrilPartialSyncError | null
): PartialSyncErrorCopy {
  if (status === 'cancelled') {
    return CANCELLED;
  }
  const byCode = error?.code
    ? COPY_BY_CODE[error.code as MithrilPartialSyncErrorCode]
    : undefined;
  if (byCode) {
    return byCode;
  }
  const byStage = error?.stage ? COPY_BY_STAGE[error.stage] : undefined;
  if (byStage) {
    return byStage;
  }
  return FAILED;
}

// Single shared fallback for a rejected Mithril Sync start request; the
// renderer must never surface the raw rejection message.
export const partialSyncStartFailureMessage: MessageDescriptor =
  messages.partialSyncStartFailure;

// Start rejections carry stable backend codes as their message. Expose a
// code-keyed lookup so the start-failure seams can reuse this copy map
// without the map itself leaking out of this module.
export const resolvePartialSyncErrorCopyByCode = (
  code: string
): PartialSyncErrorCopy | undefined =>
  COPY_BY_CODE[code as MithrilPartialSyncErrorCode];
