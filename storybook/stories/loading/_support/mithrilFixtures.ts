import { action } from '@storybook/addon-actions';
import type {
  ChainStorageValidation,
  ChainStorageValidationReason,
  MithrilBootstrapError,
  MithrilBootstrapErrorStage,
  MithrilBootstrapStatus,
  MithrilProgressItem,
  MithrilSnapshotItem,
} from '../../../../source/common/types/mithril-bootstrap.types';

export const defaultChainPath = '/home/ada/.local/share/Daedalus/mainnet/chain';
export const customChainPath = '/mnt/fast-ssd/daedalus-chain';

export const snapshots: Array<MithrilSnapshotItem> = [
  {
    digest: '9dbe4d8a5a978f9f7c60f3bdb09e6f7641f8764a7e78bc6273dc1201b24f1021',
    createdAt: '2026-03-23T15:05:00.000Z',
    size: 82 * 1024 * 1024 * 1024,
    cardanoNodeVersion: '10.2.1',
    network: 'mainnet',
  },
  {
    digest: '72f1fd0db3f9aa4c4d82fd1cc2fbd8ed57f2b58c8f65333873d44de2a9448157',
    createdAt: '2026-03-22T06:40:00.000Z',
    size: 79 * 1024 * 1024 * 1024,
    cardanoNodeVersion: '10.1.4',
    network: 'mainnet',
  },
  {
    digest: '15ee8eb13d7f1e5156b10967d8a91f0447b90db3a0a4e985f1c1a844ecdf0039',
    createdAt: '2026-03-20T11:10:00.000Z',
    size: 76 * 1024 * 1024 * 1024,
    cardanoNodeVersion: '10.1.0',
    network: 'mainnet',
  },
];

export const latestSnapshot = snapshots[0];
export const explicitSnapshot = snapshots[1];
export const snapshotSize = latestSnapshot.size;
export const snapshotFilesTotal = 980;
export const ancillaryBytesTotal = 9 * 1024 * 1024 * 1024;

export const defaultChainStorageValidation: ChainStorageValidation = {
  isValid: true,
  path: null,
  resolvedPath: defaultChainPath,
  availableSpaceBytes: 512 * 1024 * 1024 * 1024,
  requiredSpaceBytes: snapshotSize,
};

const validationPresetMap: Record<string, ChainStorageValidation> = {
  'valid-default': defaultChainStorageValidation,
  'valid-custom': {
    isValid: true,
    path: customChainPath,
    resolvedPath: customChainPath,
    availableSpaceBytes: 256 * 1024 * 1024 * 1024,
    requiredSpaceBytes: snapshotSize,
  },
  'existing-directory': {
    isValid: true,
    path: customChainPath,
    resolvedPath: customChainPath,
    availableSpaceBytes: 256 * 1024 * 1024 * 1024,
    requiredSpaceBytes: snapshotSize,
    chainSubdirectoryStatus: 'existing-directory',
  },
  'insufficient-space': {
    isValid: false,
    path: customChainPath,
    resolvedPath: customChainPath,
    availableSpaceBytes: 32 * 1024 * 1024 * 1024,
    requiredSpaceBytes: snapshotSize,
    reason: 'insufficient-space',
  },
  'not-writable': {
    isValid: false,
    path: customChainPath,
    resolvedPath: customChainPath,
    availableSpaceBytes: 256 * 1024 * 1024 * 1024,
    requiredSpaceBytes: snapshotSize,
    reason: 'not-writable',
  },
  'inside-state-dir': {
    isValid: false,
    path: defaultChainPath,
    resolvedPath: defaultChainPath,
    availableSpaceBytes: 256 * 1024 * 1024 * 1024,
    requiredSpaceBytes: snapshotSize,
    reason: 'inside-state-dir',
  },
};

export type ValidationPresetName = keyof typeof validationPresetMap;

export const validationPresetOptions: Record<string, ValidationPresetName> = {
  'Valid - Default': 'valid-default',
  'Valid - Custom': 'valid-custom',
  'Valid - Existing Directory': 'existing-directory',
  'Invalid - Insufficient Space': 'insufficient-space',
  'Invalid - Not Writable': 'not-writable',
  'Invalid - Inside State Dir': 'inside-state-dir',
};

export const getValidationPreset = (
  presetName: ValidationPresetName,
  pathOverride?: string | null,
  requiredSpaceBytes?: number,
  availableSpaceBytes?: number
): ChainStorageValidation => {
  const preset = validationPresetMap[presetName];
  const nextPath =
    pathOverride === undefined ? preset.path : pathOverride || preset.path;
  const resolvedRequiredSpaceBytes = requiredSpaceBytes || snapshotSize;

  return {
    ...preset,
    path: nextPath,
    resolvedPath:
      nextPath == null ? defaultChainPath : preset.resolvedPath || nextPath,
    requiredSpaceBytes: resolvedRequiredSpaceBytes,
    availableSpaceBytes:
      availableSpaceBytes === undefined
        ? preset.availableSpaceBytes
        : availableSpaceBytes,
  };
};

export const getValidationPresetForReason = (
  reason?: ChainStorageValidationReason
): ValidationPresetName => {
  switch (reason) {
    case 'insufficient-space':
      return 'insufficient-space';
    case 'not-writable':
      return 'not-writable';
    case 'inside-state-dir':
      return 'inside-state-dir';
    default:
      return 'valid-custom';
  }
};

// The bootstrap service emits progress items cumulatively — an item exists
// only once its stage has been reached (never as a pending placeholder), each
// new item completes the previous active one, and labels are wire ids that
// MithrilStepIndicator maps to user-facing copy. The sequence per status:
// preparing → downloading (structural) → step-1..step-3 (download) →
// step-4..step-7 (verifying) → conversion (converting) → install-snapshot
// (unpacking) → cleanup (finalizing) → everything completed.
const BOOTSTRAP_ITEM_SEQUENCE = [
  { id: 'preparing', reachedBy: 'preparing' },
  { id: 'downloading', reachedBy: 'downloading' },
  { id: 'step-1', reachedBy: 'downloading' },
  { id: 'step-2', reachedBy: 'downloading' },
  { id: 'step-3', reachedBy: 'downloading' },
  { id: 'step-4', reachedBy: 'verifying' },
  { id: 'step-5', reachedBy: 'verifying' },
  { id: 'step-6', reachedBy: 'verifying' },
  { id: 'step-7', reachedBy: 'verifying' },
  { id: 'conversion', reachedBy: 'converting' },
  { id: 'install-snapshot', reachedBy: 'unpacking' },
  { id: 'cleanup', reachedBy: 'finalizing' },
] as const;

// Canonical mid-status frames: how far through the sequence each status has
// progressed when a tester lands on it (downloading sits on the snapshot
// transfer, verifying on the long database check).
const BOOTSTRAP_STATUS_REACHED_ID: Partial<
  Record<MithrilBootstrapStatus, string>
> = {
  preparing: 'preparing',
  downloading: 'step-3',
  verifying: 'step-5',
  converting: 'conversion',
  unpacking: 'install-snapshot',
  finalizing: 'cleanup',
};

export const getBootstrapProgressItems = (
  status: MithrilBootstrapStatus
): Array<MithrilProgressItem> => {
  if (status === 'starting-node' || status === 'completed') {
    return BOOTSTRAP_ITEM_SEQUENCE.map(({ id }) => ({
      id,
      label: id,
      state: 'completed' as const,
    }));
  }

  const reachedId = BOOTSTRAP_STATUS_REACHED_ID[status];
  if (!reachedId) {
    return [];
  }
  const reachedIndex = BOOTSTRAP_ITEM_SEQUENCE.findIndex(
    ({ id }) => id === reachedId
  );
  return BOOTSTRAP_ITEM_SEQUENCE.slice(0, reachedIndex + 1).map(
    ({ id }, index) => ({
      id,
      label: id,
      state:
        index < reachedIndex ? ('completed' as const) : ('active' as const),
    })
  );
};

const errorPresetMap: Record<
  MithrilBootstrapErrorStage,
  MithrilBootstrapError
> = {
  download: {
    stage: 'download',
    code: 'MITHRIL_DOWNLOAD_TIMEOUT',
    message:
      'Snapshot download stalled after repeated retries against the selected aggregator.',
    logPath:
      '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-bootstrap.log',
  },
  verify: {
    stage: 'verify',
    code: 'MITHRIL_DIGEST_MISMATCH',
    message:
      'Downloaded artifacts did not match the expected Mithril digest set for this certificate.',
    logPath:
      '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-bootstrap.log',
  },
  convert: {
    stage: 'convert',
    code: 'MITHRIL_CONVERSION_FAILED',
    message:
      'The restored ledger state could not be converted into the local node storage layout.',
    logPath:
      '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-bootstrap.log',
  },
  'node-start': {
    stage: 'node-start',
    code: 'CARDANO_NODE_BOOT_FAILED',
    message:
      'cardano-node exited before opening its local socket after the snapshot restore completed.',
    logPath: '/home/ada/.local/share/Daedalus/mainnet/Logs/cardano-node.log',
  },
};

export const errorStageOptions: Record<string, MithrilBootstrapErrorStage> = {
  Download: 'download',
  Verify: 'verify',
  Convert: 'convert',
  'Node Start': 'node-start',
};

export const getErrorPreset = (
  stage: MithrilBootstrapErrorStage
): MithrilBootstrapError => errorPresetMap[stage];

export const createBootstrapStartedAt = (minutesAgo: number) =>
  Date.now() - minutesAgo * 60 * 1000;

export const bootstrapActions = {
  onSelectSnapshot: action('onSelectSnapshot'),
  onAccept: action('onAccept'),
  onDecline: action('onDecline'),
  onWipeRetry: action('onWipeRetry'),
  onCancel: action('onCancel'),
  onConfirmStorageLocation: action('onConfirmStorageLocation'),
  onReturnToStorageLocation: action('onReturnToStorageLocation'),
  onOpenExternalLink: action('onOpenExternalLink'),
  onSetChainStorageDirectory: action('onSetChainStorageDirectory'),
  onResetChainStorageDirectory: action('onResetChainStorageDirectory'),
  onValidateChainStorageDirectory: action('onValidateChainStorageDirectory'),
};
