import React, { useEffect, useMemo, useState } from 'react';
import type {
  ChainStorageValidation,
  MithrilBootstrapError,
  MithrilBootstrapStatus,
  MithrilProgressItem,
  MithrilSnapshotItem,
} from '../../../../source/common/types/mithril-bootstrap.types';
import ChainStorageLocationPicker from '../../../../source/renderer/app/components/chain-storage/ChainStorageLocationPicker';
import MithrilBootstrap from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap';
import MithrilDecisionView from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilDecisionView';
import {
  bootstrapActions,
  defaultChainStorageValidation,
  getValidationPreset,
  latestSnapshot,
} from './mithrilFixtures';
import type { ValidationPresetName } from './mithrilFixtures';

const resolveSelectedSnapshot = (
  allSnapshots: Array<MithrilSnapshotItem>,
  selectedDigest?: string | null
) => {
  if (selectedDigest == null) {
    return allSnapshots[0] || null;
  }

  return (
    allSnapshots.find((snapshot) => snapshot.digest === selectedDigest) ||
    allSnapshots[0] ||
    null
  );
};

interface ManagedMithrilDecisionViewProps {
  snapshots: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  isFetchingSnapshots: boolean;
  customChainPath?: string | null;
  defaultChainPath?: string | null;
  includeReturnToStorageAction?: boolean;
}

export function ManagedMithrilDecisionView({
  snapshots,
  selectedDigest = null,
  isFetchingSnapshots,
  customChainPath,
  defaultChainPath,
  includeReturnToStorageAction = true,
}: ManagedMithrilDecisionViewProps) {
  const [currentDigest, setCurrentDigest] = useState<string | null>(
    selectedDigest
  );

  useEffect(() => {
    setCurrentDigest(selectedDigest);
  }, [selectedDigest, snapshots]);

  const selectedSnapshot = useMemo(
    () => resolveSelectedSnapshot(snapshots, currentDigest),
    [currentDigest, snapshots]
  );

  return (
    <MithrilDecisionView
      snapshots={snapshots}
      selectedDigest={currentDigest}
      selectedSnapshot={selectedSnapshot}
      isFetchingSnapshots={isFetchingSnapshots}
      customChainPath={customChainPath}
      defaultChainPath={defaultChainPath}
      onSelectSnapshot={(value) => {
        bootstrapActions.onSelectSnapshot(value);
        setCurrentDigest(value);
      }}
      onReturnToStorageLocation={
        includeReturnToStorageAction
          ? () => bootstrapActions.onReturnToStorageLocation()
          : undefined
      }
      onAccept={() => bootstrapActions.onAccept()}
      onDecline={() => bootstrapActions.onDecline()}
    />
  );
}

interface ManagedChainStorageLocationPickerProps {
  customChainPath?: string | null;
  defaultChainPath?: string | null;
  validationPreset: ValidationPresetName;
  defaultChainStorageValidation?: ChainStorageValidation;
  estimatedRequiredSpaceBytes?: number;
  availableSpaceBytes?: number;
  isRecoveryFallback?: boolean;
  isChainStorageLoading?: boolean;
}

export function ManagedChainStorageLocationPicker({
  customChainPath = null,
  defaultChainPath,
  validationPreset,
  defaultChainStorageValidation:
    managedDefaultValidation = defaultChainStorageValidation,
  estimatedRequiredSpaceBytes,
  availableSpaceBytes,
  isRecoveryFallback,
  isChainStorageLoading,
}: ManagedChainStorageLocationPickerProps) {
  const [currentCustomPath, setCurrentCustomPath] = useState<string | null>(
    customChainPath
  );
  const [currentValidation, setCurrentValidation] = useState<
    ChainStorageValidation | undefined
  >(
    customChainPath == null
      ? undefined
      : getValidationPreset(
          validationPreset,
          customChainPath,
          estimatedRequiredSpaceBytes || latestSnapshot.size,
          availableSpaceBytes
        )
  );

  useEffect(() => {
    setCurrentCustomPath(customChainPath);
    setCurrentValidation(
      customChainPath == null
        ? undefined
        : getValidationPreset(
            validationPreset,
            customChainPath,
            estimatedRequiredSpaceBytes || latestSnapshot.size,
            availableSpaceBytes
          )
    );
  }, [
    availableSpaceBytes,
    customChainPath,
    estimatedRequiredSpaceBytes,
    validationPreset,
  ]);

  return (
    <ChainStorageLocationPicker
      customChainPath={currentCustomPath}
      defaultChainPath={defaultChainPath}
      defaultChainStorageValidation={managedDefaultValidation}
      chainStorageValidation={currentValidation}
      isRecoveryFallback={isRecoveryFallback}
      estimatedRequiredSpaceBytes={estimatedRequiredSpaceBytes}
      isChainStorageLoading={isChainStorageLoading}
      onSetChainStorageDirectory={async (path) => {
        bootstrapActions.onSetChainStorageDirectory(path);
        const nextValidation = getValidationPreset(
          validationPreset,
          path,
          estimatedRequiredSpaceBytes || latestSnapshot.size,
          availableSpaceBytes
        );
        setCurrentCustomPath(path);
        setCurrentValidation(nextValidation);
        return nextValidation;
      }}
      onResetChainStorageDirectory={async () => {
        bootstrapActions.onResetChainStorageDirectory();
        setCurrentCustomPath(null);
        setCurrentValidation(undefined);
        return managedDefaultValidation;
      }}
      onValidateChainStorageDirectory={async (path) => {
        bootstrapActions.onValidateChainStorageDirectory(path);
        return getValidationPreset(
          validationPreset,
          path,
          estimatedRequiredSpaceBytes || latestSnapshot.size,
          availableSpaceBytes
        );
      }}
      onConfirmStorageLocation={() =>
        bootstrapActions.onConfirmStorageLocation()
      }
    />
  );
}

interface ManagedMithrilBootstrapProps {
  status: MithrilBootstrapStatus;
  snapshots: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  initialStorageLocationConfirmed?: boolean;
  customChainPath?: string | null;
  defaultChainPath?: string | null;
  defaultChainStorageValidation?: ChainStorageValidation;
  latestSnapshotSize?: number;
  isFetchingSnapshots: boolean;
  validationPreset: ValidationPresetName;
  availableSpaceBytes?: number;
  isRecoveryFallback?: boolean;
  isChainStorageLoading?: boolean;
  bytesDownloaded?: number;
  snapshotSize?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryProgress?: number;
  progressItems?: Array<MithrilProgressItem>;
  bootstrapStartedAt?: number | null;
  error?: MithrilBootstrapError | null;
}

export function ManagedMithrilBootstrap({
  status,
  snapshots,
  selectedDigest = null,
  initialStorageLocationConfirmed = true,
  customChainPath = null,
  defaultChainPath,
  defaultChainStorageValidation:
    defaultValidation = defaultChainStorageValidation,
  latestSnapshotSize,
  isFetchingSnapshots,
  validationPreset,
  availableSpaceBytes,
  isRecoveryFallback,
  isChainStorageLoading,
  bytesDownloaded,
  snapshotSize,
  ancillaryBytesDownloaded,
  ancillaryBytesTotal,
  ancillaryProgress,
  progressItems,
  bootstrapStartedAt,
  error,
}: ManagedMithrilBootstrapProps) {
  const [currentDigest, setCurrentDigest] = useState<string | null>(
    selectedDigest
  );
  const [storageLocationConfirmed, setStorageLocationConfirmed] = useState(
    initialStorageLocationConfirmed
  );
  const [currentCustomPath, setCurrentCustomPath] = useState<string | null>(
    customChainPath
  );
  const [currentValidation, setCurrentValidation] = useState<
    ChainStorageValidation | undefined
  >(
    customChainPath == null
      ? undefined
      : getValidationPreset(
          validationPreset,
          customChainPath,
          latestSnapshotSize,
          availableSpaceBytes
        )
  );

  useEffect(() => {
    setCurrentDigest(selectedDigest);
  }, [selectedDigest, snapshots]);

  useEffect(() => {
    setStorageLocationConfirmed(initialStorageLocationConfirmed);
  }, [initialStorageLocationConfirmed]);

  useEffect(() => {
    setCurrentCustomPath(customChainPath);
    setCurrentValidation(
      customChainPath == null
        ? undefined
        : getValidationPreset(
            validationPreset,
            customChainPath,
            latestSnapshotSize,
            availableSpaceBytes
          )
    );
  }, [
    availableSpaceBytes,
    customChainPath,
    latestSnapshotSize,
    validationPreset,
  ]);

  const selectedSnapshot = useMemo(
    () => resolveSelectedSnapshot(snapshots, currentDigest),
    [currentDigest, snapshots]
  );

  return (
    <MithrilBootstrap
      status={status}
      storageLocationConfirmed={storageLocationConfirmed}
      snapshots={snapshots}
      selectedDigest={currentDigest}
      selectedSnapshot={selectedSnapshot}
      customChainPath={currentCustomPath}
      defaultChainPath={defaultChainPath}
      defaultChainStorageValidation={defaultValidation}
      chainStorageValidation={currentValidation}
      isRecoveryFallback={isRecoveryFallback}
      latestSnapshotSize={latestSnapshotSize}
      isChainStorageLoading={isChainStorageLoading}
      isFetchingSnapshots={isFetchingSnapshots}
      bytesDownloaded={bytesDownloaded}
      snapshotSize={snapshotSize}
      ancillaryBytesDownloaded={ancillaryBytesDownloaded}
      ancillaryBytesTotal={ancillaryBytesTotal}
      ancillaryProgress={ancillaryProgress}
      progressItems={progressItems}
      bootstrapStartedAt={bootstrapStartedAt}
      error={error}
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      onSetChainStorageDirectory={async (path) => {
        bootstrapActions.onSetChainStorageDirectory(path);
        const nextValidation = getValidationPreset(
          validationPreset,
          path,
          latestSnapshotSize,
          availableSpaceBytes
        );
        setCurrentCustomPath(path);
        setCurrentValidation(nextValidation);
        return nextValidation;
      }}
      onResetChainStorageDirectory={async () => {
        bootstrapActions.onResetChainStorageDirectory();
        setCurrentCustomPath(null);
        setCurrentValidation(undefined);
        return defaultValidation;
      }}
      onValidateChainStorageDirectory={async (path) => {
        bootstrapActions.onValidateChainStorageDirectory(path);
        return getValidationPreset(
          validationPreset,
          path,
          latestSnapshotSize,
          availableSpaceBytes
        );
      }}
      onConfirmStorageLocation={() => {
        bootstrapActions.onConfirmStorageLocation();
        setStorageLocationConfirmed(true);
      }}
      onReturnToStorageLocation={() => {
        bootstrapActions.onReturnToStorageLocation();
        setStorageLocationConfirmed(false);
      }}
      onSelectSnapshot={(value) => {
        bootstrapActions.onSelectSnapshot(value);
        setCurrentDigest(value);
      }}
      onAccept={() => bootstrapActions.onAccept()}
      onDecline={() => {
        bootstrapActions.onDecline();
        setStorageLocationConfirmed(initialStorageLocationConfirmed);
      }}
      onWipeRetry={() => bootstrapActions.onWipeRetry()}
      onCancel={() => bootstrapActions.onCancel()}
    />
  );
}
