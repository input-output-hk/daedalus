import React from 'react';
import type { ComponentProps } from 'react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../../_support/StoryDecorator';
import DaedalusDiagnostics from '../../../../source/renderer/app/components/status/DaedalusDiagnostics';
import MithrilPartialSyncSection from '../../../../source/renderer/app/components/status/MithrilPartialSyncSection';

const systemInfo = {
  platform: 'macOS',
  platformVersion: '17.7.0',
  cpu: 'Intel(R) Core(TM) i5-3210M CPU @ 2.50GHz',
  ram: '32.0 GB',
  availableDiskSpace: '500 GB',
  hasMetHardwareRequirements: true,
  isRTSFlagsModeEnabled: false,
};

const coreInfo = {
  daedalusVersion: '7.1.0',
  daedalusBuildNumber: '12500',
  daedalusProcessID: '98954',
  daedalusMainProcessID: '82734',
  isBlankScreenFixActive: false,
  cardanoNodeVersion: '10.2.1',
  cardanoNodePID: 87212,
  cardanoNodeUptime: '2h 15m 30s',
  cardanoWalletVersion: '2026.4.0',
  cardanoWalletPID: 87213,
  cardanoWalletUptime: '2h 15m 28s',
  cardanoWalletRestartCount: 0,
  cardanoWalletApiPort: 59982,
  cardanoNetwork: 'mainnet',
  daedalusStateDirectoryPath:
    '/Users/daedalus/Library/Application Support/Daedalus Mainnet',
};

const baseProps: ComponentProps<typeof DaedalusDiagnostics> = {
  systemInfo,
  coreInfo,
  cardanoNodeState: 'running' as const,
  isNodeResponding: true,
  isNodeSyncing: true,
  isNodeInSync: false,
  isNodeTimeCorrect: true,
  nodeConnectionError: null,
  isConnected: true,
  isSynced: false,
  syncPercentage: 62.5,
  localTimeDifference: 0,
  isSystemTimeCorrect: true,
  isSystemTimeIgnored: false,
  isCheckingSystemTime: false,
  isForceCheckingSystemTime: false,
  localTip: {
    epoch: 512,
    slot: 45678,
    absoluteSlotNumber: 15123456,
  },
  networkTip: {
    epoch: 513,
    slot: 46789,
    absoluteSlotNumber: 15134567,
  },
  onOpenStateDirectory: action('onOpenStateDirectory'),
  onOpenExternalLink: action('onOpenExternalLink'),
  onRestartNode: {
    trigger: action('onRestartNode.trigger'),
  },
  onRestartWallet: {
    trigger: action('onRestartWallet.trigger'),
  },
  onClose: action('onClose'),
  onCopyStateDirectoryPath: action('onCopyStateDirectoryPath'),
  onForceCheckNetworkClock: action('onForceCheckNetworkClock'),
};

// Drives the section's real confirmation seam: mount it, then click the
// single CTA button its recommendation view renders (no copy-text matching),
// which runs showConfirmation() → setState({ isShowingConfirmation: true }).
function AutoOpenedPartialSyncConfirmation() {
  const containerRef = React.useRef<HTMLDivElement>(null);
  React.useEffect(() => {
    containerRef.current?.querySelector('button')?.click();
  }, []);
  return (
    <div ref={containerRef}>
      <MithrilPartialSyncSection
        isActionBlocked={false}
        isMithrilPartialSyncWorking={false}
        isSignificantlyBehind
        isProbeFailed={false}
        isAtOrPastSnapshot={false}
        behindByEpochs={1}
        onRestoreFocus={action('onRestoreFocus')}
        onStartMithrilPartialSync={async () => {
          action('onStartMithrilPartialSync')();
        }}
      />
    </div>
  );
}

export default {
  title: 'Nodes / Diagnostic',
  decorators: [(story) => <StoryDecorator>{story()}</StoryDecorator>],
};

export const PartialSyncCtaReady = {
  render: () => <DaedalusDiagnostics {...baseProps} />,
  name: 'Partial Sync CTA Ready',
};

export const PartialSyncCtaBlocked = {
  render: () => <DaedalusDiagnostics {...baseProps} />,

  name: 'Partial Sync CTA Blocked',
};

export const PartialSyncAtOrPastSnapshot = () => (
  <DaedalusDiagnostics {...baseProps} />
);

export const PartialSyncConfirmation = () => (
  <AutoOpenedPartialSyncConfirmation />
);
