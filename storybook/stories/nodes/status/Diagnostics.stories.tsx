import React from 'react';
import { action } from '@storybook/addon-actions';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../../_support/StoryDecorator';
import DaedalusDiagnostics from '../../../../source/renderer/app/components/status/DaedalusDiagnostics';

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
  daedalusProcessID: 98954,
  daedalusMainProcessID: 82734,
  isBlankScreenFixActive: false,
  cardanoNodeVersion: '10.2.1',
  cardanoNodePID: 87212,
  cardanoWalletVersion: '2026.4.0',
  cardanoWalletPID: 87213,
  cardanoWalletApiPort: 59982,
  cardanoNetwork: 'mainnet',
  daedalusStateDirectoryPath:
    '/Users/daedalus/Library/Application Support/Daedalus Mainnet',
};

const baseProps = {
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
  isMithrilPartialSyncWorking: false,
  isMithrilPartialSyncEnabled: true,
  isMithrilPartialSyncSignificantlyBehind: true,
  isMithrilBootstrapActive: false,
  onStartMithrilPartialSync: action('onStartMithrilPartialSync'),
  onOpenStateDirectory: action('onOpenStateDirectory'),
  onOpenExternalLink: action('onOpenExternalLink'),
  onRestartNode: {
    trigger: action('onRestartNode.trigger'),
  },
  onClose: action('onClose'),
  onCopyStateDirectoryPath: action('onCopyStateDirectoryPath'),
  onForceCheckNetworkClock: action('onForceCheckNetworkClock'),
} as any;

class PartialSyncConfirmationStory extends React.Component {
  hasOpenedConfirmation = false;

  setDiagnosticsRef = (instance: any) => {
    if (instance == null || this.hasOpenedConfirmation) return;
    this.hasOpenedConfirmation = true;
    instance.setState({ isShowingMithrilPartialSyncConfirmation: true });
  };

  render() {
    return <DaedalusDiagnostics {...baseProps} ref={this.setDiagnosticsRef} />;
  }
}

storiesOf('Nodes / Status', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Partial Sync CTA Ready', () => <DaedalusDiagnostics {...baseProps} />)
  .add('Partial Sync CTA Blocked', () => (
    <DaedalusDiagnostics {...baseProps} isMithrilBootstrapActive />
  ))
  .add('Partial Sync Confirmation', () => <PartialSyncConfirmationStory />);
