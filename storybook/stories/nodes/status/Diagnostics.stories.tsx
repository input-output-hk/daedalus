import React from 'react';
import { action } from '@storybook/addon-actions';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../../_support/StoryDecorator';
import DaedalusDiagnostics from '../../../../source/renderer/app/components/status/DaedalusDiagnostics';
import MithrilPartialSyncRecommendation from '../../../../source/renderer/app/components/status/MithrilPartialSyncRecommendation';
import MithrilPartialSyncConfirmation from '../../../../source/renderer/app/components/status/MithrilPartialSyncConfirmation';
import SyncingConnectingMithrilPrompt from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt';

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

// Isolated CTA/recommendation fixtures (section-level component, intl pulled
// from context at render so en-US/ja-JP and theme switching stay truthful).
const recommendationBaseProps = {
  formattedSyncPercentage: '62.5',
  isActionBlocked: false,
  isSynced: false,
  onShowConfirmation: action('onShowConfirmation'),
};

// Isolated confirmation-modal fixtures (the real `isShowingConfirmation` seam is
// exercised through DaedalusDiagnostics below; here the modal is rendered direct).
const confirmationBaseProps = {
  isActionBlocked: false,
  startError: null as string | null,
  formattedSyncPercentage: '62.5',
  onCancel: action('onCancel'),
  onConfirm: action('onConfirm'),
};

// Proactive (syncing-screen) prompt fixtures. `onStart` MUST return a Promise so
// the confirm-view "Start now" await resolves like the real store call.
const proactivePromptBaseProps = {
  onStart: async () => {
    action('onStart')();
  },
  onDismiss: action('onDismiss'),
};

storiesOf('Nodes / Status', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Partial Sync CTA Ready', () => <DaedalusDiagnostics {...baseProps} />)
  .add('Partial Sync CTA Blocked', () => (
    <DaedalusDiagnostics {...baseProps} isMithrilBootstrapActive />
  ))
  // Opens the confirmation through the real state seam: the supported
  // `showMithrilPartialSyncConfirmationOnOpen` prop flows to
  // MithrilPartialSyncSection.showConfirmationOnOpen → componentDidMount →
  // showConfirmation() → setState({ isShowingConfirmation: true }).
  .add('Partial Sync Confirmation', () => (
    <DaedalusDiagnostics
      {...baseProps}
      showMithrilPartialSyncConfirmationOnOpen
    />
  ));

storiesOf('Nodes / Status / Mithril Partial Sync Recommendation', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Behind (CTA Ready)', () => (
    <MithrilPartialSyncRecommendation {...recommendationBaseProps} />
  ))
  .add('Synced', () => (
    <MithrilPartialSyncRecommendation {...recommendationBaseProps} isSynced />
  ))
  .add('Blocked', () => (
    <MithrilPartialSyncRecommendation
      {...recommendationBaseProps}
      isActionBlocked
    />
  ));

storiesOf('Nodes / Status / Mithril Partial Sync Confirmation', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Known Epochs Behind', () => (
    <MithrilPartialSyncConfirmation
      {...confirmationBaseProps}
      behindByEpochs={42}
    />
  ))
  .add('Unknown Behind', () => (
    <MithrilPartialSyncConfirmation {...confirmationBaseProps} />
  ))
  .add('Start Error', () => (
    <MithrilPartialSyncConfirmation
      {...confirmationBaseProps}
      behindByEpochs={42}
      startError="Unable to start Mithril sync. Cardano node did not stop in time."
    />
  ));

storiesOf('Nodes / Status / Mithril Proactive Prompt', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Known Epochs Behind', () => (
    <SyncingConnectingMithrilPrompt
      {...proactivePromptBaseProps}
      behindByEpochs={120}
    />
  ))
  .add('Unknown Behind', () => (
    <SyncingConnectingMithrilPrompt {...proactivePromptBaseProps} />
  ));
