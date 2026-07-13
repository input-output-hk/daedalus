import React from 'react';
import type { ComponentProps } from 'react';
import { action } from '@storybook/addon-actions';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../../_support/StoryDecorator';
import DaedalusDiagnostics from '../../../../source/renderer/app/components/status/DaedalusDiagnostics';
import MithrilPartialSyncConfirmation from '../../../../source/renderer/app/components/status/MithrilPartialSyncConfirmation';
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
  cardanoWalletVersion: '2026.4.0',
  cardanoWalletPID: 87213,
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
  isMithrilPartialSyncWorking: false,
  isMithrilPartialSyncEnabled: true,
  isMithrilPartialSyncSignificantlyBehind: true,
  isMithrilPartialSyncProbeFailed: false,
  isMithrilPartialSyncAtOrPastSnapshot: false,
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
};

type ConfirmationProps = ComponentProps<typeof MithrilPartialSyncConfirmation>;

const confirmationBaseProps: ConfirmationProps = {
  isActionBlocked: false,
  startError: null,
  onCancel: action('onCancel'),
  onConfirm: action('onConfirm'),
};

// StoryWrapper hands currentTheme to the story as a prop (first parameter);
// keying the modal on it remounts per theme switch.
const renderConfirmationStory = (
  storyProps: Partial<ConfirmationProps> = {}
): ((props: { currentTheme: string }) => JSX.Element) =>
  function RenderConfirmationStory(props: { currentTheme: string }) {
    return (
      <MithrilPartialSyncConfirmation
        key={props.currentTheme}
        {...confirmationBaseProps}
        {...storyProps}
      />
    );
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

storiesOf('Nodes / Diagnostic', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Partial Sync CTA Ready', () => <DaedalusDiagnostics {...baseProps} />)
  .add('Partial Sync CTA Blocked', () => (
    <DaedalusDiagnostics {...baseProps} isMithrilBootstrapActive />
  ))
  .add('Partial Sync At Or Past Snapshot', () => (
    <DaedalusDiagnostics
      {...baseProps}
      isMithrilPartialSyncSignificantlyBehind={false}
      isMithrilPartialSyncAtOrPastSnapshot
    />
  ))
  .add('Partial Sync Confirmation', () => (
    <AutoOpenedPartialSyncConfirmation />
  ));

storiesOf('Nodes / Diagnostic / Mithril Partial Sync Confirmation', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Known Epochs Behind', renderConfirmationStory({ behindByEpochs: 42 }))
  .add('Unknown Behind', renderConfirmationStory())
  .add(
    'At Or Past Snapshot',
    renderConfirmationStory({ isAtOrPastSnapshot: true })
  )
  .add(
    'Start Error',
    renderConfirmationStory({
      behindByEpochs: 42,
      startError:
        'Unable to start Mithril sync. Cardano node did not stop in time.',
    })
  );
