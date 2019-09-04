// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';

// Screens
import DaedalusDiagnostics from '../../../../source/renderer/app/components/status/DaedalusDiagnostics';

const systemInfo = {
  platform: 'macOS',
  platformVersion: '17.7.0',
  cpu: 'Intel(R) Core(TM) i5-3210M CPU @ 2.50GHz',
  ram: '32.0 GB',
  availableDiskSpace: '500GB',
};

const coreInfo = {
  daedalusVersion: '0.14.0',
  daedalusProcessID: '98954',
  daedalusMainProcessID: '82734',
  isBlankScreenFixActive: false,
  cardanoVersion: 'dev',
  cardanoProcessID: 87212,
  cardanoAPIPort: 59982,
  cardanoNetwork: 'development',
  daedalusStateDirectoryPath:
    '/Users/daedalus/Library/Application Support/Daedalus Demo',
};

storiesOf('Nodes|Status', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Daedalus Diagnostics', () => (
    <DaedalusDiagnostics
      systemInfo={systemInfo}
      coreInfo={coreInfo}
      cardanoNodeState="running"
      currentLocale="en-US"
      isDev={false}
      isMainnet
      isStaging={false}
      isTestnet={false}
      isNodeResponding
      isNodeSubscribed
      isNodeSyncing
      isNodeInSync
      isNodeTimeCorrect
      isConnected
      isSynced
      syncPercentage={100}
      hasBeenConnected
      localTimeDifference={29912}
      isSystemTimeCorrect
      isForceCheckingNodeTime={false}
      isSystemTimeIgnored={false}
      latestLocalBlockTimestamp={280719}
      latestNetworkBlockTimestamp={280719}
      nodeConnectionError={null}
      localBlockHeight={42539}
      networkBlockHeight={42539}
      onForceCheckLocalTimeDifference={() => null}
      onCopyStateDirectoryPath={() => null}
      onOpenStateDirectory={() => null}
      onOpenExternalLink={() => null}
      onRestartNode={() => null}
      onClose={() => null}
    />
  ));
