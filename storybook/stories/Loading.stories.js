// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import {
  withKnobs,
  number,
  boolean,
  text,
  select,
} from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import { action } from '@storybook/addon-actions';

// Assets and helpers
import StoryDecorator from './support/StoryDecorator';

// Screens
import Loading from '../../source/renderer/app/components/loading/Loading';
import currencyIcon from '../../source/renderer/app/assets/images/ada-logo.inline.svg';
import apiIcon from '../../source/renderer/app/assets/images/cardano-logo.inline.svg';
import { CardanoNodeStates } from '../../source/common/types/cardano-node.types';

storiesOf('Loading', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))

  // ====== Stories ======

  .add('Default', () => (
    <Loading
      onStatusIconClick={linkTo('Diagnostics', () => 'default')}
      currencyIcon={currencyIcon}
      apiIcon={apiIcon}
      cardanoNodeState={select(
        'cardanoNodeState',
        CardanoNodeStates,
        CardanoNodeStates.STARTING
      )}
      availableAppVersion={null}
      hasBeenConnected={boolean('hasBeenConnected', true)}
      isConnected={boolean('isConnected', true)}
      isSynced={boolean('isSynced', true)}
      isNodeStopping={boolean('isNodeStopping', false)}
      isNodeStopped={boolean('isNodeStopped', false)}
      isNotEnoughDiskSpace={boolean('isNotEnoughDiskSpace', false)}
      isTlsCertInvalid={boolean('isTlsCertInvalid', false)}
      diskSpaceRequired={`${text('diskSpaceRequired (GB)', 4)} GB`}
      diskSpaceMissing={`${text('diskSpaceRequired (GB)', 1)} GB`}
      diskSpaceRecommended={`${text('diskSpaceRequired (GB)', 8)} GB`}
      syncPercentage={number('syncPercentage', 100)}
      loadingDataForNextScreenMessage={{
        id: 'loading.screen.loadingWalletData',
        defaultMessage: '!!!Loading wallet data',
        description: 'Message "Loading wallet data" on the loading screen.',
      }}
      hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
      hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
      localTimeDifference={number('localTimeDifference', 0)}
      isSystemTimeCorrect={boolean('isSystemTimeCorrect', true)}
      isCheckingSystemTime={boolean('isCheckingSystemTime', false)}
      isNodeResponding={boolean('isNodeResponding', true)}
      isNodeSubscribed={boolean('isNodeSubscribed', true)}
      isNodeSyncing={boolean('isNodeSyncing', true)}
      isNodeTimeCorrect={boolean('isNodeTimeCorrect', true)}
      currentLocale="en-US"
      currentAppVersion="1.1.1"
      isNewAppVersionAvailable={boolean('isNewAppVersionAvailable', false)}
      isNewAppVersionLoading={boolean('isNewAppVersionLoading', false)}
      isNewAppVersionLoaded={boolean('isNewAppVersionLoaded', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      onReportIssueClick={action('onReportIssueClick')}
      onReadSyncIssueHelpClick={action('onReadSyncIssueHelpClick')}
      onReadConnectivityIssueHelpClick={action(
        'onReadConnectivityIssueHelpClick'
      )}
      onCheckTheTimeAgain={action('onCheckTheTimeAgain')}
      onContinueWithoutClockSyncCheck={action(
        'onContinueWithoutClockSyncCheck'
      )}
      onDownloadLogs={action('onDownloadLogs')}
      onGetAvailableVersions={action('onGetAvailableVersions')}
      disableDownloadLogs={boolean('disableDownloadLogs', true)}
    />
  ));
