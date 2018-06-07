// @flow
import React from 'react';
import { storiesOf, action } from '@storybook/react';
import { linkTo } from '@storybook/addon-links';
import { withKnobs, boolean, number } from '@storybook/addon-knobs';

import StoryDecorator from './support/StoryDecorator';
import Loading from '../../source/renderer/app/components/loading/Loading';

import cardanoLogo from '../../source/renderer/app/assets/images/cardano-logo.inline.svg';
import adaLogo from '../../source/renderer/app/assets/images/ada-logo.inline.svg';

import CenteredLayout from '../../source/renderer/app/components/layout/CenteredLayout';

const issues = [
  { id: 227, category: 'Daedalus', subCategory: 'Installation', title: 'Your computer time is out of sync.' },
  { id: 228, category: 'Daedalus', subCategory: 'Connection', title: 'Local block data is corrupted.' },
  { id: 229, category: 'Daedalus', subCategory: 'Operation', title: 'Launching node without admin rights.' },
  { id: 230, category: 'Daedalus', subCategory: 'Installation', title: 'File(s) missing.' },
  { id: 231, category: 'Daedalus', subCategory: 'Installation', title: 'Not enough space to store block data.' },
  { id: 234, category: 'Daedalus', subCategory: 'Operation', title: 'Network error.' },
  { id: 235, category: 'Daedalus', subCategory: 'Installation', title: 'User name contains non-Latin characters.' },
  { id: 236, category: 'Daedalus', subCategory: 'Operation', title: '‘open.lock’ file has been corrupted.' },
  { id: 237, category: 'Daedalus', subCategory: 'Operation', title: 'Firewall is blocking connection' },
];

const getIssuesDetectedOptions = (isAnalyzing: boolean, issuesFound: number) => (
  !isAnalyzing
    ? issues.slice(0, issuesFound)
    : null
);

storiesOf('LoadingCategory', module)

  .addDecorator((story, context) => {

    const storyWithKnobs = withKnobs(story, context);

    return (
      <StoryDecorator>
        <div
          style={{ height: '100vh' }}
        >
          <CenteredLayout>
            { storyWithKnobs }
          </CenteredLayout>
        </div>
      </StoryDecorator>
    );
  })

  // ====== Stories ======

  .add('default', () => (
    <Loading
      currencyIcon={adaLogo}
      apiIcon={cardanoLogo}
      isConnecting={boolean('isConnecting', true)}
      hasBeenConnected={boolean('hasBeenConnected', false)}
      hasBlockSyncingStarted={boolean('hasBlockSyncingStarted', false)}
      isSyncing={boolean('isSyncing', false)}
      syncPercentage={number('syncPercentage', 0, { min: 0, max: 100 })}
      isLoadingDataForNextScreen={boolean('isLoadingDataForNextScreen', true)}
      loadingDataForNextScreenMessage={'Connecting to network'}
      hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
      hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
      localTimeDifference={number('localTimeDifference', 0)}
      isSystemTimeCorrect={boolean('isSystemTimeCorrect', true)}
      currentLocale="en-US"
      handleReportIssue={() => {}}
      onProblemSolutionClick={() => {}}
      issuesDetected={getIssuesDetectedOptions(boolean('Is analyzing', false), number('Issues found', 2, { min: 0, max: 9 }))}
    />
  ));

