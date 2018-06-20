// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, boolean, number } from '@storybook/addon-knobs';

import StoryDecorator from './support/StoryDecorator';
import Loading from '../../source/renderer/app/components/loading/Loading';

import cardanoLogo from '../../source/renderer/app/assets/images/cardano-logo.inline.svg';
import adaLogo from '../../source/renderer/app/assets/images/ada-logo.inline.svg';

import CenteredLayout from '../../source/renderer/app/components/layout/CenteredLayout';

import getIssuesDetectedOptions from './support/getIssuesDetectedOptions';

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
      loadingDataForNextScreenMessage={{
        id: 'loadingDataForNextScreenMessage',
        defaultMessage: 'Connecting to network',
        description: '"Connecting to network" message',
      }}
      hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
      hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
      localTimeDifference={number('localTimeDifference', 0)}
      isSystemTimeCorrect={boolean('isSystemTimeCorrect', true)}
      currentLocale="en-US"
      handleReportIssue={() => {}}
      onProblemSolutionClick={() => {}}
      isAnalyzingIssues={!!boolean('isAnalyzingIssues', true)}
      issuesDetected={getIssuesDetectedOptions(number('Issues found', 2, { min: 0, max: 9 }))}
    />
  ));

