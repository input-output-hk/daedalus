// @flow
import React from 'react';
import { storiesOf, action } from '@storybook/react';
import { linkTo } from '@storybook/addon-links';
import { withKnobs, boolean, number } from '@storybook/addon-knobs';

import StoryDecorator from './support/StoryDecorator';

import Loading from '../../source/renderer/app/components/loading/Loading';
import adaLogo from '../../source/renderer/app/assets/images/ada-logo.inline.svg';
import cardanoLogo from '../../source/renderer/app/assets/images/cardano-logo.inline.svg';
// import { messages } from '../../source/renderer/app/containers/LoadingPage';

storiesOf('LoadingCategory', module)

  .addDecorator((story, context) => {

    const storyWithKnobs = withKnobs(story, context);

    return (
        <StoryDecorator>
      <div
        style={{
          height: '100vh'
        }}
      >
          { storyWithKnobs }
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
      syncPercentage={number('syncPercentage', 0)}
      isLoadingDataForNextScreen={boolean('isLoadingDataForNextScreen', true)}
      loadingDataForNextScreenMessage={'Connecting to network'}
      hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
      hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
      localTimeDifference={number('localTimeDifference', 0)}
      isSystemTimeCorrect={boolean('isSystemTimeCorrect', true)}
      currentLocale="en-US"
      handleReportIssue={() => {}}
      onProblemSolutionClick={() => {}}
    />
  ));

