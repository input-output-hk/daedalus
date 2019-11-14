// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import SplashNetwork from '../../../../source/renderer/app/components/splash/Network';
import StoryDecorator from '../../_support/StoryDecorator';

const currentTheme = sessionStorage.getItem('themeName') || 'light-blue';

storiesOf('Nodes|Environment|Splash', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .add(
    'Network',
    <SplashNetwork
      isIncentivizedTestnet={currentTheme === 'IncentivizedTestnet'}
      onClose={() => null}
      onLearnMoreClick={() => null}
    />
  );
