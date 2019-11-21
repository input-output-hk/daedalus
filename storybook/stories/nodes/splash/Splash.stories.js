// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import SplashNetwork from '../../../../source/renderer/app/components/splash/Network';
import StoryDecorator from '../../_support/StoryDecorator';
import { isIncentivizedTestnetTheme } from '../../_support/utils';

storiesOf('Nodes|Splash', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Network Info', props => (
    <SplashNetwork
      isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
      onClose={() => null}
      onLearnMoreClick={() => null}
    />
  ));
