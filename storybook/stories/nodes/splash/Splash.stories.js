// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import SplashNetworkFlight from '../../../../source/renderer/app/components/splash/SplashNetworkFlight';
import SplashNetworkITN from '../../../../source/renderer/app/components/splash/SplashNetworkITN';
import StoryDecorator from '../../_support/StoryDecorator';

storiesOf('Nodes|Splash Network Info', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Flight', () => (
    <SplashNetworkFlight onClose={() => null} openExternalLink={() => null} />
  ))
  .add('Incentivized Testnet', () => (
    <SplashNetworkITN
      onClose={() => null}
      openExternalLink={() => null}
      isIncentivizedTestnetTheme
    />
  ));
