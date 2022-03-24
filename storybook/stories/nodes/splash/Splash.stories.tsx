import React from 'react';
import { storiesOf } from '@storybook/react';
import SplashNetworkFlight from '../../../../source/renderer/app/components/splash/SplashNetworkFlight';
import StoryDecorator from '../../_support/StoryDecorator';

storiesOf('Nodes|Splash Network Info', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Flight', () => (
    <SplashNetworkFlight onClose={() => null} openExternalLink={() => null} />
  ));
