import React from 'react';
import SplashNetworkFlight from '../../../../source/renderer/app/components/splash/SplashNetworkFlight';
import StoryDecorator from '../../_support/StoryDecorator';

export default {
  title: 'Nodes / Splash Network Info',
  decorators: [(story) => <StoryDecorator>{story()}</StoryDecorator>],
};

export const Flight = () => (
  <SplashNetworkFlight onClose={() => null} openExternalLink={() => null} />
);
