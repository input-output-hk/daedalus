// @flow
import React from 'react';
import SplashNetwork from '../../source/renderer/app/components/splash/Network';

const currentTheme = sessionStorage.getItem('themeName') || 'light-blue';

export const SplashNetworkStory = () => (
  <SplashNetwork
    isIncentivizedTestnet={currentTheme === 'IncentivizedTestnet'}
    onClose={() => null}
    onLearnMoreClick={() => null}
  />
);
