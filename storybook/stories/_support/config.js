// @flow
import cardano from '../../../source/renderer/app/themes/daedalus/cardano.js';
import darkBlue from '../../../source/renderer/app/themes/daedalus/dark-blue.js';
import lightBlue from '../../../source/renderer/app/themes/daedalus/light-blue.js';
import darkCardano from '../../../source/renderer/app/themes/daedalus/dark-cardano.js';
import white from '../../../source/renderer/app/themes/daedalus/white.js';
import yellow from '../../../source/renderer/app/themes/daedalus/yellow.js';
import incentivizedTestnet from '../../../source/renderer/app/themes/daedalus/incentivized-testnet.js';

export const themes = {
  Cardano: cardano,
  DarkBlue: darkBlue,
  LightBlue: lightBlue,
  DarkCardano: darkCardano,
  Yellow: yellow,
  White: white,
  IncentivizedTestnet: incentivizedTestnet,
};
export const themeNames: Array<any> = Object.keys(themes);
export const themesIds = {
  Cardano: 'cardano',
  DarkBlue: 'dark-blue',
  LightBlue: 'light-blue',
  DarkCardano: 'dark-cardano',
  Yellow: 'yellow',
  White: 'white',
  IncentivizedTestnet: 'incentivized-testnet',
};

export const locales = {
  English: 'en-US',
  Japanese: 'ja-JP',
};
export const localeNames: Array<any> = Object.keys(locales);

export const operatingSystems = {
  Windows: 'windows',
  Linux: 'linux',
  Mac: 'mac',
};
export const osNames: Array<any> = Object.keys(operatingSystems);

// These differences are due to the different menu heights on each OS
export const osMinWindowHeights = {
  Windows: '641px',
  Linux: '660px',
  Mac: '700px',
};

/* eslint-disable no-restricted-globals */
const getParams = (param: string) => {
  const { hash, search } = parent.window.location;
  const queries = hash || search;
  const params = new URLSearchParams(queries.slice(1));
  return params.get(param);
};

export const getInitialState = () => {
  const themeName =
    getParams('themeName') ||
    sessionStorage.getItem('themeName') ||
    themeNames[0];

  const localeName =
    getParams('localeName') ||
    sessionStorage.getItem('localeName') ||
    localeNames[0];

  const osName =
    getParams('osName') || sessionStorage.getItem('osName') || osNames[0];

  return {
    themeName,
    localeName,
    osName,
  };
};
