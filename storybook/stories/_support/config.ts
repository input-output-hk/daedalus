import cardano from '../../../source/renderer/app/themes/daedalus/cardano';
import darkBlue from '../../../source/renderer/app/themes/daedalus/dark-blue';
import lightBlue from '../../../source/renderer/app/themes/daedalus/light-blue';
import darkCardano from '../../../source/renderer/app/themes/daedalus/dark-cardano';
import flightCandidate from '../../../source/renderer/app/themes/daedalus/flight-candidate';
import white from '../../../source/renderer/app/themes/daedalus/white';
import yellow from '../../../source/renderer/app/themes/daedalus/yellow';
import incentivizedTestnet from '../../../source/renderer/app/themes/daedalus/incentivized-testnet';
import shelleyTestnet from '../../../source/renderer/app/themes/daedalus/shelley-testnet';

export const themes = {
  Cardano: cardano,
  DarkBlue: darkBlue,
  LightBlue: lightBlue,
  DarkCardano: darkCardano,
  FlightCandidate: flightCandidate,
  Yellow: yellow,
  White: white,
  IncentivizedTestnet: incentivizedTestnet,
  ShelleyTestnet: shelleyTestnet,
};
export const themeNames: Array<any> = Object.keys(themes);
export const themesIds = {
  Cardano: 'cardano',
  DarkBlue: 'dark-blue',
  LightBlue: 'light-blue',
  DarkCardano: 'dark-cardano',
  FlightCandidate: 'flight-candidate',
  Yellow: 'yellow',
  White: 'white',
  IncentivizedTestnet: 'incentivized-testnet',
  ShelleyTestnet: 'shelley-testnet',
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
