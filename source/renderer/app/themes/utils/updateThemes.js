/* eslint-disable no-console */
// @flow
import { isEmpty } from 'lodash';
import { updateTheme } from './createTheme';
// these throw the error
import { CARDANO_THEME_CONFIG } from '../daedalus/cardano';
import { DARK_BLUE_THEME_CONFIG } from '../daedalus/dark-blue';
import { LIGHT_BLUE_THEME_CONFIG } from '../daedalus/light-blue';
import type { PendingThemesUpdates } from '../types';

export const updateThemes = (pendingUpdates: PendingThemesUpdates) => {
  const { cardanoUpdates, darkBlueUpdates, lightBlueUpdates } = pendingUpdates;
  const updatedThemes = {};
  if (cardanoUpdates && !isEmpty(cardanoUpdates)) {
    const updatedCardanoTheme = updateTheme(
      CARDANO_THEME_CONFIG,
      cardanoUpdates
    );
    updateThemes.cardano = updatedCardanoTheme;
  }

  if (darkBlueUpdates && !isEmpty(darkBlueUpdates)) {
    const updatedDarkBlueTheme = updateTheme(
      DARK_BLUE_THEME_CONFIG,
      darkBlueUpdates
    );
    updateThemes['dark-blue'] = updatedDarkBlueTheme;

    // write updatedDarkBlueTheme theme object to dark-blue.js
    // $FlowFixMe
    // console.log(
    //   `Dark Blue theme updated!\n${JSON.stringify(updatedDarkBlueTheme, 0, 2)}`
    // );
  }

  if (lightBlueUpdates && !isEmpty(lightBlueUpdates)) {
    const updatedLightBlueTheme = updateTheme(
      LIGHT_BLUE_THEME_CONFIG,
      lightBlueUpdates
    );
    updateThemes['light-blue'] = updatedLightBlueTheme;

    // write updatedLightBlueTheme theme object to light-blue.js
    // $FlowFixMe
    // console.log(
    //   `Light Blue theme updated!\n${JSON.stringify(
    //     updatedLightBlueTheme,
    //     0,
    //     2
    //   )}`
    // );
  }
  return updatedThemes;
};
