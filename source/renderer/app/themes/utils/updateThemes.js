/* eslint-disable no-console */
// @flow
import { isEmpty } from 'lodash';
import { updateTheme } from './createTheme';
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
  }

  if (lightBlueUpdates && !isEmpty(lightBlueUpdates)) {
    const updatedLightBlueTheme = updateTheme(
      LIGHT_BLUE_THEME_CONFIG,
      lightBlueUpdates
    );
    updateThemes['light-blue'] = updatedLightBlueTheme;
  }
  return updatedThemes;
};
