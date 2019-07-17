// @flow
import { isEmpty } from 'lodash';
import { updateTheme } from './createTheme';
import { CARDANO_THEME_CONFIG } from '../daedalus/cardano';
import { DARK_BLUE_THEME_CONFIG } from '../daedalus/dark-blue';
import { DARK_CARDANO_THEME_CONFIG } from '../daedalus/dark-cardano';
import { LIGHT_BLUE_THEME_CONFIG } from '../daedalus/light-blue';
import { YELLOW_THEME_CONFIG } from '../daedalus/yellow';
import { WHITE_THEME_CONFIG } from '../daedalus/white';
import type { PendingThemesUpdates } from '../types';

export const updateThemes = (pendingUpdates: PendingThemesUpdates) => {
  const {
    cardanoUpdates,
    darkBlueUpdates,
    darkCardanoUpdates,
    lightBlueUpdates,
    yellowUpdates,
    whiteUpdates,
  } = pendingUpdates;

  const updatedThemes = {};
  if (cardanoUpdates && !isEmpty(cardanoUpdates)) {
    const updatedCardanoTheme = updateTheme(
      CARDANO_THEME_CONFIG,
      cardanoUpdates
    );
    updatedThemes.cardano = updatedCardanoTheme;
  }

  if (darkBlueUpdates && !isEmpty(darkBlueUpdates)) {
    const updatedDarkBlueTheme = updateTheme(
      DARK_BLUE_THEME_CONFIG,
      darkBlueUpdates
    );
    updatedThemes['dark-blue'] = updatedDarkBlueTheme;
  }

  if (lightBlueUpdates && !isEmpty(lightBlueUpdates)) {
    const updatedLightBlueTheme = updateTheme(
      LIGHT_BLUE_THEME_CONFIG,
      lightBlueUpdates
    );
    updatedThemes['light-blue'] = updatedLightBlueTheme;
  }

  if (yellowUpdates && !isEmpty(yellowUpdates)) {
    const updatedYellowTheme = updateTheme(
      YELLOW_THEME_CONFIG,
      yellowUpdates
    );
    updatedThemes.yellow = updatedYellowTheme;
  }

  if (whiteUpdates && !isEmpty(whiteUpdates)) {
    const updatedWhiteTheme = updateTheme(
      WHITE_THEME_CONFIG,
      whiteUpdates
    );
    updatedThemes.white = updatedWhiteTheme;
  }

  if (darkCardanoUpdates && !isEmpty(darkCardanoUpdates)) {
    const updatedDarkCardanoTheme = updateTheme(
      DARK_CARDANO_THEME_CONFIG,
      darkCardanoUpdates
    );
    updatedThemes['dark-cardano'] = updatedDarkCardanoTheme;
  }
  return updatedThemes;
};
