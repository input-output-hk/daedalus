// @flow
import { createTheme } from '../utils/createTheme';
import { findUpdates } from '../utils/findUpdates';
import { runUpdateThemesCLI } from '../utils/updateThemesCLI';
import {
  CREATE_CARDANO_THEME_PARAMS,
  CREATE_DARK_BLUE_THEME_PARAMS,
  CREATE_LIGHT_BLUE_THEME_PARAMS,
} from '../utils/constants';

const findUpdatesParams = {
  cardano: createTheme(CREATE_CARDANO_THEME_PARAMS),
  darkBlue: createTheme(CREATE_DARK_BLUE_THEME_PARAMS),
  lightBlue: createTheme(CREATE_LIGHT_BLUE_THEME_PARAMS),
};

const pendingUpdates = findUpdates(findUpdatesParams);

if (pendingUpdates !== null) {
  // opens CLI which will allow user to update theme objects in 'themes/daedalus'
  runUpdateThemesCLI(pendingUpdates);
}
