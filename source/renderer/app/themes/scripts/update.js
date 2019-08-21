// @flow
import { isEmpty } from 'lodash';
import { createTheme } from '../utils/createTheme';
import { findUpdates } from '../utils/findUpdates';
import { runUpdateThemesCLI } from '../utils/updateThemesCLI';
import { CREATE_THEME_PARAMS } from '../utils/constants';

const createThemeOutputs = CREATE_THEME_PARAMS.reduce(
  (outputs, theme) => [[theme[0], createTheme(theme[1])], ...outputs],
  []
);

const pendingUpdates = findUpdates(createThemeOutputs);

if (!isEmpty(pendingUpdates)) {
  // opens CLI which will allow user to update theme outputs in 'themes/daedalus'
  runUpdateThemesCLI(pendingUpdates);
}
