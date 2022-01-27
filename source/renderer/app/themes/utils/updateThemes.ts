import { updateTheme } from './createTheme';
import { EXISTING_THEME_OUTPUTS_OBJ } from '../daedalus/index';
import type { PendingThemesUpdates } from '../types';

export const updateThemes = (
  pendingUpdates: PendingThemesUpdates
): PendingThemesUpdates => {
  const updatedThemes = {};

  for (const themeName in pendingUpdates) {
    if (themeName) {
      updatedThemes[themeName] = updateTheme(
        EXISTING_THEME_OUTPUTS_OBJ[themeName],
        pendingUpdates[themeName]
      );
    }
  }

  return updatedThemes;
};
