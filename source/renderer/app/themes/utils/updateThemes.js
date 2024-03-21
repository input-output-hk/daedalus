'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.updateThemes = void 0;
const createTheme_1 = require('./createTheme');
const index_1 = require('../daedalus/index');
const updateThemes = (pendingUpdates) => {
  const updatedThemes = {};
  for (const themeName in pendingUpdates) {
    if (themeName) {
      updatedThemes[themeName] = (0, createTheme_1.updateTheme)(
        index_1.EXISTING_THEME_OUTPUTS_OBJ[themeName],
        pendingUpdates[themeName]
      );
    }
  }
  return updatedThemes;
};
exports.updateThemes = updateThemes;
//# sourceMappingURL=updateThemes.js.map
