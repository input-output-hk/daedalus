'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.writeThemeUpdate = void 0;
const fs_1 = __importDefault(require('fs'));
const path_1 = __importDefault(require('path'));
const formatConstNames = (fileName) => {
  const constNames = {};
  let PREFIX = '';
  const fileNameParts = fileName.split('-');
  if (fileNameParts.length > 1) {
    PREFIX = `${fileNameParts[0].toUpperCase()}_${fileNameParts[1].toUpperCase()}`;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'themeOutput' does not exist on type '{}'... Remove this comment to see the full error message
    constNames.themeOutput = `${PREFIX}_THEME_OUTPUT`;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'themeParams' does not exist on type '{}'... Remove this comment to see the full error message
    constNames.themeParams = `${PREFIX}_THEME_PARAMS`;
    // @ts-ignore ts-migrate(2739) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    return constNames;
  }
  PREFIX = `${fileNameParts[0].toUpperCase()}`;
  // @ts-ignore ts-migrate(2339) FIXME: Property 'themeOutput' does not exist on type '{}'... Remove this comment to see the full error message
  constNames.themeOutput = `${PREFIX}_THEME_OUTPUT`;
  // @ts-ignore ts-migrate(2339) FIXME: Property 'themeParams' does not exist on type '{}'... Remove this comment to see the full error message
  constNames.themeParams = `${PREFIX}_THEME_PARAMS`;
  // @ts-ignore ts-migrate(2322) FIXME: Type '{}' is not assignable to type 'FormattedCons... Remove this comment to see the full error message
  return constNames;
};
const writeThemeUpdate = ({ fileName, updatedThemeObj }) => {
  const pathBase = __dirname.includes('dist')
    ? '../../source/renderer/app/themes/daedalus'
    : '../daedalus';
  const THEME_FILE = path_1.default.join(__dirname, pathBase, `${fileName}.js`);
  const { themeOutput, themeParams } = formatConstNames(fileName);
  const FILE_CONTENT = `
    // @flow
    import { createTheme } from '../utils/createTheme';
    import type { CreateThemeParams } from '../types';

    //  ==== ${fileName} theme output for Daedalus and react-polymorph components === //
    export const ${themeOutput} = ${JSON.stringify(updatedThemeObj, null, 2)};

    const ${themeParams}: CreateThemeParams = {
      config: ${themeOutput},
    };

    export default createTheme(${themeParams});
  `;
  // @TODO - remove flow fix and move fs to main process
  // @ts-ignore
  fs_1.default.writeFileSync(THEME_FILE, FILE_CONTENT, {}); // eslint-disable-line
};
exports.writeThemeUpdate = writeThemeUpdate;
//# sourceMappingURL=writeThemeUpdate.js.map
