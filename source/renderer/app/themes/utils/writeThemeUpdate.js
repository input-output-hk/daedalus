// @flow
import fs from 'fs';
import path from 'path';
import type { FormattedConstNames, WriteThemeUpdateParams } from '../types';

const formatConstNames = (fileName: string): FormattedConstNames => {
  const constNames = {};
  let PREFIX = '';

  const fileNameParts = fileName.split('-');
  if (fileNameParts.length > 1) {
    PREFIX = `${fileNameParts[0].toUpperCase()}_${fileNameParts[1].toUpperCase()}`;
    constNames.themeOutput = `${PREFIX}_THEME_OUTPUT`;
    constNames.themeParams = `${PREFIX}_THEME_PARAMS`;
    return constNames;
  }

  PREFIX = `${fileNameParts[0].toUpperCase()}`;
  constNames.themeOutput = `${PREFIX}_THEME_OUTPUT`;
  constNames.themeParams = `${PREFIX}_THEME_PARAMS`;
  return constNames;
};

export const writeThemeUpdate = ({
  fileName,
  updatedThemeObj,
}: WriteThemeUpdateParams) => {
  const THEME_FILE = path.join(
    __dirname,
    `../../source/renderer/app/themes/daedalus/${fileName}.js`
  );
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

  fs.writeFileSync(THEME_FILE, FILE_CONTENT, {});
};
