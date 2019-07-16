// @flow
/* eslint-disable */
import fs from 'fs';
import path from 'path';
import type { WriteThemeUpdateParams } from '../types';

export const writeThemeUpdate = ({
  fileName,
  updatedThemeObj,
}: WriteThemeUpdateParams) => {
  const THEME_FILE = path.join(
    __dirname,
    `../../source/renderer/app/themes/daedalus/${fileName}.js`
  );
  const TEMP_THEME_FILE = path.join(__dirname, `../daedalus/${fileName}.js`);
  const fileNameParts = fileName.split('-');
  fs.writeFileSync(
    TEMP_THEME_FILE,
    `
    // @flow
    import chroma from 'chroma-js';
    import { createTheme } from '../utils/createTheme';

    export const CARDANO_THEME_CONFIG = ${JSON.stringify(
      updatedThemeObj,
      null,
      2
    )};
  
    export default createTheme({
      config: CARDANO_THEME_CONFIG
    });
  `,
    {}
  );
};
