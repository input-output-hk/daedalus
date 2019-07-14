// @flow
/* eslint-disable */
import fs from 'fs';
import chalk from 'chalk';
import type { WriteThemeUpdateParams } from '../types';

export const writeThemeUpdate = ({
  fileName,
  updatedThemeObj,
}: WriteThemeUpdateParams) => {
  console.log(`fileName: ${fileName}`);
  console.log(`updatedThemeObj: ${updatedThemeObj}`);
  // create write file logic
};
