import fs from 'fs';
import path from 'path';
import { generateFileNameWithTimestamp } from '../../../source/common/fileName';
import ensureDirectoryExists from '../../../source/main/utils/ensureDirectoryExists';

export const generateScreenshotFilePath = (testName) => {
  const filePath = path.resolve(__dirname, '../../screenshots', testName);
  const fileName = generateFileNameWithTimestamp(testName, 'png');
  ensureDirectoryExists(filePath);
  return `${filePath}/${fileName}`;
};

export const getTestNameFromTestFile = (testFile) => testFile
  .replace('features/', '')
  .replace('.feature', '');

export const saveScreenshot = async (context, file) => {
  await context.app.browserWindow.capturePage()
    .then((imageBuffer) => fs.writeFile(file, imageBuffer))
    .catch((err) => {
      console.log(err);
    });
};
