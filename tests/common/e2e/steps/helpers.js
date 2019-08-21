import fs from 'fs';
import path from 'path';
import { expect } from 'chai';
import { generateFileNameWithTimestamp } from '../../../../source/common/utils/files';
import ensureDirectoryExists from '../../../../source/main/utils/ensureDirectoryExists';

export const expectTextInSelector = async (client, { selector, text }) => {
  await client.waitForText(selector);
  let textOnScreen = await client.getText(selector);
  // The selector could exist multiple times in the DOM
  if (typeof textOnScreen === 'string') textOnScreen = [textOnScreen];
  // We only compare the first result
  expect(textOnScreen[0]).to.equal(text);
};

export const generateScreenshotFilePath = prefix => {
  const filePath = path.resolve(__dirname, '../screenshots', prefix);
  const extension = 'png';
  const fileName = generateFileNameWithTimestamp({ prefix, extension });
  ensureDirectoryExists(filePath);
  return `${filePath}/${fileName}`;
};

export const getTestNameFromTestFile = testFile =>
  testFile.replace('features/', '').replace('.feature', '');

export const getVisibleElementsCountForSelector = async (
  client,
  selectSelector,
  waitSelector = selectSelector,
  ...waitArgs
) => {
  const elements = await getVisibleElementsForSelector(
    client,
    selectSelector,
    waitSelector,
    ...waitArgs
  );
  return elements.value ? elements.value.length : 0;
};

export const getVisibleElementsForSelector = async (
  client,
  selectSelector,
  waitSelector = selectSelector,
  ...waitArgs
) => {
  await client.waitForVisible(waitSelector, ...waitArgs);
  return client.elements(selectSelector);
};

export const getVisibleTextsForSelector = async (client, selector) => {
  await client.waitForVisible(selector);
  const texts = await client.getText(selector);
  return [].concat(texts);
};

export const saveScreenshot = async (context, file) => {
  await context.browserWindow
    .capturePage()
    .then(imageBuffer => fs.writeFile(file, imageBuffer))
    .catch(err => {
      // eslint-disable-next-line no-console
      console.log(err);
    });
};

export const waitAndClick = async (client, selector, ...waitArgs) => {
  await client.waitForVisible(selector, ...waitArgs);
  await client.waitForEnabled(selector, ...waitArgs);
  return client.click(selector);
};

export const waitUntilTextInSelector = async (client, { selector, text }) =>
  client.waitUntil(async () => {
    await client.waitForText(selector);
    let textOnScreen = await client.getText(selector);
    // The selector could exist multiple times in the DOM
    if (typeof textOnScreen === 'string') textOnScreen = [textOnScreen];
    // We only compare the first result
    return textOnScreen[0] === text;
  });
