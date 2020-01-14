// @flow
import fs from 'fs';
import path from 'path';
import { expect } from 'chai';
import { generateFileNameWithTimestamp } from '../../../../source/common/utils/files';
import ensureDirectoryExists from '../../../../source/main/utils/ensureDirectoryExists';
import type { WebdriverClient } from '../../../types';

export const expectTextInSelector = async (
  client: Object,
  { selector, text }: { selector: string, text: string }
) => {
  await client.waitForText(selector);
  let textOnScreen = await client.getText(selector);
  // The selector could exist multiple times in the DOM
  if (typeof textOnScreen === 'string') textOnScreen = [textOnScreen];
  // We only compare the first result
  expect(textOnScreen[0]).to.equal(text);
};

export const generateScreenshotFilePath = (prefix: string) => {
  const prefixParts = prefix.split('/');
  const testName = prefixParts.pop();
  const testPath = prefixParts.slice(1).join('/');
  const filePath = path.resolve(__dirname, '../../../screenshots/', testPath);
  const extension = 'png';
  const fileName = generateFileNameWithTimestamp({ prefix: testName, extension });
  ensureDirectoryExists(filePath);
  return `${filePath}/${fileName}`;
};

export const getTestNameFromTestFile = (testFile: string) => testFile.split('.feature').join('');

export const getVisibleElementsCountForSelector = async (
  client: Object,
  selectSelector: string,
  waitSelector: string = selectSelector,
  ...waitArgs: Array<*>
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
  client: Object,
  selectSelector: string,
  waitSelector: string = selectSelector,
  ...waitArgs: Array<*>
) => {
  await client.waitForVisible(waitSelector, ...waitArgs);
  return client.elements(selectSelector);
};

export const getVisibleTextsForSelector = async (
  client: Object,
  selector: string
): Promise<Array<string>> => {
  await client.waitForVisible(selector);
  const texts = await client.getText(selector);
  return [].concat(texts);
};

export const saveScreenshot = async (
  context: Object,
  file: any
) => await context.browserWindow
      .capturePage()
      .then(imageBuffer => fs.writeFile(file, imageBuffer))
      .catch(err => {
        // eslint-disable-next-line no-console
        console.log(err);
      });

export const waitAndClick = async (
  client: Object,
  selector: string,
  ...waitArgs: Array<*>
) => {
  await client.waitForVisible(selector, ...waitArgs);
  await client.waitForEnabled(selector, ...waitArgs);
  return client.click(selector);
};

export const waitUntilTextInSelector = async (
  client: Object,
  { selector, text }: { selector: string, text: string }
) =>
  client.waitUntil(async () => {
    await client.waitForText(selector);
    let textOnScreen = await client.getText(selector);
    // The selector could exist multiple times in the DOM
    if (typeof textOnScreen === 'string') textOnScreen = [textOnScreen];
    // We only compare the first result
    return textOnScreen[0] === text;
  });
