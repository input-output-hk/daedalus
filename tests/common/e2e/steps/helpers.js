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
  let textOnScreen = await waitAndGetText.call({ client }, selector);
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
  const texts = await waitAndGetText.call({ client }, selector);
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

export const waitAndClick = async function(
  selector: string,
  ...waitArgs: Array<*>
) {
  await this.client.waitForVisible(selector, ...waitArgs);
  await this.client.waitForEnabled(selector, ...waitArgs);
  return this.client.click(selector);
};

export const waitAndGetText = async function(
  selector: string,
  ...waitArgs: Array<*>
) {
  await this.client.waitForText(selector);
  return this.client.getText(selector);
};

export const waitUntilTextInSelector = async (
  client: Object,
  { selector, text, ignoreCase = false }: { selector: string, text: string, ignoreCase?: boolean }
) =>
  client.waitUntil(async () => {
    let textOnScreen = await waitAndGetText.call({ client }, selector);
    // The selector could exist multiple times in the DOM
    if (typeof textOnScreen === 'string') textOnScreen = [textOnScreen];
    // We only compare the first result
    if (ignoreCase) {
      return textOnScreen[0].toLowerCase() === text.toLowerCase();
    } else {
      return textOnScreen[0] === text;
    }
  });

export const timeout = (ms: number) => {
  return new Promise<void>(resolve => setTimeout(resolve, ms));
};

export const scrollIntoView = async (client: Object, targetSelector: string) => {
  const isVisibleWithinViewport = await client.isVisibleWithinViewport(targetSelector);
  if (!isVisibleWithinViewport) {
    await client.execute((target) => {
      const targetElement = window.document.evaluate(
        target,
        window.document,
        null,
        window.XPathResult.FIRST_ORDERED_NODE_TYPE,
        null
      ).singleNodeValue;
      targetElement.scrollIntoView();
    }, targetSelector);
    // awaits for smooth scroll-behavior
    await timeout(500);
  }
};

export const clickInputByLabel = async function(label: string, isExactText?: boolean = true) {
  const className = 'SimpleFormField_label'
  const textSelector = isExactText
    ? `text()="${label}"`
    : `contains(text(),'${label}'`;
  const selector = `//label[@class="${className}" and ${textSelector}]//following-sibling::div//input`;
  await this.waitAndClick(selector);
}

export const clickOptionByValue = async function(value: string) {
  const selector = `(//li[contains(@class, 'SimpleOptions_option')]//span[text()="${value}"])`;
  await this.waitAndClick(selector);
}

export const clickOptionByIndex = async function(index: number) {
  const selector = `(//div[contains(@class, 'SimpleSelect_isOpen')]//li[contains(@class, 'SimpleOptions_option')])[${index + 1}]`;
  await this.waitAndClick(selector);
}

export const getInputValueByLabel = async function(label: string, isExactText?: boolean = true) {
  const className = 'SimpleFormField_label'
  const textSelector = isExactText
    ? `text()="${label}"`
    : `contains(text(),'${label}'`;
  const selector = `//label[@class="${className}" and ${textSelector}]//following-sibling::div//input`;
  await this.client.waitForVisible(selector);
  const text = await this.client.getValue(selector);
  return text;
}
