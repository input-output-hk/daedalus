import { expect } from 'chai';

export const waitAndClick = async (client, selector, ...waitArgs) => {
  await client.waitForVisible(selector, ...waitArgs);
  await client.waitForEnabled(selector, ...waitArgs);
  return client.click(selector);
};

export const expectTextInSelector = async (client, { selector, text }) => {
  await client.waitForText(selector);
  let textOnScreen = await client.getText(selector);
  // The selector could exist multiple times in the DOM
  if (typeof textOnScreen === 'string') textOnScreen = [textOnScreen];
  // We only compare the first result
  expect(textOnScreen[0]).to.equal(text);
};

export const waitUntilTextInSelector = async (client, { selector, text }) => (
  await client.waitUntil(async () => {
    await client.waitForText(selector);
    let textOnScreen = await client.getText(selector);
    // The selector could exist multiple times in the DOM
    if (typeof textOnScreen === 'string') textOnScreen = [textOnScreen];
    // We only compare the first result
    return textOnScreen[0] === text;
  })
);

export const getVisibleElementsForSelector = async (client, selectSelector, waitSelector = selectSelector, ...waitArgs) => {
  await client.waitForVisible(waitSelector, ...waitArgs);
  return await client.elements(selectSelector);
};

export const getVisibleElementsCountForSelector = async (client, selectSelector, waitSelector = selectSelector, ...waitArgs) => {
  const elements = await getVisibleElementsForSelector(client, selectSelector, waitSelector, ...waitArgs);
  return elements.value ? elements.value.length : 0;
};

export const getVisibleTextsForSelector = async (client, selector) => {
  await client.waitForVisible(selector);
  const texts = await client.getText(selector);
  return [].concat(texts);
};
