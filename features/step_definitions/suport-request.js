import { Given, Then } from 'cucumber';

const IFRAME_ID = '#webWidget';
const SELECTORS = '[data-garden-id="select.select_view"]';
const ATTACHED_LOG_CONTAINER = '.src-component-attachment-Attachment-container';

const zenDeskFormSelects = [
  'product',
  'supportLanguage',
  'operatingSystem',
  'productVersion',
  'productAttribute',
];

const switchToWindow = async (context, index) => {
  let windows;
  await context.client.waitUntil(async () => {
    windows = await context.client.getTabIds();
    return windows.length > index;
  });
  context.client.switchTab(windows[index]);
  return windows;
};

Given(/^I am on the main window$/, async function () {
  await switchToWindow(this, 0);
  return this.client.frameParent();
});

Given(/^I open the Support Request window$/, { timeout: 40000 }, async function () {
  await switchToWindow(this, 0);
  await this.waitAndClick('.SupportSettings_component button');
  await switchToWindow(this, 1);
  await this.client.waitForExist(IFRAME_ID);
  const iframe = await this.client.element(IFRAME_ID);
  return this.client.frame(iframe.value);
});

Then(/^The following fields are filled:$/, async function (table) {
  await this.client.waitForExist(SELECTORS);
  const selects = await this.client.elements(SELECTORS);
  const fields = table.hashes();

  const getFieldValue = async (index) => {
    const fieldText = await this.client.elementIdText(selects.value[index].ELEMENT);
    return fieldText.value;
  };

  const fieldHasValue = (value) => value && value !== '-';

  await this.client.waitUntil(async () => {
    const fieldText = await getFieldValue(0);
    return fieldHasValue(fieldText);
  });

  for (const item of fields) {
    const index = zenDeskFormSelects.indexOf(item.field);
    const fieldText = await getFieldValue(index);
    if (!fieldHasValue(fieldText)) throw new Error(`${item.field} field is not filled`);
  }

});

Then(/^The compressed logs zip was attached$/, async function () {
  return await this.client.waitForExist(ATTACHED_LOG_CONTAINER);
});

Given(/^I click the cancel button$/, async function () {
  return this.waitAndClick('footer button');
});

Then(/^The window should close$/, async function () {
  return await this.client.waitUntil(async () => {
    const windows = await this.client.getTabIds();
    return windows.length === 1;
  });
});
