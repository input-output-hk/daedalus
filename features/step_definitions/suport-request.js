import { Given, Then } from 'cucumber';
import {
  switchToWindow, getFieldValue, fieldHasValue, resetSupportWindow
} from '../support/helpers/support-request-helpers';

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

  await this.client.waitUntil(async () => {
    const fieldText = await getFieldValue(this, selects, 0);
    return fieldHasValue(fieldText);
  });

  for (const item of fields) {
    const index = zenDeskFormSelects.indexOf(item.field);
    const fieldText = await getFieldValue(this, selects, index);
    if (!fieldHasValue(fieldText)) throw new Error(`${item.field} field is not filled`);
  }

  await resetSupportWindow(this);

});

Then(/^The compressed logs zip file was attached$/, async function () {
  await this.client.waitForExist(ATTACHED_LOG_CONTAINER);
  await resetSupportWindow(this);
});

Given(/^I click the cancel button$/, async function () {
  return this.waitAndClick('footer button');
});

Then(/^The window should close$/, async function () {
  await this.client.waitUntil(async () => {
    const windows = await this.client.getTabIds();
    return windows.length === 1;
  });
  await switchToWindow(this, 0);
});
