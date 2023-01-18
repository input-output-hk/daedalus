// @flow
import { When, Then } from '@cucumber/cucumber';
import en from '../../../../source/renderer/app/i18n/locales/en-US.json';

const SELECT_CURRENCY_TEXT = en['settings.wallets.currency.selectLabel'];

const CONVERTED_CURRENCY_WALLET_DISPLAY_GENERAL =
  '//*[@class="WalletSummaryCurrency_currencyRate"]';
const DISPLAY_BALANCE_OTHER_CURRENCY_TOGGLE_STATUS_OFF =
  '//*[@class="SimpleSwitch_switch SwitchOverrides_switch"]';
const DISPLAY_BALANCE_OTHER_CURRENCY_TOGGLE_STATUS_ON =
  '//*[@class="NormalSwitch_component SimpleSwitch_root SwitchOverrides_root SimpleSwitch_checked SwitchOverrides_checked"]';
const SELECT_CURRENCY_DROPDOWN = `//*[@label="${SELECT_CURRENCY_TEXT}"]`;

When(/^I open currency selection dropdown$/, function () {
  return this.client.click(SELECT_CURRENCY_DROPDOWN);
});

When(/^I select "([^"]*)" as the selected displayed currency$/, function (
  currency
) {
  return this.client.click(`//*[contains(text(), "${currency}")]`);
});

Then(/^I should see "([^"]*)" displayed beside wallet balance$/, function (
  ticker
) {
  return this.client.waitForVisible(
    `//*[@class="WalletSummaryCurrency_currencyRate" and text()="${ticker}"]`
  );
});

When(
  /^I toggle the button (on|off) to change if I want to see my ada balance in other currency's$/,
  function (switchStatus) {
    const selector =
      switchStatus === 'on'
        ? DISPLAY_BALANCE_OTHER_CURRENCY_TOGGLE_STATUS_OFF
        : DISPLAY_BALANCE_OTHER_CURRENCY_TOGGLE_STATUS_ON;
    return this.waitAndClick(selector);
  }
);

Then(/^the currency selection box is (hidden|visible)$/, function (state) {
  const invisibility: boolean = state !== 'visible';
  return this.client.waitForVisible(
    SELECT_CURRENCY_DROPDOWN,
    null,
    invisibility
  );
});

Then(
  /^The wallet summary screen (does|does not) show ada balance in other currency's placeholder$/,
  function (status) {
    const invisible: boolean = status !== 'does';
    return this.client.waitForVisible(
      CONVERTED_CURRENCY_WALLET_DISPLAY_GENERAL,
      null,
      invisible
    );
  }
);
