// @flow
import { When, Then} from 'cucumber';

const CONVERTED_CURRENCY_WALLET_DISPLAY_GENERAL = '//*[@class="WalletSummaryCurrency_currencyRate"]';
const DISPLAY_BALANCE_OTHER_CURRENCYS_TOGGLE_STATUS_ON = '//*[@class="NormalSwitch_component SimpleSwitch_root SwitchOverrides_root SimpleSwitch_checked SwitchOverrides_checked"]';
const DISPLAY_BALANCE_OTHER_CURRENCYS_TOGGLE_STATUS_OFF = '//*[@class="SimpleSwitch_switch SwitchOverrides_switch"]';
const SELECT_CURRENCY_DROPDOWN = '//*[@label="Select currency"]';

When(/^I open currency selection dropdown$/, function() {
  return this.waitAndClick(SELECT_CURRENCY_DROPDOWN);
});

When(/^I select "([^"]*)" as the selected displayed currency$/, function(currency) {
  return this.waitAndClick(`//*[contains(text(), "${currency}")]`);
});

Then(/^I should see "([^"]*)" displayed beside wallet balance$/, function(ticker) {
  return this.client.waitForVisible(`//*[@class="WalletSummaryCurrency_currencyRate" and text()="${ticker}"]`);
});

When(/^I toggle the button (on|off) to change if I want to see my ada balance in other currencys$/, function(switchStatus) {
  let selector = DISPLAY_BALANCE_OTHER_CURRENCYS_TOGGLE_STATUS_ON;
  if(switchStatus === 'on'){
    selector = DISPLAY_BALANCE_OTHER_CURRENCYS_TOGGLE_STATUS_OFF;
  }
  return this.waitAndClick(selector);
});

Then(/^the currency selection box is (hidden|visible)$/, function(state) {
  let invisibility = false;
  if (state === 'hidden') {
    invisibility = true;
  }
  return this.client.waitForVisible(SELECT_CURRENCY_DROPDOWN, null, invisibility);
});

Then(/^The wallet summary screen (does|does not) show ada balance in other currencys placeholder$/, function(switchstatus) {
  let invisible = true;
  if (switchstatus === 'does') {
    invisible = false;
  }
  return this.client.waitForVisible(CONVERTED_CURRENCY_WALLET_DISPLAY_GENERAL, null, invisible);
});
