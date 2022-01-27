import { Given, Then, When } from "cucumber";
import { expect } from "chai";
import { find } from "lodash";
import { getVisibleElementsCountForSelector } from "../../../common/e2e/steps/helpers";
import { getWalletByName } from "../../../wallets/e2e/steps/helpers";

const SELECTORS = {
  ADDRESS_ACTIVE: '.WalletReceive_hash',
  ADDRESS_COMPONENT: '.Address',
  ADDRESS_USED: '.AddressSequential_usedWalletAddress',
  GENERATE_ADDRESS_BTN: '.generateAddressButton:not(.WalletReceive_spinning)',
  GENERATE_ADDRESS_PASSWORD_INPUT: '.WalletReceiveRandom_spendingPassword .SimpleFormField_inputWrapper input',
  SHOW_USED_SWITCH: '.SimpleSwitch_switch'
};
Given('I create {int} addresses', async function (numberOfAddresses) {
  for (let i = 0; i < numberOfAddresses; i++) {
    await this.client.waitForExist(SELECTORS.GENERATE_ADDRESS_PASSWORD_INPUT);
    await this.client.setValue(SELECTORS.GENERATE_ADDRESS_PASSWORD_INPUT, 'Secret1234');
    await this.waitAndClick(SELECTORS.GENERATE_ADDRESS_BTN);
  }
});
Given('I have {int} generated wallet addresses', async function (numberOfAddresses) {
  for (let i = 0; i < numberOfAddresses; i++) {
    await this.client.executeAsync(done => {
      const {
        active
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      } = daedalus.stores.wallets;

      if (!active) {
        return done();
      }

      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      return daedalus.stores.addresses._createByronWalletAddress({
        walletId: active.id,
        passphrase: 'Secret1234'
      }).then(done);
    });
  }
});
When('I click the ShowUsed switch', async function () {
  await this.waitAndClick(SELECTORS.SHOW_USED_SWITCH);
});
When(/^I enter wallet password in generate address input field "([^"]*)"$/, async function (password) {
  const selector = '.WalletReceiveRandom_spendingPassword .SimpleFormField_inputWrapper input';
  await this.client.waitForExist(selector);
  await this.client.setValue(selector, password);
});
When('I click "Generate a new address" button', async function () {
  await this.waitAndClick('.generateAddressButton');
});
Then('I should see {int} used addresses', {
  timeout: 60000
}, async function (numberOfAddresses) {
  const addressSelector = SELECTORS.ADDRESS_USED;
  await this.client.waitForVisible('.VirtualAddressesList_list');
  await this.client.execute(() => {
    const scrollableListContainer = window.document.getElementsByClassName('ReactVirtualized__Grid__innerScrollContainer');
    const scrollableList = window.document.getElementsByClassName('VirtualAddressesList_list');
    const listHeight = scrollableListContainer[0].getBoundingClientRect().height;
    // Scroll to bottom
    scrollableList[0].scroll(0, listHeight);
  });
  const addressesFound = await getVisibleElementsCountForSelector(this.client, addressSelector, addressSelector, 60000);
  expect(addressesFound).to.equal(numberOfAddresses);
});
Then('I should not see any used addresses', {
  timeout: 60000
}, async function () {
  await this.client.waitForVisible('.VirtualAddressesList_list');
  await this.client.execute(() => {
    const scrollableListContainer = window.document.getElementsByClassName('ReactVirtualized__Grid__innerScrollContainer');
    const scrollableList = window.document.getElementsByClassName('VirtualAddressesList_list');
    const listHeight = scrollableListContainer[0].getBoundingClientRect().height;
    // Scroll to bottom
    scrollableList[0].scroll(0, listHeight);
  });
  await this.client.waitForVisible(SELECTORS.ADDRESS_USED, null, true);
});
Then('I should see {int} addresses', async function (numberOfAddresses) {
  let addresses = await this.client.getAttribute('.Address', 'class');

  if (!Array.isArray(addresses)) {
    addresses = [addresses];
  }

  ;
  expect(addresses.length).to.equal(numberOfAddresses);
});
Then('I should see the following addresses:', async function (table) {
  const expectedAdresses = table.hashes();
  let addresses;
  await this.client.waitUntil(async () => {
    addresses = await this.client.getAttribute(SELECTORS.ADDRESS_COMPONENT, 'class');
    return addresses.length === expectedAdresses.length;
  });

  if (addresses) {
    addresses.forEach((address, index) => expect(address).to.include(expectedAdresses[index].ClassName));
  }
});
Then('The active address should be the newest one', async function () {
  const {
    value: {
      id: lastGeneratedAddress
    }
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  } = await this.client.execute(() => daedalus.stores.addresses.lastGeneratedAddress);
  const activeAddress = await this.client.getText('.WalletReceiveRandom_hash');
  expect(lastGeneratedAddress).to.equal(activeAddress);
});
Then(/^I should see the following error messages on the wallet receive screen:$/, async function (data) {
  let errorsOnScreen = await this.waitAndGetText('.WalletReceiveRandom_error');
  if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
  const errors = data.hashes();

  for (let i = 0; i < errors.length; i++) {
    const expectedError = await this.intl(errors[i].message);
    expect(errorsOnScreen[i]).to.equal(expectedError);
  }
});
Then(/^The active address belongs to "([^"]*)" wallet$/, async function (walletName) {
  const {
    id: walletId,
    isLegacy
  } = await getWalletByName.call(this, walletName);
  const walletAddresses = await this.client.executeAsync((walletId, isLegacy, done) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.getAddresses({
      walletId,
      isLegacy
    }).then(response => done(response)).catch(error => done(error));
  }, walletId, isLegacy);
  const activeAddress = await this.client.getText('.WalletReceiveRandom_hash');
  const walletAddress = find(walletAddresses.value, address => address.id === activeAddress);
  expect(walletAddress.id).to.equal(activeAddress);
});