// @flow
import { Given, Then, When } from 'cucumber';
import { expect } from 'chai';
import {
  getVisibleElementsCountForSelector,
  waitAndClick,
} from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;
const SELECTORS = {
  ADDRESS_ACTIVE: '.WalletReceive_hash',
  ADDRESS_COMPONENT: '.Address',
  ADDRESS_USED_ITN: '.AddressItn_usedWalletAddress',
  ADDRESS_USED: '.AddressRandom_usedWalletAddress',
  GENERATE_ADDRESS_BTN: '.generateAddressButton:not(.WalletReceive_spinning)',
  GENERATE_ADDRESS_PASSWORD_INPUT: '.WalletReceiveRandom_spendingPassword .SimpleFormField_inputWrapper input',
  SHOW_USED_SWITCH: '.SimpleSwitch_switch',
};

Given('I create {int} addresses', async function(numberOfAddresses) {
  for (let i = 0; i < numberOfAddresses; i++) {
    await this.client.waitForExist(SELECTORS.GENERATE_ADDRESS_PASSWORD_INPUT);
    await this.client.setValue(SELECTORS.GENERATE_ADDRESS_PASSWORD_INPUT, 'Secret1234');
    await this.waitAndClick(SELECTORS.GENERATE_ADDRESS_BTN);
  }
});

Given('I have {int} generated wallet addresses', async function(numberOfAddresses) {
  for (let i = 0; i < numberOfAddresses; i++) {
    await this.client.executeAsync(
      (done) => {
        const { active } = daedalus.stores.wallets;
        daedalus.stores.addresses._createByronWalletAddress({
          walletId: active ? active.id : null,
          passphrase: 'Secret1234',
        })
        .then(done)
      }
    );
  }
});

When('I click the ShowUsed switch', async function() {
  await this.waitAndClick(SELECTORS.SHOW_USED_SWITCH);
});

When(
  /^I enter wallet password in generate address input field "([^"]*)"$/,
  async function(password) {
    const selector = '.WalletReceiveRandom_spendingPassword .SimpleFormField_inputWrapper input';
    await this.client.waitForExist(selector);
    await this.client.setValue(selector, password);
  }
);

Then('I should see {int} used addresses', { timeout: 60000 }, async function(
  numberOfAddresses
) {
  let addressSelector = SELECTORS.ADDRESS_USED;
  const isIncentivizedTestnet = await this.client.execute(() => global.isIncentivizedTestnet);
  if (isIncentivizedTestnet.value) {
    addressSelector = SELECTORS.ADDRESS_USED_ITN;
    await this.client.waitForVisible('.VirtualAddressesList_list');

    await this.client.execute(() => {
      const scrollableListContainer = window.document.getElementsByClassName(
        'ReactVirtualized__Grid__innerScrollContainer'
      );
      const scrollableList = window.document.getElementsByClassName(
        'VirtualAddressesList_list'
      );
      const listHeight = scrollableListContainer[0].getBoundingClientRect()
        .height;

      // Scroll to bottom
      scrollableList[0].scroll(0, listHeight);
    });
  }

  const addressesFound = await getVisibleElementsCountForSelector(
    this.client,
    addressSelector,
    addressSelector,
    60000
  );
  expect(addressesFound).to.equal(numberOfAddresses);
});

Then('I should not see any used addresses', { timeout: 60000 }, async function() {
  await this.client.waitForVisible('.VirtualAddressesList_list');

  await this.client.execute(() => {
    const scrollableListContainer = window.document.getElementsByClassName(
      'ReactVirtualized__Grid__innerScrollContainer'
    );
    const scrollableList = window.document.getElementsByClassName(
      'VirtualAddressesList_list'
    );
    const listHeight = scrollableListContainer[0].getBoundingClientRect()
      .height;

    // Scroll to bottom
    scrollableList[0].scroll(0, listHeight);
  });
  const isIncentivizedTestnet = await this.client.execute(() => global.isIncentivizedTestnet);
  await this.client.waitForVisible(isIncentivizedTestnet.value ? SELECTORS.ADDRESS_USED_ITN : SELECTORS.ADDRESS_USED, null, true);
});

Then('I should see {int} addresses', async function(numberOfAddresses) {
  let addresses = await this.client.getAttribute(
    '.Address',
    'class'
  );
  if (!Array.isArray(addresses)) {
    addresses = [addresses];
  };
  expect(addresses.length).to.equal(numberOfAddresses);
});

Then('I should see the following addresses:', async function(table) {
  const expectedAdresses = table.hashes();
  let addresses;
  await this.client.waitUntil(async () => {
    addresses = await this.client.getAttribute(SELECTORS.ADDRESS_COMPONENT, 'class');
    return addresses.length === expectedAdresses.length;
  });
  if (addresses) {
    addresses.forEach((address, index) =>
      expect(address).to.include(expectedAdresses[index].ClassName)
    );
  }
});

Then('The active address should be the newest one', async function() {
  const {
    value: { id: lastGeneratedAddress },
  } = await this.client.execute(
    () => daedalus.stores.addresses.lastGeneratedAddress
  );
  const activeAddress = await this.client.getText('.WalletReceiveRandom_hash');
  expect(lastGeneratedAddress).to.equal(activeAddress);
});
