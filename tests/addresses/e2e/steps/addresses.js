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
  ADDRESS_COMPONENT: '.Address_component',
  ADDRESS_USED: '.Address_usedWalletAddress',
  GENERATE_ADDRESS_BTN: '.generateAddressButton:not(.WalletReceive_spinning)',
  SHOW_USED_SWITCH: '.SimpleSwitch_switch',
};

Given('I generate {int} addresses', async function(numberOfAddresses) {
  for (let i = 0; i < numberOfAddresses; i++) {
    await waitAndClick(
      this.client,
      SELECTORS.GENERATE_ADDRESS_BTN
    );
  }
});

When('I click the ShowUsed switch', async function() {
  await waitAndClick(this.client, SELECTORS.SHOW_USED_SWITCH);
});

Then('I should see {int} used addresses', { timeout: 60000 }, async function(
  numberOfAddresses
) {
  const addressesFound = await getVisibleElementsCountForSelector(
    this.client,
    SELECTORS.ADDRESS_USED,
    SELECTORS.ADDRESS_USED,
    60000
  );
  expect(addressesFound).to.equal(numberOfAddresses);
});

Then('I should see {int} addresses', async function(numberOfAddresses) {
  const addressesFound = await getVisibleElementsCountForSelector(
    this.client,
    SELECTORS.ADDRESS_COMPONENT
  );
  expect(addressesFound).to.equal(numberOfAddresses);
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
  const activeAddress = await this.client.getText(SELECTORS.ADDRESS_ACTIVE);
  expect(lastGeneratedAddress).to.equal(activeAddress);
});
