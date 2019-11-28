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

  const addressesFound = await getVisibleElementsCountForSelector(
    this.client,
    SELECTORS.ADDRESS_USED,
    SELECTORS.ADDRESS_USED,
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

  await this.client.waitForVisible(SELECTORS.ADDRESS_USED, null, true);
});

Then('I should see {int} addresses', async function(numberOfAddresses) {
  const addresses = await this.client.getAttribute(
    '.Address_component',
    'class'
  );
  const lastAddressClass = addresses[addresses.length - 1];
  const lastGeneratedAddressClasses = lastAddressClass.split(' ');
  const lastGeneratedAddressNumber = lastGeneratedAddressClasses[0].split(
    '-'
  )[1];

  expect(parseInt(lastGeneratedAddressNumber, 10)).to.equal(numberOfAddresses);
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
