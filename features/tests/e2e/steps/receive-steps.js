import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import {
  waitAndClick,
  getVisibleElementsCountForSelector,
} from '../helpers/shared-helpers';

Given('I generate {int} addresses', async function(numberOfAddresses) {
  for (let i = 0; i < numberOfAddresses; i++) {
    await waitAndClick(
      this.client,
      '.generateAddressButton:not(.WalletReceive_spinning)'
    );
  }
});

When('I click the ShowUsed switch', async function() {
  await waitAndClick(this.client, '.SimpleSwitch_switch');
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
    '.Address_usedWalletAddress',
    '.Address_usedWalletAddress',
    60000
  );
  expect(addressesFound).to.equal(numberOfAddresses);
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
    addresses = await this.client.getAttribute('.Address_component', 'class');
    return addresses.length === expectedAdresses.length;
  });
  addresses.forEach((address, index) =>
    expect(address).to.include(expectedAdresses[index].ClassName)
  );
});

Then('The active address should be the newest one', async function() {
  const {
    value: { id: lastGeneratedAddress },
  } = await this.client.execute(
    () => daedalus.stores.addresses.lastGeneratedAddress
  );
  const activeAddress = await this.client.getText('.WalletReceive_hash');
  expect(lastGeneratedAddress).to.equal(activeAddress);
});
