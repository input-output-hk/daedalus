import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import {
  waitAndClick,
  getVisibleElementsCountForSelector,
} from '../support/helpers/shared-helpers';

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
  const addressesFound = await getVisibleElementsCountForSelector(
    this.client,
    '.Address_usedWalletAddress',
    '.Address_usedWalletAddress',
    60000
  );
  expect(addressesFound).to.equal(numberOfAddresses);
});

Then('I should see {int} addresses', async function(numberOfAddresses) {
  const addressesFound = await getVisibleElementsCountForSelector(
    this.client,
    '.Address_component'
  );
  expect(addressesFound).to.equal(numberOfAddresses);
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
