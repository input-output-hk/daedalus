import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { waitAndClick, getVisibleElementsCountForSelector } from '../support/helpers/shared-helpers';

Given('I generate {int} addresses', async function (numberOfAddresses) {
  for (let i = 0; i < numberOfAddresses; i++) {
    await waitAndClick(this.client, '.generateAddressButton:not(.WalletReceive_spinning)');
  }
});

When('I click the ShowUsed switch', async function () {
  await waitAndClick(this.client, '.SimpleSwitch_switch');
});

Then('I should see {int} addresses', async function (numberOfAddresses) {
  const addressesFound = await getVisibleElementsCountForSelector(this.client, '.WalletReceive_walletAddress', `.generatedAddress-${numberOfAddresses}`);
  expect(addressesFound).to.equal(numberOfAddresses);
});

Then('They should be ordered by created date descending', async function () {
  const { value: { id: lastGeneratedAddress } } = await this.client.execute(() => daedalus.stores.ada.addresses.lastGeneratedAddress);
  const firstAddressInTheList = await this.client.getText('.generatedAddress-1 .WalletReceive_addressId');
  expect(lastGeneratedAddress).to.equal(firstAddressInTheList);
});

Then('The active address should be the newest one', async function () {
  const { value: { id: lastGeneratedAddress } } = await this.client.execute(() => daedalus.stores.ada.addresses.lastGeneratedAddress);
  const activeAddress = await this.client.getText('.WalletReceive_hash');
  expect(lastGeneratedAddress).to.equal(activeAddress);
});
