import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { waitAndClick, waitAndSelect } from '../support/helpers/shared-helpers';

Given('I generate {int} addresses', async function (numberOfAddresses) {
  for (let i = 0; i < numberOfAddresses; i++) {
    await waitAndClick(this.client, '.generateAddressButton:not(.WalletReceive_spinning)');
  }
  this.addresses = await waitAndSelect(this.client, '.WalletReceive_walletAddress', '.generatedAddress-' + numberOfAddresses, 50000);
  this.numberOfAddressesGenerated = numberOfAddresses;
});

When('I mark the last {int} addresses as used', (addressesToBeMarkedAsUsed) => {
  console.log('addressesToBeMarkedAsUsed', addressesToBeMarkedAsUsed);
  return addressesToBeMarkedAsUsed;
});

Then('I should see {int} addresses', async function (numberOfAddresses) {

  expect(this.addresses.value.length).to.equal(numberOfAddresses);

});
