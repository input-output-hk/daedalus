import { Given, Then } from 'cucumber';
import { expect } from 'chai';
import { waitAndClick } from '../support/helpers/shared-helpers';

Given('I generate {int} addresses', async function (numberOfAddresses) {
  for (var i=0;i<numberOfAddresses;i++) {
    await waitAndClick(this.client, '.generateAddressButton:not(.WalletReceive_spinning)')
  }
});

Then('I should see {int} addresses', async function (numberOfAddresses) {

  await this.client.waitForVisible('.generatedAddress-' + numberOfAddresses);
  const elements = await this.client.elements('.WalletReceive_walletAddress');

  expect(elements.value.length).to.equal(numberOfAddresses);

});
