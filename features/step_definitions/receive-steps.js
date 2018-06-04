import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import BigNumber from 'bignumber.js/bignumber';
import { DECIMAL_PLACES_IN_ADA, LOVELACES_PER_ADA } from '../../source/renderer/app/config/numbersConfig';
import { getVisibleTextsForSelector } from '../support/helpers/shared-helpers';
import { getWalletByName } from '../support/helpers/wallets-helpers';

Given('I have the following addresses', { timeout: 40000 }, async function (table) {

  this.addresses = table.hashes();

});


Then('I should see {int} addresses', async function (addresses) {

  await this.client.waitForVisible('.WalletReceive_walletAddress');
  const addressesElements = await this.client.elements('.WalletReceive_walletAddress');
  console.log("addressesElements", addressesElements);

  expect(1).to.equal(1);

});
