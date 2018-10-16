import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import BigNumber from 'bignumber.js/bignumber';
import { DECIMAL_PLACES_IN_ADA, LOVELACES_PER_ADA } from '../../source/renderer/app/config/numbersConfig';
import { getVisibleTextsForSelector } from '../support/helpers/shared-helpers';
import { getWalletByName } from '../support/helpers/wallets-helpers';

// This step ensures sequential creation of given transactions
// use only when the order is important because it's slower!
Given(/^I have made the following transactions:$/, { timeout: 40000 }, async function (table) {
  const txData = table.hashes().map((t) => ({
    walletId: getWalletByName.call(this, t.source).id,
    destinationWalletId: getWalletByName.call(this, t.destination).id,
    amount: parseInt(new BigNumber(t.amount).times(LOVELACES_PER_ADA), 10),
    spendingPassword: t.password || null,
  }));
  this.transactions = [];
  // Sequentially (and async) create transactions with for loop
  for (const tx of txData) {
    const txResponse = await this.client.executeAsync((transaction, done) => (
      new window.Promise((resolve) => (
        // Need to fetch the wallets data async and wait for all results
        window.Promise.all([
          daedalus.stores.addresses.getAccountIndexByWalletId(transaction.walletId),
          daedalus.stores.addresses.getAddressesByWalletId(transaction.destinationWalletId)
        ]).then(results => (
          daedalus.api.ada.createTransaction(window.Object.assign(transaction, {
            accountIndex: results[0], // Account index of sender wallet
            address: results[1][0].id // First address of receiving wallet
          })).then(resolve)
        ))
      )).then(done)
    ), tx);
    this.transactions.push(txResponse);
  }
});

Then(/^I should not see any transactions$/, async function () {
  await this.client.waitForVisible('.Transaction_component', null, true);
});

Then(/^I should see the no recent transactions message$/, async function () {
  await this.client.waitForVisible('.WalletNoTransactions_label');
});

Then(/^I should see the following transactions:$/, async function (table) {
  // Prepare expected transaction data
  const expectedTxs = await Promise.all(table.hashes().map(async (tx) => {
    let title;
    switch (tx.type) {
      case 'income': title = 'wallet.transaction.received'; break;
      case 'expend': title = 'wallet.transaction.sent'; break;
      default: throw new Error('unknown transaction type');
    }
    return {
      title: await this.intl(title, { currency: 'ADA' }),
      amount: new BigNumber(tx.amount).toFormat(DECIMAL_PLACES_IN_ADA),
    };
  }));

  // Collect data of visible transactions on screen
  const txTitles = await getVisibleTextsForSelector(this.client, '.Transaction_title');
  const txAmounts = await getVisibleTextsForSelector(this.client, '.Transaction_amount');
  const visibleTxs = txTitles.map((title, index) => ({
    title,
    amount: txAmounts[index],
  }));

  expect(expectedTxs).to.deep.equal(visibleTxs);
});

When(/^I click on the show more transactions button$/, async function () {
  await this.waitAndClick('.WalletTransactionsList_showMoreTransactionsButton');
});
