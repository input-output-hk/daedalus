// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import BigNumber from 'bignumber.js/bignumber';
import { DECIMAL_PLACES_IN_ADA, LOVELACES_PER_ADA } from '../../../../source/renderer/app/config/numbersConfig';
import { getVisibleTextsForSelector } from '../../../common/e2e/steps/helpers';
import { getWalletByName, fillOutWalletSendForm } from '../../../wallets/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

// This step ensures sequential creation of given transactions
// use only when the order is important because it's slower!
Given(
  /^I have made the following transactions:$/,
  { timeout: 40000 },
  async function(table) {
    const txData = table.hashes().map(t => ({
      walletId: getWalletByName.call(this, t.source).id,
      destinationWalletId: getWalletByName.call(this, t.destination).id,
      amount: parseInt(new BigNumber(t.amount).times(LOVELACES_PER_ADA), 10),
      passphrase: 'Secret1234',
    }));
    this.transactions = [];
    // Sequentially (and async) create transactions with for loop
    for (const tx of txData) {
      const txResponse = await this.client.executeAsync((transaction, done) => {
        daedalus.stores.addresses
          .getAddressesByWalletId(transaction.destinationWalletId)
          .then(addresses =>
            daedalus.api.ada.createTransaction(
              window.Object.assign(transaction, {
                address: addresses[0].id, // First address of receiving wallet
              })
            )
          )
          .then(done);
      }, tx);
      this.transactions.push(txResponse);
    }
  }
);

When(/^I click on the show more transactions button$/, async function() {
  await this.waitAndClick('.WalletTransactionsList_showMoreTransactionsButton');
});

When(/^I can see the send form$/, function() {
  return this.client.waitForVisible('.WalletSendForm');
});

When(/^I fill out the wallet send form with:$/, function(table) {
  return fillOutWalletSendForm.call(this, table.hashes()[0]);
});

When(
  /^I fill out the send form with a transaction to "([^"]*)" wallet:$/,
  async function(walletName, table) {
    const values = table.hashes()[0];
    const walletId = getWalletByName.call(this, walletName).id;
    const walletAddress = await this.client.executeAsync((id, done) => {
      daedalus.api.ada
        .getAddresses({ walletId: id, isLegacy: false })
        .then(response => done(response[0].id))
        .catch(error => done(error));
    }, walletId);
    values.address = walletAddress.value;
    return fillOutWalletSendForm.call(this, values);
  }
);

When(/^the transaction fees are calculated$/, async function() {
  this.fees = await this.client.waitUntil(async () => {
    // Expected transactionFeeText format "+ 0.000001 of fees"
    const transactionFeeText = await this.client.getText(
      '.AmountInputSkin_fees'
    );
    const transactionFeeAmount = new BigNumber(transactionFeeText.substr(2, 8));
    return transactionFeeAmount.greaterThan(0) ? transactionFeeAmount : false;
  });
});

When(/^I click on the next button in the wallet send form$/, async function() {
  const submitButton = '.WalletSendForm_nextButton';
  await this.client.waitForVisible(submitButton);
  return this.client.click(submitButton);
});

When(/^I see send money confirmation dialog$/, function() {
  return this.client.waitForVisible('.WalletSendConfirmationDialog_dialog');
});

When(
  /^I enter wallet spending password in confirmation dialog "([^"]*)"$/,
  async function(password) {
    await this.client.setValue(
      '.WalletSendConfirmationDialog_passphrase input',
      password
    );
  }
);

When(/^I submit the wallet send form$/, async function() {
  await this.client.waitForEnabled(
    '.WalletSendConfirmationDialog_dialog .confirmButton'
  );
  return this.client.click(
    '.WalletSendConfirmationDialog_dialog .confirmButton'
  );
});

Then(
  /^I should see the following error messages on the wallet send form:$/,
  async function(data) {
    const errorSelector = '.WalletSendForm_component .SimpleFormField_error';
    await this.client.waitForText(errorSelector);
    let errorsOnScreen = await this.client.getText(errorSelector);
    if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
    const errors = data.hashes();
    for (let i = 0; i < errors.length; i++) {
      const expectedError = await this.intl(errors[i].message);
      expect(errorsOnScreen[i]).to.equal(expectedError);
    }
  }
);

// TODO: refactor this to a less hackish solution (fees cannot easily be calculated atm)
Then(/^the latest transaction should show:$/, async function(table) {
  const expectedData = table.hashes()[0];
  await this.client.waitForVisible('.Transaction_title');
  let transactionTitles = await this.client.getText('.Transaction_title');
  transactionTitles = [].concat(transactionTitles);
  const expectedTransactionTitle = await this.intl(expectedData.title, {
    currency: 'Ada',
  });
  expect(expectedTransactionTitle).to.equal(transactionTitles[0]);
  let transactionAmounts = await this.client.getText('.Transaction_amount');
  transactionAmounts = [].concat(transactionAmounts);
  // Transaction amount includes transaction fees so we need to
  // substract them in order to get a match with expectedData.amountWithoutFees.
  // NOTE: we use "add()" as this is outgoing transaction and amount is a negative value!
  const transactionAmount = new BigNumber(transactionAmounts[0]);
  const transactionAmountWithoutFees = transactionAmount
    .add(this.fees)
    .toFormat(DECIMAL_PLACES_IN_ADA);
  expect(expectedData.amountWithoutFees).to.equal(transactionAmountWithoutFees);
});

Then(/^I should not see any transactions$/, async function() {
  await this.client.waitForVisible('.Transaction_component', null, true);
});

Then(/^I should see the no recent transactions message$/, async function() {
  await this.client.waitForVisible('.WalletNoTransactions_label');
});

Then(/^I should see the following transactions:$/, async function(table) {
  // Prepare expected transaction data
  const expectedTxs = await Promise.all(
    table.hashes().map(async tx => {
      let title;
      switch (tx.type) {
        case 'income':
          title = 'wallet.transaction.received';
          break;
        case 'expend':
          title = 'wallet.transaction.sent';
          break;
        default:
          throw new Error('unknown transaction type');
      }
      return {
        title: await this.intl(title, { currency: 'Ada' }),
        amount: new BigNumber(tx.amount).toFormat(DECIMAL_PLACES_IN_ADA),
      };
    })
  );

  // Collect data of visible transactions on screen
  const txTitles = await getVisibleTextsForSelector(
    this.client,
    '.Transaction_title'
  );
  const txAmounts = await getVisibleTextsForSelector(
    this.client,
    '.Transaction_amount'
  );
  const visibleTxs = txTitles.map((title, index) => ({
    title,
    amount: txAmounts[index],
  }));

  expect(expectedTxs).to.deep.equal(visibleTxs);
});
