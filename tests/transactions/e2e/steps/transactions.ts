import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import BigNumber from "bignumber.js/bignumber";
import { DECIMAL_PLACES_IN_ADA, LOVELACES_PER_ADA } from "../../../../source/renderer/app/config/numbersConfig";
import { getVisibleTextsForSelector, clickInputByLabel, clickOptionByIndex } from "../../../common/e2e/steps/helpers";
import { getWalletByName, fillOutWalletSendForm } from "../../../wallets/e2e/steps/helpers";
import { getRawWalletId } from "../../../../source/renderer/app/api/utils";
// This step ensures sequential creation of given transactions
// use only when the order is important because it's slower!
Given(/^I have made the following transactions:$/, {
  timeout: 40000
}, async function (table) {
  const txData = await Promise.all(table.hashes().map(async t => {
    const sourceWallet = await getWalletByName.call(this, t.source);
    const destinationWallet = await getWalletByName.call(this, t.destination);
    return {
      walletId: sourceWallet.id,
      destinationWalletId: destinationWallet.id,
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BigNumber' is not assignable to ... Remove this comment to see the full error message
      amount: parseInt(new BigNumber(t.amount).times(LOVELACES_PER_ADA), 10),
      passphrase: 'Secret1234',
      isLegacy: sourceWallet.isLegacy
    };
  }));
  this.transactions = [];

  // Sequentially (and async) create transactions with for loop
  for (const tx of txData) {
    const txResponse = await this.client.executeAsync((transaction, done) => {
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      daedalus.stores.addresses.getAddressesByWalletId(transaction.destinationWalletId).then(addresses => daedalus.api.ada.createTransaction(window.Object.assign(transaction, {
        address: addresses[0].id // First address of receiving wallet

      }))).then(done);
    }, tx);
    this.transactions.push(txResponse);
  }
});
When(/^I fill out the send form with value equals to "([^"]*)" wallet amount$/, async function (walletName) {
  const wallet = await getWalletByName.call(this, walletName);
  const walletId = getRawWalletId(wallet.id);
  const walletAddress = await this.client.executeAsync((walletId, isLegacy, done) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.getAddresses({
      walletId,
      isLegacy
    }).then(response => done(response[0].id)).catch(error => done(error));
  }, walletId, wallet.isLegacy);
  // Check for pending transactions
  await this.client.executeAsync((wallet, walletAddress, done) => {
    const checkPendingTransactions = () => {
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      if (!daedalus.stores.transactions.pendingTransactionsCount) {
        done();
      } else {
        setTimeout(checkPendingTransactions, 500);
      }
    };

    checkPendingTransactions();
  }, wallet, walletAddress);
  // Fill form when there are no pending transactions
  const values = {
    amount: wallet.amount,
    address: walletAddress.value
  };
  return fillOutWalletSendForm.call(this, values);
});
When(/^I click on the show more transactions button$/, async function () {
  await this.waitAndClick('.WalletTransactionsList_showMoreTransactionsButton');
});
When(/^I can see the send form$/, function () {
  return this.client.waitForVisible('.WalletSendForm_receiverInput');
});
When(/^I fill out the wallet send form with:$/, function (table) {
  return fillOutWalletSendForm.call(this, table.hashes()[0]);
});
When(/^I fill out the send form with a transaction to "([^"]*)" wallet:$/, async function (walletName, table) {
  const values = table.hashes()[0];
  const wallet = await getWalletByName.call(this, walletName);
  const walletId = getRawWalletId(wallet.id);
  // Get Destination wallet address
  const walletAddress = await this.client.executeAsync((walletId, isLegacy, done) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.getAddresses({
      walletId,
      isLegacy
    }).then(response => done(response[0].id)).catch(error => done(error));
  }, walletId, wallet.isLegacy);
  values.address = walletAddress.value;
  return fillOutWalletSendForm.call(this, values);
});
When(/^the transaction fees are calculated$/, async function () {
  this.fees = await this.client.waitUntil(async () => {
    // Expected transactionFeeText format "+ 0.000001 of fees"
    const transactionFeeText = await this.waitAndGetText('.AmountInputSkin_fees');
    const transactionFeeAmount = new BigNumber(transactionFeeText.substr(2, 8));
    return transactionFeeAmount.isGreaterThan(0) ? transactionFeeAmount : false;
  });
});
When(/^I click on the next button in the wallet send form$/, async function () {
  const submitButton = '.WalletSendForm_nextButton';
  await this.client.waitForVisible(submitButton);
  return this.client.click(submitButton);
});
When(/^I see send money confirmation dialog$/, function () {
  return this.client.waitForVisible('.WalletSendConfirmationDialog_dialog');
});
When(/^I enter wallet spending password in confirmation dialog "([^"]*)"$/, async function (password) {
  await this.client.setValue('.WalletSendConfirmationDialog_passphrase input', password);
});
When(/^I submit the wallet send form$/, async function () {
  await this.client.waitForEnabled('.WalletSendConfirmationDialog_dialog .confirmButton');
  return this.client.click('.WalletSendConfirmationDialog_dialog .confirmButton');
});
When(/^I open the transactions filter$/, async function () {
  return this.waitAndClick('.FilterButton_actionButton');
});
When(/^I choose the first time filter option$/, async function () {
  await clickInputByLabel.call(this, 'Time');
  await clickOptionByIndex.call(this, 0);
});
const screenElementSelectors = {
  fromAmount: 'input[name="fromAmount"]',
  toAmount: 'input[name="toAmount"]'
};
When(/^I enter the following filter values:$/, async function (filterTable) {
  const filterValues = filterTable.hashes();

  for (let i = 0; i < filterValues.length; i++) {
    const {
      param: filterParam,
      value: filterValue
    } = filterValues[i];
    const selector = screenElementSelectors[filterParam];
    await this.client.setValue(selector, filterValue);
  }
});
Then(/^I should see the following filter values:$/, async function (filterTable) {
  const filterValues = filterTable.hashes();

  for (let i = 0; i < filterValues.length; i++) {
    const {
      param: filterParam,
      value: expectedValue
    } = filterValues[i];
    const selector = screenElementSelectors[filterParam];
    let currentValue = await this.client.getValue(selector);
    if (Array.isArray(currentValue)) currentValue = currentValue[0];
    expect(currentValue).to.equal(expectedValue);
  }
});
Then(/^I should see the following error messages on the wallet send form:$/, async function (data) {
  const errorSelector = '.WalletSendForm_component .SimpleFormField_error';
  let errorsOnScreen = await this.waitAndGetText(errorSelector);
  if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
  const errors = data.hashes();

  for (let i = 0; i < errors.length; i++) {
    const expectedError = await this.intl(errors[i].message);
    expect(errorsOnScreen[i]).to.equal(expectedError);
  }
});
Then(/^I should see the following error messages on the wallet send confirmation dialog:$/, async function (data) {
  const errorSelector = '.WalletSendConfirmationDialog_dialog .WalletSendConfirmationDialog_error';
  let errorsOnScreen = await this.waitAndGetText(errorSelector);
  if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
  const errors = data.hashes();

  for (let i = 0; i < errors.length; i++) {
    const expectedError = await this.intl(errors[i].message);
    expect(errorsOnScreen[i]).to.equal(expectedError);
  }
});
// TODO: refactor this to a less hackish solution (fees cannot easily be calculated atm)
Then(/^the latest transaction should show:$/, async function (table) {
  const expectedData = table.hashes()[0];
  let transactionTitles = await this.waitAndGetText('.Transaction_title');
  transactionTitles = [].concat(transactionTitles);
  const expectedTransactionTitle = await this.intl(expectedData.title, {
    currency: 'Ada'
  });
  expect(expectedTransactionTitle).to.equal(transactionTitles[0]);
  let transactionAmounts = await this.waitAndGetText('.Transaction_amount');
  transactionAmounts = [].concat(transactionAmounts);
  // Transaction amount includes transaction fees so we need to
  // substract them in order to get a match with expectedData.amountWithoutFees.
  // NOTE: we use "add()" as this is outgoing transaction and amount is a negative value!
  const transactionAmount = new BigNumber(transactionAmounts[0]);
  const transactionAmountWithoutFees = transactionAmount.plus(this.fees).toFormat(DECIMAL_PLACES_IN_ADA);
  expect(expectedData.amountWithoutFees).to.equal(transactionAmountWithoutFees);
});
Then(/^I should not see any transactions$/, async function () {
  await this.client.waitForVisible('.Transaction_component', null, true);
});
Then(/^I should see the no recent transactions message$/, async function () {
  await this.client.waitForVisible('.WalletNoTransactions_label');
});
Then(/^I should see the following transactions:$/, async function (table) {
  // Prepare expected transaction data
  const expectedTxs = await Promise.all(table.hashes().map(async tx => {
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
      title: await this.intl(title, {
        currency: 'Ada'
      }),
      amount: new BigNumber(tx.amount).toFormat(DECIMAL_PLACES_IN_ADA)
    };
  }));
  // Collect data of visible transactions on screen
  const txTitles = await getVisibleTextsForSelector(this.client, '.Transaction_title');
  const txAmounts = await getVisibleTextsForSelector(this.client, '.Transaction_amount');
  const visibleTxs = txTitles.map((title, index) => ({
    title,
    amount: txAmounts[index]
  }));
  expect(expectedTxs).to.deep.equal(visibleTxs);
});