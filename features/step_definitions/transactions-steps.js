import faker from 'faker';
import { expect } from 'chai';

export default function () {
  this.Given(/^I made the following transactions with my wallet:$/, async (table) => {
    const data = table.hashes().map((t) => ({
      title: t.title,
      date: t.date || null,
      walletId: this.wallet.id,
      sender: this.wallet.address,
      receiver: faker.finance.bitcoinAddress(),
      amount: parseFloat(faker.finance.amount(), 10),
    }));
    const result = await this.client.execute((transactions) => (
      transactions.map((t) => {
        const transaction = daedalus.api.repository.generateTransaction(t, t);
        transaction.date = transaction.date.toUTCString();
        return transaction;
      })
    ), data);
    this.transactions = result.value.map((t) => {
      t.date = new Date(t.date);
      return t;
    });
  });

  this.Given(/^I see all expected transactions on screen$/, async () => {
    const visibleTitles = await this.client.getText('.Transaction_title');
    this.transactions.forEach((t, i) => expect(visibleTitles[i]).to.equal(t.title));
  });

  this.When(/^I enter "([^"]*)" into the transaction search$/, (searchTerm) => {
    const searchField = '.WalletTransactionsSearch_component .input_inputElement';
    return this.client.setValue(searchField, searchTerm);
  });

  this.Then(/^I should only see the following transactions:$/, async (table) => {
    await this.client.waitForVisible('.Transaction_title');
    let visibleTitles = await this.client.getText('.Transaction_title');
    visibleTitles = [].concat(visibleTitles);
    const expectedTitles = table.hashes().map(t => t.title);
    expect(visibleTitles).to.deep.equal(expectedTitles);
  });

  this.Then(/^I should see the transactions grouped by their date$/, async () => {
    // TODO: this is not testing for correct nesting into groups etc. (could be done with XPATH)
    const sortedTransactions = this.transactions.sort((a, b) => new Date(a.date) < new Date(b.date));
    const visibleGroupDates = await this.client.getText('.WalletTransactionsList_groupDate');
    const visibleTransactionTitles = await this.client.getText('.Transaction_title');
    expect(sortedTransactions.map(t => t.date)).to.deep.equal(visibleGroupDates.map(d => new Date(d)));
    expect(sortedTransactions.map(t => t.title)).to.deep.equal(visibleTransactionTitles);
  });
}
