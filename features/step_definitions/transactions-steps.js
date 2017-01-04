import faker from 'faker';
import { expect } from 'chai';

export default function () {
  this.Given(/^I made the following transactions with my wallet:$/, async function (table) {
    const data = table.hashes().map((t) => ({
      title: t.title,
      walletId: this.wallet.id,
      sender: this.wallet.address,
      receiver: faker.finance.bitcoinAddress(),
      amount: parseFloat(faker.finance.amount(), 10),
      currency: 'ada',
    }));
    const result = await this.client.execute(function (transactions) {
      return transactions.map(function(t) {
        return daedalus.api.repository.generateTransaction(t, t);
      });
    }, data);
    this.transactions = result.value;
  });

  this.Given(/^I see all expected transactions on screen$/, async function () {
    const visibleTitles = await this.client.getText('.Transaction_title');
    this.transactions.forEach(async (t, i) => {
      expect(visibleTitles[i]).to.equal(t.title);
    });
  });

  this.When(/^I enter "([^"]*)" into the transaction search$/, function (searchTerm) {
    const searchField = '.WalletTransactionsSearch_component .input_inputElement';
    return this.client.setValue(searchField, searchTerm);
  });

  this.Then(/^I should only see the following transactions:$/, async function (table) {
    await this.client.waitForVisible('.Transaction_title');
    let visibleTitles = await this.client.getText('.Transaction_title');
    visibleTitles = [].concat(visibleTitles);
    const expectedTitles = table.hashes().map(t => t.title);
    expect(visibleTitles).to.deep.equal(expectedTitles);
  });
}
