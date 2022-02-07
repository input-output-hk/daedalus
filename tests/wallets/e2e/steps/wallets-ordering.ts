import { Then } from "cucumber";
import { expect } from "chai";

Then(/^I should see the wallets in the following order:$/, async function (table) {
  const expectedWallets = table.hashes();
  const wallets = await this.waitAndGetText('.SidebarWalletMenuItem_title');
  wallets.forEach((wallet, index) => expect(wallet).to.equal(expectedWallets[index].name));
});