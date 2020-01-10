// @flow
import { Then } from 'cucumber';
import { expect } from 'chai';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Then(/^I should see the wallets in the following order:$/, async function(
  table
) {
  const expectedWallets = table.hashes();
  const wallets = await this.client.getText('.SidebarWalletMenuItem_title');
  wallets.forEach((wallet, index) =>
    expect(wallet).to.equal(expectedWallets[index].name)
  );
});

Then(/^"([^"]*)" balance wallet should show on the bottom of the list below Rewards wallet$/, async function(
  walletName
) {
  const menuItemTitle = await this.client.getText('.SidebarWalletsMenu_wallets button:nth-child(3) .SidebarWalletMenuItem_title');
  expect(walletName).to.equal(menuItemTitle);
});