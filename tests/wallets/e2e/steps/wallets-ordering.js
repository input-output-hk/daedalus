// @flow
import { Then } from 'cucumber';
import { expect } from 'chai';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Then(/^I should see the wallets in the following order:$/, async function(
  table
) {
  const expectedWallets = table.hashes();
  const wallets = await this.waitAndGetText.call(this, '.SidebarWalletMenuItem_title');
  wallets.forEach((wallet, index) =>
    expect(wallet).to.equal(expectedWallets[index].name)
  );
});
