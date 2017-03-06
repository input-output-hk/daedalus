import { expect } from 'chai';

export const getNameOfActiveWalletInSidebar = async function() {
  await this.client.waitForVisible('.SidebarWalletMenuItem_active');
  return this.client.getText('.SidebarWalletMenuItem_active .SidebarWalletMenuItem_title');
};

export const expectActiveWallet = async function(walletName) {
  const displayedWalletName = await getNameOfActiveWalletInSidebar.call(this);
  expect(displayedWalletName.toLowerCase().trim()).to.equal(walletName.toLowerCase().trim());
};

export const fillOutWalletSendForm = async function(values) {
  const formSelector = '.WalletSendForm_fields';
  // await this.client.setValue(`${formSelector} .title .input_inputElement`, values.title);
  await this.client.setValue(`${formSelector} .receiver .input_inputElement`, values.address);
  await this.client.setValue(`${formSelector} .amount .input_inputElement`, values.amount);
  // await this.client.setValue(`${formSelector} .description .input_inputElement`, values.description);
  this.walletSendFormValues = values;
};

export const getWalletByName = function(walletName) {
  return this.wallets.find((w) => w.name === walletName);
};
