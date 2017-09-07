import { expect } from 'chai';

export const getNameOfActiveWalletInSidebar = async () => {
  await this.client.waitForVisible('.SidebarWalletMenuItem_active');
  return this.client.getText('.SidebarWalletMenuItem_active .SidebarWalletMenuItem_title');
};

export const expectActiveWallet = async (walletName) => {
  const displayedWalletName = await getNameOfActiveWalletInSidebar.call(this);
  expect(displayedWalletName.toLowerCase().trim()).to.equal(walletName.toLowerCase().trim());
};

export const fillOutWalletSendForm = async (values) => {
  const formSelector = '.WalletSendForm_component';
  await this.client.setValue(`${formSelector} .receiver .SimpleInput_input`, values.address);
  await this.client.setValue(`${formSelector} .amount .SimpleInput_input`, values.amount);
  if (values.walletPassword) {
    await this.client.setValue(`${formSelector} .walletPassword .SimpleInput_input`, values.walletPassword);
  }
  this.walletSendFormValues = values;
};

export const getWalletByName = (walletName) => (
  this.wallets.find((w) => w.name === walletName)
);

export const waitUntilWaletNamesEqual = (walletName) => {
  const context = this;
  return context.client.waitUntil(async () => {
    const currentWalletName = await getNameOfActiveWalletInSidebar.call(context);
    return currentWalletName === walletName;
  });
};

export const waitUntilWalletIsLoaded = async (walletName) => {
  let wallet = null;
  await this.client.waitUntil(async () => {
    const result = await this.client.execute((name) => (
      daedalus.stores.wallets.getWalletByName(name)
    ), walletName);
    if (result.value) {
      wallet = result.value;
      return true;
    }
    return false;
  });
  return wallet;
};

export const addOrSetWalletsForScenario = (wallet) => {
  this.wallet = wallet;
  if (this.wallets != null) {
    this.wallets.push(this.wallet);
  } else {
    this.wallets = [this.wallet];
  }
};
