import { expect } from 'chai';

export const getNameOfActiveWalletInSidebar = async function () {
  await this.client.waitForVisible('.SidebarWalletMenuItem_active');
  return this.client.getText('.SidebarWalletMenuItem_active .SidebarWalletMenuItem_title');
};

export const expectActiveWallet = async function (walletName) {
  const displayedWalletName = await getNameOfActiveWalletInSidebar.call(this);
  expect(displayedWalletName.toLowerCase().trim()).to.equal(walletName.toLowerCase().trim());
};

export const fillOutWalletSendForm = async function (values) {
  const formSelector = '.WalletSendForm_component';
  await this.client.setValue(`${formSelector} .receiver .SimpleInput_input`, values.address);
  await this.client.setValue(`${formSelector} .amount .SimpleInput_input`, values.amount);
  if (values.walletPassword) {
    await this.client.setValue(`${formSelector} .walletPassword .SimpleInput_input`, values.walletPassword);
  }
  this.walletSendFormValues = values;
};

export const getWalletByName = function (walletName) {
  return this.wallets.find((w) => w.name === walletName);
};

export const waitUntilWaletNamesEqual = function (walletName) {
  const context = this;
  return context.client.waitUntil(async () => {
    const currentWalletName = await getNameOfActiveWalletInSidebar.call(context);
    return currentWalletName === walletName;
  });
};

export const waitUntilWalletIsLoaded = async function (walletName) {
  let wallet = null;
  const context = this;
  await context.client.waitUntil(async () => {
    const result = await context.client.execute((name) => (
      daedalus.stores.ada.wallets.getWalletByName(name)
    ), walletName);
    if (result.value) {
      wallet = result.value;
      return true;
    }
    return false;
  });
  return wallet;
};

export const addOrSetWalletsForScenario = function (wallet) {
  this.wallet = wallet;
  if (this.wallets != null) {
    this.wallets.push(this.wallet);
  } else {
    this.wallets = [this.wallet];
  }
};

export const importWalletWithFunds = async (client, { keyFilePath, password }) => (
  await client.executeAsync((filePath, walletPassword, done) => {
    daedalus.api.ada.importWalletFromKey({ filePath, walletPassword })
      .then(() => (
        daedalus.stores.ada.wallets.refreshWalletsData()
          .then(done)
          .catch((error) => done(error))
      ))
      .catch((error) => done(error));
  }, keyFilePath, password)
);
