import { expect } from 'chai';

export const getNameOfActiveWalletInSidebar = async function() {
  await this.client.waitForVisible('.SidebarWalletMenuItem_active');
  return this.client.getText(
    '.SidebarWalletMenuItem_active .SidebarWalletMenuItem_title'
  );
};

export const expectActiveWallet = async function(walletName) {
  const displayedWalletName = await getNameOfActiveWalletInSidebar.call(this);
  expect(displayedWalletName.toLowerCase().trim()).to.equal(
    walletName.toLowerCase().trim()
  );
};

export const fillOutWalletSendForm = async function(values) {
  const formSelector = '.WalletSendForm_component';
  await this.client.setValue(
    `${formSelector} .receiver .SimpleInput_input`,
    values.address
  );
  await this.client.setValue(
    `${formSelector} .amount .SimpleInput_input`,
    values.amount
  );
  if (values.spendingPassword) {
    await this.client.setValue(
      `${formSelector} .spendingPassword .SimpleInput_input`,
      values.spendingPassword
    );
  }
  this.walletSendFormValues = values;
};

export const getWalletByName = function(walletName) {
  return this.wallets.find(w => w.name === walletName);
};

export const waitUntilWaletNamesEqual = function(walletName) {
  const context = this;
  return context.client.waitUntil(async () => {
    const currentWalletName = await getNameOfActiveWalletInSidebar.call(
      context
    );
    return currentWalletName === walletName;
  });
};

export const waitUntilWalletIsLoaded = async function(walletName) {
  let wallet = null;
  const context = this;
  await context.client.waitUntil(async () => {
    const result = await context.client.execute(
      name => daedalus.stores.wallets.getWalletByName(name),
      walletName
    );
    if (result.value) {
      wallet = result.value;
      return true;
    }
    return false;
  });
  return wallet;
};

export const addOrSetWalletsForScenario = function(wallet) {
  this.wallet = wallet;
  if (this.wallets != null) {
    this.wallets.push(this.wallet);
  } else {
    this.wallets = [this.wallet];
  }
};

export const importWalletWithFunds = async (client, { walletName }) =>
  client.executeAsync((name, done) => {
    const axios = require('axios');
    const API_PORT = process.env.API_PORT || 8088;
    const mnemonic = [
      'pass',
      'proud',
      'clarify',
      'cargo',
      'control',
      'fancy',
      'question',
      'option',
      'bring',
      'recall',
      'dolphin',
      'meat',
      'comic',
      'version',
      'pitch',
    ];
    const payload = {
      name,
      mnemonic_sentence: mnemonic,
      passphrase: 'Secret1234',
      address_pool_gap: 20,
    };
    axios
      .post(`http://localhost:${API_PORT}/v2/wallets`, payload)
      .then(() =>
        daedalus.stores.wallets
          .refreshWalletsData()
          .then(done)
          .catch(error => done(error))
      )
      .catch(error => done(error));
  }, walletName);

const createWalletsAsync = async (table, context) => {
  const result = await context.client.executeAsync((wallets, done) => {
    const mnemonics = {};
    window.Promise.all(
      wallets.map(wallet => {
        const mnemonic = daedalus.utils.crypto.generateMnemonic();
        mnemonics[wallet.name] = mnemonic.split(' ');
        return daedalus.api.ada.createWallet({
          name: wallet.name,
          mnemonic,
          spendingPassword: wallet.password || 'Secret1234',
        });
      })
    )
      .then(() =>
        daedalus.stores.wallets.walletsRequest
          .execute()
          .then(storeWallets =>
            daedalus.stores.wallets
              .refreshWalletsData()
              .then(() => done({ storeWallets, mnemonics }))
              .catch(error => done(error))
          )
          .catch(error => done(error))
      )
      .catch(error => done(error.stack));
  }, table);
  // Add or set the wallets for this scenario
  if (context.wallets != null) {
    context.wallets.push(...result.value.storeWallets);
  } else {
    context.wallets = result.value.storeWallets;
  }
  if (context.mnemonics != null) {
    context.mnemonics.push(...result.value.mnemonics);
  } else {
    context.mnemonics = result.value.mnemonics;
  }
};

const createWalletsSequentially = async (wallets, context) => {
  context.wallets = [];
  for (const walletData of wallets) {
    const result = await context.client.executeAsync((wallet, done) => {
      daedalus.api.ada
        .createWallet({
          name: wallet.name,
          mnemonic: daedalus.utils.crypto.generateMnemonic(),
          spendingPassword: wallet.password || 'Secret1234',
        })
        .then(() =>
          daedalus.stores.wallets.walletsRequest
            .execute()
            .then(storeWallets =>
              daedalus.stores.wallets
                .refreshWalletsData()
                .then(() => done(storeWallets))
                .catch(error => done(error))
            )
            .catch(error => done(error))
        )
        .catch(error => done(error.stack));
    }, walletData);
    context.wallets = result.value;
  }
};

export const createWallets = async (wallets, context, options = {}) => {
  if (options.sequentially === true) {
    await createWalletsSequentially(wallets, context);
  } else {
    await createWalletsAsync(wallets, context);
  }
};
