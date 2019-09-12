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

export const importWalletWithFunds = async (
  client,
  { keyFilePath, password }
) =>
  client.executeAsync(
    (filePath, spendingPassword, done) => {
      daedalus.api.ada
        .importWalletFromKey({ filePath, spendingPassword })
        .then(() =>
          daedalus.stores.wallets
            .refreshWalletsData()
            .then(done)
            .catch(error => done(error))
        )
        .catch(error => done(error));
    },
    keyFilePath,
    password
  );

const createWalletsAsync = async (table, context) => {
  const result = await context.client.executeAsync((wallets, done) => {
    window.Promise.all(
      wallets.map(wallet =>
        daedalus.api.ada.createWallet({
          name: wallet.name,
          mnemonic: daedalus.utils.crypto.generateMnemonic(),
          spendingPassword: wallet.password || 'Secret123',
        })
      )
    )
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
  }, table);
  // Add or set the wallets for this scenario
  if (context.wallets != null) {
    context.wallets.push(...result.value);
  } else {
    context.wallets = result.value;
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
          spendingPassword: wallet.password || 'Secret123',
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
