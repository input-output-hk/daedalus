// @flow
import { expect } from 'chai';
import { expectTextInSelector, waitAndClick } from '../../../common/e2e/steps/helpers';
import { WalletSyncStateStatuses } from '../../../../source/renderer/app/domains/Wallet';
import type { Daedalus, WebdriverClient } from '../../../types';

declare var daedalus: Daedalus;

const ADD_WALLET = '.WalletAdd';
const IMPORT_WALLET_BUTTON = '.importWalletButton';
const IMPORT_WALLET_DIALOG = '.WalletFileImportDialog';
const DEFAULT_LANGUAGE = 'en-US';

export const addOrSetWalletsForScenario = function(wallet: Object) {
  this.wallet = wallet;
  if (this.wallets != null) {
    this.wallets.push(this.wallet);
  } else {
    this.wallets = [this.wallet];
  }
};

export const restoreWalletWithFunds = async (client: WebdriverClient, { walletName }: { walletName: string }) =>
  client.executeAsync((name, done) => {
    daedalus.api.ada
      .restoreWallet({
        walletName: name,
        recoveryPhrase:
          'pass proud clarify cargo control fancy question option bring recall dolphin meat comic version pitch',
        spendingPassword: 'Secret1234',
      })
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

export const fillOutWalletSendForm = async function(values: Object) {
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

export const getNameOfActiveWalletInSidebar = async function() {
  await this.client.waitForVisible('.SidebarWalletMenuItem_active');
  return this.client.getText(
    '.SidebarWalletMenuItem_active .SidebarWalletMenuItem_title'
  );
};

export const getWalletByName = function(walletName: string) {
  return this.wallets.find(w => w.name === walletName);
};

export const importWalletHelpers = {
  waitForDialog: (
    client: WebdriverClient,
    { isHidden } : { isHidden: boolean } = {}
  ) =>
    client.waitForVisible(IMPORT_WALLET_DIALOG, null, isHidden),
  clickImport: (
    client: WebdriverClient
  ) =>
    waitAndClick(client, `${IMPORT_WALLET_DIALOG} .primary`),
  expectError: (
    client: WebdriverClient,
    { error }: { error: string }
  ) =>
    expectTextInSelector(client, {
      selector: `${IMPORT_WALLET_DIALOG}_error`,
      text: error,
    }),
};

export const importWalletWithFunds = async (
  client: WebdriverClient,
  { keyFilePath, password }: { keyFilePath: string, password: ?string }
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

export const isActiveWalletBeingRestored = async (client: WebdriverClient) => {
  const result = await client.execute(
    expectedSyncTag =>
      daedalus.stores.wallets.active === expectedSyncTag,
    WalletSyncStateStatuses.RESTORING
  );
  return result.value ? result.value.syncState.tag : false;
};

export const waitUntilWalletIsLoaded = async function(walletName: string): Promise<any> {
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

export const waitUntilWaletNamesEqual = function(walletName: string) {
  const context = this;
  return context.client.waitUntil(async () => {
    const currentWalletName = await getNameOfActiveWalletInSidebar.call(
      context
    );
    return currentWalletName === walletName;
  });
};

export const expectActiveWallet = async function(walletName) {
  const displayedWalletName = await getNameOfActiveWalletInSidebar.call(this);
  expect(displayedWalletName.toLowerCase().trim()).to.equal(
    walletName.toLowerCase().trim()
  );
};

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


export const getCurrentAppRoute = async function() {
  const url = (await this.client.url()).value;
  return url.substring(url.indexOf('#/') + 1); // return without the hash
};

export const waitUntilUrlEquals = function(expectedUrl) {
  const context = this;
  return context.client.waitUntil(async () => {
    const url = await getCurrentAppRoute.call(context);
    return url === expectedUrl;
  });
};

export const navigateTo = function(requestedRoute) {
  return this.client.execute(route => {
    daedalus.actions.router.goToRoute.trigger({ route });
  }, requestedRoute);
};

export const sidebar = {
  activateCategory: async (client, { category }) => {
    await client.execute(cat => {
      daedalus.actions.sidebar.activateSidebarCategory.trigger({
        category: cat,
        showSubMenu: true,
      });
    }, `/${category}`);
    return client.waitForVisible(`.SidebarCategory_active.${category}`);
  },
  clickAddWalletButton: client =>
    waitAndClick(client, '.SidebarWalletsMenu_addWalletButton'),
};

export const addWalletPage = {
  waitForVisible: (client, { isHidden } = {}) =>
    client.waitForVisible(ADD_WALLET, null, isHidden),
  clickImportButton: client =>
    waitAndClick(client, `${ADD_WALLET} ${IMPORT_WALLET_BUTTON}`),
};

export default {
  waitForDialog: (client, { isHidden } = {}) =>
    client.waitForVisible(IMPORT_WALLET_DIALOG, null, isHidden),
  selectFile: (client, { filePath }) =>
    client.chooseFile(
      `${IMPORT_WALLET_DIALOG} .FileUploadWidget_dropZone input`,
      filePath
    ),
  clickImport: client =>
    waitAndClick(client, `${IMPORT_WALLET_DIALOG} .primary`),
  expectError: (client, { error }) =>
    expectTextInSelector(client, {
      selector: `${IMPORT_WALLET_DIALOG}_error`,
      text: error,
    }),
};

export const i18n = {
  formatMessage: async (client, { id, values }) => {
    const translation = await client.execute(
      (translationId, translationValues) => {
        const IntlProvider = require('react-intl').IntlProvider; // eslint-disable-line
        const locale = daedalus.stores.profile.currentLocale;
        const messages = daedalus.translations;
        const intlProvider = new IntlProvider(
          { locale, messages: messages[locale] },
          {}
        );
        return intlProvider
          .getChildContext()
          .intl.formatMessage({ id: translationId }, translationValues);
      },
      id,
      values || {}
    );
    return translation.value;
  },
  setActiveLanguage: async (client, { language } = {}) =>
    client.execute(locale => {
      daedalus.actions.profile.updateLocale.trigger({ locale });
    }, language || DEFAULT_LANGUAGE),
};

export const waitForActiveRestoreNotification = (client, { isHidden }: { isHidden?: boolean } = {}) =>
  client.waitForVisible('.ActiveRestoreNotification', null, isHidden);
