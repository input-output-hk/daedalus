// @flow
import { expect } from 'chai';
import { expectTextInSelector, waitAndClick } from '../../../common/e2e/steps/helpers';
import { rewardsMnemonics, balanceMnemonics, balanceItnMnemonics, testStorageKeys } from '../../../common/e2e/steps/config';
import { WalletSyncStateStatuses } from '../../../../source/renderer/app/domains/Wallet';
import {
  WALLET_KINDS,
  WALLET_DAEDALUS_KINDS,
} from '../../../../source/renderer/app/config/walletRestoreConfig';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

const ADD_WALLET = '.WalletAdd';
const IMPORT_WALLET_BUTTON = '.importWalletButton';
const IMPORT_WALLET_DIALOG = '.WalletFileImportDialog';
const DEFAULT_LANGUAGE = 'en-US';

let rewardsMnemonicsIndex = 0;
export const noWalletsErrorMessage = `The balance wallet for funds transfering was already used and has no longer funds.
    Remove the "Daedalus Selfnode" directory and run \`nix:dev\` again.`;

export const restoreWalletWithFunds = async (client: Object, { walletName }: { walletName: string }) => {
  const recoveryPhrase = rewardsMnemonics[rewardsMnemonicsIndex++];
  if (rewardsMnemonicsIndex === rewardsMnemonics.length) rewardsMnemonicsIndex = 0;
  client.executeAsync((name, recoveryPhrase, done) => {

    daedalus.api.ada
      .restoreWallet({
        walletName: name,
        recoveryPhrase,
        spendingPassword: 'Secret1234',
      })
      .then(() =>
        daedalus.stores.wallets
          .refreshWalletsData()
          .then(done)
          .catch(error => done(error))
      )
      .catch(error => done(error));
  }, walletName, recoveryPhrase);
};

const getMnemonicsIndex = async function(maxIndex: number) {
  let index = await this.localStorage('GET', testStorageKeys.BALANCE_MNEMONICS_INDEX) || { value: 0 };
  index = parseInt(index.value, 10);
  if (isNaN(index)) index = 0;
  const newIndex = (index < maxIndex)
    ? index + 1
    : 0;
  await this.localStorage('POST', {
    key: testStorageKeys.BALANCE_MNEMONICS_INDEX,
    value: String(newIndex),
  });
  return index;
};

export const restoreLegacyWallet = async (
  client: Object,
  {
    walletName,
    hasFunds,
    transferFunds,
  }: {
    walletName: string,
    hasFunds?: boolean,
    transferFunds?: boolean,
  }
) => {
  let recoveryPhrase;
  if (hasFunds) {
    const isIncentivizedTestnetRequest = await client.execute(() => global.isIncentivizedTestnet);
    const mnemonics = isIncentivizedTestnetRequest.value ? balanceItnMnemonics : balanceMnemonics;
    const mnemonicsIndex = await getMnemonicsIndex.call(client, (mnemonics.length - 1));
    recoveryPhrase = mnemonics[mnemonicsIndex];
  } else {
    recoveryPhrase = null;
  }
  await client.executeAsync((name, recoveryPhrase, transferFunds, noWalletsErrorMessage, done) => {
    const mnemonics = recoveryPhrase || daedalus.utils.crypto.generateMnemonic(12);
    const recoveryPhraseArray = typeof mnemonics === 'string' ? mnemonics.split(' ') : mnemonics;
    daedalus.api.ada
      .restoreByronRandomWallet({
        walletName: name,
        recoveryPhrase: recoveryPhraseArray,
        spendingPassword: 'Secret1234',
      })
      .then(() =>
        daedalus.stores.wallets
          .refreshWalletsData()
          .then(() => {
            const wallet = daedalus.stores.wallets.getWalletByName(name);
            if (transferFunds && wallet.amount.isZero()) {
              throw new Error(noWalletsErrorMessage);
            }
            done();
          })
          .catch(error => done(error))
      )
      .catch(error => done(error));
  }, walletName, recoveryPhrase, transferFunds, noWalletsErrorMessage);
};

export const fillOutWalletSendForm = async function(values: Object) {
  const formSelector = '.WalletSendForm_component';
  await this.waitAndSetValue(
    `${formSelector} .receiver .SimpleInput_input`,
    values.address
  );
  await this.waitAndSetValue(
    `${formSelector} .amount .SimpleInput_input`,
    values.amount
  );
  if (values.spendingPassword) {
    await this.waitAndSetValue(
      `${formSelector} .spendingPassword .SimpleInput_input`,
      values.spendingPassword
    );
  }
  this.context.walletsendFormValues = values;
};

export const getNameOfActiveWalletInSidebar = async function() {
  return this.waitAndGetText(
    '.SidebarWalletMenuItem_active .SidebarWalletMenuItem_title'
  );
};

export const getWalletByName = async function(walletName: string) {
  const wallet = await this.client.execute(walletName => (
    daedalus.stores.wallets.getWalletByName(walletName)
  ), walletName);
  return wallet.value;
};

/**
 * It is not safe to create a BigNumber out the amount
 * got from `client.execute`.
 * This method grabs the Fixed wallet amount
 * which can safely be used to create a BigNumber.
 */
export const getFixedAmountByName = async function(walletName: string) {
  await this.client.waitUntil(async () => {
    const isRestoring = await this.client.execute(
      (walletName) => {
        const { isRestoring } = daedalus.stores.wallets.getWalletByName(walletName);
        return isRestoring;
      },
      walletName,
    );
    return !isRestoring.value;
  });
  const walletAmount =
    await this.client.execute(
      (walletName) => {
        const { amount } = daedalus.stores.wallets.getWalletByName(walletName);
        return amount.toFixed();
      },
      walletName,
    );
  return walletAmount.value;
};

export const importWalletHelpers = {
  waitForDialog: (
    client: Object,
    { isHidden } : { isHidden: boolean } = {}
  ) =>
    client.waitForVisible(IMPORT_WALLET_DIALOG, null, isHidden),
  clickImport: (
    client: Object
  ) =>
    waitAndClick(client, `${IMPORT_WALLET_DIALOG} .primary`),
  expectError: (
    client: Object,
    { error }: { error: string }
  ) =>
    expectTextInSelector(client, {
      selector: `${IMPORT_WALLET_DIALOG}_error`,
      text: error,
    }),
};

export const importWalletWithFunds = async (
  client: Object,
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

export const isActiveWalletBeingRestored = async (client: Object) => {
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
      (name) => daedalus.stores.wallets.getWalletByName(name),
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

export const expectActiveWallet = async function(walletName: string) {
  const displayedWalletName = await getNameOfActiveWalletInSidebar.call(this);
  expect(displayedWalletName.toLowerCase().trim()).to.equal(
    walletName.toLowerCase().trim()
  );
};

export const createWallets = async function(
  wallets: Array<any>,
  options?: {
    sequentially?: boolean,
    isLegacy?: boolean,
  } = {}
) {
  if (options.sequentially === true) {
    await createWalletsSequentially.call(this, wallets);
  } else {
    await createWalletsAsync.call(this, wallets, options.isLegacy);
  }
};

const createWalletsSequentially = async function(wallets: Array<any>) {
  const isIncentivizedTestnetRequest = await this.client.execute(() => global.isIncentivizedTestnet);
  for (const walletData of wallets) {
    await this.client.executeAsync((wallet, isIncentivizedTestnet, done) => {
      const mnemonic = daedalus.utils.crypto.generateMnemonic(isIncentivizedTestnet ? 15 : 12);
      daedalus.api.ada
        .createWallet({
          name: wallet.name,
          mnemonic,
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
    }, walletData, isIncentivizedTestnetRequest.value);
    await waitUntilWalletIsLoaded.call(this, walletData.name);
  }
};

const createWalletsAsync = async function(table, isLegacy?: boolean) {
  const result = await this.client.executeAsync((wallets, isLegacyWallet, done) => {
    const mnemonics = {};
    const { restoreByronRandomWallet, createWallet } = daedalus.api.ada;
    const apiEndpoint = isLegacyWallet ? restoreByronRandomWallet : createWallet;
    const mnemonicsLength = isLegacyWallet ? 12 : 15;

    window.Promise.all(
      wallets.map(wallet => {
        const mnemonic = daedalus.utils.crypto.generateMnemonic(mnemonicsLength);
        const recoveryPhrase = !isLegacyWallet
          ? mnemonic
          : mnemonic.split(' ');
        mnemonics[wallet.name] = mnemonic.split(' ');
        return apiEndpoint({
          name: wallet.name,
          walletName: wallet.name,
          mnemonic,
          recoveryPhrase,
          spendingPassword: wallet.password || 'Secret1234',
        });
      })
    )
      .then(() =>
        daedalus.stores.wallets
          .refreshWalletsData()
          .then(() => done({ mnemonics }))
          .catch(error => done(error))
      )
      .catch(error => done(error));
  }, table, isLegacy);

  this.mnemonics = Object.assign(
    {},
    result.value.mnemonics,
    this.mnemonics,
  );
};

export const getCurrentAppRoute = async function() {
  const url = (await this.client.url()).value;
  return url.substring(url.indexOf('#/') + 1); // return without the hash
};

export const waitUntilUrlEquals = function(expectedUrl: string) {
  const context = this;
  return context.client.waitUntil(async () => {
    const url = await getCurrentAppRoute.call(context);
    return url === expectedUrl;
  });
};

export const navigateTo = function(requestedRoute: string) {
  return this.client.execute(route => {
    daedalus.actions.router.goToRoute.trigger({ route });
  }, requestedRoute);
};

export const sidebar = {
  activateCategory: async (client: Object, { category }: { category: string }) => {
    await client.execute(cat => {
      daedalus.actions.sidebar.activateSidebarCategory.trigger({
        category: cat,
        showSubMenu: true,
      });
    }, `/${category}`);
    return client.waitForVisible(`.SidebarCategory_active.${category}`);
  },
  clickAddWalletButton: (client: Object) =>
    waitAndClick(client, '.SidebarWalletsMenu_addWalletButton'),
};

export const addWalletPage = {
  waitForVisible: (client: Object, { isHidden }: { isHidden?: boolean } = {}) =>
    client.waitForVisible(ADD_WALLET, null, isHidden),
  clickImportButton: (client: Object) =>
    waitAndClick(client, `${ADD_WALLET} ${IMPORT_WALLET_BUTTON}`),
};

export default {
  waitForDialog: (client: Object, { isHidden }: { isHidden?: boolean } = {}) =>
    client.waitForVisible(IMPORT_WALLET_DIALOG, null, isHidden),
  selectFile: (client: Object, { filePath }: { filePath: string }) =>
    client.chooseFile(
      `${IMPORT_WALLET_DIALOG} .FileUploadWidget_dropZone input`,
      filePath
    ),
  clickImport: (client: Object) =>
    waitAndClick(client, `${IMPORT_WALLET_DIALOG} .primary`),
  expectError: (client: Object, { error }: { error: string }) =>
    expectTextInSelector(client, {
      selector: `${IMPORT_WALLET_DIALOG}_error`,
      text: error,
    }),
};

export const i18n = {
  formatMessage: async (
    client: Object,
    { id, values }: { id: string, values?: Object }
  ) => {
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
  setActiveLanguage: async (
    client: Object,
    { language }: { language: string } = {}
  ) =>
    client.execute(value => {
      daedalus.actions.profile.updateUserLocalSetting.trigger({ param: 'locale', value });
    }, language || DEFAULT_LANGUAGE),
};

export const waitForActiveRestoreNotification = (client: Object, { isHidden }: { isHidden?: boolean } = {}) =>
  client.waitForVisible('.ActiveRestoreNotification', null, isHidden);

export const getWalletType = async function(_type?: string = '') {
  let type = _type ? _type.trim() : null;
  if (type === 'balance') return 'byron';

  if (!type) {
    const isIncentivizedTestnetRequest = await this.client.execute(() => {
      return daedalus.environment.isIncentivizedTestnet
    });
    type = isIncentivizedTestnetRequest.value ? 'shelley' : 'byron';
  }
  return type;
}

export const restoreWallets = async function(wallets: Array<Object>) {
  await this.client.executeAsync((done) => {
    // daedalus.stores.wallets._pausePolling().then(done)
    const p = daedalus.stores.wallets._pausePolling()
    console.log('_pausePolling ----->', p.then);
    done();
  });

  for (const wallet of wallets) {
    let { recovery_phrase: recoveryPhrase } = wallet;
    if (
      kind === WALLET_KINDS.DAEDALUS &&
      subkind === WALLET_DAEDALUS_KINDS.BALANCE_27_WORD
    ) {
      const unscrambledRecoveryPhrase = await this.client.executeAsync((done) => {
        daedalus.stores.wallets
          ._getUnscrambledMnemonics(recoveryPhrase)
          .then(done);
      }, recoveryPhrase);
      recoveryPhrase = unscrambledRecoveryPhrase.value;
    }
    await this.client.executeAsync((wallet, recoveryPhrase, done) => {
      const {
        restoreWalletSetKind,
        restoreWalletSetMnemonics,
        restoreWalletSetConfig,
      } = daedalus.actions.wallets;
      const {
        // restoreWallet,
        restoreLegacyWallet,
        restoreByronRandomWallet,
        restoreByronIcarusWallet,
        restoreByronTrezorWallet,
        restoreByronLedgerWallet,
        restoreExportedByronWallet,
      } = daedalus.api.ada;

      console.log('---------------');
      const { name: walletName, kind, subkind } = wallet;
      restoreWalletSetKind.trigger({ kind });
      restoreWalletSetKind.trigger({ param: kind, kind: subkind });
      const { restoreRequest } = daedalus.stores.wallets;
      const spendingPassword = 'Secret1234';
      console.log('WALLET:', {
        walletName, kind, subkind, recoveryPhrase: recoveryPhrase.length,
      });
      const data = {
        recoveryPhrase,
        walletName,
        spendingPassword,
      };
      restoreRequest.execute(data).promise;


    }, wallet, recoveryPhrase);
  }



  await this.client.executeAsync((wallets, done) => {
    const {
      restoreWalletSetKind,
      restoreWalletSetMnemonics,
      restoreWalletSetConfig,
    } = daedalus.actions.wallets;
    const {
      // restoreWallet,
      restoreLegacyWallet,
      restoreByronRandomWallet,
      restoreByronIcarusWallet,
      restoreByronTrezorWallet,
      restoreByronLedgerWallet,
      restoreExportedByronWallet,
    } = daedalus.api.ada;

    const restoreWallets = async () => {
      for (const wallet of wallets) {
        console.log('---------------');
        const { name: walletName, kind, subkind, recovery_phrase } = wallet;
        restoreWalletSetKind.trigger({ kind });
        restoreWalletSetKind.trigger({ param: kind, kind: subkind });
        const { restoreRequest, _getUnscrambledMnemonics } = daedalus.stores.wallets;
        let recoveryPhrase = recovery_phrase.split(' ');
        if (
          kind === WALLET_KINDS.DAEDALUS &&
          subkind === WALLET_DAEDALUS_KINDS.BALANCE_27_WORD
        ) {
          recoveryPhrase = await _getUnscrambledMnemonics(recoveryPhrase);
        }
        const spendingPassword = 'Secret1234';
        console.log('WALLET:', {
          walletName, kind, subkind, recoveryPhrase: recoveryPhrase.length,
        });
        const data = {
          recoveryPhrase,
          walletName,
          spendingPassword,
        };
        restoreRequest.execute(data).promise;
      }
    }

    restoreWallets()
      .then(() => {
        const resume = daedalus.stores.wallets
          ._resumePolling()
        console.log('resume', resume.then);
        done();
          // .then(done)
          // .catch(error => done(error))
      })
      .catch(error => done(error));

  }, wallets);
};
