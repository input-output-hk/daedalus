import { expect } from 'chai';
import BigNumber from 'bignumber.js';
import {
  expectTextInSelector,
  waitAndClick,
  notFoundWalletsErrorMessage,
} from '../../../common/e2e/steps/helpers';
import {
  byronMnemonics,
  shelleyMnemonics,
} from '../../../../utils/api-importer/mnemonics';
import { testStorageKeys } from '../../../common/e2e/steps/config';
import { WalletSyncStateStatuses } from '../../../../source/renderer/app/domains/Wallet';

const ADD_WALLET = '.WalletAdd';
const IMPORT_WALLET_BUTTON = '.importWalletButton';
const IMPORT_WALLET_DIALOG = '.WalletFileImportDialog';
const DEFAULT_LANGUAGE = 'en-US';
let shelleyMnemonicsIndex = 0;
export const noWalletsErrorMessage = `The byron wallet for funds transferring was already used and has no longer funds.
    Remove the "Daedalus Selfnode" directory and run \`nix:dev\` again.`;
export const restoreWalletWithFunds = async (
  client: Record<string, any>,
  {
    walletName,
  }: {
    walletName: string;
  }
) => {
  const recoveryPhrase = shelleyMnemonics[shelleyMnemonicsIndex++];
  if (shelleyMnemonicsIndex === shelleyMnemonics.length)
    shelleyMnemonicsIndex = 0;
  client.executeAsync(
    (name, recoveryPhrase, done) => {
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
            .catch((error) => done(error))
        )
        .catch((error) => done(error));
    },
    walletName,
    recoveryPhrase
  );
};

const getMnemonicsIndex = async function (maxIndex: number) {
  let index = (await this.localStorage(
    'GET',
    testStorageKeys.BYRON_MNEMONICS_INDEX
  )) || {
    value: 0,
  };
  index = parseInt(index.value, 10);
  if (isNaN(index)) index = 0;
  const newIndex = index < maxIndex ? index + 1 : 0;
  await this.localStorage('POST', {
    key: testStorageKeys.BYRON_MNEMONICS_INDEX,
    value: String(newIndex),
  });
  return index;
};

export const restoreLegacyWallet = async (
  client: Record<string, any>,
  {
    walletName,
    hasFunds,
    transferFunds,
  }: {
    walletName: string;
    hasFunds?: boolean;
    transferFunds?: boolean;
  }
) => {
  let recoveryPhrase;

  if (hasFunds) {
    const mnemonics = byronMnemonics;
    const mnemonicsIndex = await getMnemonicsIndex.call(
      client,
      mnemonics.length - 1
    );
    recoveryPhrase = mnemonics[mnemonicsIndex];
  } else {
    recoveryPhrase = null;
  }

  await client.executeAsync(
    (name, recoveryPhrase, transferFunds, noWalletsErrorMessage, done) => {
      const mnemonics =
        recoveryPhrase || daedalus.utils.crypto.generateMnemonic(12);
      const recoveryPhraseArray =
        typeof mnemonics === 'string' ? mnemonics.split(' ') : mnemonics;
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

              if (!wallet) {
                throw new Error(notFoundWalletsErrorMessage);
              }

              const walletAmount = wallet.amount || new BigNumber(0);

              if (transferFunds && walletAmount.isZero()) {
                throw new Error(noWalletsErrorMessage);
              }

              done();
            })
            .catch((error) => done(error))
        )
        .catch((error) => done(error));
    },
    walletName,
    recoveryPhrase,
    transferFunds,
    noWalletsErrorMessage
  );
};
export const fillOutWalletSendForm = async function (
  values: Record<string, any>
) {
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
export const getNameOfActiveWalletInSidebar = async function () {
  return this.waitAndGetText(
    '.SidebarWalletMenuItem_active .SidebarWalletMenuItem_title'
  );
};
export const getWalletByName = async function (walletName: string) {
  const wallet = await this.client.execute(
    (walletName) => daedalus.stores.wallets.getWalletByName(walletName),
    walletName
  );
  return wallet.value;
};

/**
 * It is not safe to create a BigNumber out the amount
 * got from `client.execute`.
 * This method grabs the Fixed wallet amount
 * which can safely be used to create a BigNumber.
 */
export const getFixedAmountByName = async function (walletName: string) {
  await this.client.waitUntil(async () => {
    const isRestoring = await this.client.execute((walletName) => {
      const wallet = daedalus.stores.wallets.getWalletByName(walletName);

      if (!wallet) {
        throw new Error(notFoundWalletsErrorMessage);
      }

      return wallet.isRestoring;
    }, walletName);
    return !isRestoring.value;
  });
  const walletAmount = await this.client.execute((walletName) => {
    const wallet = daedalus.stores.wallets.getWalletByName(walletName);

    if (!wallet) {
      throw new Error(notFoundWalletsErrorMessage);
    }

    const amount = wallet.amount || new BigNumber(0);
    return amount.toFixed();
  }, walletName);
  return walletAmount.value;
};
export const importWalletWithFunds = async (
  client: Record<string, any>,
  {
    keyFilePath,
    password,
  }: {
    keyFilePath: string;
    password: string | null | undefined;
  }
) =>
  client.executeAsync(
    (filePath, spendingPassword, done) => {
      daedalus.api.ada
        .importWalletFromKey({
          filePath,
          spendingPassword,
        })
        .then(() =>
          daedalus.stores.wallets
            .refreshWalletsData()
            .then(done)
            .catch((error) => done(error))
        )
        .catch((error) => done(error));
    },
    keyFilePath,
    password
  );
export const isActiveWalletBeingRestored = async (
  client: Record<string, any>
) => {
  const result = await client.execute(
    (expectedSyncTag) => daedalus.stores.wallets.active === expectedSyncTag,
    WalletSyncStateStatuses.RESTORING
  );
  return result.value ? result.value.syncState.tag : false;
};
export const waitUntilWalletIsLoaded = async function (
  walletName: string
): Promise<any> {
  let wallet = null;
  // eslint-disable-next-line @typescript-eslint/no-this-alias
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
export const waitUntilWalletNamesEqual = function (walletName: string) {
  // eslint-disable-next-line @typescript-eslint/no-this-alias
  const context = this;
  return context.client.waitUntil(async () => {
    const currentWalletName = await getNameOfActiveWalletInSidebar.call(
      context
    );
    return currentWalletName === walletName;
  });
};
export const expectActiveWallet = async function (walletName: string) {
  const displayedWalletName = await getNameOfActiveWalletInSidebar.call(this);
  expect(displayedWalletName.toLowerCase().trim()).to.equal(
    walletName.toLowerCase().trim()
  );
};
export const createWallets = async function (
  wallets: Array<any>,
  options: {
    sequentially?: boolean;
    isLegacy?: boolean;
  } = {}
) {
  if (options.sequentially === true) {
    await createWalletsSequentially.call(this, wallets);
  } else {
    await createWalletsAsync.call(this, wallets, options.isLegacy);
  }
};

const createWalletsSequentially = async function (wallets: Array<any>) {
  for (const walletData of wallets) {
    await this.client.executeAsync((wallet, done) => {
      const mnemonic = daedalus.utils.crypto.generateMnemonic(24);
      const walletDetails = {
        name: wallet.name,
        mnemonic,
        spendingPassword: wallet.password || 'Secret1234',
      };
      daedalus.api.ada
        .createWallet(walletDetails)
        .then(() =>
          daedalus.stores.wallets.walletsRequest
            .execute()
            .then((storeWallets) =>
              daedalus.stores.wallets
                .refreshWalletsData()
                .then(() => done(storeWallets))
                .catch((error) => done(error))
            )
            .catch((error) => done(error))
        )
        .catch((error) => done(error.stack));
    }, walletData);
    await waitUntilWalletIsLoaded.call(this, walletData.name);
  }
};

const createWalletsAsync = async function (table, isLegacy?: boolean) {
  const result = await this.client.executeAsync(
    (wallets, isLegacyWallet, done) => {
      const mnemonics = {};
      const { restoreByronRandomWallet, createWallet } = daedalus.api.ada;
      const mnemonicsLength = isLegacyWallet ? 12 : 24;
      window.Promise.all(
        wallets.map((wallet) => {
          const mnemonic = daedalus.utils.crypto.generateMnemonic(
            mnemonicsLength
          );
          const recoveryPhrase = !isLegacyWallet
            ? mnemonic
            : mnemonic.split(' ');
          mnemonics[wallet.name] = mnemonic.split(' ');
          const walletDetails = {
            name: wallet.name,
            walletName: wallet.name,
            mnemonic,
            recoveryPhrase,
            spendingPassword: wallet.password || 'Secret1234',
          };

          if (!isLegacyWallet) {
            return createWallet(walletDetails);
          }

          return restoreByronRandomWallet({
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
            .then(() =>
              done({
                mnemonics,
              })
            )
            .catch((error) => done(error))
        )
        .catch((error) => done(error));
    },
    table,
    isLegacy
  );
  this.mnemonics = Object.assign({}, result.value.mnemonics, this.mnemonics);
};

export const getCurrentAppRoute = async function () {
  const url = (await this.client.url()).value;
  return url.substring(url.indexOf('#/') + 1); // return without the hash
};
export const waitUntilUrlEquals = function (expectedUrl: string) {
  // eslint-disable-next-line @typescript-eslint/no-this-alias
  const context = this;
  return context.client.waitUntil(async () => {
    const url = await getCurrentAppRoute.call(context);
    return url === expectedUrl;
  });
};
export const navigateTo = function (requestedRoute: string) {
  return this.client.execute((route) => {
    daedalus.actions.router.goToRoute.trigger({
      route,
    });
  }, requestedRoute);
};
export const addWalletPage = {
  waitForVisible: (
    client: Record<string, any>,
    {
      isHidden,
    }: {
      isHidden?: boolean;
    } = {}
  ) => client.waitForVisible(ADD_WALLET, null, isHidden),
};
export const waitForActiveRestoreNotification = (
  client: Record<string, any>,
  {
    isHidden,
  }: {
    isHidden?: boolean;
  } = {}
) => client.waitForVisible('.ActiveRestoreNotification', null, isHidden);
export const getWalletType = async function (_type = '') {
  let type = _type ? _type.trim() : null;
  if (type === 'byron') return 'byron';

  if (!type) {
    type = 'shelley';
  }

  return type;
};
export const restoreWallet = async function (
  walletName: string,
  kind: string,
  subkind: string,
  recovery_phrase: string
) {
  await this.client.executeAsync((done) => {
    daedalus.stores.wallets._pausePolling().then(done);
  });
  const recoveryPhrase = recovery_phrase.split(' ');
  await this.client.executeAsync(
    (walletName, kind, subkind, recoveryPhrase, done) => {
      const { restoreWalletSetKind } = daedalus.actions.wallets;
      restoreWalletSetKind.trigger({
        kind,
      });
      restoreWalletSetKind.trigger({
        param: kind,
        kind: subkind,
      });
      const { restoreRequest } = daedalus.stores.wallets;
      const spendingPassword = 'Secret1234';
      const data = {
        recoveryPhrase,
        walletName,
        spendingPassword,
      };
      restoreRequest
        .execute(data)
        .then(() => {
          daedalus.stores.wallets._resumePolling();
          daedalus.stores.wallets.refreshWalletsData().then(done);
        })
        .catch((error) => done(error));
    },
    walletName,
    kind,
    subkind,
    recoveryPhrase
  );
};
