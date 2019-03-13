
// @flow
import { findIndex } from 'lodash';
import { observable, action, runInAction, toJS } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Wallet from '../domains/Wallet';
import { extractWalletsChannel } from '../ipc/extractWalletsChannel';
import { generateRawSecretChannel } from '../ipc/generateRawSecretChannel';
import { matchWalletsPasswordsChannel } from '../ipc/matchWalletsPasswordsChannel';
import { formattedWalletAmount } from '../utils/formatters';
import type { WalletBalance } from '../api/wallets/types';
import type {
  ExtractedWallet,
  ExtractedWallets,
} from '../../../common/types/wallet-importer.types';

export default class WalletImporterStore extends Store {

  @observable keyFile: ?File = null;
  @observable isMatchingPasswords = false;
  @observable isExtractingWallets = false;
  @observable hasExtractedWallets = false;
  @observable extractedWallets: ExtractedWallets = [];

  /* eslint-disable max-len */
  @observable importWalletFromRawSecretRequest: Request<Wallet> = new Request(this.api.ada.importWalletFromRawSecret);
  @observable getWalletBalanceRequest: Request<WalletBalance> = new Request(this.api.ada.getWalletBalance);
  /* eslint-disable max-len */

  setup() {
    const a = this.actions.walletImporter;
    a.extractWallets.listen(this._extractWallets);
    a.matchPasswords.listen(this._matchPasswords);
    a.importKeyFile.listen(this._importKeyFile);
    this.actions.app.initAppEnvironment.listen(() => {});
  }

  @action _extractWallets = async (params: { keyFile: File }) => {
    // Purge any existing extracted wallets data
    this._resetExtractedWalletsData();

    const { keyFile } = params;
    runInAction('start wallet extraction process', () => {
      this.keyFile = keyFile;
      this.isExtractingWallets = true;
    });

    const { path: secretKeyFilePath } = keyFile;
    let wallets = await extractWalletsChannel.send({ secretKeyFilePath });
    wallets = await matchWalletsPasswordsChannel.send({ wallets, passwords: [''] });
    wallets = await this._extractBalances(wallets);

    runInAction('finish wallet extraction process', () => {
      this.extractedWallets = wallets;
      this.isExtractingWallets = false;
      this.hasExtractedWallets = true;
    });
  };

  @action _matchPasswords = async (params: { passwords: Array<string> }) => {
    const { passwords } = params;

    if (!passwords.length) return;

    // Start the password matching process
    this.isMatchingPasswords = true;

    let wallets = await matchWalletsPasswordsChannel.send({
      wallets: toJS(this.extractedWallets), passwords
    });
    wallets = await this._extractBalances(wallets);

    runInAction('finish wallet password matching process', () => {
      this.extractedWallets = wallets;
      this.isMatchingPasswords = false;
      this.actions.walletImporter.matchPasswordsEnd.trigger();
    });
  };

  @action _extractBalances = async (wallets: ExtractedWallets) => {
    const walletsWithBalances = [];
    for (const wallet of toJS(wallets)) {
      const { balance } = wallet;
      if (balance == null) {
        const rawSecret = await generateRawSecretChannel.send({ wallet });
        try {
          const walletBalance =
            await this.getWalletBalanceRequest.execute({ rawSecret }).promise;
          const { balance: amount, walletId } = walletBalance;
          const isImported = this.stores.wallets.getWalletById(walletId) != null;
          wallet.id = walletId;
          wallet.balance = formattedWalletAmount(amount, true);
          wallet.imported = isImported;
        } catch (error) {} // eslint-disable-line
      }
      walletsWithBalances.push(wallet);
    }
    return walletsWithBalances;
  };

  @action _importKeyFile = async (params: { wallet: ExtractedWallet }) => {
    const { wallet } = params;
    const rawSecret = await generateRawSecretChannel.send({ wallet: toJS(wallet) });
    const spendingPassword = wallet.password;
    const importedWallet = await this.importWalletFromRawSecretRequest.execute({
      rawSecret, spendingPassword,
    }).promise;
    if (!importedWallet) throw new Error('Imported wallet was not received correctly');
    await this.stores.wallets._patchWalletRequestWithNewWallet(importedWallet);
    this.stores.wallets.refreshWalletsData();

    runInAction('mark wallet as imported', () => {
      if (wallet.id) {
        const wIndex = findIndex(this.extractedWallets, { id: wallet.id });
        this.extractedWallets[wIndex] = { ...wallet, imported: true };
      }
    });
  };

  @action _revertWalletImport = (walletId: string) => {
    const wIndex = findIndex(this.extractedWallets, { id: walletId });
    if (wIndex >= 0) {
      const wallet = this.extractedWallets[wIndex];
      this.extractedWallets[wIndex] = { ...wallet, imported: false };
    }
  };

  @action _resetExtractedWalletsData = () => {
    this.keyFile = null;
    this.isMatchingPasswords = false;
    this.isExtractingWallets = false;
    this.hasExtractedWallets = false;
    this.extractedWallets = [];
  };

}
