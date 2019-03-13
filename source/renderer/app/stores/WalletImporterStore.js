// @flow
import { findIndex } from 'lodash';
import { observable, action, runInAction, toJS } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Wallet from '../domains/Wallet';
import { deleteKeyFileChannel } from '../ipc/deleteKeyFileChannel';
import { extractWalletsChannel } from '../ipc/extractWalletsChannel';
import { generateKeyFileChannel } from '../ipc/generateKeyFileChannel';
import { matchWalletsPasswordsChannel } from '../ipc/matchWalletsPasswordsChannel';
import { formattedWalletAmount } from '../utils/formatters';
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

  @observable importFromKeyRequest: Request<Wallet> = new Request(this.api.ada.importWalletFromKey);
  @observable deleteWalletRequest: Request<boolean> = new Request(this.api.ada.deleteWallet);

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
    // Pause polling in order to avoid fetching data for extracted wallets
    this.stores.wallets._pausePolling();
    const walletsWithBalances = [];

    for (const wallet of toJS(wallets)) {
      const { password: spendingPassword, balance } = wallet;
      if (spendingPassword != null && balance == null) {
        // Temporarily save key file to the disk
        const filePath = await generateKeyFileChannel.send({ wallet });

        let importedWallet;
        try {
          // Import temporary key file and extract wallet's balance
          importedWallet =
            await this.importFromKeyRequest.execute({ filePath, spendingPassword }).promise;
        } catch (error) {
          const { message, diagnostic } = error.values;
          if (message === 'WalletAlreadyExists') {
            importedWallet = this.stores.wallets.getWalletById(diagnostic.walletId);
            wallet.imported = true; // Wallet is already imported
          }
        }

        if (importedWallet) {
          // Save wallet balance
          const { amount, id: walletId } = importedWallet;
          wallet.id = walletId;
          wallet.balance = formattedWalletAmount(amount, true);

          if (!wallet.imported) {
            // Delete the imported wallet to cancel restoration
            await this.deleteWalletRequest.execute({ walletId });
          }
        }

        // Delete the temporary key file!
        await deleteKeyFileChannel.send({ filePath });
      }
      walletsWithBalances.push(wallet);
    }

    // Resume polling
    this.stores.wallets._resumePolling();

    return walletsWithBalances;
  };

  @action _importKeyFile = async (params: { wallet: ExtractedWallet }) => {
    const { wallet } = params;
    const filePath = await generateKeyFileChannel.send({ wallet: toJS(wallet) });
    const spendingPassword = wallet.password;
    const importedWallet = await this.importFromKeyRequest.execute({
      filePath, spendingPassword,
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

    // Delete the temporary key file!
    await deleteKeyFileChannel.send({ filePath });
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
