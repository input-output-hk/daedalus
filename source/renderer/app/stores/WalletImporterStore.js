// @flow
import { observable, action, runInAction, toJS } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Wallet from '../domains/Wallet';
import { extractWalletsChannel } from '../ipc/extractWalletsChannel';
import { downloadKeyFileChannel } from '../ipc/downloadKeyFileChannel';
import { matchWalletsPasswordsChannel } from '../ipc/matchWalletsPasswordsChannel';
import { formattedWalletAmount } from '../utils/formatters';
import type {
  ExtractedWallet,
  ExtractedWallets,
} from '../../../common/types/wallet-importer.types';

export default class WalletImporterStore extends Store {

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
    a.downloadKeyFile.listen(this._downloadKeyFile);
    a.importKeyFile.listen(this._importKeyFile);
    this.actions.app.initAppEnvironment.listen(() => {});
  }

  @action _extractWallets = async (params: { secretKeyFilePath: string }) => {
    // Purge any existing extracted wallets data
    this._resetExtractedWalletsData();

    runInAction('start wallet extraction process', () => {
      this.isExtractingWallets = true;
    });

    const { secretKeyFilePath } = params;
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
    });
  };

  @action _extractBalances = async (wallets: ExtractedWallets) => {
    // Pause polling in order to avoid fetching data for extracted wallets
    this.stores.wallets._pausePolling();

    const walletsWithBalances = await Promise.all(wallets.map(async (wallet) => {
      const { password, balance } = wallet;
      if (password != null && balance == null) {
        // Temporarily save key file to the disk
        const keyFilePath = await downloadKeyFileChannel.send({ wallet });

        // Import temporary key file and extract wallet's balance
        const importedWallet = await this.importFromKeyRequest.execute({
          filePath: keyFilePath,
          spendingPassword: password,
        }).promise;
        if (!importedWallet) throw new Error('Imported wallet was not received correctly');

        // Save wallet balance
        wallet.balance = formattedWalletAmount(importedWallet.amount, true);

        // Delete the imported wallet to cancel restoration
        await this.deleteWalletRequest.execute({ walletId: importedWallet.id });

        // TODO: delete temporary key file!
      }
      return wallet;
    }));

    // Resume polling
    this.stores.wallets._resumePolling();

    return walletsWithBalances;
  };

  @action _importKeyFile = async (params: { wallet: ExtractedWallet }) => {
    const { wallet } = params;
    const filePath = await downloadKeyFileChannel.send({ wallet: toJS(wallet) });
    const spendingPassword = wallet.password;
    await this.stores.wallets._importWalletFromFile({ filePath, spendingPassword });
    // TODO: delete imported key file!
  };

  @action _downloadKeyFile = (params: { wallet: ExtractedWallet, filePath: string }) => {
    const { wallet, filePath } = params;
    downloadKeyFileChannel.send({ wallet: toJS(wallet), filePath });
  };

  @action _resetExtractedWalletsData = () => {
    this.isMatchingPasswords = false;
    this.isExtractingWallets = false;
    this.hasExtractedWallets = false;
    this.extractedWallets = [];
  };

}
