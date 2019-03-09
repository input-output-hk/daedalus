// @flow
import { observable, action, runInAction, toJS } from 'mobx';
import Store from './lib/Store';
import { extractWalletsChannel } from '../ipc/extractWalletsChannel';
import { matchWalletsPasswordsChannel } from '../ipc/matchWalletsPasswordsChannel';
import type { ExtractedWallets } from '../../../common/types/wallet-importer.types';

export default class WalletImporterStore extends Store {

  @observable isMatchingPasswords = false;
  @observable isExtractingWallets = false;
  @observable hasExtractedWallets = false;
  @observable extractedWallets: ExtractedWallets = [];

  setup() {
    const a = this.actions.walletImporter;
    a.extractWallets.listen(this._extractWallets);
    a.matchPasswords.listen(this._matchPasswords);
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

    const wallets = await matchWalletsPasswordsChannel.send({
      wallets: toJS(this.extractedWallets), passwords
    });

    runInAction('finish wallet password matching process', () => {
      this.extractedWallets = wallets;
      this.isMatchingPasswords = false;
    });
  };

  @action _resetExtractedWalletsData = () => {
    this.isMatchingPasswords = false;
    this.isExtractingWallets = false;
    this.hasExtractedWallets = false;
    this.extractedWallets = [];
  };

}
