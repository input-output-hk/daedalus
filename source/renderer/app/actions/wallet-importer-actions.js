// @flow
import Action from './lib/Action';

// ======= WALLET IMPORTER ACTIONS =======

export default class WalletImporterActions {
  extractWallets: Action<{ secretKeyFilePath: string }> = new Action();
  matchPasswords: Action<{ passwords: Array<string> }> = new Action();
}
