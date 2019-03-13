// @flow
import Action from './lib/Action';
import type { ExtractedWallet } from '../../../common/types/wallet-importer.types';

// ======= WALLET IMPORTER ACTIONS =======

export default class WalletImporterActions {
  extractWallets: Action<{ keyFile: File }> = new Action();
  matchPasswords: Action<{ passwords: Array<string> }> = new Action();
  matchPasswordsEnd: Action<any> = new Action();
  importKeyFile: Action<{ wallet: ExtractedWallet }> = new Action();
}
