// @flow
import Action from './lib/Action';
import type { ExtractedWallet } from '../../../common/types/wallet-importer.types';

// ======= WALLET IMPORTER ACTIONS =======

export default class WalletImporterActions {
  extractWallets: Action<{ secretKeyFilePath: string }> = new Action();
  matchPasswords: Action<{ passwords: Array<string> }> = new Action();
  downloadKeyFile: Action<{ wallet: ExtractedWallet, filePath: string }> = new Action();
}
