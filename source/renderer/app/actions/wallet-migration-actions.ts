import Action from './lib/Action';
import type { ImportFromOption } from '../types/walletExportTypes';

export default class WalletMigrationActions {
  initiateMigration: Action<any> = new Action();
  startMigration: Action<any> = new Action();
  finishMigration: Action<any> = new Action();
  resetMigration: Action<any> = new Action();
  toggleWalletImportSelection: Action<{
    index: number;
  }> = new Action();
  updateWalletName: Action<{
    index: number;
    name: string;
  }> = new Action();
  nextStep: Action<any> = new Action();
  selectExportSourcePath: Action<{
    importFrom: ImportFromOption;
  }> = new Action();
  resetExportSourcePath: Action<any> = new Action();
}
