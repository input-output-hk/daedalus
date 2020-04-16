// @flow
import Action from './lib/Action';

export default class WalletMigrationActions {
  startMigration: Action<any> = new Action();
  finishMigration: Action<any> = new Action();
  toggleWalletImportSelection: Action<string> = new Action();
  updateWalletName: Action<{ id: string, name: string }> = new Action();
  nextStep: Action<any> = new Action();
  selectExportSourcePath: Action<any> = new Action();
}
