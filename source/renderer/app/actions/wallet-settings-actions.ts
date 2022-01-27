import Action from './lib/Action';

export type WalletExportToFileParams = {
  walletId: string;
  exportType: string;
  filePath: string;
  password: string | null | undefined;
};
export default class WalletSettingsActions {
  cancelEditingWalletField: Action<any> = new Action();
  startEditingWalletField: Action<{
    field: string;
  }> = new Action();
  stopEditingWalletField: Action<any> = new Action();
  updateWalletField: Action<{
    field: string;
    value: string;
  }> = new Action();
  updateSpendingPassword: Action<{
    walletId: string;
    oldPassword: string;
    newPassword: string;
    isLegacy: boolean;
  }> = new Action();
  exportToFile: Action<WalletExportToFileParams> = new Action();

  /* ----------  UTXO  ---------- */
  startWalletUtxoPolling: Action<any> = new Action();
  stopWalletUtxoPolling: Action<any> = new Action();

  /* ----------  Recovery Phrase Verification  ---------- */
  recoveryPhraseVerificationContinue: Action<any> = new Action();
  recoveryPhraseVerificationCheck: Action<{
    recoveryPhrase: Array<string>;
  }> = new Action();
  recoveryPhraseVerificationClose: Action<any> = new Action();
  toggleShowUsedAddresses: Action<any> = new Action();
}
