import Action from './lib/Action';

export default class VotingActions {
  selectWallet: Action<string> = new Action();
  sendTransaction: Action<{
    amount: number;
    passphrase: string | null | undefined;
  }> = new Action();
  generateQrCode: Action<number> = new Action();
  saveAsPDF: Action<any> = new Action();
  saveAsPDFSuccess: Action<any> = new Action();
  nextRegistrationStep: Action<any> = new Action();
  previousRegistrationStep: Action<any> = new Action();
  resetRegistration: Action<any> = new Action();
  showConfirmationDialog: Action<any> = new Action();
  closeConfirmationDialog: Action<any> = new Action();
}
