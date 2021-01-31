// @flow
import Action from './lib/Action';

export default class VotingActions {
  selectWallet: Action<string> = new Action();
  generateQrCode: Action<number> = new Action();
  sendTransaction: Action<{
    amount: number,
    passphrase: string,
  }> = new Action();
  resetRegistration: Action<any> = new Action();
}
