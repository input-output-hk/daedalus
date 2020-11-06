// @flow
import Action from './lib/Action';

export default class VotingActions {
  selectVotingWallet: Action<string> = new Action();
  generateQrCode: Action<number> = new Action();
  sendTransaction: Action<{
    amount: number,
    passphrase: string,
  }> = new Action();
  resetVotingRegistration: Action<any> = new Action();
  initializeCountdownInterval: Action<any> = new Action();
}
