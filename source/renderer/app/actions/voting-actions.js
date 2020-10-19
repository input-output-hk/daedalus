// @flow
import Action from './lib/Action';

export default class VotingActions {
  selectVotingWallet: Action<string> = new Action();
  setPinCode: Action<number> = new Action();
  sendTransaction: Action<{
    amount: number,
    passphrase: string,
  }> = new Action();
  resetVotingRegistration: Action<any> = new Action();
}
