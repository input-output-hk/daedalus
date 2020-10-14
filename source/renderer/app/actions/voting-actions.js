// @flow
import Action from './lib/Action';

export default class VotingActions {
  selectVotingWallet: Action<string> = new Action();
  setPinCode: Action<number> = new Action();
  sendTransaction: Action<{
    receiver: string,
    amount: string,
    passphrase: string,
  }> = new Action();
  resetVotingRegistration: Action<> = new Action();
}
