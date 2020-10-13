// @flow
import Action from './lib/Action';

export default class VotingActions {
  selectVotingWallet: Action<string> = new Action();
  sendTransaction: Action<{
    receiver: string,
    amount: string,
    passphrase: string,
  }> = new Action();
}
