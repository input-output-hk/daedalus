// @flow
import { Action } from './lib/actions';

// ======= ADA REDEMPTION ACTIONS =======

export default class AdaRedemptionActions {
  setCertificate: Action<{ certificate: File }> = new Action();
  removeCertificate: Action<any> = new Action();
  setPassPhrase: Action<{ passPhrase: string }> = new Action();
  setRedemptionCode: Action<{ redemptionCode: string }> = new Action();
  redeemAda: Action<{ walletId: string }> = new Action();
  adaSuccessfullyRedeemed: Action<any> = new Action();
  // TODO: refactor dialog toggles to use dialog-actions instead
  closeAdaRedemptionSuccessOverlay: Action<any> = new Action();
}
