// @flow
import { Action } from './lib/actions';

// ======= ADA REDEMPTION ACTIONS =======
export type AdaRedemptionActions = {
  setCertificate: Action<{ certificate: File }>,
  removeCertificate: Action<any>,
  setPassPhrase: Action<{ passPhrase: string }>,
  setRedemptionCode: Action<{ redemptionCode: string }>,
  redeemAda: Action<{ walletId: string }>,
  adaSuccessfullyRedeemed: Action<any>,
  // TODO: refactor dialog toggles to use dialog-actions instead
  closeAdaRedemptionSuccessOverlay: Action<any>,
};

const adaRedemptionActions: AdaRedemptionActions = {
  setCertificate: new Action(),
  removeCertificate: new Action(),
  setPassPhrase: new Action(),
  setRedemptionCode: new Action(),
  redeemAda: new Action(),
  adaSuccessfullyRedeemed: new Action(),
  closeAdaRedemptionSuccessOverlay: new Action(),
};

export default adaRedemptionActions;
