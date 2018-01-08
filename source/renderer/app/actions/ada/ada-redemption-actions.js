// @flow
import Action from '../lib/Action';
import type { RedemptionTypeChoices } from '../../types/redemptionTypes';

// ======= ADA REDEMPTION ACTIONS =======

export default class AdaRedemptionActions {
  chooseRedemptionType: Action<{ redemptionType: RedemptionTypeChoices }> = new Action();
  setCertificate: Action<{ certificate: File }> = new Action();
  removeCertificate: Action<any> = new Action();
  setPassPhrase: Action<{ passPhrase: string }> = new Action();
  setRedemptionCode: Action<{ redemptionCode: string }> = new Action();
  setEmail: Action<{ email: string }> = new Action();
  setAdaPasscode: Action<{ adaPasscode: string }> = new Action();
  setAdaAmount: Action<{ adaAmount: string }> = new Action();
  redeemAda: Action<{ walletId: string, walletPassword: ?string }> = new Action();
  // eslint-disable-next-line max-len
  redeemPaperVendedAda: Action<{ walletId: string, shieldedRedemptionKey: string, walletPassword: ?string }> = new Action();
  adaSuccessfullyRedeemed: Action<any> = new Action();
  acceptRedemptionDisclaimer: Action<any> = new Action();
  // TODO: refactor dialog toggles to use dialog-actions instead
  closeAdaRedemptionSuccessOverlay: Action<any> = new Action();
}
