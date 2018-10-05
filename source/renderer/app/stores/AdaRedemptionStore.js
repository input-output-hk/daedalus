// @flow
import { action, computed, observable, runInAction } from 'mobx';
import { ipcRenderer } from 'electron';
import { isString } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import WalletTransaction from '../domains/WalletTransaction';
import { Logger } from '../../../common/logging';
import { encryptPassphrase } from '../api/utils';
import { matchRoute } from '../utils/routing';
import { PARSE_REDEMPTION_CODE } from '../../../common/ipc-api';
import {
  InvalidMnemonicError,
  AdaRedemptionCertificateParseError,
  AdaRedemptionEncryptedCertificateParseError
} from '../i18n/errors';
import { DECIMAL_PLACES_IN_ADA } from '../config/numbersConfig';
import LocalizableError from '../i18n/LocalizableError';
import { ROUTES } from '../routes-config';
import { ADA_REDEMPTION_TYPES } from '../types/redemptionTypes';
import type { RedemptionTypeChoices } from '../types/redemptionTypes';
import type { RedeemAdaParams } from '../api/transactions/requests/redeemAda';
import type { RedeemPaperVendedAdaParams } from '../api/transactions/requests/redeemPaperVendedAda';

export default class AdaRedemptionStore extends Store {

  @observable redemptionType: RedemptionTypeChoices = ADA_REDEMPTION_TYPES.REGULAR;
  @observable certificate: ?File = null;
  @observable isCertificateEncrypted = false;
  @observable passPhrase: ?string = null;
  @observable shieldedRedemptionKey: ?string = null;
  @observable email: ?string = null;
  @observable adaPasscode: ?string = null;
  @observable adaAmount: ?string = null;
  @observable decryptionKey: ?string = null;
  @observable redemptionCode: string = '';
  @observable walletId: ?string = null;
  @observable error: ?LocalizableError = null;
  @observable amountRedeemed: number = 0;
  @observable showAdaRedemptionSuccessMessage: boolean = false;
  @observable redeemAdaRequest: Request<RedeemAdaParams> = new Request(this.api.ada.redeemAda);
  // eslint-disable-next-line
  @observable redeemPaperVendedAdaRequest: Request<RedeemPaperVendedAdaParams> = new Request(this.api.ada.redeemPaperVendedAda);
  @observable isRedemptionDisclaimerAccepted = false;

  setup() {
    const actions = this.actions.adaRedemption;
    actions.chooseRedemptionType.listen(this._chooseRedemptionType);
    actions.setCertificate.listen(this._setCertificate);
    actions.setPassPhrase.listen(this._setPassPhrase);
    actions.setRedemptionCode.listen(this._setRedemptionCode);
    actions.setEmail.listen(this._setEmail);
    actions.setAdaPasscode.listen(this._setAdaPasscode);
    actions.setAdaAmount.listen(this._setAdaAmount);
    actions.setDecryptionKey.listen(this._setDecryptionKey);
    actions.redeemAda.listen(this._redeemAda);
    actions.redeemPaperVendedAda.listen(this._redeemPaperVendedAda);
    actions.adaSuccessfullyRedeemed.listen(this._onAdaSuccessfullyRedeemed);
    actions.closeAdaRedemptionSuccessOverlay.listen(this._onCloseAdaRedemptionSuccessOverlay);
    actions.removeCertificate.listen(this._onRemoveCertificate);
    actions.acceptRedemptionDisclaimer.listen(this._onAcceptRedemptionDisclaimer);
    ipcRenderer.on(PARSE_REDEMPTION_CODE.SUCCESS, this._onCodeParsed);
    ipcRenderer.on(PARSE_REDEMPTION_CODE.ERROR, this._onParseError);
    this.registerReactions([
      this._resetRedemptionFormValuesOnAdaRedemptionPageLoad,
    ]);
  }

  teardown() {
    super.teardown();
    ipcRenderer.removeAllListeners(PARSE_REDEMPTION_CODE.SUCCESS);
    ipcRenderer.removeAllListeners(PARSE_REDEMPTION_CODE.ERROR);
  }

  isValidRedemptionKey = (redemptionKey: string) => (
    this.api.ada.isValidRedemptionKey(redemptionKey)
  );

  isValidRedemptionMnemonic = (mnemonic: string) => (
    this.api.ada.isValidRedemptionMnemonic(mnemonic)
  );

  isValidPaperVendRedemptionKey = (mnemonic: string) => (
    this.api.ada.isValidPaperVendRedemptionKey(mnemonic)
  );

  @computed get isAdaRedemptionPage(): boolean {
    return matchRoute(ROUTES.ADA_REDEMPTION, this.stores.app.currentRoute);
  }

  @action _chooseRedemptionType = (params: {
    redemptionType: RedemptionTypeChoices,
  }) => {
    if (this.redemptionType !== params.redemptionType) {
      this._reset();
      this.redemptionType = params.redemptionType;
    }
  };

  _onAcceptRedemptionDisclaimer = action(() => {
    this.isRedemptionDisclaimerAccepted = true;
  });

  _setCertificate = action(({ certificate }) => {
    this.certificate = certificate;
    this.isCertificateEncrypted = certificate.type !== 'application/pdf';
    if (this.isCertificateEncrypted && (!this.passPhrase || !this.decryptionKey)) {
      this.redemptionCode = '';
      this.passPhrase = null;
      this.decryptionKey = null;
      return; // We cannot decrypt it yet!
    }
    this._parseCodeFromCertificate();
  });

  _setPassPhrase = action(({ passPhrase } : { passPhrase: string }) => {
    this.passPhrase = passPhrase;
    if (this.isValidRedemptionMnemonic(passPhrase)) this._parseCodeFromCertificate();
  });

  _setRedemptionCode = action(({ redemptionCode } : { redemptionCode: string }) => {
    this.redemptionCode = redemptionCode;
  });

  _setEmail = action(({ email } : { email: string }) => {
    this.email = email;
    this._parseCodeFromCertificate();
  });

  _setAdaPasscode = action(({ adaPasscode } : { adaPasscode: string }) => {
    this.adaPasscode = adaPasscode;
    this._parseCodeFromCertificate();
  });

  _setAdaAmount = action(({ adaAmount } : { adaAmount: string }) => {
    this.adaAmount = adaAmount;
    this._parseCodeFromCertificate();
  });

  _setDecryptionKey = action(({ decryptionKey } : { decryptionKey: string }) => {
    this.decryptionKey = decryptionKey;
    this._parseCodeFromCertificate();
  });

  _parseCodeFromCertificate() {
    if (
      this.redemptionType === ADA_REDEMPTION_TYPES.REGULAR ||
      this.redemptionType === ADA_REDEMPTION_TYPES.RECOVERY_REGULAR
    ) {
      if (!this.passPhrase && this.isCertificateEncrypted) return;
    }
    if (this.redemptionType === ADA_REDEMPTION_TYPES.FORCE_VENDED) {
      if ((!this.email || !this.adaAmount || !this.adaPasscode) && this.isCertificateEncrypted) {
        return;
      }
    }
    if (this.redemptionType === ADA_REDEMPTION_TYPES.RECOVERY_FORCE_VENDED) {
      if (!this.decryptionKey && this.isCertificateEncrypted) return;
    }
    if (this.redemptionType === ADA_REDEMPTION_TYPES.PAPER_VENDED) return;
    if (this.certificate == null) throw new Error('Certificate File is required for parsing.');
    const path = this.certificate.path; // eslint-disable-line
    Logger.debug('Parsing ADA Redemption code from certificate: ' + path);
    let decryptionKey = null;
    if ((
      this.redemptionType === ADA_REDEMPTION_TYPES.REGULAR ||
      this.redemptionType === ADA_REDEMPTION_TYPES.RECOVERY_REGULAR) &&
      this.isCertificateEncrypted
    ) {
      decryptionKey = this.passPhrase;
    }
    if (
      this.redemptionType === ADA_REDEMPTION_TYPES.FORCE_VENDED &&
      this.isCertificateEncrypted
    ) {
      decryptionKey = [this.email, this.adaPasscode, this.adaAmount];
    }
    if (
      this.redemptionType === ADA_REDEMPTION_TYPES.RECOVERY_FORCE_VENDED &&
      this.isCertificateEncrypted
    ) {
      decryptionKey = this.decryptionKey;
    }
    ipcRenderer.send(PARSE_REDEMPTION_CODE.REQUEST, path, decryptionKey, this.redemptionType);
  }

  _onCodeParsed = action((event, code) => {
    Logger.debug('Redemption code parsed from certificate: ' + code);
    this.redemptionCode = code;
  });

  _onParseError = action((event, error) => {
    const errorMessage = isString(error) ? error : error.message;
    if (errorMessage.includes('Invalid mnemonic')) {
      this.error = new InvalidMnemonicError();
    } else if (this.redemptionType === ADA_REDEMPTION_TYPES.REGULAR) {
      if (this.isCertificateEncrypted) {
        this.error = new AdaRedemptionEncryptedCertificateParseError();
      } else {
        this.error = new AdaRedemptionCertificateParseError();
      }
    }
    this.redemptionCode = '';
    this.passPhrase = null;
    this.decryptionKey = null;
  });

  _redeemAda = async ({ walletId, spendingPassword } : {
    walletId: string,
    spendingPassword: ?string,
  }) => {
    runInAction(() => this.walletId = walletId);

    const accountIndex = await this.stores.addresses.getAccountIndexByWalletId(walletId);
    if (!accountIndex) throw new Error('Active account required before redeeming Ada.');

    try {
      const transaction: WalletTransaction = await this.redeemAdaRequest.execute({
        accountIndex,
        redemptionCode: this.redemptionCode,
        spendingPassword: spendingPassword && encryptPassphrase(spendingPassword)
      });
      this._reset();
      this.actions.adaRedemption.adaSuccessfullyRedeemed.trigger({
        walletId,
        amount: transaction.amount.toFormat(DECIMAL_PLACES_IN_ADA),
      });
    } catch (error) {
      runInAction(() => this.error = error);
    }
  };

  _redeemPaperVendedAda = async ({ walletId, shieldedRedemptionKey, spendingPassword } : {
    walletId: string,
    shieldedRedemptionKey: string,
    spendingPassword: ?string,
  }) => {
    runInAction(() => this.walletId = walletId);

    const accountIndex = await this.stores.addresses.getAccountIndexByWalletId(walletId);
    if (!accountIndex) throw new Error('Active account required before redeeming Ada.');

    try {
      const transaction: WalletTransaction = await this.redeemPaperVendedAdaRequest.execute({
        mnemonics: this.passPhrase && this.passPhrase.split(' '),
        accountIndex,
        redemptionCode: shieldedRedemptionKey,
        spendingPassword: spendingPassword && encryptPassphrase(spendingPassword)
      });
      this._reset();
      this.actions.adaRedemption.adaSuccessfullyRedeemed.trigger({
        walletId,
        amount: transaction.amount.toFormat(DECIMAL_PLACES_IN_ADA)
      });
    } catch (error) {
      runInAction(() => this.error = error);
    }
  };

  _onAdaSuccessfullyRedeemed = action(({ walletId, amount } : {
    walletId: string,
    amount: number,
  }) => {
    Logger.debug('ADA successfully redeemed for wallet: ' + walletId);
    this.stores.wallets.goToWalletRoute(walletId);
    this.amountRedeemed = amount;
    this.showAdaRedemptionSuccessMessage = true;
    this.redemptionCode = '';
    this.passPhrase = null;
    this.decryptionKey = null;
  });

  _onCloseAdaRedemptionSuccessOverlay = action(() => {
    this.showAdaRedemptionSuccessMessage = false;
  });

  _resetRedemptionFormValuesOnAdaRedemptionPageLoad = () => {
    if (this.isAdaRedemptionPage) this._reset();
  };

  _onRemoveCertificate = action(() => {
    this.error = null;
    this.certificate = null;
    this.redemptionCode = '';
    this.passPhrase = null;
    this.email = null;
    this.adaPasscode = null;
    this.adaAmount = null;
    this.decryptionKey = null;
  });

  @action _reset = () => {
    this.error = null;
    this.certificate = null;
    this.isCertificateEncrypted = false;
    this.walletId = null;
    this.redemptionType = ADA_REDEMPTION_TYPES.REGULAR;
    this.redemptionCode = '';
    this.shieldedRedemptionKey = null;
    this.passPhrase = null;
    this.email = null;
    this.adaPasscode = null;
    this.adaAmount = null;
    this.decryptionKey = null;
  };

}
