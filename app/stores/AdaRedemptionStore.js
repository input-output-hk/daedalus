// @flow
import { action, observable } from 'mobx';
import { ipcRenderer } from 'electron';
import { isString } from 'lodash';
import Log from 'electron-log';
import Store from './lib/Store';
import Request from './lib/Request';
import WalletTransaction from '../domain/WalletTransaction';
import { PARSE_REDEMPTION_CODE } from '../../electron/ipc-api/parse-redemption-code-from-pdf';
import { InvalidMnemonicError, AdaRedemptionCertificateParseError } from '../i18n/errors';
import { DECIMAL_PLACES_IN_ADA } from '../config/numbersConfig';
import LocalizableError from '../i18n/LocalizableError';

type redemptionTypeChoices = 'regular' | 'forceVended' | 'paperVended';

export default class AdaRedemptionStore extends Store {

  @observable redemptionType: redemptionTypeChoices = 'regular';
  @observable certificate: ?File = null;
  @observable isCertificateEncrypted = false;
  @observable passPhrase: ?string = null;
  @observable shieldedRedemptionKey: ?string = null;
  @observable email: ?string = null;
  @observable adaPasscode: ?string = null;
  @observable adaAmount: ?string = null;
  @observable redemptionCode: string = '';
  @observable walletId: ?string = null;
  @observable error: ?LocalizableError = null;
  @observable amountRedeemed: number = 0;
  @observable showAdaRedemptionSuccessMessage: boolean = false;
  @observable redeemAdaRequest = new Request(this.api, 'redeemAda');
  @observable redeemPaperVendedAdaRequest = new Request(this.api, 'redeemPaperVendedAda');

  setup() {
    const actions = this.actions.adaRedemption;
    actions.chooseRedemptionType.listen(this._chooseRedemptionType);
    actions.setCertificate.listen(this._setCertificate);
    actions.setPassPhrase.listen(this._setPassPhrase);
    actions.setRedemptionCode.listen(this._setRedemptionCode);
    actions.setEmail.listen(this._setEmail);
    actions.setAdaPasscode.listen(this._setAdaPasscode);
    actions.setAdaAmount.listen(this._setAdaAmount);
    actions.redeemAda.listen(this._redeemAda);
    actions.redeemPaperVendedAda.listen(this._redeemPaperVendedAda);
    actions.adaSuccessfullyRedeemed.listen(this._onAdaSuccessfullyRedeemed);
    actions.closeAdaRedemptionSuccessOverlay.listen(this._onCloseAdaRedemptionSuccessOverlay);
    actions.removeCertificate.listen(this._onRemoveCertificate);
    ipcRenderer.on(PARSE_REDEMPTION_CODE.SUCCESS, this._onCodeParsed);
    ipcRenderer.on(PARSE_REDEMPTION_CODE.ERROR, this._onParseError);
  }

  teardown() {
    super.teardown();
    ipcRenderer.removeAllListeners(PARSE_REDEMPTION_CODE.SUCCESS);
    ipcRenderer.removeAllListeners(PARSE_REDEMPTION_CODE.ERROR);
  }

  isValidRedemptionKey = (redemptionKey: string) => this.api.isValidRedemptionKey(redemptionKey);
  isValidRedemptionMnemonic = (mnemonic: string) => this.api.isValidRedemptionMnemonic(mnemonic);
  isValidPaperVendRedemptionKey = (mnemonic: string) => this.api.isValidPaperVendRedemptionKey(mnemonic);

  @action _chooseRedemptionType = (params: {
    redemptionType: redemptionTypeChoices,
  }) => {
    if (this.redemptionType !== params.redemptionType) {
      this._reset();
      this.redemptionType = params.redemptionType;
    }
  };

  _setCertificate = action(({ certificate }) => {
    this.certificate = certificate;
    this.isCertificateEncrypted = certificate.type !== 'application/pdf';
    if (this.isCertificateEncrypted && !this.passPhrase) {
      this.redemptionCode = '';
      this.passPhrase = null;
      return; // We cannot decrypt it yet!
    }
    this._parseCodeFromCertificate();
  });

  _setPassPhrase = action(({ passPhrase }) => {
    this.passPhrase = passPhrase;
    this._parseCodeFromCertificate();
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

  _parseCodeFromCertificate() {
    if (this.redemptionType === 'regular') {
      if (!this.passPhrase && this.isCertificateEncrypted) return;
    }
    if (this.redemptionType === 'forceVended') {
      if ((!this.email || !this.adaAmount || !this.adaPasscode) && this.isCertificateEncrypted) {
        return;
      }
    }
    if (this.redemptionType === 'paperVended') return;
    if (this.certificate == null) throw new Error('Certificate File is required for parsing.');
    const path = this.certificate.path; // eslint-disable-line
    Log.debug('Parsing ADA Redemption code from certificate', path);
    let decryptionKey = null;
    if (this.redemptionType === 'regular' && this.isCertificateEncrypted) {
      decryptionKey = this.passPhrase;
    }
    if (this.redemptionType === 'forceVended' && this.isCertificateEncrypted) {
      decryptionKey = [this.email, this.adaPasscode, this.adaAmount];
    }
    ipcRenderer.send(PARSE_REDEMPTION_CODE.REQUEST, path, decryptionKey, this.redemptionType);
  }

  _onCodeParsed = action((event, code) => {
    Log.debug('Redemption code parsed from certificate:', code);
    this.redemptionCode = code;
  });

  _onParseError = action((event, error) => {
    const errorMessage = isString(error) ? error : error.message;
    if (errorMessage.includes('Invalid mnemonic')) {
      this.error = new InvalidMnemonicError();
    } else if (this.redemptionType === 'regular') {
      this.error = new AdaRedemptionCertificateParseError();
    }
    this.redemptionCode = '';
    this.passPhrase = '';
  });

  _redeemAda = action(({ walletId } : { walletId: string }) => {
    this.walletId = walletId;
    this.redeemAdaRequest.execute({ redemptionCode: this.redemptionCode, walletId })
      .then(action((transaction: WalletTransaction) => {
        this._reset();
        this.actions.adaRedemption.adaSuccessfullyRedeemed({
          walletId,
          amount: transaction.amount.toFormat(DECIMAL_PLACES_IN_ADA),
        });
      }))
      .catch(action((error) => {
        this.error = error;
      }));
  });

  _redeemPaperVendedAda = action(({ walletId, shieldedRedemptionKey } : {
    walletId: string,
    shieldedRedemptionKey: string
  }) => {
    this.walletId = walletId;

    this.redeemPaperVendedAdaRequest.execute({
      shieldedRedemptionKey,
      mnemonics: this.passPhrase,
      walletId
    })
      .then(action((transaction: WalletTransaction) => {
        this._reset();
        this.actions.adaRedemption.adaSuccessfullyRedeemed({
          walletId,
          amount: transaction.amount.toFormat(DECIMAL_PLACES_IN_ADA),
        });
      }))
      .catch(action((error) => {
        this.error = error;
      }));
  });

  _onAdaSuccessfullyRedeemed = action(({ walletId, amount }) => {
    Log.debug('ADA successfully redeemed for wallet', walletId);
    this.stores.wallets.goToWalletRoute(walletId);
    this.amountRedeemed = amount;
    this.showAdaRedemptionSuccessMessage = true;
    this.redemptionCode = '';
    this.passPhrase = '';
  });

  _onCloseAdaRedemptionSuccessOverlay = action(() => {
    this.showAdaRedemptionSuccessMessage = false;
  });

  _onRemoveCertificate = action(() => {
    this.certificate = null;
    this.redemptionCode = '';
    this.passPhrase = '';
    this.error = null;
  });

  @action _reset = () => {
    this.error = null;
    this.certificate = null;
    this.isCertificateEncrypted = false;
    this.passPhrase = null;
    this.redemptionCode = '';
    this.walletId = null;
    this.redemptionType = 'regular';
    this.shieldedRedemptionKey = null;
    this.email = null;
    this.adaPasscode = null;
    this.adaAmount = null;
  };

}
