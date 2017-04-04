// @flow
import { action, observable } from 'mobx';
import { ipcRenderer } from 'electron';
import { isString } from 'lodash';
import Log from 'electron-log';
import Store from './lib/Store';
import Request from './lib/Request';
import { PARSE_REDEMPTION_CODE } from '../../electron/ipc-api/parse-redemption-code-from-pdf';
import { InvalidMnemonicError, AdaRedemptionCertificateParseError } from '../i18n/errors';
import LocalizableError from '../i18n/LocalizableError';

type redemptionTypeChoices = 'regular' | 'forceVended' | 'paperVended';

export default class AdaRedemptionStore extends Store {

  @observable redemptionType: redemptionTypeChoices = 'regular';
  @observable certificate: ?File = null;
  @observable isCertificateEncrypted = false;
  @observable passPhrase: ?string = null;
  @observable email: ?string = null;
  @observable adaPasscode: ?string = null;
  @observable adaAmount: ?string = null;
  @observable redemptionCode: string = '';
  @observable walletId: ?string = null;
  @observable error: ?LocalizableError = null;
  @observable amountRedeemed: number = 0;
  @observable showAdaRedemptionSuccessMessage: boolean = false;
  @observable redeemAdaRequest = new Request(this.api, 'redeemAda');

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

  @action _chooseRedemptionType = (params: {
    redemptionType: redemptionTypeChoices,
  }) => {
    this.redemptionType = params.redemptionType;
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

  _setRedemptionCode = action(({ redemptionCode }) => {
    this.redemptionCode = redemptionCode;
  });

  _setEmail = action(({ email }) => {
    this.email = email;
  });

  _setAdaPasscode = action(({ adaPasscode }) => {
    this.adaPasscode = adaPasscode;
  });

  _setAdaAmount = action(({ adaAmount }) => {
    this.adaAmount = adaAmount;
  });

  _parseCodeFromCertificate() {
    if (this.certificate == null) throw new Error('Certificate File is required for parsing.');
    const path = this.certificate.path;
    Log.debug('Parsing ADA Redemption code from certificate', path);
    ipcRenderer.send(PARSE_REDEMPTION_CODE.REQUEST, path, this.passPhrase);
  }

  _onCodeParsed = action((event, code) => {
    Log.debug('Redemption code parsed from certificate:', code);
    this.redemptionCode = code;
  });

  _onParseError = action((event, error) => {
    console.error('Error while parsing certificate:', error);
    const errorMessage = isString(error) ? error : error.message;
    if (errorMessage.includes('Invalid mnemonic')) {
      this.error = new InvalidMnemonicError();
    } else {
      this.error = new AdaRedemptionCertificateParseError();
    }
    this.redemptionCode = '';
    this.passPhrase = '';
  });

  _redeemAda = action(({ walletId }) => {
    this.walletId = walletId;
    this.redeemAdaRequest.execute({ redemptionCode: this.redemptionCode, walletId })
      .then(action((wallet) => {
        this.error = null;
        // TODO: Use amount returned by backend (when implemented!)
        this.actions.adaRedemption.adaSuccessfullyRedeemed({
          walletId: wallet.id,
          amount: 1000000,
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

}
