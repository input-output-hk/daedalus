// @flow
import { action, observable } from 'mobx';
import { ipcRenderer } from 'electron';
import { isString } from 'lodash';
import Store from './lib/Store';
import Request from './lib/Request';
import { PARSE_REDEMPTION_CODE } from '../../electron/ipc-api/parse-redemption-code-from-pdf';
import LocalizableError from '../i18n/LocalizableError';
import { InvalidMnemonicError } from '../i18n/global-errors';
import constants from '../constants';

export class AdaRedemptionCertificateParseError extends LocalizableError {
  constructor() {
    super({
      id: 'errors.AdaRedemptionCertificateParseError',
      defaultMessage: '!!!The ADA redemption code could not be parsed from the given document.',
    });
  }
}

export default class AdaRedemptionStore extends Store {

  @observable certificate: ?File = null;
  @observable isCertificateEncrypted = false;
  @observable passPhrase: ?string = null;
  @observable redemptionCode: string = '';
  @observable isPaperVendingKey: boolean = false;
  @observable walletId: ?string = null;
  @observable error: ?LocalizableError = null;
  @observable amountRedeemed: number = 0;
  @observable showAdaRedemptionSuccessMessage: bool = false;
  @observable redeemAdaRequest = new Request(this.api, 'redeemAda');

  setup() {
    const actions = this.actions.adaRedemption;
    actions.setCertificate.listen(this._setCertificate);
    actions.setPassPhrase.listen(this._setPassPhrase);
    actions.setRedemptionCode.listen(this._setRedemptionCode);
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
    if (!this.isPaperVendingKey) {
      // Normal key, so the pass phrase is for decrypting the cert
      this._parseCodeFromCertificate();
    }
  });

  @action _setRedemptionCode = ({ redemptionCode } : { redemptionCode: string }) => {
    this.redemptionCode = redemptionCode;
    if (redemptionCode.length === constants.ADA_REDEMPTION_PAPER_KEY_LENGTH) {
      this.isPaperVendingKey = true;
    } else {
      this.isPaperVendingKey = false;
    }
  };

  _parseCodeFromCertificate() {
    if (this.certificate == null) throw new Error('Certificate File is required for parsing.');
    const path = this.certificate.path;
    console.debug('Parsing ADA Redemption code from certificate', path);
    ipcRenderer.send(PARSE_REDEMPTION_CODE.REQUEST, path, this.passPhrase);
  }

  _onCodeParsed = action((event, code) => {
    console.debug('Redemption code parsed from certificate:', code);
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
    const requestData = {
      isPaperVendingKey: this.isPaperVendingKey,
      redemptionCode: this.redemptionCode,
      mnemonic: this.passPhrase,
      walletId,
    };
    this.redeemAdaRequest.execute(requestData)
      .then(action((wallet) => {
        this._reset();
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
    console.debug('ADA successfully redeemed for wallet', walletId);
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
    this.isPaperVendingKey = false;
    this.certificate = null;
    this.isCertificateEncrypted = false;
    this.passPhrase = null;
    this.redemptionCode = '';
    this.walletId = null;
  };

}
