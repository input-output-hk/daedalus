// @flow
import { action, observable } from 'mobx';
import { ipcRenderer } from 'electron';
import Store from './lib/Store';
import Request from './lib/Request';
import { PARSE_REDEMPTION_CODE } from '../../electron/ipc-api/parse-redemption-code-from-pdf';

export default class AdaRedemptionStore extends Store {

  @observable isProcessing = false;
  @observable certificate = null;
  @observable isCertificateEncrypted = false;
  @observable walletId: string = null;
  @observable error = null;
  @observable redeemAdaRequest = new Request(this.api, 'redeemAda');

  constructor(...args) {
    super(...args);
    this.actions.updateRedemptionCertificate.listen(this._setCertificate);
    this.actions.redeemAda.listen(this._redeemAda);
    this.actions.adaSuccessfullyRedeemed.listen(this._redirectToRedeemWallet);
    ipcRenderer.on(PARSE_REDEMPTION_CODE.SUCCESS, this._onCodeParsed);
    ipcRenderer.on(PARSE_REDEMPTION_CODE.ERROR, this._onParseError);
  }

  _setCertificate = action(({ certificate }) => {
    this.certificate = certificate;
    this.isCertificateEncrypted = certificate.type !== 'application/pdf';
  });

  _redeemAda = action(({ certificate, walletId, passPhraseTokens }) => {
    this._setCertificate({ certificate });
    this.walletId = walletId;
    this.isProcessing = true;
    const passPhrase = this.isCertificateEncrypted ? passPhraseTokens.join(' ') : null;
    console.debug('Parsing ADA Redemption code from certificate', this.certificate.path);
    ipcRenderer.send(PARSE_REDEMPTION_CODE.REQUEST, this.certificate.path, passPhrase);
  });

  _onCodeParsed = (event, code) => {
    console.debug('Redemption code parsed from certificate:', code);
    this.redeemAdaRequest.execute(code, this.walletId)
      .then(action(() => {
        this.isProcessing = false;
        this.error = null;
        this.actions.adaSuccessfullyRedeemed();
      }))
      .catch(action((error) => {
        this.isProcessing = false;
        this.error = error;
      }));
  };

  _onParseError = action((event, error) => {
    this.isProcessing = false;
    this.error = error;
    console.error('Error while parsing certificate:', error);
  });

  _redirectToRedeemWallet = () => {
    console.debug('ADA redeemed for wallet', this.walletId);
  }

}
