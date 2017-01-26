// @flow
import { action, observable } from 'mobx';
import { ipcRenderer } from 'electron';
import Store from './lib/Store';
import Request from './lib/Request';
import { PARSE_REDEMPTION_CODE } from '../../electron/ipc-api/parse-redemption-code-from-pdf';

export default class AdaRedemptionStore extends Store {

  @observable isProcessing = false;
  @observable certificateFilePath: string = null;
  @observable token: string = null;
  @observable walletId: string = null;
  @observable error = null;
  @observable redeemAdaRequest = new Request(this.api, 'redeemAda');

  constructor(...args) {
    super(...args);
    this.actions.redeemAda.listen(this._redeemAda);
    this.actions.adaSuccessfullyRedeemed.listen(this._redirectToRedeemWallet);
  }

  _redeemAda = action(({ certificate, token, walletId }) => {
    this.certificateFilePath = certificate.path;
    this.token = token;
    this.walletId = walletId;
    this.isProcessing = true;
    console.debug('Parsing ADA Redemption code from certificate', this.certificateFilePath);
    ipcRenderer.on(PARSE_REDEMPTION_CODE.SUCCESS, this._onCodeParsed);
    ipcRenderer.on(PARSE_REDEMPTION_CODE.ERROR, this._onParseError);
    ipcRenderer.send(PARSE_REDEMPTION_CODE.REQUEST, this.certificateFilePath);
  });

  _onCodeParsed = (event, code) => {
    console.log('Redemption code parsed from certificate', code);
    this.redeemAdaRequest.execute(code, this.token, this.walletId)
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

  _onParseError = (event, error) => {
    this.isProcessing = false;
    this.error = error;
    console.log('Error while parsing certificate', error);
  };

  _redirectToRedeemWallet = () => {
    console.log('ADA redeemed for wallet', this.walletId);
  }

}
