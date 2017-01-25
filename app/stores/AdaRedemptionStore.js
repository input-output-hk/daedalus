// @flow
import { observable } from 'mobx';
import { PDFExtract } from 'pdf.js-extract';
import Store from './lib/Store';
import Request from './lib/Request';

export default class AdaRedemptionStore extends Store {

  @observable redeemAdaRequest = new Request(this.api, 'redeemAda');

  constructor(...args) {
    super(...args);
    this.actions.redeemAda.listen(this._redeemAda);
  }

  _redeemAda = (params) => {
    console.log(params);

    // TODO: Make this work -> i think this does not work in the render process
    const pdfExtract = new PDFExtract();
    pdfExtract.extract(params.certificate.path, {}, function (err, data) {
      if (err) return console.log(err);
      console.log('seed', data.pages[0].content[8].str);
    });
  };

}
