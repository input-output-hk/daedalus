import { expect } from 'chai';

export default function () {

  this.Given(/^I am on the wallet send screen$/, function() {
    const { client, browserWindow } = this.app;
    await client.waitUntilWindowLoaded();
    const title = await browserWindow.getTitle();
    expect(title).to.equal('Daedalus');
  });

};
