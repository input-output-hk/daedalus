import { When } from "cucumber";

When(/^I inject fault named "([^"]*)"$/, async function (faultName) {
  await this.client.executeAsync((name, done) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setCardanoNodeFault([name, true]).then(done).catch(e => {
      throw e;
    });
  }, faultName);
});