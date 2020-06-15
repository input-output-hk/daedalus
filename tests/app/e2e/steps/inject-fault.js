// @flow
import { When } from 'cucumber';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

When(/^I inject fault named "([^"]*)"$/, async function(faultName) {
  await this.client.executeAsync((name, done) => {
    daedalus.api.ada
      .setCardanoNodeFault([name, true])
      .then(done)
      .catch(e => {
        throw e;
      });
  }, faultName);
});