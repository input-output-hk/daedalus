import { expect } from 'chai';

export default function () {
  this.When(/^I press the "([^"]*)" key$/, async function (key) {
    await this.client.keys(key);
  });
}
