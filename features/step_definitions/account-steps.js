import { expect } from 'chai';

export default function () {
  this.Given(/^I have an account$/, async function () {
    const result = await this.client.execute(function () {
      const account = daedalus.api.repository.generateUser();
      daedalus.controller.user.loadUser();
      return account;
    });
    this.account = result.value;
  });
}
