import { expect } from 'chai';

export default function () {
  this.Given(/^I have an account$/, async function () {
    const result = await this.client.execute(function () {
      return daedalus.api.repository.generateUser();
    });
    this.account = result.value;
  });
}
