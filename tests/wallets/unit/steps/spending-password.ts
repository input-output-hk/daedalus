import { Given, Then } from "cucumber";
import { expect } from "chai";
import { isValidSpendingPassword } from "../../../../source/renderer/app/utils/validations";

/* eslint-disable no-unused-expressions */
Given('I use the spending password {string}', function (password) {
  this.context.spendingPassword = password;
});
Then('the spending password validation should fail', function () {
  expect(isValidSpendingPassword(this.context.spendingPassword)).to.be.false;
});
Then('the spending password validation should succeed', function () {
  expect(isValidSpendingPassword(this.context.spendingPassword)).to.be.true;
});