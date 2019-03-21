import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { isValidSpendingPassword } from '../../../renderer/app/utils/validations';

Given('I use the spending password {string}', function(password) {
  this.context.spendingPassword = password;
});

Then('the spending password validation is {bool}', function(result) {
  const isValid = isValidSpendingPassword(this.context.spendingPassword);
  expect(isValid).to.equal(result);
});
