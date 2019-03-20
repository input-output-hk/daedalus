import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { isValidSpendingPassword } from '../../../renderer/app/utils/validations';

Given('I use the password {string}', function(password) {
  this.context.password = password;
});

Then('the password validation should be {bool}', function(result) {
  const isValid = isValidSpendingPassword(this.context.password);
  expect(isValid).to.equal(result);
});
