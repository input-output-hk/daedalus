import { Given, When, Then } from 'cucumber';

Given(/^I use the password {string}"$/, async function(password) {
  console.log(password);
  return Promise.resolve(true);
});

When(/^When I apply our password validation rules"$/, async function() {
  console.log('applying rules');
  return Promise.resolve(true);
});

Then(/^Then it should give me {string}"$/, async function(result) {
  console.log(result);
  return Promise.resolve(true);
});
