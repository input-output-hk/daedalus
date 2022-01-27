import { Given, Then } from "cucumber";
import { navigateTo, waitUntilUrlEquals } from "./helpers";

Given(/^I am on the General Settings "([^"]*)" screen$/, async function (screen) {
  await navigateTo.call(this, `/settings/${screen}`);
});
Then(/^I should see General Settings "([^"]*)" screen$/, async function (screenName) {
  return waitUntilUrlEquals.call(this, `/settings/${screenName}`);
});