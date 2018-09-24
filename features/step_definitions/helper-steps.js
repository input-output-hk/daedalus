import { When, Then } from 'cucumber';

const oneHour = 60 * 60 * 1000;
// Helper step to pause execution for up to an hour ;)
When(/^I freeze$/, { timeout: oneHour }, (callback) => {
  setTimeout(callback, oneHour);
});

When(/^I refresh the application$/, async function () {
  return this.client.refresh();
});

Then(/^I should see the initial screen$/, function () {
  return this.client.waitForVisible('.SidebarLayout_component');
});
