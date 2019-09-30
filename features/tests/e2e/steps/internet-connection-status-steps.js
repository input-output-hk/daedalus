import { Given, When, Then } from 'cucumber';

Given(/^I set Internet Connection to: "([^"]*)"$/, async function(status) {
  await this.client.execute(s => {
    daedalus.stores.networkStatus.setIsInternetConnected(s !== 'offline');
  }, status);
});

Then('I should see Internet Connection status dialog', async function() {
  return this.client.waitForVisible(
    '.InternetConnectionOfflineStatus_component'
  );
});

When('I click on "Check again" button', async function() {
  return this.waitAndClick('.InternetConnectionOfflineStatus_checkAgainButton');
});

When('I see loading spinner', async function() {
  return this.client.waitForVisible('.SimpleLoadingSpinner_root');
});

Then(
  'I should not see the Internet connection status dialog anymore',
  function() {
    return this.client.waitForVisible(
      '.InternetConnectionOfflineStatus_component',
      null,
      true
    );
  }
);
