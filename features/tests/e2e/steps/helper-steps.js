import { When, Then } from 'cucumber';
import {
  generateScreenshotFilePath,
  saveScreenshot,
} from '../helpers/screenshot';

const oneHour = 60 * 60 * 1000;
// Helper step to pause execution for up to an hour ;)
When(/^I freeze$/, { timeout: oneHour }, callback => {
  setTimeout(callback, oneHour);
});

Then(/^I should see the initial screen$/, function() {
  return this.client.waitForVisible('.SidebarLayout_component');
});

When(/^I take a screenshot named "([^"]*)"$/, async function(testName) {
  const file = generateScreenshotFilePath(testName);
  await saveScreenshot(this, file);
});

When(/^I inject fault named "([^"]*)"$/, async function(faultName) {
  await this.client.executeAsync((name, done) => {
    daedalus.api.ada
      .setCardanoNodeFault([name, true])
      .then(done)
      .catch(e => {
        throw e;
      });
  }, faultName);
});
