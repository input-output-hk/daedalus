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

When(/^I trigger the apply-update endpoint$/, async function() {
  await this.client.executeAsync(done => {
    daedalus.api.ada
      .applyUpdate()
      .then(done)
      .catch(e => {
        throw e;
      });
  });
});

When(/^I set next update version to "([^"]*)"$/, async function(
  applicationVersion
) {
  await this.client.executeAsync((applicationVersion, done) => {
    daedalus.api.ada
      .setNextUpdate(parseInt(applicationVersion))
      .then(done)
      .catch(e => {
        throw e;
      });
  }, applicationVersion);
});
