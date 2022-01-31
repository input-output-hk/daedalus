import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import { environment } from "../../../../source/main/environment";
import { getVisibleTextsForSelector } from "../../../common/e2e/steps/helpers";
import { i18nHelpers } from "../../../settings/e2e/steps/helpers";

const currentAppVersion = environment.version;
const currentAppVersionChunks = currentAppVersion.split('.');
const {
  formatMessage
} = i18nHelpers;
const nextAppVersion = [currentAppVersionChunks[0], parseInt(currentAppVersionChunks[1], 10) + 1, currentAppVersionChunks[2]].join('.');
const SELECTORS = {
  DESCRIPTION: 'appUpdate.manualUpdateOverlay.description2',
  OVERLAY: '.ManualUpdate_content',
  VERSION_INFO: '.ManualUpdate_content .ManualUpdate_description p:nth-child(2)'
};
Given(/^There is a newer application version available$/, async function () {// await this.client.execute(version => {
  //   daedalus.api.ada.setLatestAppVersion(version);
  // }, nextAppVersion);
});
Then(/^I should see the "Manual Update" overlay$/, function () {
  return this.client.waitForVisible(SELECTORS.OVERLAY);
});
Then(/^The overlay should accurately display the version info$/, async function () {
  const [renderedText] = await getVisibleTextsForSelector(this.client, SELECTORS.VERSION_INFO);
  let expectedText = await formatMessage(this.client, {
    id: SELECTORS.DESCRIPTION,
    values: {
      currentAppVersion,
      availableAppVersion: nextAppVersion
    }
  });
  // Expected text contains HTML tags so we need to strip them before running comparison
  expectedText = expectedText.replace(/<[^>]*>?/gm, '');
  expect(renderedText).to.equal(expectedText);
});
When(/^I trigger the apply-update endpoint$/, async function () {// await this.client.executeAsync(done => {
  //   daedalus.api.ada
  //     .applyUpdate()
  //     .then(done)
  //     .catch(e => {
  //       throw e;
  //     });
  // });
});