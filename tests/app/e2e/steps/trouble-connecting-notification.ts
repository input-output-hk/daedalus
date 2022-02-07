import { Then } from "cucumber";
import { waitUntilTextInSelector } from "../../../common/e2e/steps/helpers";

const SELECTORS = {
  REPORT_ISSUE_BTN: '.ReportIssue_actionButton.reportIssueButton',
  REPORT_ISSUE_HEADER: '.ReportIssue_reportIssueText',
  SYNCING_CONNECTING_COMPONENT: '.SyncingConnecting_component'
};
Then(/^I should not see the loading screen$/, async function () {
  await this.client.waitForVisible(SELECTORS.SYNCING_CONNECTING_COMPONENT, null, true);
});
Then(/^I should see the report issue notification displaying "([^"]*)"$/, async function (text) {
  await waitUntilTextInSelector(this.client, {
    selector: SELECTORS.REPORT_ISSUE_HEADER,
    text
  });
});
Then(/^I should not see the report issue notification$/, async function () {
  await this.client.waitForVisible(SELECTORS.REPORT_ISSUE_HEADER, null, true);
});
Then(/^The report issue button should be (hidden|visible)$/, async function (state) {
  const waitForHidden = state === 'hidden';
  await this.client.waitForVisible(SELECTORS.REPORT_ISSUE_BTN, null, waitForHidden);
});