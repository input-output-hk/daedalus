import { Then } from "cucumber";
import { waitUntilTextInSelector } from "../../../common/e2e/steps/helpers";

const SYNCING_STATUS_HEADER = '.SyncingConnectingTitle_syncing h1';
Then(/^I should see the syncing status with "([^"]*)"$/, async function (text) {
  await waitUntilTextInSelector(this.client, {
    selector: SYNCING_STATUS_HEADER,
    text
  });
});