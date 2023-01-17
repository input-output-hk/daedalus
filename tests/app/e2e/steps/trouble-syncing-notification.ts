// @flow
import { Then } from '@cucumber/cucumber';
import { waitUntilTextInSelector } from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

const SYNCING_STATUS_HEADER = '.SyncingConnectingTitle_syncing h1';

Then(/^I should see the syncing status with "([^"]*)"$/, async function(text) {
  await waitUntilTextInSelector(this.client, {
    selector: SYNCING_STATUS_HEADER,
    text,
  });
});
