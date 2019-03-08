import { When, Then } from 'cucumber';
import { expect } from 'chai';
import { waitUntilTextInSelector } from '../support/helpers/shared-helpers';

// const { epochsConsolidated, syncProgress, currentEpoch } = networkStatus;

const BLOCK_CONSOLIDATION_COMPONENT = '.BlockConsolidationStatus_component';
const EPOCHS_CONSOLIDATION_STATUS = '.BlockConsolidationStatus_epochs p span';
const EPOCHS_CONSOLIDATED = '.BlockConsolidationStatus_indicatorEpochsConsolidated p'; // `${epochsConsolidated} epochs consolidated`
const EPOCHS_BEHIND = '.BlockConsolidationStatus_indicatorEpochsBehind p'; // `epoch ${Math.max(currentEpoch - 2, 0)}`
const SYNC_PROGRESS = '.BlockConsolidationStatus_indicatorEpochsSynced p span'; // `${daedalus.stores.networkStatus.syncProgress}% blocks synced`
const FULL_EPOCH = '.BlockConsolidationStatus_fullEpoch'; // `epoch ${currentEpoch}`

When(/^I (open|close) the Block Consolidation Status Page$/, async function (state) {
  return await this.client.execute(() => daedalus.actions.app.toggleBlockConsolidationStatusScreen.trigger());
});

Then(/^the Block Consolidation Status Page is (hidden|visible)/, async function (state) {
  const isVisible = state === 'visible';
  return await this.client.waitForVisible(BLOCK_CONSOLIDATION_COMPONENT, null, !isVisible);
});

Then(/^the page accurately renders how many epochs are consolidated out of the total$/, async function () {
  await this.client.waitForText(EPOCHS_CONSOLIDATION_STATUS);
  const consolidationStatus = await this.client.getText(EPOCHS_CONSOLIDATION_STATUS);

  const { value: { epochsConsolidated, currentEpoch } } = await this.client.executeAsync(done => (
    done({
      epochsConsolidated: daedalus.stores.networkStatus.epochsConsolidated,
      currentEpoch: daedalus.stores.networkStatus.currentEpoch,
    })
  ));

  expect(consolidationStatus).to.equal(`${epochsConsolidated} of ${currentEpoch}\nepochs consolidated`);
});
