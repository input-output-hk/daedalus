import { When, Then } from 'cucumber';
import { expect } from 'chai';

const SELECTORS = {
  BLOCK_CONSOLIDATION_COMPONENT: '.BlockConsolidationStatus_component',
  BLOCK_CONSOLIDATION_EXPLANATION:
    '.BlockConsolidationStatus_content p:nth-child(3)',
  EPOCHS_CONSOLIDATION_STATUS: '.BlockConsolidationStatus_epochs p span',
  EPOCHS_CONSOLIDATED:
    '.BlockConsolidationStatus_indicatorEpochsConsolidated p',
  TRAILING_BY_2_EPOCH: '.BlockConsolidationStatus_indicatorEpochsBehind p',
  MAXIMUM_EPOCH: '.BlockConsolidationStatus_fullEpoch',
  SYNC_PROGRESS: '.BlockConsolidationStatus_indicatorEpochsSynced p span',
};

When(/^I toggle the Block Consolidation Status Page$/, async function() {
  await this.client.execute(() =>
    daedalus.actions.app.toggleBlockConsolidationStatusScreen.trigger()
  );
});

Then(/^the Block Consolidation Status Page is (hidden|visible)/, async function(
  state
) {
  const isVisible = state === 'visible';
  await this.client.waitForVisible(
    SELECTORS.BLOCK_CONSOLIDATION_COMPONENT,
    null,
    !isVisible
  );
});

Then(
  /^the page accurately renders an explanation of how block consolidation works in file storage$/,
  async function() {
    await this.client.waitForText(SELECTORS.BLOCK_CONSOLIDATION_EXPLANATION);
    const explanationText = await this.client.getText(
      SELECTORS.BLOCK_CONSOLIDATION_EXPLANATION
    );

    const {
      value: { currentEpoch },
    } = await this.client.executeAsync(done =>
      done({ currentEpoch: daedalus.stores.networkStatus.currentEpoch })
    );

    expect(explanationText).to.equal(
      `Blocks for the current epoch (${currentEpoch}) and the previous epoch (${Math.max(
        currentEpoch - 1,
        0
      )}) are stored as one file per block. All previous epochs will be consolidated to two files per epoch.`
    );
  }
);

Then(
  /^the page accurately renders epochs consolidated out of the total in the main blocks graphic$/,
  async function() {
    await this.client.waitForText(SELECTORS.EPOCHS_CONSOLIDATION_STATUS);
    const consolidationStatus = await this.client.getText(
      SELECTORS.EPOCHS_CONSOLIDATION_STATUS
    );

    const {
      value: { epochsConsolidated, currentEpoch },
    } = await this.client.executeAsync(done =>
      done({
        epochsConsolidated: daedalus.stores.networkStatus.epochsConsolidated,
        currentEpoch: daedalus.stores.networkStatus.currentEpoch,
      })
    );

    expect(consolidationStatus).to.equal(
      `${epochsConsolidated} of ${currentEpoch}\nepochs consolidated`
    );
  }
);

Then(
  /^the page accurately renders epochs consolidated above the progress bar$/,
  async function() {
    await this.client.waitForText(SELECTORS.EPOCHS_CONSOLIDATED);
    const consolidationStatus = await this.client.getText(
      SELECTORS.EPOCHS_CONSOLIDATED
    );

    const {
      value: { epochsConsolidated },
    } = await this.client.executeAsync(done =>
      done({
        epochsConsolidated: daedalus.stores.networkStatus.epochsConsolidated,
      })
    );

    expect(consolidationStatus).to.equal(
      `${epochsConsolidated} epochs consolidated`
    );
  }
);

Then(
  /^the page accurately renders the epoch trailing 2 behind the current epoch above the progress bar$/,
  async function() {
    await this.client.waitForText(SELECTORS.TRAILING_BY_2_EPOCH);
    const trailingEpochText = await this.client.getText(
      SELECTORS.TRAILING_BY_2_EPOCH
    );

    const {
      value: { currentEpoch },
    } = await this.client.executeAsync(done =>
      done({ currentEpoch: daedalus.stores.networkStatus.currentEpoch })
    );

    expect(trailingEpochText).to.equal(
      `epoch ${Math.max(currentEpoch - 2, 0)}`
    );
  }
);

Then(
  /^the page accurately renders the current epoch signifying the max end of the progress bar$/,
  async function() {
    await this.client.waitForText(SELECTORS.MAXIMUM_EPOCH);
    const currentEpochText = await this.client.getText(SELECTORS.MAXIMUM_EPOCH);

    const {
      value: { currentEpoch },
    } = await this.client.executeAsync(done =>
      done({ currentEpoch: daedalus.stores.networkStatus.currentEpoch })
    );

    expect(currentEpochText).to.equal(`epoch ${currentEpoch}`);
  }
);

Then(
  /^the page accurately renders the node's sync progress as a percentage below the progress bar$/,
  async function() {
    await this.client.waitForText(SELECTORS.SYNC_PROGRESS);

    const {
      value: { syncProgress },
    } = await this.client.executeAsync(done => {
      daedalus.stores.networkStatus
        ._updateNetworkStatus()
        .then(() =>
          done({ syncProgress: daedalus.stores.networkStatus.syncProgress })
        )
        .catch(error => done(error));
    });

    const blocksSyncedText = await this.client.getText(SELECTORS.SYNC_PROGRESS);
    expect(blocksSyncedText).to.equal(`${syncProgress}% blocks synced`);
  }
);
