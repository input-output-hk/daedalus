// @flow
import { When, Then } from 'cucumber';
import { expect } from 'chai';
import { getVisibleTextsForSelector } from '../../../common/e2e/steps/helpers';
import { i18nHelpers } from '../../../settings/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

const SELECTORS = {
  BLOCK_CONSOLIDATION_COMPONENT: '.BlockConsolidationStatus_component',
  BLOCK_CONSOLIDATION_EXPLANATION: '.BlockConsolidationStatus_content p:nth-child(3)',
  EPOCHS_CONSOLIDATED: '.BlockConsolidationStatus_indicatorEpochsConsolidated p',
  EPOCHS_CONSOLIDATION_STATUS: '.BlockConsolidationStatus_epochs p span b',
  MAXIMUM_EPOCH: '.BlockConsolidationStatus_fullEpoch',
  SYNC_PROGRESS: '.BlockConsolidationStatus_indicatorEpochsSynced p span',
  SYNC_PROGRESS_LOADING_STATE: '.BlockConsolidationStatus_indicatorContainerNoCurrentEpochs',
  TRAILING_BY_2_EPOCH: '.BlockConsolidationStatus_indicatorEpochsBehind p',
};
const { formatMessage } = i18nHelpers;

When(/^I open the Block Consolidation Status Dialog$/, async function() {
  await this.client.execute(() =>
    daedalus.actions.app.openBlockConsolidationStatusDialog.trigger()
  );
});

When(/^I close the Block Consolidation Status Dialog$/, async function() {
  await this.client.execute(() =>
    daedalus.actions.app.closeBlockConsolidationStatusDialog.trigger()
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
  /^the page accurately renders the follow explanation of how block consolidation works in file storage:$/,
  async function(data) {
    const [consolidationText] = data.hashes();

    let [renderedText] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.BLOCK_CONSOLIDATION_EXPLANATION
    );

    const {
      value: { currentEpochValue },
    } = await this.client.executeAsync(done =>
      done({
        currentEpochValue: daedalus.stores.blockConsolidation.currentEpoch,
      })
    );

    let currentEpoch = '';
    let currentEpochBehind = '';

    if (currentEpochValue && currentEpochValue > 0) {
      currentEpoch = `(${currentEpochValue})`;
      currentEpochBehind = `(${Math.max(currentEpochValue - 1, 0)})`;
    }

    let expectedText = await formatMessage(this.client, {
      id: consolidationText.message,
      values: {
        currentEpoch,
        currentEpochBehind,
      },
    });

    // Removes double spaces caused by missing currentEpoch
    renderedText = renderedText.replace(/\s+/g, ' ');
    expectedText = expectedText.replace(/\s+/g, ' ');
    expect(renderedText).to.equal(expectedText);
  }
);

Then(
  /^the page accurately renders epochs consolidated out of the total in the main blocks graphic$/,
  async function() {
    const [
      renderedEpochsConsolidated,
      renderedCurrentEpoch,
    ] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.EPOCHS_CONSOLIDATION_STATUS
    );

    const {
      value: { expectedEpochsConsolidated, expectedCurrentEpochValue },
    } = await this.client.executeAsync(done =>
      done({
        expectedEpochsConsolidated:
          daedalus.stores.blockConsolidation.epochsConsolidated,
        expectedCurrentEpochValue:
          daedalus.stores.blockConsolidation.currentEpoch,
      })
    );

    expect(parseInt(renderedEpochsConsolidated, 10)).to.equal(
      expectedEpochsConsolidated
    );
    expect(parseInt(renderedCurrentEpoch, 10)).to.equal(
      expectedCurrentEpochValue
    );
  }
);

Then(
  /^the page accurately renders epochs consolidated above the progress bar:$/,
  async function(data) {
    const {
      value: { epochsConsolidated },
    } = await this.client.executeAsync(done =>
      done({
        epochsConsolidated:
          daedalus.stores.blockConsolidation.epochsConsolidated,
      })
    );
    const [expectedTextData] = data.hashes();
    const expectedTextMessage = await this.intl(expectedTextData.message);
    const expectedText = `${epochsConsolidated} ${expectedTextMessage}`;

    const [renderedText] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.EPOCHS_CONSOLIDATED
    );

    expect(renderedText).to.equal(expectedText);
  }
);

Then(
  /^the page accurately renders the epoch trailing 2 behind the current epoch above the progress bar:$/,
  async function(data) {
    const {
      value: { currentEpoch },
    } = await this.client.executeAsync(done =>
      done({ currentEpoch: daedalus.stores.blockConsolidation.currentEpoch })
    );
    const epochBehind = Math.max(currentEpoch - 2, 0);
    const [expectedTextData] = data.hashes();
    const expectedTextMessage = await this.intl(expectedTextData.message);
    const expectedText = `${expectedTextMessage} ${epochBehind}`;

    const [renderedText] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.TRAILING_BY_2_EPOCH
    );

    expect(renderedText).to.equal(expectedText);
  }
);

Then(
  /^the page accurately renders the current epoch signifying the max end of the progress bar:$/,
  async function(data) {
    const {
      value: { currentEpoch },
    } = await this.client.executeAsync(done =>
      done({ currentEpoch: daedalus.stores.blockConsolidation.currentEpoch })
    );
    const [expectedTextData] = data.hashes();
    const expectedTextMessage = await this.intl(expectedTextData.message);
    const expectedText = `${expectedTextMessage} ${currentEpoch}`;

    const [renderedText] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.MAXIMUM_EPOCH
    );

    expect(renderedText).to.equal(expectedText);
  }
);

Then(
  /^the page accurately renders the node's sync progress as a percentage below the progress bar:$/,
  async function(data) {
    const {
      value: { epochsSynced },
    } = await this.client.executeAsync(done => {
      daedalus.stores.networkStatus
        ._updateNetworkStatus()
        .then(() =>
          done({
            epochsSynced: daedalus.stores.networkStatus.syncProgress,
          })
        )
        .catch(error => done(error));
    });
    const [expectedTextData] = data.hashes();
    const expectedText = await formatMessage(this.client, {
      id: expectedTextData.message,
      values: { epochsSynced },
    });

    const [renderedText] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.SYNC_PROGRESS
    );

    expect(renderedText).to.equal(expectedText);
  }
);

Then(
  /^the page hides the node's sync progress as a percentage below the progress bar$/,
  async function() {
    return this.client.waitForVisible(SELECTORS.SYNC_PROGRESS, null, true);
  }
);

Then(/^the page renders the progress bar in loading state$/, async function() {
  return this.client.waitForVisible(
    SELECTORS.SYNC_PROGRESS_LOADING_STATE,
    null,
    true
  );
});

When(/^the fallback function returns the current epoch$/, async function() {
  return this.client.waitForVisible(SELECTORS.MAXIMUM_EPOCH);
});

When(
  /^I set the Node Setting Api Request to return faulty response$/,
  function() {
    return this.client.execute(() => {
      daedalus.api.setFaultyNodeSettingsApi = true;
    });
  }
);
