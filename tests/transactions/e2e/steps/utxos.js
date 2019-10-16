// @flow
import { Then } from 'cucumber';
import { expect } from 'chai';
import { getVisibleTextsForSelector } from '../../../common/e2e/steps/helpers';
import { getWalletUtxosTotalAmount } from '../../../../source/renderer/app/utils/utxoUtils';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

const container = '.WalletUtxo_container';

const selectors = {
  container,
  title: `${container} > h1`,
  description: `${container} > p`,
  chart: '.WalletUtxo_responsiveContainer',
  walletAmount: `${container} > p b:nth-child(1)`,
  walletUtxosAmount: `${container} > p b:nth-child(2)`,
};

Then('the {string} element renders the following text:', async function(
  element,
  data
) {
  const [expectedTextData] = data.hashes();
  const [renderedText] = await getVisibleTextsForSelector(
    this.client,
    selectors[element]
  );
  const expectedText = await this.intl(expectedTextData.message);
  expect(renderedText).to.equal(expectedText);
});

Then(
  'the page description displays the correct wallet and UTXOs amount',
  async function() {
    const [renderedWalletAmount] = await getVisibleTextsForSelector(
      this.client,
      selectors.walletAmount
    );
    const [renderedWalletUtxosAmount] = await getVisibleTextsForSelector(
      this.client,
      selectors.walletUtxosAmount
    );
    const {
      value: { expextedWalletAmount, histogram },
    } = await this.client.executeAsync(done => {
      const histogram = daedalus.stores.walletSettings.walletUtxos
        ? daedalus.stores.walletSettings.walletUtxos.histogram
        : null;
      done({
        expextedWalletAmount: daedalus.stores.wallets.activeValue,
        histogram,
      });
    });
    const expectedWalletUtxosAmount = getWalletUtxosTotalAmount(histogram);
    expect(expextedWalletAmount).to.equal(renderedWalletAmount);
    expect(expectedWalletUtxosAmount).to.equal(
      parseInt(renderedWalletUtxosAmount, 10)
    );
  }
);

Then(/^the UTXOs chart is (hidden|visible)/, async function(state) {
  const isVisible = state === 'visible';
  await this.client.waitForVisible(selectors.chart, null, !isVisible);
});
