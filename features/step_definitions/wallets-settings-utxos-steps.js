import { Then } from 'cucumber';
import { expect } from 'chai';
import { BigNumber } from 'bignumber.js';
import { getVisibleTextsForSelector } from '../support/helpers/shared-helpers';
import { getWalletUtxosTotalAmount } from '../../source/renderer/app/utils/utxoUtils';
import { formattedWalletAmount } from '../../source/renderer/app/utils/formatters';

const component = '.WalletUtxoSettings_component';

const selectors = {
  component,
  title: `${component} > h1`,
  description: `${component} > p`,
  chart: '.WalletUtxoSettings_responsiveContainer',
  walletAmount: `${component} > p b:nth-child(1)`,
  walletUtxosAmount: `${component} > p b:nth-child(2)`,
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

    // TODO: Get the real active wallet amount
    const {
      value: { /* activeWalletAmount, */ histogram },
    } = await this.client.executeAsync(done =>
      done({
        // activeWalletAmount: daedalus.stores.wallets.active.amount,
        histogram: daedalus.stores.walletSettings.walletUtxos.histogram,
      })
    );

    // const expextedWalletAmount = formattedWalletAmount(walletLovelaceAmount);
    const activeWalletAmount = new BigNumber(
      parseFloat(renderedWalletAmount.replace(/,/g, ''))
    );
    const expextedWalletAmount = formattedWalletAmount(activeWalletAmount);

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
