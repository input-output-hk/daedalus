import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { isValidSpendingPassword } from '../../../../source/renderer/app/utils/validations';
import {
  formattedAmountToLovelace,
  formattedLovelaceToAmount,
} from '../../../../source/renderer/app/utils/formatters';
import {
  getUtxoChartData,
  getUtxoWalletPrettyAmount,
  getWalletUtxosTotalAmount,
} from '../../../../source/renderer/app/utils/utxoUtils';
import { getHistogramFromTable } from '../setup/utxo-helpers';

Given('the `getUtxoChartData` function receives the following props:', function(
  data
) {
  const histogram = getHistogramFromTable(data);
  const utxoChartData = getUtxoChartData(histogram);
  this.context.histogram = histogram;
  this.context.utxoChartData = utxoChartData;
  this.context.response = utxoChartData;
  this.context.sortedHistogram = Object.entries(histogram).sort();
});

Then('the wallet amounts should be sorted sorted ascending', function() {
  const { utxoChartData: arr } = this.context;
  const isSorted = arr.every(({ walletAmount }, i) =>
    i < arr.length - 1 ? walletAmount <= arr[i + 1].walletAmount : true
  );
  expect(isSorted).to.be.true;
});

Then(
  'the wallet amounts should be formatted into human-readable text',
  function() {
    const { utxoChartData, sortedHistogram } = this.context;
    const isFormatted = utxoChartData.every(
      ({ walletAmount }, index) =>
        walletAmount === formattedLovelaceToAmount(sortedHistogram[index][0])
    );
    expect(isFormatted).to.be.true;
  }
);

Then('there should be no wallet amounts greater than 100K', function() {
  return this.context.utxoChartData.every(
    ({ walletRawAmount }) => walletRawAmount <= 100000
  );
});

Then(
  'the wallet UTXO amounts for wallet amounts greater than {int} should be aggregated',
  function(walletAmount) {
    const walletAmountThreshold = formattedAmountToLovelace(
      String(walletAmount)
    );
    const { histogram, utxoChartData, sortedHistogram } = this.context;
    const expectedAggregatedUtxosAmount = sortedHistogram.reduce(
      (sum, [walletAmount, walletUtxosAmount]) => {
        if (walletAmount >= walletAmountThreshold)
          sum += parseInt(walletUtxosAmount, 10);
        return sum;
      },
      0
    );
    const {
      walletUtxosAmount: calculatedAggregatedUtxosAmount,
    } = utxoChartData.find(({ walletRawAmount }) => walletRawAmount === 100000);
    expect(expectedAggregatedUtxosAmount).to.equal(
      calculatedAggregatedUtxosAmount
    );
  }
);

function getUtxoChartDataReceivesAWalletAmount(walletAmount) {
  this.context.walletRawAmount = walletAmount;
  const walletPrettyAmount = getUtxoWalletPrettyAmount(walletAmount);
  this.context.walletAmount = walletPrettyAmount;
  this.context.response = walletPrettyAmount;
}

Given(
  'the `getUtxoWalletPrettyAmount` function receives the following {float}',
  getUtxoChartDataReceivesAWalletAmount
);
Given(
  'the `getUtxoWalletPrettyAmount` function receives the following {int}',
  getUtxoChartDataReceivesAWalletAmount
);

Then('the response should have type {string}', function(type) {
  const { response } = this.context;
  if (type === 'array') {
    return expect(Array.isArray(response)).to.be.true;
  }
  expect(typeof response).to.equal(type);
});

Then('wallet amounts less than {int} should not be modified', function(amount) {
  const { walletAmount, walletRawAmount } = this.context;
  if (walletRawAmount < amount) {
    expect(walletAmount).to.equal(String(walletRawAmount));
    expect(/[a-zA-Z]/.test(walletAmount)).to.be.false;
  }
});

Then(
  'wallet amounts equal or greater than {int} should be formatted into human-readable text',
  function(amount) {
    const { walletAmount, walletRawAmount } = this.context;
    if (walletRawAmount >= amount) {
      expect(walletAmount).to.not.equal(String(walletRawAmount));
      expect(/[a-zA-Z]/.test(walletAmount)).to.be.true;
    }
  }
);

Given(
  'the `getWalletUtxosTotalAmount` function receives the following props:',
  function(data) {
    const histogram = getHistogramFromTable(data);
    this.context.histogram = histogram;
    this.context.response = getWalletUtxosTotalAmount(histogram);
  }
);

Then('the response should be the number {int}', function(response) {
  expect(response).to.equal(this.context.response);
});
