import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { isValidSpendingPassword } from '../../../renderer/app/utils/validations';
import {
  formattedAmountToLovelace,
  formattedLovelaceToAmount,
} from '../../../renderer/app/utils/formatters';
import {
  getUtxoChartData,
  getUtxoWalletPrettyAmount,
} from '../../../renderer/app/utils/utxoUtils';

Given('the `getUtxoChartData` function receives the following props:', function(
  data
) {
  let histogram = {};
  data.hashes().forEach(({ walletAmount, walletUtxosAmount }) => {
    histogram[walletAmount] = walletUtxosAmount;
  });
  this.context.histogram = histogram;
  this.context.utxoChartData = getUtxoChartData(histogram);
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
  'the wallet amounts should be formatted from lovelace values into wallet amount',
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
    ({ walletAmount }) => walletAmount <= 100000
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
    } = utxoChartData.find(({ walletAmount }) => walletAmount === 100000);
    expect(expectedAggregatedUtxosAmount).to.equal(
      calculatedAggregatedUtxosAmount
    );
  }
);

function getUtxoChartDataReceivesAWalletAmount(walletAmount) {
  this.context.walletAmount = walletAmount;
  this.context.walletPrettyAmount = getUtxoWalletPrettyAmount(walletAmount);
}

Given(
  'the `getUtxoWalletPrettyAmount` function receives the following {float}',
  getUtxoChartDataReceivesAWalletAmount
);
Given(
  'the `getUtxoWalletPrettyAmount` function receives the following {int}',
  getUtxoChartDataReceivesAWalletAmount
);

Then('the response should be a string', function() {
  const responseType = typeof this.context.walletPrettyAmount;
  expect(responseType).to.equal('string');
});

Then('amounts less than {int} should not be modified', function(amount) {
  const { walletAmount, walletPrettyAmount } = this.context;
  if (walletAmount < amount) {
    expect(walletPrettyAmount).to.equal(String(walletAmount));
    expect(/[a-zA-Z]/.test(walletPrettyAmount)).to.be.false;
  }
});

Then(
  'amounts equal or greater than {int} should be formatted to human-readable format',
  function(amount) {
    const { walletAmount, walletPrettyAmount } = this.context;
    if (walletAmount >= amount) {
      expect(walletPrettyAmount).to.not.equal(String(walletAmount));
      expect(/[a-zA-Z]/.test(walletPrettyAmount)).to.be.true;
    }
  }
);
