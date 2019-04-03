// @flow
import type { Histogram } from '../api/wallets/types';
import { formattedLovelaceToAmount } from './formatters';

type UtxoChartData = Array<{
  walletAmount: number,
  walletUtxosAmount: number,
}>;

export const getUtxoChartData = (histogram: Histogram): UtxoChartData =>
  Object.entries(histogram)
    .sort()
    .reduce((data, [rawWalletAmount, walletUtxosAmount]) => {
      const walletAmount = formattedLovelaceToAmount(
        parseInt(rawWalletAmount, 10)
      );
      if (walletAmount > 100000) {
        let lastItem: { walletAmount: number, walletUtxosAmount: any } =
          data[data.length - 1];
        const { walletUtxosAmount: lastWalletUtxosAmount } = lastItem;
        lastItem = {
          ...lastItem,
          walletUtxosAmount:
            parseInt(walletUtxosAmount, 10) +
            parseInt(lastWalletUtxosAmount, 10),
        };
        data[data.length - 1] = lastItem;
      } else {
        data.push({
          walletAmount,
          walletUtxosAmount,
        });
      }
      return data;
    }, []);

export const getUtxoWalletPrettyAmount = (amount: number) => {
  let prettyAmount = String(amount);
  if (amount === 1000) prettyAmount = '1K';
  if (amount === 10000) prettyAmount = '10K';
  if (amount === 100000) prettyAmount = '10K+';
  return prettyAmount;
};

export const getWalletUtxosAmount = (histogram: Histogram): number => {
  const histogramArr = Object.values(histogram);
  const walletUtxosAmount = histogramArr.length
    ? histogramArr.reduce(
        (amount, value) => parseInt(amount, 10) + parseInt(value, 10)
      )
    : 0;
  return parseInt(walletUtxosAmount, 10);
};
