// @flow
import type { Histogram } from '../api/wallets/types';
import { LOVELACES_PER_ADA } from '../config/numbersConfig';

export const getChartData = (histogram: Histogram) /* : Array<any> */ =>
  Object.entries(histogram)
    .sort()
    .reduce((data, [rawWalletAmount, walletUtxosAmount]) => {
      const walletAmount = parseInt(rawWalletAmount, 10) / LOVELACES_PER_ADA;
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

export const getPrettyAmount = (amount: number) => {
  let prettyAmount = String(amount);
  if (amount === 1000) prettyAmount = '1K';
  if (amount === 10000) prettyAmount = '10K';
  if (amount === 100000) prettyAmount = '10K+';
  return prettyAmount;
};
