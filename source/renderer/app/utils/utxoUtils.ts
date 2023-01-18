import { last } from 'lodash';
import type { Histogram } from '../api/wallets/types';
import { formattedLovelaceToAmount } from './formatters';

type UtxoChartItem = {
  walletRawAmount: number;
  walletAmount: string;
  walletUtxosAmount: number;
};
type UtxoChartData = Array<UtxoChartItem>;
export const getUtxoChartData = (histogram: Histogram): UtxoChartData =>
  Object.entries(histogram)
    .sort()
    .reduce((data, [lovelaceWalletAmount, walletUtxosAmount]) => {
      const walletAmount = formattedLovelaceToAmount(
        parseInt(lovelaceWalletAmount, 10)
      );

      if (walletAmount > 100000) {
        let lastItem: UtxoChartItem = last(data);
        const { walletUtxosAmount: lastWalletUtxosAmount } = lastItem;
        lastItem = {
          ...lastItem,
          walletUtxosAmount:
            // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
            parseInt(walletUtxosAmount, 10) +
            // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
            parseInt(lastWalletUtxosAmount, 10),
        };
        data[data.length - 1] = lastItem;
      } else {
        data.push({
          walletRawAmount: walletAmount,
          walletAmount: getUtxoWalletPrettyAmount(walletAmount),
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
          walletUtxosAmount: parseInt(walletUtxosAmount, 10),
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
export const getWalletUtxosTotalAmount = (histogram: Histogram): number => {
  const histogramArr = Object.values(histogram);
  const walletUtxosAmount = histogramArr.length
    ? histogramArr.reduce(
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
        (amount, value) => parseInt(amount, 10) + parseInt(value, 10)
      )
    : 0;
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
  return parseInt(walletUtxosAmount, 10);
};
