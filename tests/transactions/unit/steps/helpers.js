// @flow
export const getHistogramFromTable = (data: Object) => {
  let histogram = {};
  data.hashes().forEach(({ walletAmount, walletUtxosAmount }) => {
    histogram[walletAmount] = walletUtxosAmount;
  });
  return histogram;
};
