export const getHistogramFromTable = data => {
  let histogram = {};
  data.hashes().forEach(({ walletAmount, walletUtxosAmount }) => {
    histogram[walletAmount] = walletUtxosAmount;
  });
  return histogram;
};
