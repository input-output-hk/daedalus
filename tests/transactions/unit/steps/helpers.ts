export const getHistogramFromTable = data => {
  const histogram = {};
  data.hashes().forEach(({
    walletAmount,
    walletUtxosAmount
  }) => {
    histogram[walletAmount] = walletUtxosAmount;
  });
  return histogram;
};