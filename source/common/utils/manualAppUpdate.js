// @flow
export const getLatestVersionInfo = async (network: string) => {
  const url = `https://s3-ap-northeast-1.amazonaws.com/update.cardano-${network}.iohk.io/daedalus-latest-version.json`;
  return fetch(url)
    .then(res => res.json())
    .catch(error => {
      throw error;
    });
};
