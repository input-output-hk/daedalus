// @flow
import news from './newsfeed_mainnet.json';

const UPDATE_VERSION = '2.2.0';
const DARWIN_INSTALLER_URL =
  'https://update-cardano-mainnet.iohk.io/daedalus-2.2.0-mainnet-14276.pkg';
const WIN32_INSTALLER_URL =
  'https://update-cardano-mainnet.iohk.io/daedalus-2.2.0-mainnet-14276.bin';
const LINUX_INSTALLER_URL =
  'https://update-cardano-mainnet.iohk.io/daedalus-2.2.0-mainnet-14276.exe';
const DARWIN_HASH =
  'b546db6f06065ddce601b1dd6b9afdce8a3b46c01e0058503207be99f3e4976b';
const WIN32_HASH =
  'e8d3879b6402062f2626e0fe16ea452992c68f70c639adbf083015d3b24c1f03';
const LINUX_HASH =
  'a3826e1b05d211c1c21de4a28a967d46530a1c318ae970bb024001ad05684f74';

const { updatedAt, items } = news;

export const getDummyNews = () => ({
  updatedAt,
  items: items.reduce((arr, item) => {
    const { daedalusVersion } = item.target;
    if (daedalusVersion === `<${UPDATE_VERSION}`) {
      item.type = 'software-update';
      item.softwareUpdate = {
        darwin: {
          version: UPDATE_VERSION,
          hash: DARWIN_HASH,
          url: DARWIN_INSTALLER_URL,
        },
        win32: {
          version: UPDATE_VERSION,
          hash: WIN32_HASH,
          url: WIN32_INSTALLER_URL,
        },
        linux: {
          version: UPDATE_VERSION,
          hash: LINUX_HASH,
          url: LINUX_INSTALLER_URL,
        },
      };
    }
    arr.push(item);
    return arr;
  }, []),
});
