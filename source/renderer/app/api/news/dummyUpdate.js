// @flow
import { version } from '../../../../../package.json';
import News from '../../domains/News';
import news from './newsfeed_mainnet.json';
import type { NewsType } from './types';

const UPDATE_VERSION = '2.2.0';
const DARWIN_INSTALLER_URL =
  'https://update-cardano-mainnet.iohk.io/daedalus-2.2.0-mainnet-14276.pkg';
const WIN32_INSTALLER_URL =
  'https://update-cardano-mainnet.iohk.io/daedalus-2.2.0-mainnet-14276.exe';
const LINUX_INSTALLER_URL =
  'https://update-cardano-mainnet.iohk.io/daedalus-2.2.0-mainnet-14276.bin';
const DARWIN_HASH =
  'b546db6f06065ddce601b1dd6b9afdce8a3b46c01e0058503207be99f3e4976b';
const WIN32_HASH =
  'a3826e1b05d211c1c21de4a28a967d46530a1c318ae970bb024001ad05684f74';
const LINUX_HASH =
  'e8d3879b6402062f2626e0fe16ea452992c68f70c639adbf083015d3b24c1f03';

export const getDummyNews = () => {
  const { updatedAt, items: updateItems } = news;

  const items = [
    // Update Items
    ...updateItems.reduce((arr, item) => {
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
    // Newsfeed items
    getNewsItem(1, 'announcement', true),
    getNewsItem(2, 'announcement', true),
    getNewsItem(3, 'announcement', true),
    getNewsItem(4, 'announcement', true),
    getNewsItem(5, 'announcement', true),
    getNewsItem(6, 'announcement'),
    getNewsItem(7, 'announcement'),
    getNewsItem(8, 'announcement'),
    getNewsItem(9, 'announcement'),
    getNewsItem(10, 'announcement'),
    getNewsItem(11, 'announcement'),
    getNewsItem(12, 'announcement'),
    getNewsItem(13, 'announcement'),
  ];

  return {
    updatedAt,
    items,
  };
};

export const getNewsItem = (id: number, type: NewsType, read?: boolean) => ({
  title: {
    'en-US': `Item ${id} EN`,
    'ja-JP': `Item ${id} JP`,
  },
  content: {
    'en-US': `Description ${id} EN`,
    'ja-JP': `Description ${id} JP`,
  },
  target: {
    daedalusVersion: version,
    platforms: ['darwin', 'win32', 'linux'],
  },
  action: {
    label: {
      'en-US': 'Download Daedalus at daedaluswallet.io',
      'ja-JP': 'daedaluswallet.ioでDaedalusをダウンロードする',
    },
    url: {
      'en-US': 'https://daedaluswallet.io/en/download',
      'ja-JP': 'https://daedaluswallet.io/ja/download',
    },
  },
  date: new Date().getTime() - 100 - id,
  type,
  read: read || false,
});
