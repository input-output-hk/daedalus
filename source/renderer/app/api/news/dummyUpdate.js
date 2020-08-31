// @flow
import inc from 'semver/functions/inc';
import { version as currentVersion } from '../../../../../package.json';
// import News from '../../domains/News';

// CONSTANTS
// const OLD_VERSION = '2.1.0';
// const NEW_VERSION = currentVersion;
const DARWIN_INSTALLER_URL =
  'https://update-cardano-mainnet.iohk.io/daedalus-2.2.0-mainnet-14276.pkg?t=1598888039830';
const WIN32_INSTALLER_URL =
  'https://update-cardano-mainnet.iohk.io/daedalus-2.2.0-mainnet-14276.bin?t=1598888039853';
const LINUX_INSTALLER_URL =
  'https://update-cardano-mainnet.iohk.io/daedalus-2.2.0-mainnet-14276.exe?t=1598888039860';
const DARWIN_HASH =
  'b546db6f06065ddce601b1dd6b9afdce8a3b46c01e0058503207be99f3e4976b';
const WIN32_HASH =
  'e8d3879b6402062f2626e0fe16ea452992c68f70c639adbf083015d3b24c1f03';
const LINUX_HASH =
  'a3826e1b05d211c1c21de4a28a967d46530a1c318ae970bb024001ad05684f74';

export const getDummyRawNews = (shouldRequestUpdate: boolean) => {
  const version = shouldRequestUpdate
    ? inc(currentVersion, 'minor')
    : currentVersion;
  const update = {
    updatedAt: 1598566800000,
    items: [
      // ITEM FOR UPDATED APS
      {
        title: {
          'en-US': `Daedalus ${version} - Release notes`,
          'ja-JP': `Daedalus ${version} - リリースノート`,
        },
        content: {
          'en-US':
            'RELEASE NOTES DESCRIPTION EN \n\n **bold** and *italic* example texts',
          'ja-JP': 'RELEASE NOTES DESCRIPTION JP',
        },
        target: {
          daedalusVersion: version,
          platforms: ['darwin', 'win32', 'linux'],
        },
        action: {
          label: {
            'en-US': 'Release notes',
            'ja-JP': 'リリースノート',
          },
          url: {
            'en-US': 'https://iohk.zendesk.com/hc/en-us/articles/900002374526',
            'ja-JP': 'https://iohk.zendesk.com/hc/ja/articles/900002374526',
          },
        },
        date: 1598566800000,
        type: 'announcement',
      },
      // ITEM FOR OUTDATED APS
      {
        title: {
          'en-US': `Daedalus ${version} available`,
          'ja-JP': `Daedalus ${version}配信開始`,
        },
        content: {
          'en-US':
            'UPDATE SCREEN DESCRIPTION EN \n\n **bold** and *italic* example texts',
          'ja-JP': 'UPDATE SCREEN DESCRIPTION JP',
        },
        target: {
          daedalusVersion: `<${version}`,
          platforms: ['darwin', 'win32', 'linux'],
        },
        action: {
          label: {
            'en-US': 'Download Daedalus at daedaluswallet.io',
            'ja-JP': 'daedaluswallet.ioでDaedalusをダウンロードする',
          },
          url: {
            'en-US':
              'https://daedaluswallet.io/en/download/?utm_source=newsfeed&utm_medium=newsfeed&utm_campaign=newsfeed',
            'ja-JP':
              'https://daedaluswallet.io/ja/download/?utm_source=newsfeed&utm_medium=newsfeed&utm_campaign=newsfeed',
          },
        },
        date: 1598566800000,
        type: 'software-update',
        softwareUpdate: {
          darwin: {
            version,
            hash: DARWIN_HASH,
            url: DARWIN_INSTALLER_URL,
          },
          win32: {
            version,
            hash: WIN32_HASH,
            url: WIN32_INSTALLER_URL,
          },
          linux: {
            version,
            hash: LINUX_HASH,
            url: LINUX_INSTALLER_URL,
          },
        },
      },
    ],
  };

  return JSON.stringify(update);
};
