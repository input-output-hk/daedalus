// @flow
import inc from 'semver/functions/inc';
import { version as currentVersion } from '../../../../../package.json';
import News from '../../domains/News';

export const version = currentVersion;
export const availableAppVersion = inc(version, 'minor');

// Real files data
const softwareUpdate = {
  darwin: {
    version: availableAppVersion,
    hash: 'c541091d7368359eb7046e36525a03949a391e81455c879f94fb02d53e5c2620',
    url:
      'https://update-cardano-mainnet.iohk.io/daedalus-2.1.0-mainnet-14079.pkg?t=1597274058138',
  },
  win32: {
    version: availableAppVersion,
    hash: 'a13487d93c7cff51e1b162b1575b137696b25964dbf0160022d74fdaca5a8cb7',
    url:
      'https://update-cardano-mainnet.iohk.io/daedalus-2.1.0-mainnet-14079.bin?t=1597274058196',
  },
  linux: {
    version: availableAppVersion,
    hash: 'a2c60a80c8b34dd0ab57de33de970a2bca30fb9e04c3f4559708ecbe9f717112',
    url:
      'https://update-cardano-mainnet.iohk.io/daedalus-2.1.0-mainnet-14079.exe?t=1597274058204',
  },
};

const commonUpdateData = {
  target: {
    daedalusVersion: version,
    platforms: ['win32', 'linux', 'darwin'],
  },
  date: 1594043606135,
  softwareUpdate,
  type: 'software-update',
  id: 'dswkljhfksdhfksdhf',
};

export const updateEN = {
  title: `Daedalus ${availableAppVersion} update`,
  content:
    'This release brings **cool new features** and *some bug fixes*. It provides a more seamless experience.  \n\nYou can find more information in the [release notes](https://daedalus.io).',
  action: {
    label: 'Download Daedalus at daedalus.io',
    url: 'https://daedalus.io',
  },
  ...commonUpdateData,
};

export const updateJP = {
  title: `Daedalus${availableAppVersion}アップデート`,
  content:
    'このリリースでは、**クールな新機能**と*いくつかのバグ修正*が行われています。よりシームレスなエクスペリエンスを提供します。[リリースノート](https://daedalus.io)で詳細を確認できます。',
  action: {
    label: 'daedalus.ioからダウンロードする',
    url: 'https://daedalus.io',
  },
  ...commonUpdateData,
};

export const update = {
  'en-US': updateEN,
  'ja-JP': updateJP,
};

export const getNewsUpdateItem = (
  read?: boolean,
  locale: string
): News.News => {
  const date = new Date().getTime();
  return new News.News({
    id: date,
    title: update[locale].title,
    content: update[locale].content,
    target: { daedalusVersion: version, platform: 'darwin' },
    action: {
      label: 'Visit daedalus.io',
      url: 'https://daedalus.io',
    },
    date,
    type: 'software-update',
    read: read || false,
  });
};

export const newsFeedApiItemUpdate = {
  title: {
    'en-US': updateEN.title,
    'ja-JP': updateJP.title,
  },
  content: {
    'en-US': updateEN.content,
    'ja-JP': updateJP.content,
  },
  target: {
    daedalusVersion: version,
    platforms: ['darwin', 'win32', 'linux'],
  },
  action: {
    label: {
      'en-US': updateEN.action.label,
      'ja-JP': updateJP.action.label,
    },
    url: {
      'en-US': updateEN.action.url,
      'ja-JP': updateJP.action.url,
    },
  },
  date: 1571901607418,
  type: 'software-update',
  softwareUpdate,
};

export const getNewsFeedApiItemUpdate = (targetVersion: string = version) => ({
  ...newsFeedApiItemUpdate,
  target: {
    daedalusVersion: targetVersion,
    platforms: ['darwin', 'win32', 'linux'],
  },
});
