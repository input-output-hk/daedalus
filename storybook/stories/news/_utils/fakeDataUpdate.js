// @flow
import inc from 'semver/functions/inc';
import { version as currentVersion } from '../../../../package.json';
export const version = currentVersion;
export const availableAppVersion = inc(version, 'minor');
import type { NewsItem } from '../../../../source/renderer/app/api/news/types';

const commonUpdateData = {
  target: {
    daedalusVersion: version,
    platforms: ['win32', 'linux', 'darwin'],
  },
  date: 1594043606135,
  softwareUpdate: {
    darwin: {
      version: availableAppVersion,
      hash: '97d336d45b022b0390446497dbe8b43bb6174436df12d43c4fc2b953ce22b703',
      url:
        'https://update-cardano-mainnet.iohk.io/daedalus-2.0.0-mainnet-13980.pkg',
    },
    win32: {
      version: availableAppVersion,
      hash: '97d336d45b022b0390446497dbe8b43bb6174436df12d43c4fc2b953ce22b703',
      url:
        'https://update-cardano-mainnet.iohk.io/daedalus-2.0.0-mainnet-13980.exe',
    },
    linux: {
      version: availableAppVersion,
      hash: '97d336d45b022b0390446497dbe8b43bb6174436df12d43c4fc2b953ce22b703',
      url:
        'https://update-cardano-mainnet.iohk.io/daedalus-2.0.0-mainnet-13980.bin',
    },
  },
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

export const newsFeedApiItemUpdate: NewsItem = {
  title: {
    'en-US': updateEN.title,
    'ja-JP': updateJP.title,
  },
  content: {
    'en-US': updateEN.content,
    'ja-JP': updateJP.content,
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
  date: new Date().getTime(),
  target: {
    daedalusVersion: version,
    platform: 'darwin',
  },
  type: 'software-update',
};

export default {
  version,
};
