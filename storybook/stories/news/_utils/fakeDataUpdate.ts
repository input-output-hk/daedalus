import inc from 'semver/functions/inc';
import { version as currentVersion } from '../../../../package.json';
import News from '../../../../source/renderer/app/domains/News';
import type { NewsItem } from '../../../../source/renderer/app/api/news/types';

export const version = currentVersion;
export const availableAppVersion = inc(version, 'minor');
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
export const getNewsUpdateItem = (
  read?: boolean,
  // @ts-ignore ts-migrate(1016) FIXME: A required parameter cannot follow an optional par... Remove this comment to see the full error message
  locale: string
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
): News.News => {
  const date = new Date().getTime();
  return new News.News({
    id: date,
    title: update[locale].title,
    content: update[locale].content,
    target: {
      daedalusVersion: version,
      platform: 'darwin',
    },
    action: {
      label: 'Visit daedalus.io',
      url: 'https://daedalus.io',
    },
    date,
    type: 'software-update',
    read: read || false,
  });
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
  target: {
    daedalusVersion: version,
    platform: 'linux',
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
};
