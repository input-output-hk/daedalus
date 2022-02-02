// @flow
import type { RebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import type { StoresMap } from '../../stores';

export type StoresOfInterest = {
  app: $PropertyType<StoresMap, 'app'>,
  profile: $PropertyType<StoresMap, 'profile'>,
  router: $PropertyType<StoresMap, 'router'>,
  staking: $PropertyType<StoresMap, 'staking'>,
  uiDialogs: $PropertyType<StoresMap, 'uiDialogs'>,
};

export type UseMenuUpdaterArgs = {
  stores: StoresOfInterest,
  rebuildApplicationMenu: RebuildApplicationMenu,
};

export type MenuUpdaterProps = {
  stores: StoresMap,
};
