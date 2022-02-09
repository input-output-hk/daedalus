import type { RebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import type { StoresMap } from '../../stores';

export type StoresOfInterest = {
  app: StoresMap['app'];
  profile: StoresMap['profile'];
  router: StoresMap['router'];
  staking: StoresMap['staking'];
  uiDialogs: StoresMap['uiDialogs'];
};
export type UseMenuUpdaterArgs = {
  stores: StoresOfInterest;
  rebuildApplicationMenu: RebuildApplicationMenu;
};
export type MenuUpdaterProps = {
  stores: StoresMap;
};
