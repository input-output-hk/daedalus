// @flow
import { RouterStore } from 'mobx-react-router';
import type { Node } from 'react';
import type { RebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import ProfileStore from '../../stores/ProfileStore';
import UiDialogsStore from '../../stores/UiDialogsStore';

export type StoresOfInterest = {
  profile: ProfileStore,
  router: RouterStore,
  uiDialogs: UiDialogsStore,
};

export type MakeReactionCallbackArgs = {
  stores: StoresOfInterest,
  rebuildApplicationMenu: RebuildApplicationMenu,
};

export type MenuUpdaterProps = StoresOfInterest & {
  children: Node,
};
