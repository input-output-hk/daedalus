// @flow
import { Feature } from '../../utils/mobx-features/feature';
import { rebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import { createReactions } from '../../utils/mobx-features/reaction';
import makeReactionCallback from './makeReactionCallback';
import type { StoresOfInterest } from './types';

class MenuUpdaterFeature extends Feature {
  constructor({ profile, router, uiDialogs }: StoresOfInterest) {
    super();
    const stores = { profile, router, uiDialogs };
    const reactions = createReactions([
      makeReactionCallback({ stores, rebuildApplicationMenu }),
    ]);
    this.registerReactions(reactions);
  }
}

export default MenuUpdaterFeature;
