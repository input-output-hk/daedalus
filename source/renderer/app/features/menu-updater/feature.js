// @flow
import { Feature } from '../../utils/mobx-features/feature';
import { rebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import { createReactions } from '../../utils/mobx-features/reaction';
import makeReactionCallback from './makeReactionCallback';
import type { StoresOfInterest } from './types';

class MenuUpdaterFeature extends Feature {
  constructor(stores: StoresOfInterest) {
    super();
    const reactions = createReactions([
      makeReactionCallback({ stores, rebuildApplicationMenu }),
    ]);
    this.registerReactions(reactions);
  }
}

export default MenuUpdaterFeature;
