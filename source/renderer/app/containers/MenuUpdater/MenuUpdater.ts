import { observer } from 'mobx-react';
import { rebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import type { MenuUpdaterProps } from './types';
import useMenuUpdater from './useMenuUpdater';

function MenuUpdaterComponent({
  stores: { app, profile, router, staking, uiDialogs },
}: MenuUpdaterProps) {
  useMenuUpdater({
    stores: {
      app,
      profile,
      router,
      staking,
      uiDialogs,
    },
    rebuildApplicationMenu,
  });
  return null;
}

export default observer(MenuUpdaterComponent);
