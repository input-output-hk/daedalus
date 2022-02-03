import { inject, observer } from 'mobx-react';
import { FC } from 'react';
import { rebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import type { MenuUpdaterProps } from './types';
import useMenuUpdater from './useMenuUpdater';

const MenuUpdater: FC<MenuUpdaterProps> = ({
  stores: { app, profile, router, staking, uiDialogs },
}) => {
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
};

export default inject('stores')(observer(MenuUpdater));
