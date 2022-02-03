import { inject, observer } from 'mobx-react';
import { FC } from 'react';
import { rebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import type { MenuUpdaterProps } from './types';
import useMenuUpdater from './useMenuUpdater';

const MenuUpdaterComponent: FC<MenuUpdaterProps> = ({
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

const MenuUpdater: FC = inject('stores')(observer(MenuUpdaterComponent));

export default MenuUpdater;
