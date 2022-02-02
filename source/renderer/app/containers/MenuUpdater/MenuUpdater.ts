import { inject, observer } from 'mobx-react';
import type { StatelessFunctionalComponent } from 'react';
import { rebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import type { MenuUpdaterProps } from './types';
import useMenuUpdater from './useMenuUpdater';

const MenuUpdater: StatelessFunctionalComponent<MenuUpdaterProps> = ({
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
