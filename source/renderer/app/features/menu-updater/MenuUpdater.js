// @flow
import { inject } from 'mobx-react';
import type { StatelessFunctionalComponent } from 'react';
import type { StoresMap } from '../../stores';
import { useFeature } from '../../utils/mobx-features/hooks';
import MenuUpdaterFeature from './feature';
import type { MenuUpdaterProps } from './types';

const MenuUpdater: StatelessFunctionalComponent<MenuUpdaterProps> = ({
  app,
  children,
  profile,
  router,
  staking,
  uiDialogs,
}) => {
  useFeature(
    new MenuUpdaterFeature({ app, profile, router, staking, uiDialogs })
  );
  return children;
};

export default inject(
  ({
    stores: { app, profile, router, staking, uiDialogs },
  }: {
    stores: StoresMap,
  }) => ({
    app,
    profile,
    router,
    staking,
    uiDialogs,
  })
)(MenuUpdater);
