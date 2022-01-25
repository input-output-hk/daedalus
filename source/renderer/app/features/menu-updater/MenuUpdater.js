// @flow
import { inject } from 'mobx-react';
import type { StatelessFunctionalComponent } from 'react';
import { useFeature } from '../../utils/mobx-features/hooks';
import MenuUpdaterFeature from './feature';
import type { MenuUpdaterProps } from './types';

const MenuUpdater: StatelessFunctionalComponent<MenuUpdaterProps> = ({
  children,
  profile,
  router,
  uiDialogs,
}) => {
  useFeature(new MenuUpdaterFeature({ profile, router, uiDialogs }));
  return children;
};

export default inject(({ stores: { profile, router, uiDialogs } }) => ({
  profile,
  router,
  uiDialogs,
}))(MenuUpdater);
