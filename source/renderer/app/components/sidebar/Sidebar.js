// @flow
import React from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import { shouldShowWalletSubMenu } from './helpers';
import SidebarCategory from './SidebarCategory';
import SidebarCategoryNetworkInfo from './SidebarCategoryNetworkInfo';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import { CATEGORIES_BY_NAME } from '../../config/sidebarConfig';
import { ROUTES } from '../../routes-config';
import type { SidebarMenus } from './types';
import type { networkType } from '../../types/networkTypes';
import type { SidebarCategoryInfo } from '../../config/sidebarConfig';

type Props = {
  menus: SidebarMenus,
  categories: Array<SidebarCategoryInfo>,
  activeSidebarCategory: string,
  isShowingSubMenus: boolean,
  pathname: string,
  network: networkType,
  onActivateCategory: Function,
  onAddWallet: Function,
  isShelleyActivated: boolean,
};

const getCategoryContent = (categoryName: string, network) => {
  if (categoryName === 'NETWORK_INFO') {
    return <SidebarCategoryNetworkInfo network={network} />;
  }
  return null;
};

const Sidebar = ({
  menus,
  categories,
  activeSidebarCategory,
  pathname,
  isShowingSubMenus = false,
  onAddWallet,
  isShelleyActivated,
  onActivateCategory,
  network,
}: Props) => {
  const hasSubMenu = shouldShowWalletSubMenu({
    activeSidebarCategory,
    walletRoute: CATEGORIES_BY_NAME.WALLETS.route,
    menus,
  });
  const isMinimized = !isShowingSubMenus || !hasSubMenu;
  const sidebarStyles = classNames(
    styles.component,
    isMinimized && styles.minimized
  );

  return (
    <div className={sidebarStyles}>
      <div className={styles.minimized}>
        {categories.map((category: SidebarCategoryInfo) => {
          const content = getCategoryContent(category.name, network);
          const isActive = activeSidebarCategory === category.route;

          return (
            <SidebarCategory
              category={category}
              isActive={isActive}
              onClick={onActivateCategory}
              content={content}
            />
          );
        })}
      </div>
      {hasSubMenu && (
        <SidebarWalletsMenu
          wallets={menus?.wallets?.items || []}
          onAddWallet={onAddWallet}
          onWalletItemClick={menus?.wallets?.actions?.onWalletItemClick || null}
          isActiveWallet={(id) =>
            id === (menus?.wallets?.activeWalletId || null)
          }
          isAddWalletButtonActive={pathname === ROUTES.WALLETS.ADD}
          isShelleyActivated={isShelleyActivated}
          visible={isShowingSubMenus}
        />
      )}
    </div>
  );
};

export default observer(Sidebar);
