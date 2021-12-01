// @flow
import React from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import { shouldShowWalletSubMenu } from './helpers';
import SidebarCategory from './SidebarCategory';
import SidebarCategoryWrapper from './SidebarCategoryWrapper';
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

const getCategoryContent = (categoryName: string, network) =>
  categoryName === 'NETWORK_INFO' ? (
    <SidebarCategoryNetworkInfo network={network} />
  ) : null;

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
            <SidebarCategoryWrapper
              key={category.name}
              categoryName={category.name}
            >
              <SidebarCategory
                category={category}
                isActive={isActive}
                onClick={onActivateCategory}
                content={content}
              />
            </SidebarCategoryWrapper>
          );
        })}
      </div>
      {hasSubMenu && (
        <SidebarWalletsMenu
          wallets={menus?.wallets?.items || []}
          onAddWallet={onAddWallet}
          onWalletItemClick={menus?.wallets?.actions?.onWalletItemClick}
          isActiveWallet={(id) => id === menus?.wallets?.activeWalletId}
          isAddWalletButtonActive={pathname === ROUTES.WALLETS.ADD}
          isShelleyActivated={isShelleyActivated}
          visible={isShowingSubMenus}
          onWalletSortBy={menus?.wallets?.actions?.onWalletSortBy}
          sortBy={menus?.wallets?.walletSortConfig?.sortBy}
          sortOrder={menus?.wallets?.walletSortConfig?.sortOrder}
          searchValue={menus?.wallets?.searchValue}
          onSearch={menus?.wallets?.actions?.onSearch}
        />
      )}
    </div>
  );
};

export default observer(Sidebar);
