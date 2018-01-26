// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { find, kebabCase } from 'lodash';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import WalletAddDialog from '../../components/wallet/WalletAddDialog';
import WalletSupportRequestDialog from '../../components/wallet/WalletSupportRequestDialog';
import supportIcon from '../../assets/images/sidebar/bug-report-ic.inline.svg';
import type { SidebarWalletType } from '../../stores/SidebarStore';

type Props = {
  menus: {
    wallets: {
      items: Array<SidebarWalletType>,
      activeWalletId: ?string,
      actions: {
        onWalletItemClick: Function,
      }
    }
  },
  categories: Array<{
    name: string,
    route: string,
    icon: string,
  }>,
  activeSidebarCategory: string,
  onCategoryClicked: Function,
  isShowingSubMenus: boolean,
  openDialogAction: Function,
  isDialogOpen: Function,
};

@observer
export default class Sidebar extends Component<Props> {

  static defaultProps = {
    isShowingSubMenus: false,
  };

  render() {
    const {
      menus, categories, activeSidebarCategory,
      isShowingSubMenus, onCategoryClicked,
      openDialogAction, isDialogOpen,
    } = this.props;
    let subMenu = null;

    const walletsCategory = find(categories, { name: 'WALLETS' }).route;
    if (menus && activeSidebarCategory === walletsCategory) {
      subMenu = (
        <SidebarWalletsMenu
          wallets={menus.wallets.items}
          onAddWallet={() => openDialogAction({
            dialog: WalletAddDialog,
          })}
          onWalletItemClick={menus.wallets.actions.onWalletItemClick}
          isActiveWallet={id => id === menus.wallets.activeWalletId}
          visible={isShowingSubMenus}
        />
      );
    }

    const sidebarStyles = classNames([
      styles.component,
      !isShowingSubMenus || subMenu == null ? styles.minimized : null
    ]);

    return (
      <div className={sidebarStyles}>
        <div className={styles.minimized}>
          {categories.map((category, index) => {
            const categoryClassName = kebabCase(category.name);
            return (
              <SidebarCategory
                key={index}
                className={categoryClassName}
                icon={category.icon}
                active={activeSidebarCategory === category.route}
                onClick={() => onCategoryClicked(category.route)}
              />
            );
          })}

          <SidebarCategory
            className="supportRequest"
            icon={supportIcon}
            active={isDialogOpen(WalletSupportRequestDialog)}
            onClick={this.handleSupportRequestClick}
          />

        </div>
        {subMenu}
      </div>
    );
  }

  handleSupportRequestClick = () => {
    this.props.openDialogAction({
      dialog: WalletSupportRequestDialog
    });
  }

}
