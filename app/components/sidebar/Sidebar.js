// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { find, kebabCase } from 'lodash';
import SvgInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import WalletAddDialog from '../../components/wallet/WalletAddDialog';
import WalletSupportRequestDialog from '../../components/wallet/WalletSupportRequestDialog';
import supportIcon from '../../assets/images/sidebar/bug-report-ic.inline.svg';
import supportIconActive from '../../assets/images/sidebar/bug-report-active-ic.inline.svg';
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

type State = {
  isSupportRequestHovered: boolean,
};

@observer
export default class Sidebar extends Component<Props, State> {

  static defaultProps = {
    isShowingSubMenus: false,
  };

  state = {
    isSupportRequestHovered: false,
  };

  render() {
    const {
      menus, categories, activeSidebarCategory,
      isShowingSubMenus, onCategoryClicked,
      openDialogAction, isDialogOpen
    } = this.props;
    const { isSupportRequestHovered } = this.state;
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

    const supportRequsetIcon = (
      isSupportRequestHovered || isDialogOpen(WalletSupportRequestDialog)
    ) ? supportIconActive : supportIcon;

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
          <button
            className={styles.settingsSupport}
            onClick={this.handleSupportRequestClick}
            onMouseEnter={this.handleSupportRequestHover}
            onMouseLeave={this.handleSupportRequestHover}
          >
            <SvgInline svg={supportRequsetIcon} className={styles.icon} />
          </button>
        </div>

        {subMenu}
      </div>
    );
  }

  handleSupportRequestHover = () => {
    this.setState({ isSupportRequestHovered: !this.state.isSupportRequestHovered });
  }

  handleSupportRequestClick = () => {
    this.props.openDialogAction({
      dialog: WalletSupportRequestDialog
    });
  }

}
