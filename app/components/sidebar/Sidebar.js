// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classNames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import walletsIcon from '../../assets/images/sidebar/wallet-ic.svg';
import adaRedemptionIcon from '../../assets/images/sidebar/ada.svg';

const messages = defineMessages({
  walletsCategoryLabel: {
    id: 'Sidebar.categories.wallets',
    defaultMessage: '!!!Wallets',
    description: 'Category label for wallets'
  },
  settingsCategoryLabel: {
    id: 'Sidebar.categories.settings',
    defaultMessage: '!!!Settings',
    description: 'Category label for settings'
  },
  adaRedemptionCategoryLabel: {
    id: 'Sidebar.categories.adaRedemption',
    defaultMessage: '!!!ADA Redemption',
    description: 'Category label for ada redemption'
  },
});

@observer
export default class Sidebar extends Component {

  static propTypes = {
    menus: PropTypes.shape({
      wallets: PropTypes.shape({
        items: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.object).isRequired,
        actions: PropTypes.shape({
          onAddWallet: PropTypes.func,
          onWalletItemClick: PropTypes.func
        })
      })
    }).isRequired,
    categories: PropTypes.shape({
      WALLETS: PropTypes.string.isRequired,
      ADA_REDEMPTION: PropTypes.string.isRequired,
    }).isRequired,
    currentCategory: PropTypes.string.isRequired,
    onCategoryClicked: PropTypes.func,
    hidden: PropTypes.bool,
    isMaximized: PropTypes.bool,
    isSynced: PropTypes.bool,
    activeWalletId: PropTypes.string,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      hidden, isMaximized, menus, onCategoryClicked, activeWalletId, categories, currentCategory, isSynced
    } = this.props;

    let sidebarStyle = null;
    let categoriesStyle = null;
    let subMenu = null;
    let hasMinimizedCategories = true;

    if (hidden) {
      sidebarStyle = styles.hidden;
    } else if (isMaximized) {
      categoriesStyle = styles.maximized;
      hasMinimizedCategories = false;
    } else if (currentCategory === categories.WALLETS) {
      subMenu = (
        <SidebarWalletsMenu
          wallets={menus.wallets.items}
          onAddWallet={menus.wallets.actions.onAddWallet}
          onWalletItemClick={menus.wallets.actions.onWalletItemClick}
          isActiveWallet={id => id === activeWalletId}
          visible
          isSynced={isSynced}
        />
      );
      categoriesStyle = styles.minimized;
    } else {
      sidebarStyle = styles.minimized;
    }

    const sidebarStyles = classNames([styles.component, sidebarStyle]);

    return (
      <div className={sidebarStyles}>
        <div className={categoriesStyle}>
          <SidebarCategory
            className="wallets"
            label={intl.formatMessage(messages.walletsCategoryLabel)}
            icon={walletsIcon}
            active={currentCategory === categories.WALLETS}
            minimized={hasMinimizedCategories}
            onClick={() => onCategoryClicked(categories.WALLETS)}
          />
          <SidebarCategory
            className="ada-redemption"
            label={intl.formatMessage(messages.adaRedemptionCategoryLabel)}
            icon={adaRedemptionIcon}
            active={currentCategory === categories.ADA_REDEMPTION}
            minimized={hasMinimizedCategories}
            onClick={() => onCategoryClicked(categories.ADA_REDEMPTION)}
          />
        </div>
        {subMenu}
      </div>
    );
  }

}
