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
    route: PropTypes.string.isRequired,
    menus: PropTypes.shape({
      wallets: PropTypes.shape({
        items: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.object).isRequired,
        actions: PropTypes.shape({
          onAddWallet: PropTypes.func,
          onWalletItemClick: PropTypes.func
        })
      })
    }).isRequired,
    onCategoryClicked: PropTypes.func,
    hidden: PropTypes.bool,
    isMaximized: PropTypes.bool,
    activeWalletId: PropTypes.string
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  matches(path: string) {
    return this.props.route.indexOf(path) !== -1;
  }

  render() {
    const { hidden, isMaximized, menus, onCategoryClicked, activeWalletId } = this.props;
    const { intl } = this.context;

    let sidebarStyle = null;
    let categoriesStyle = null;
    let subMenu = null;
    let hasMinimizedCategories = true;

    if (hidden) {
      sidebarStyle = styles.hidden;
    } else if (isMaximized) {
      categoriesStyle = styles.maximized;
      hasMinimizedCategories = false;
    } else if (this.matches('/wallets')) {
      subMenu = (
        <SidebarWalletsMenu
          visible={this.matches('/wallets')}
          wallets={menus.wallets.items}
          onAddWallet={menus.wallets.actions.onAddWallet}
          onWalletItemClick={menus.wallets.actions.onWalletItemClick}
          isActiveWallet={id => id === activeWalletId}
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
            active={this.matches('/wallets')}
            minimized={hasMinimizedCategories}
            onClick={() => onCategoryClicked('/wallets')}
          />
          <SidebarCategory
            className="redeem-ada"
            label={intl.formatMessage(messages.adaRedemptionCategoryLabel)}
            icon={adaRedemptionIcon}
            active={this.matches('/ada-redemption')}
            minimized={hasMinimizedCategories}
            onClick={() => onCategoryClicked('/ada-redemption')}
          />
        </div>
        {subMenu}
      </div>
    );
  }

}
