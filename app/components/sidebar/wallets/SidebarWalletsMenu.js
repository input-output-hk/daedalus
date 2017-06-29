// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SidebarSubMenu from '../SidebarMenu';
import styles from './SidebarWalletsMenu.scss';
import addWalletIcon from '../../../assets/images/sidebar/add-wallet-ic.svg';
import SidebarWalletMenuItem from './SidebarWalletMenuItem';
import type { SidebarWalletType } from '../../../stores/SidebarStore';

const messages = defineMessages({
  addAdaWallet: {
    id: 'sidebar.wallets.addWallet',
    defaultMessage: '!!!Add wallet',
    description: 'Label for the "Add wallet" button in wallet sidebar menu.',
  },
});

@observer
export default class SidebarWalletsMenu extends Component {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  props: {
    wallets: Array<SidebarWalletType>,
    isActiveWallet: Function,
    onAddWallet: Function,
    onWalletItemClick: Function,
    visible: boolean,
  };

  render() {
    const { intl } = this.context;
    const { wallets, onAddWallet, isActiveWallet, onWalletItemClick } = this.props;
    return (
      <SidebarSubMenu visible={this.props.visible}>
        <div className={styles.wallets}>
          {wallets.map((wallet) => (
            <SidebarWalletMenuItem
              title={wallet.title}
              info={wallet.info}
              active={isActiveWallet(wallet.id)}
              onClick={() => onWalletItemClick(wallet.id)}
              key={wallet.id}
              className={`Wallet_${wallet.id}`}
            />
          ))}
        </div>
        <button className={styles.addWalletButton} onClick={onAddWallet}>
          <img src={addWalletIcon} role="presentation" />
          <span>{intl.formatMessage(messages.addAdaWallet)}</span>
        </button>
      </SidebarSubMenu>
    );
  }

}
