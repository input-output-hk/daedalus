// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SidebarSubMenu from '../SidebarMenu';
import styles from './SidebarWalletsMenu.scss';
import addWalletIcon from '../../../assets/images/sidebar/add-wallet-ic.svg';
import SidebarWalletMenuItem from './SidebarWalletMenuItem';
import type { SidebarWalletType } from '../../../stores/SidebarStore';

@observer
export default class SidebarWalletsMenu extends Component {

  props: {
    wallets: Array<SidebarWalletType>,
    isActiveWallet: Function,
    onAddWallet: Function,
    onWalletItemClick: Function,
    visible: boolean,
  };

  render() {
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
            />
          ))}
        </div>
        <button className={styles.addWalletButton} onClick={onAddWallet}>
          <img src={addWalletIcon} role="presentation" />
          <span>Add Wallet</span>
        </button>
      </SidebarSubMenu>
    );
  }

}
