// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import SidebarSubMenu from '../SidebarMenu';
import styles from './SidebarWalletsMenu.scss';
import addWalletIcon from '../../../assets/images/sidebar/add-wallet-ic.svg';
import SidebarMenuItem from '../SidebarMenuItem';

@observer
export default class SidebarWalletsMenu extends Component {

  static propTypes = {
    wallets: PropTypes.arrayOf(PropTypes.shape({
      id: PropTypes.string.isRequired,
      title: PropTypes.string.isRequired,
      info: PropTypes.string.isRequired
    })).isRequired,
    isActiveWallet: PropTypes.func.isRequired,
    onAddWallet: PropTypes.func,
    visible: PropTypes.bool
  };

  render() {
    const { wallets, onAddWallet, isActiveWallet } = this.props;
    return (
      <SidebarSubMenu visible={this.props.visible}>
        <div className={styles.wallets}>
          {wallets.map((wallet) => (
            <SidebarMenuItem
              title={wallet.title}
              info={wallet.info}
              active={isActiveWallet(wallet.id)}
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
