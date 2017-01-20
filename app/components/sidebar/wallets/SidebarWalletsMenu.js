// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import SidebarSubMenu from '../SidebarMenu';
import styles from './SidebarWalletsMenu.scss';
import addWalletIcon from '../../../assets/images/sidebar/add-wallet-ic.svg';
import SidebarWalletMenuItem from './SidebarWalletMenuItem';

@observer
export default class SidebarWalletsMenu extends Component {

  static propTypes = {
    wallets: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      id: PropTypes.string.isRequired,
      title: PropTypes.string.isRequired,
      info: PropTypes.string.isRequired
    })).isRequired,
    isActiveWallet: PropTypes.func.isRequired,
    onAddWallet: PropTypes.func,
    onWalletItemClick: PropTypes.func,
    visible: PropTypes.bool
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
              isConnected={wallet.isConnected}
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
