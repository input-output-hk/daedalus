// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import { Scrollbars } from 'react-custom-scrollbars';
import { map } from 'lodash';
import SidebarSubMenu from '../SidebarMenu';
import styles from './SidebarWalletsMenu.scss';
import addWalletIcon from '../../../assets/images/sidebar/add-wallet-ic.inline.svg';
import SidebarWalletMenuItem from './SidebarWalletMenuItem';
import type { SidebarWalletType } from '../../../types/sidebarTypes';

const messages = defineMessages({
  addAdaWallet: {
    id: 'sidebar.wallets.addWallet',
    defaultMessage: '!!!Add wallet',
    description: 'Label for the "Add wallet" button in wallet sidebar menu.',
  },
});

type Props = {
  wallets: Array<SidebarWalletType>,
  isActiveWallet: Function,
  onAddWallet: Function,
  onWalletItemClick: Function,
  visible: boolean,
  isAddWalletButtonActive: boolean,
  isShelleyActivated: boolean,
};

@observer
export default class SidebarWalletsMenu extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  renderThumb = (props: any) => (
    <div {...props} className={styles.scrollbarThumb} />
  );

  render() {
    const { intl } = this.context;
    const {
      wallets,
      onAddWallet,
      isActiveWallet,
      onWalletItemClick,
      isAddWalletButtonActive,
      isShelleyActivated,
    } = this.props;

    const addWalletButtonStyles = classNames([
      styles.addWalletButton,
      isAddWalletButtonActive ? styles.active : null,
    ]);

    return (
      <SidebarSubMenu visible={this.props.visible}>
        <div className={styles.wallets}>
          <Scrollbars
            renderThumbHorizontal={() => <div className={styles.hideThumb} />}
            renderThumbVertical={this.renderThumb}
            hideTracksWhenNotNeeded
          >
            {map(wallets, (wallet) => (
              <SidebarWalletMenuItem
                title={wallet.title}
                info={wallet.info}
                active={isActiveWallet(wallet.id)}
                onClick={() => onWalletItemClick(wallet.id)}
                key={wallet.id}
                className={`Wallet_${wallet.id}`}
                isRestoreActive={wallet.isRestoreActive}
                isShelleyActivated={isShelleyActivated}
                restoreProgress={wallet.restoreProgress}
                isNotResponding={wallet.isNotResponding}
                isLegacy={wallet.isLegacy}
                isHardwareWallet={wallet.isHardwareWallet}
                hasNotification={wallet.hasNotification}
                isHardwareWalletDisconnected={
                  wallet.isHardwareWalletDisconnected
                }
              />
            ))}
          </Scrollbars>
        </div>
        <button className={addWalletButtonStyles} onClick={onAddWallet}>
          <SVGInline svg={addWalletIcon} className={styles.icon} />
          <span>{intl.formatMessage(messages.addAdaWallet)}</span>
        </button>
      </SidebarSubMenu>
    );
  }
}
