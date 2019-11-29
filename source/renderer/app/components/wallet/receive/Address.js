// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import CopyToClipboard from 'react-copy-to-clipboard';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './Address.scss';
import iconQR from '../../../assets/images/qr-code.inline.svg';
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import WalletAddress from '../../../domains/WalletAddress';
import { ellipsis } from '../../../utils/strings';

type Props = {
  address: WalletAddress,
  onShareAddress: Function,
  onCopyAddress: Function,
  shareAddressLabel: string,
  copyAddressLabel: string,
  currentLocale: string,
  isIncentivizedTestnet: boolean,
  isShowingSubMenus: boolean,
  minCharsInit: number,
  minCharsEnd?: ?number,
};

@observer
export default class Address extends Component<Props> {
  get hasEllipsis() {
    const {
      isIncentivizedTestnet,
      isShowingSubMenus,
      currentLocale,
    } = this.props;
    return (
      isShowingSubMenus || (isIncentivizedTestnet && currentLocale === 'ja-JP')
    );
  }

  render() {
    const {
      address,
      onShareAddress,
      onCopyAddress,
      shareAddressLabel,
      copyAddressLabel,
      isIncentivizedTestnet,
      isShowingSubMenus,
      minCharsInit,
      minCharsEnd,
    } = this.props;
    const addressClasses = classnames([
      `receiveAddress-${address.id}`,
      styles.component,
      address.used ? styles.usedWalletAddress : null,
    ]);
    const addressIdClasses = classnames([
      styles.addressId,
      isIncentivizedTestnet ? styles.isIncentivizedTestnet : null,
      isShowingSubMenus ? styles.isShowingSubMenus : null,
    ]);
    return (
      <div className={addressClasses}>
        <div className={addressIdClasses} id={`address-${address.id}`}>
          {this.hasEllipsis
            ? ellipsis(address.id, minCharsInit, minCharsEnd)
            : address.id}
        </div>
        <div className={styles.addressActions}>
          {!isIncentivizedTestnet ? (
            <button
              className={styles.shareAddressButton}
              onClick={() => onShareAddress(address)}
            >
              <SVGInline svg={iconQR} className={styles.shareIcon} />
              <span className={styles.shareAddressLabel}>
                {shareAddressLabel}
              </span>
            </button>
          ) : (
            <CopyToClipboard
              text={address.id}
              onCopy={() => onCopyAddress(address.id)}
            >
              <span className={styles.copyAddress}>
                <SVGInline svg={iconCopy} className={styles.copyIcon} />
                <span className={styles.copyAddressLabel}>
                  {copyAddressLabel}
                </span>
              </span>
            </CopyToClipboard>
          )}
        </div>
      </div>
    );
  }
}
