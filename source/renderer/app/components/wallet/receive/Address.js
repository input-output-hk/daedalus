// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import CopyToClipboard from 'react-copy-to-clipboard';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './Address.scss';
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import WalletAddress from '../../../domains/WalletAddress';

type Props = {
  address: WalletAddress,
  onCopyAddress: Function,
  copyAddressLabel: Function,
  index: number,
};

@observer
export class Address extends Component<Props> {
  render() {
    const { address, onCopyAddress, copyAddressLabel, index } = this.props;
    const addressClasses = classnames([
      `generatedAddress-${index + 1}`,
      styles.component,
      address.used ? styles.usedWalletAddress : null,
    ]);
    return (
      <div className={addressClasses}>
        <div className={styles.addressId} id={`address-${address.id}`}>
          {address.id}
        </div>
        <div className={styles.addressActions}>
          <CopyToClipboard
            text={address.id}
            // eslint-disable-next-line react/jsx-no-bind
            onCopy={onCopyAddress.bind(this, address.id)}
          >
            <span className={styles.copyAddress}>
              <SVGInline svg={iconCopy} className={styles.copyIcon} />
              <span className={styles.copyAddressLabel}>
                {copyAddressLabel}
              </span>
            </span>
          </CopyToClipboard>
        </div>
      </div>
    );
  }
}
