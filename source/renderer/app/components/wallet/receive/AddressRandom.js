// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import AddressActions from './AddressActions';
import styles from './AddressRandom.scss';
import WalletAddress from '../../../domains/WalletAddress';

type Props = {
  address: WalletAddress,
  index: number,
  onCopyAddress: Function,
  onShareAddress: Function,
};

@observer
export default class AddressRandom extends Component<Props> {
  render() {
    const { address, onCopyAddress, onShareAddress, index } = this.props;
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
        <AddressActions
          address={address}
          onShareAddress={onShareAddress}
          onCopyAddress={onCopyAddress}
        />
      </div>
    );
  }
}
