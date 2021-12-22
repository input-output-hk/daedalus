import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import AddressActions from './AddressActions';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AddressRandom.scss' or its c... Remove this comment to see the full error message
import styles from './AddressRandom.scss';
import WalletAddress from '../../../domains/WalletAddress';

type Props = {
  address: WalletAddress;
  index: number;
  onCopyAddress: (...args: Array<any>) => any;
  onShareAddress: (...args: Array<any>) => any;
};

@observer
class AddressRandom extends Component<Props> {
  render() {
    const { address, onCopyAddress, onShareAddress, index } = this.props;
    const addressClasses = classnames([
      'Address',
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

export default AddressRandom;
