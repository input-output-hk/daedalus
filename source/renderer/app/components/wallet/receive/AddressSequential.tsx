import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import AddressActions from './AddressActions';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AddressSequential.scss' or i... Remove this comment to see the full error message
import styles from './AddressSequential.scss';
import WalletAddress from '../../../domains/WalletAddress';

type Props = {
  address: WalletAddress;
  onShareAddress: (...args: Array<any>) => any;
  onCopyAddress: (...args: Array<any>) => any;
  shouldRegisterAddressElement: boolean;
  onRegisterHTMLElements: (...args: Array<any>) => any;
  addressSlice: number;
};

@observer
class AddressSequential extends Component<Props> {
  addressElement: HTMLElement | null | undefined;
  addressContainerElement: HTMLElement | null | undefined;

  componentDidMount() {
    if (this.props.shouldRegisterAddressElement) {
      this.props.onRegisterHTMLElements(
        this.addressElement,
        this.addressContainerElement
      );
    }
  }

  get rawAddress() {
    return this.props.address.id;
  }

  get renderAddress() {
    const { rawAddress } = this;
    const { addressSlice } = this.props;
    if (!addressSlice) return rawAddress;
    const addressBegin = rawAddress.slice(0, addressSlice);
    const addressEnd = rawAddress.slice(-addressSlice);
    return `${addressBegin}…${addressEnd}`;
  }

  render() {
    const {
      address,
      onShareAddress,
      onCopyAddress,
      shouldRegisterAddressElement,
    } = this.props;
    const { renderAddress, rawAddress } = this;
    const addressClasses = classnames([
      'Address',
      `receiveAddress-${rawAddress}`,
      styles.component,
      address.used ? styles.usedWalletAddress : null,
    ]);
    return (
      <div
        className={addressClasses}
        onClick={() => onShareAddress(address)}
        role="link"
        aria-hidden
      >
        <div
          className={styles.addressId}
          ref={(ref) => {
            this.addressContainerElement = ref;
          }}
          id={`address-${rawAddress}`}
        >
          {renderAddress}
          {shouldRegisterAddressElement && (
            <span
              ref={(ref) => {
                this.addressElement = ref;
              }}
              className={styles.addressElement}
            >
              {rawAddress}
            </span>
          )}
        </div>
        <AddressActions
          address={address}
          onShareAddress={onShareAddress}
          onCopyAddress={onCopyAddress}
          type="share"
        />
      </div>
    );
  }
}

export default AddressSequential;
