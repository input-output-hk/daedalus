// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import AddressActions from './AddressActions';
import styles from './AddressSequential.scss';
import WalletAddress from '../../../domains/WalletAddress';

type Props = {
  address: WalletAddress,
  onShareAddress: Function,
  onCopyAddress: Function,
  shouldRegisterAddressElement: boolean,
  onRegisterHTMLElements: Function,
  addressSlice: number,
};

@observer
export default class AddressSequential extends Component<Props> {
  addressElement: ?HTMLElement;
  addressContainerElement: ?HTMLElement;

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
    const addressIdClasses = classnames([styles.addressId]);
    return (
      <div className={addressClasses}>
        <div
          className={addressIdClasses}
          ref={ref => {
            this.addressContainerElement = ref;
          }}
          id={`address-${rawAddress}`}
        >
          {renderAddress}
          {shouldRegisterAddressElement && (
            <span
              ref={ref => {
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
        />
      </div>
    );
  }
}
