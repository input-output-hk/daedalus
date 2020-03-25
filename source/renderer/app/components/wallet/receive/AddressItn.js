// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import CopyToClipboard from 'react-copy-to-clipboard';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './AddressItn.scss';
import iconQR from '../../../assets/images/qr-code.inline.svg';
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import WalletAddress from '../../../domains/WalletAddress';

type Props = {
  address: WalletAddress,
  onShareAddress: Function,
  onCopyAddress: Function,
  shareAddressLabel: string,
  copyAddressLabel: string,
  isIncentivizedTestnet: boolean,
  shouldRegisterAddressElement: boolean,
  onRegisterHTMLElements: Function,
  addressSlice: number,
};

@observer
export default class AddressItn extends Component<Props> {
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
      shareAddressLabel,
      copyAddressLabel,
      isIncentivizedTestnet,
      shouldRegisterAddressElement,
    } = this.props;
    const { renderAddress, rawAddress } = this;
    const addressClasses = classnames([
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
              text={rawAddress}
              onCopy={() => onCopyAddress(rawAddress)}
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
