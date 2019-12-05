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

type Props = {
  address: WalletAddress,
  onShareAddress: Function,
  onCopyAddress: Function,
  shareAddressLabel: string,
  copyAddressLabel: string,
  isIncentivizedTestnet: boolean,
  shouldRegisterAddressElement: boolean,
  onRegisterHTMLElements: Function,
  ellipsisIsVisible: boolean,
};

@observer
export default class Address extends Component<Props> {
  componentDidMount() {
    if (this.props.shouldRegisterAddressElement) {
      this.props.onRegisterHTMLElements(
        this.addressElement,
        this.addressContainerElement
      );
    }
  }
  get address() {
    return this.props.address.id;
  }
  get addressBegin() {
    return this.address.substring(0, parseInt(this.address.length / 2, 10));
  }
  get addressEnd() {
    return this.address.substring(
      parseInt(this.address.length / 2, 10),
      this.address.length
    );
  }

  addressElement: ?HTMLElement;
  addressContainerElement: ?HTMLElement;

  render() {
    const {
      address,
      onShareAddress,
      onCopyAddress,
      shareAddressLabel,
      copyAddressLabel,
      isIncentivizedTestnet,
      shouldRegisterAddressElement,
      ellipsisIsVisible,
    } = this.props;
    const addressClasses = classnames([
      `receiveAddress-${address.id}`,
      styles.component,
      address.used ? styles.usedWalletAddress : null,
    ]);
    const addressIdClasses = classnames([
      styles.addressId,
      ellipsisIsVisible ? styles.ellipsisIsVisible : null,
    ]);
    return (
      <div className={addressClasses}>
        <div
          className={addressIdClasses}
          ref={ref => {
            this.addressContainerElement = ref;
          }}
          id={`address-${address.id}`}
        >
          <span className={styles.addressIdBegin}>{this.addressBegin}</span>
          <span className={styles.addressIdEnd}>{this.addressEnd}</span>
          {ellipsisIsVisible && <span className={styles.ellipsis}>…</span>}
          {shouldRegisterAddressElement && (
            <span
              ref={ref => {
                this.addressElement = ref;
              }}
              className={styles.addressElement}
            >
              {this.address}
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
