import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import CopyToClipboard from 'react-copy-to-clipboard';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AddressActions.scss' or its ... Remove this comment to see the full error message
import styles from './AddressActions.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/qr-code... Remove this comment to see the full error message
import iconQR from '../../../assets/images/qr-code.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboa... Remove this comment to see the full error message
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import WalletAddress from '../../../domains/WalletAddress';

const messages = defineMessages({
  instructionsTitle: {
    id: 'wallet.receive.page.instructions.instructionsTitle',
    defaultMessage: '!!!Available wallet addresses',
    description: 'Instructions Title on the wallet "Receive page"',
  },
  instructionsDescription: {
    id: 'wallet.receive.page.instructions.instructionsDescription',
    defaultMessage:
      '!!!Share this wallet address to receive payments. To protect your privacy, new addresses are generated automatically once you use them.',
    description: 'Instructions Description on the wallet "Receive page"',
  },
  addressesTitle: {
    id: 'wallet.receive.page.addresses.addressesTitle',
    defaultMessage: '!!!Addresses',
    description: 'Addresses Title on the wallet "Receive page"',
  },
  showUsedLabel: {
    id: 'wallet.receive.page.showUsedLabel',
    defaultMessage: '!!!Show used',
    description:
      'Label for "show used" wallet addresses link on the wallet "Receive page"',
  },
  shareAddressLabel: {
    id: 'wallet.receive.page.shareAddressLabel',
    defaultMessage: '!!!Share',
    description: 'Label for "Share" link on the wallet "Receive page"',
  },
  copyAddressLabel: {
    id: 'wallet.receive.page.copyAddressLabel',
    defaultMessage: '!!!Copy address',
    description: 'Label for "Copy address" link on the wallet "Receive page"',
  },
});
type Props = {
  address: WalletAddress;
  onShareAddress: (...args: Array<any>) => any;
  onCopyAddress: (...args: Array<any>) => any;
  type?: 'share' | 'copy';
};

@observer
class AddressActions extends Component<Props> {
  static defaultProps = {
    type: 'copy',
  };
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  addressElement: HTMLElement | null | undefined;
  addressContainerElement: HTMLElement | null | undefined;

  render() {
    const { intl } = this.context;
    const {
      address,
      onShareAddress,
      onCopyAddress,
      type = 'copy',
    } = this.props;
    const { id: addressId, used: isUsed } = address;
    const componentClasses = classnames(styles[`${type}Actions`], {
      [styles.isUsed]: isUsed,
    });
    return (
      <div className={componentClasses}>
        {type === 'copy' ? (
          <CopyToClipboard
            text={addressId}
            onCopy={() => onCopyAddress(addressId)}
          >
            <span className={styles.copyAddress}>
              <SVGInline svg={iconCopy} className={styles.copyIcon} />
              <span className={styles.copyAddressLabel}>
                {intl.formatMessage(messages.copyAddressLabel)}
              </span>
            </span>
          </CopyToClipboard>
        ) : (
          <button
            className={styles.shareAddressButton}
            onClick={() => onShareAddress(address)}
          >
            <SVGInline svg={iconQR} className={styles.shareIcon} />
            <span className={styles.shareAddressLabel}>
              {intl.formatMessage(messages.shareAddressLabel)}
            </span>
          </button>
        )}
      </div>
    );
  }
}

export default AddressActions;
