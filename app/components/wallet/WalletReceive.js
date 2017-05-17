// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import CopyToClipboard from 'react-copy-to-clipboard';
import QRCode from 'qrcode.react';
import Button from 'react-toolbox/lib/button/Button';
import BorderedBox from '../widgets/BorderedBox';
import iconCopy from '../../assets/images/clipboard-ic.svg';
import iconProtected from '../../assets/images/protected-off.svg';
import styles from './WalletReceive.scss';

const messages = defineMessages({
  walletAddressLabel: {
    id: 'wallet.receive.page.walletAddressLabel',
    defaultMessage: '!!!Your wallet address',
    description: 'Label for wallet address on the wallet "Receive page"',
  },
  walletReceiveInstructions: {
    id: 'wallet.receive.page.walletReceiveInstructions',
    defaultMessage: '!!!Share this wallet address to receive payments. To protect your privacy, new addresses are generated automatically once you use them.',
    description: 'Wallet receive payments instructions on the wallet "Receive page"',
  },
  generateNewAddressButtonLabel: {
    id: 'wallet.receive.page.generateNewAddressButtonLabel',
    defaultMessage: '!!!Generate new address',
    description: 'Label for "Generate new address" button on the wallet "Receive page"',
  },
  generatedAddressesSectionTitle: {
    id: 'wallet.receive.page.generatedAddressesSectionTitle',
    defaultMessage: '!!!Generated addresses',
    description: '"Generated addresses" section title on the wallet "Receive page"',
  },
  hideUsedLabel: {
    id: 'wallet.receive.page.hideUsedLabel',
    defaultMessage: '!!!hide used',
    description: 'Label for "hide used" wallet addresses link on the wallet "Receive page"',
  },
  showUsedLabel: {
    id: 'wallet.receive.page.showUsedLabel',
    defaultMessage: '!!!show used',
    description: 'Label for "show used" wallet addresses link on the wallet "Receive page"',
  },
});

@observer
export default class WalletReceive extends Component {

  props: {
    walletAddresses: Array<{
      value: string, isUsed: boolean,
    }>, // TODO: use proper type when API is implemented
    onCopyAddress: Function,
    isSidebarExpanded: boolean,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isSubmitting: false,
    showUsed: true,
  };

  submit = () => {
    // TODO: add real API call to generate new address
    this.setState({ isSubmitting: !this.state.isSubmitting });
  };

  toggleUsedAddresses = () => {
    this.setState({ showUsed: !this.state.showUsed });
  };

  render() {
    const { walletAddresses, onCopyAddress, isSidebarExpanded } = this.props;
    const { intl } = this.context;
    const { isSubmitting, showUsed } = this.state;
    const walletAddress = walletAddresses[0].value;
    return (
      <div className={styles.component}>

        <BorderedBox>

          <div className={styles.qrCodeAndInstructions}>
            <div className={styles.qrCode}>
              <QRCode
                value={walletAddress}
                bgColor="transparent"
                size={160}
              />
            </div>

            <div className={styles.instructions}>
              <div className={styles.hash}>
                {walletAddress}
                <CopyToClipboard
                  text={walletAddress}
                  onCopy={onCopyAddress.bind(this, walletAddress)}
                >
                  <img className={styles.copyIconBig} src={iconCopy} role="presentation" />
                </CopyToClipboard>
              </div>

              <div className={styles.hashLabel}>
                {intl.formatMessage(messages.walletAddressLabel)}
              </div>

              <div className={styles.instructionsText}>
                {intl.formatMessage(messages.walletReceiveInstructions)}
              </div>

              <Button
                className={isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
                label={intl.formatMessage(messages.generateNewAddressButtonLabel)}
                onMouseUp={this.submit.bind(this)}
                primary
              />
            </div>

            <img className={styles.protectedIcon} src={iconProtected} role="presentation" />
          </div>

          <div className={styles.generatedAddresses}>
            <h2>
              {intl.formatMessage(messages.generatedAddressesSectionTitle)}
              <button onClick={this.toggleUsedAddresses}>
                {intl.formatMessage(messages[showUsed ? 'hideUsedLabel' : 'showUsedLabel'])}
              </button>
            </h2>

            {walletAddresses.map((address, index) => {
              const isAddressVisible = !address.isUsed || showUsed;
              if (!isAddressVisible) return null;

              const addressClasses = classnames([
                styles.walletAddress,
                address.isUsed ? styles.usedWalletAddress : null,
                isSidebarExpanded ? styles.fullWidthOnSmallScreen : null,
              ]);
              return (
                <div key={index} className={addressClasses}>
                  {address.value}
                  {address.isUsed ? (
                    <img className={styles.copyIcon} src={iconCopy} role="presentation" />
                  ) : (
                    <CopyToClipboard
                      text={address.value}
                      onCopy={onCopyAddress.bind(this, address.value)}
                    >
                      <img className={styles.copyIcon} src={iconCopy} role="presentation" />
                    </CopyToClipboard>
                  )}
                </div>
              );
            })}
          </div>

        </BorderedBox>

      </div>
    );
  }

}
