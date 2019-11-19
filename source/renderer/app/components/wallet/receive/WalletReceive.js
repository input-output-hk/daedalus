// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import BorderedBox from '../../widgets/BorderedBox';
import TinySwitch from '../../widgets/forms/TinySwitch';
import WalletAddress from '../../../domains/WalletAddress';
import globalMessages from '../../../i18n/global-messages';
import { VirtualAddressesList } from './VirtualAddressesList';
import styles from './WalletReceive.scss';
import Address from './Address';

const messages = defineMessages({
  instructionsTitle: {
    id: 'wallet.receive.page.instructions.instructionsTitle',
    defaultMessage: '!!!Your wallet addresses',
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
    defaultMessage: '!!!show used',
    description:
      'Label for "show used" wallet addresses link on the wallet "Receive page"',
  },
  shareAddressLabel: {
    id: 'wallet.receive.page.shareAddressLabel',
    defaultMessage: '!!!Share',
    description: 'Label for "Share" link on the wallet "Receive page"',
  },
  invalidAddressTooltipLabel: {
    id: 'wallet.receive.page.invalidAddressTooltipLabel',
    defaultMessage:
      '!!!This address does not match your delegation preferences. Do not use it to receive ada.',
    description:
      'Label for "invalid address tooltip" link on the wallet "Receive page"',
  },
  copyAddressLabel: {
    id: 'wallet.receive.page.copyAddressLabel',
    defaultMessage: '!!!Copy address',
    description: 'Label for "Copy address" link on the wallet "Receive page"',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  walletAddresses: Array<WalletAddress>,
  onShareAddress: Function,
  onCopyAddress: Function,
  isIncentivizedTestnet: boolean,
};

type State = {
  showUsed: boolean,
};

@observer
export default class WalletReceive extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    showUsed: true,
  };

  toggleUsedAddresses = () => {
    this.setState(prevState => ({ showUsed: !prevState.showUsed }));
  };

  renderRow = (address: WalletAddress) => (
    <Address
      address={address}
      onShareAddress={this.props.onShareAddress}
      onCopyAddress={this.props.onCopyAddress}
      shareAddressLabel={this.context.intl.formatMessage(
        messages.shareAddressLabel
      )}
      invalidAddressTooltipLabel={this.context.intl.formatMessage(
        messages.invalidAddressTooltipLabel
      )}
      copyAddressLabel={this.context.intl.formatMessage(
        messages.copyAddressLabel
      )}
      isIncentivizedTestnet={this.props.isIncentivizedTestnet}
    />
  );

  getFilteredAddresses = (
    walletAddresses: Array<WalletAddress>
  ): Array<WalletAddress> =>
    walletAddresses.filter(
      (address: WalletAddress) => !address.used || this.state.showUsed
    );

  render() {
    const { walletAddresses } = this.props;
    const { intl } = this.context;
    const { showUsed } = this.state;

    return (
      <div className={styles.component}>
        <BorderedBox fullHeight>
          <div className={styles.container}>
            <div className={styles.instructions}>
              <h2 className={styles.instructionsTitle}>
                {intl.formatMessage(messages.instructionsTitle)}
              </h2>
              <p className={styles.instructionsDescription}>
                {intl.formatMessage(messages.instructionsDescription)}
              </p>
            </div>
            <div className={styles.addresses}>
              <h3 className={styles.addressesTitle}>
                {intl.formatMessage(messages.addressesTitle)}
                <div className={styles.hideUsed}>
                  <TinySwitch
                    label={intl.formatMessage(messages.showUsedLabel)}
                    onChange={this.toggleUsedAddresses}
                    checked={showUsed}
                  />
                </div>
              </h3>

              <VirtualAddressesList
                rows={this.getFilteredAddresses(walletAddresses)}
                renderRow={this.renderRow}
              />
            </div>
          </div>
        </BorderedBox>
      </div>
    );
  }
}
