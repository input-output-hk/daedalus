// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { debounce } from 'lodash';
import BorderedBox from '../../widgets/BorderedBox';
import TinySwitch from '../../widgets/forms/TinySwitch';
import WalletAddress from '../../../domains/WalletAddress';
import globalMessages from '../../../i18n/global-messages';
import { VirtualAddressesList } from './VirtualAddressesList';
import styles from './WalletReceiveItn.scss';
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
  onToggleSubMenus: Object,
  isIncentivizedTestnet: boolean,
};

type State = {
  addressSlice: number,
  addressWidth: number,
  charWidth: number,
  showUsed: boolean,
};

@observer
export default class WalletReceiveItn extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  containerElement: ?HTMLElement;

  state = {
    addressSlice: 0,
    addressWidth: 0,
    charWidth: 0,
    showUsed: true,
  };

  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
    window.addEventListener('resize', this.debounceAddressCalculation);
    this.props.onToggleSubMenus.listen(this.debounceAddressCalculation);
  }

  componentWillUnmount() {
    this._isMounted = false;
    window.removeEventListener('resize', this.debounceAddressCalculation);
    this.props.onToggleSubMenus.remove(this.debounceAddressCalculation);
  }

  debounceAddressCalculation = debounce(
    () => this.calculateAddressSlice(),
    300
  );

  get addressLength() {
    const [address: WalletAddress] = this.props.walletAddresses;
    return address.id.length;
  }

  handleRegisterHTMLElements = (
    addressElement: HTMLElement,
    containerElement: HTMLElement
  ) => {
    setTimeout(() => {
      if (this._isMounted) {
        this.containerElement = containerElement;
        const addressWidth = addressElement.offsetWidth;
        const charWidth = addressWidth / this.addressLength;
        this.setState({ charWidth, addressWidth }, this.calculateAddressSlice);
      }
    }, 500);
  };

  calculateAddressSlice = () => {
    if (this._isMounted) {
      const { charWidth, addressWidth } = this.state;
      const { addressLength, containerElement } = this;
      if (!containerElement || !charWidth || !addressLength) return;
      const containerWidth = containerElement.offsetWidth;
      const addressSlice =
        containerWidth < addressWidth
          ? Math.floor(containerWidth / charWidth / 2) - 1
          : 0;
      this.setState({
        addressSlice,
      });
    }
  };

  toggleUsedAddresses = () => {
    this.setState(prevState => ({ showUsed: !prevState.showUsed }));
  };

  renderRow = (address: WalletAddress, index: number) => {
    const { onShareAddress, onCopyAddress, isIncentivizedTestnet } = this.props;
    const { addressSlice } = this.state;
    const { intl } = this.context;
    return (
      <Address
        address={address}
        onShareAddress={onShareAddress}
        onCopyAddress={onCopyAddress}
        shareAddressLabel={intl.formatMessage(messages.shareAddressLabel)}
        copyAddressLabel={intl.formatMessage(messages.copyAddressLabel)}
        isIncentivizedTestnet={isIncentivizedTestnet}
        shouldRegisterAddressElement={index === 0}
        onRegisterHTMLElements={this.handleRegisterHTMLElements}
        addressSlice={addressSlice}
      />
    );
  };

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
