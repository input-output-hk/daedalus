import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { debounce } from 'lodash';
import BorderedBox from '../../widgets/BorderedBox';
import TinySwitch from '../../widgets/forms/TinySwitch';
import WalletAddress from '../../../domains/WalletAddress';
import globalMessages from '../../../i18n/global-messages';
import { VirtualAddressesList } from './VirtualAddressesList';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletReceiveSequential.scss... Remove this comment to see the full error message
import styles from './WalletReceiveSequential.scss';
import AddressSequential from './AddressSequential';

const messages = defineMessages({
  instructionsTitle: {
    id: 'wallet.receive.page.instructions.instructionsTitle',
    defaultMessage: '!!!Available wallet addresses',
    description: 'Instructions Title on the wallet "Receive page"',
  },
  instructionsDescription: {
    id: 'wallet.receive.page.instructions.instructionsDescription',
    defaultMessage:
      '!!!Share any of these wallet addresses <b>to receive payments in ada or a native Cardano token</b>.',
    description: 'Instructions Description on the wallet "Receive page"',
  },
  privacyWarning: {
    id: 'wallet.receive.page.instructions.privacyWarning',
    defaultMessage:
      '!!!Privacy warning: Please note that all of your receiving addresses include your stake key. When you share a receiving address, the recipient can search the blockchain using your stake key to locate all addresses associated with your wallet, and also discover your wallet balance and transaction history.',
    description: 'Privacy warning on the wallet "Receive page"',
  },
  addressesTitle: {
    id: 'wallet.receive.page.addresses.addressesTitle',
    defaultMessage: '!!!Receiving addresses',
    description: 'Addresses Title on the wallet "Receive page"',
  },
  showUsedLabel: {
    id: 'wallet.receive.page.showUsedLabel',
    defaultMessage: '!!!Show used',
    description:
      'Label for "show used" wallet addresses link on the wallet "Receive page"',
  },
});
messages.fieldIsRequired = globalMessages.fieldIsRequired;
type Props = {
  walletAddresses: Array<WalletAddress>;
  onShareAddress: (...args: Array<any>) => any;
  onCopyAddress: (...args: Array<any>) => any;
  onToggleSubMenus: Record<string, any>;
  showUsed: boolean;
  onToggleUsedAddresses: (...args: Array<any>) => any;
};
type State = {
  addressSlice: number;
  addressWidth: number;
  charWidth: number;
};

@observer
class WalletReceiveSequential extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  containerElement: HTMLElement | null | undefined;
  state = {
    addressSlice: 0,
    addressWidth: 0,
    charWidth: 0,
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
    const [address] = this.props.walletAddresses;
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
        this.setState(
          {
            charWidth,
            addressWidth,
          },
          this.calculateAddressSlice
        );
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
    const { onToggleUsedAddresses } = this.props;
    onToggleUsedAddresses();
  };
  renderRow = (address: WalletAddress, index: number) => {
    const { onShareAddress, onCopyAddress } = this.props;
    const { addressSlice } = this.state;
    return (
      <AddressSequential
        address={address}
        onShareAddress={onShareAddress}
        onCopyAddress={onCopyAddress}
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
      (address: WalletAddress) => !address.used || this.props.showUsed
    );

  render() {
    const { walletAddresses, showUsed } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>
        <BorderedBox fullHeight>
          <div className={styles.container}>
            <div className={styles.instructions}>
              <h2 className={styles.instructionsTitle}>
                {intl.formatMessage(messages.instructionsTitle)}
              </h2>
              <p className={styles.instructionsDescription}>
                <FormattedHTMLMessage {...messages.instructionsDescription} />
              </p>
              <p className={styles.privacyWarning}>
                {intl.formatMessage(messages.privacyWarning)}
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

export default WalletReceiveSequential;
