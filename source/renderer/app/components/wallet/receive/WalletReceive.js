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
  copyAddressLabel: {
    id: 'wallet.receive.page.copyAddressLabel',
    defaultMessage: '!!!Copy address',
    description: 'Label for "Copy address" link on the wallet "Receive page"',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

const BREAKPOINTS = [
  {
    minCharsInit: 22,
    minCharsEnd: 22,
  },
  {
    minCharsInit: 24,
    minCharsEnd: 24,
  },
  {
    minCharsInit: 27,
    minCharsEnd: 27,
  },
  {
    minCharsInit: 29,
    minCharsEnd: 29,
  },
  {
    minCharsInit: 999999999999,
    minCharsEnd: null,
  },
];

type Props = {
  walletAddresses: Array<WalletAddress>,
  onShareAddress: Function,
  onCopyAddress: Function,
  currentLocale: string,
  isIncentivizedTestnet: boolean,
  isShowingSubMenus: boolean,
};

type State = {
  showUsed: boolean,
  currentBreakPoint: number,
  minCharsInit: number,
  minCharsEnd?: ?number,
};

@observer
export default class WalletReceive extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    showUsed: true,
    currentBreakPoint: -1,
    minCharsInit: 0,
    minCharsEnd: 0,
  };

  componentDidMount() {
    this.updateWindowDimensions();
    window.addEventListener('resize', this.updateWindowDimensions);
  }

  componentWillReceiveProps(nextProps: Props) {
    const { isShowingSubMenus: isShowingSubMenusNext } = nextProps;
    const { isShowingSubMenus: isShowingSubMenusCurrent } = this.props;
    if (isShowingSubMenusNext !== isShowingSubMenusCurrent) {
      setTimeout(this.updateWindowDimensions, 300);
    }
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.updateWindowDimensions);
  }

  toggleUsedAddresses = () => {
    this.setState(prevState => ({ showUsed: !prevState.showUsed }));
  };

  updateWindowDimensions = () => {
    const { currentBreakPoint } = this.state;
    const newBreakpoint = this.getBreakpoint(window.innerWidth);
    if (currentBreakPoint !== newBreakpoint) {
      const { minCharsInit, minCharsEnd } = BREAKPOINTS[newBreakpoint];
      this.setState({
        currentBreakPoint: newBreakpoint,
        minCharsInit,
        minCharsEnd,
      });
    }
  };

  getBreakpoint = (windowWidth: number) => {
    const {
      isIncentivizedTestnet,
      isShowingSubMenus,
      currentLocale,
    } = this.props;

    if (
      isIncentivizedTestnet &&
      !isShowingSubMenus &&
      currentLocale === 'ja-JP'
    ) {
      if (windowWidth < 950) return 3;
      return 4;
    }

    if (windowWidth >= 1081) return 4;
    if (windowWidth >= 1050) return 3;
    if (windowWidth >= 1000) return 2;
    if (windowWidth >= 950) return 1;
    return 0;
  };

  renderRow = (address: WalletAddress) => {
    const {
      onShareAddress,
      onCopyAddress,
      isIncentivizedTestnet,
      isShowingSubMenus,
      currentLocale,
    } = this.props;
    const { minCharsInit, minCharsEnd } = this.state;
    const { intl } = this.context;
    return (
      <Address
        address={address}
        onShareAddress={onShareAddress}
        onCopyAddress={onCopyAddress}
        shareAddressLabel={intl.formatMessage(messages.shareAddressLabel)}
        copyAddressLabel={intl.formatMessage(messages.copyAddressLabel)}
        currentLocale={currentLocale}
        isIncentivizedTestnet={isIncentivizedTestnet}
        isShowingSubMenus={isShowingSubMenus}
        minCharsInit={minCharsInit}
        minCharsEnd={minCharsEnd}
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
