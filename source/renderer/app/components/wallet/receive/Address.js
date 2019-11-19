// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import CopyToClipboard from 'react-copy-to-clipboard';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './Address.scss';
import tooltipStyles from './AddressTooltip.scss';
import iconQR from '../../../assets/images/qr-code.inline.svg';
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import iconExclamationPoint from '../../../assets/images/exclamation-point.inline.svg';
import WalletAddress from '../../../domains/WalletAddress';
import { ellipsis } from '../../../utils/strings';

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
  address: WalletAddress,
  onShareAddress: Function,
  onCopyAddress: Function,
  shareAddressLabel: string,
  copyAddressLabel: string,
  invalidAddressTooltipLabel: string,
  isIncentivizedTestnet: boolean,
};

type State = {
  currentBreakPoint: 0,
  minCharsInit: number,
  minCharsEnd: ?number,
};

@observer
export default class Address extends Component<Props, State> {
  state = {
    ...BREAKPOINTS[0],
  };

  componentDidMount() {
    this.updateWindowDimensions();
    window.addEventListener('resize', this.updateWindowDimensions, 250);
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.updateWindowDimensions);
  }

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
    if (windowWidth >= 1081) return 4;
    if (windowWidth >= 1050) return 3;
    if (windowWidth >= 1000) return 2;
    if (windowWidth >= 950) return 1;
    return 0;
  };

  render() {
    const {
      address,
      onShareAddress,
      onCopyAddress,
      shareAddressLabel,
      copyAddressLabel,
      invalidAddressTooltipLabel,
      isIncentivizedTestnet,
    } = this.props;
    const addressClasses = classnames([
      `receiveAddress-${address.id}`,
      styles.component,
      address.used ? styles.usedWalletAddress : null,
    ]);
    const { minCharsInit, minCharsEnd } = this.state;
    return (
      <div className={addressClasses}>
        <div className={styles.addressId} id={`address-${address.id}`}>
          {ellipsis(address.id, minCharsInit, minCharsEnd)}
        </div>
        <div className={styles.addressActions}>
          {address.isInvalid && (
            <Tooltip
              skin={TooltipSkin}
              tip={invalidAddressTooltipLabel}
              className={styles.invalidAddressTooltip}
              themeOverrides={tooltipStyles}
              arrowRelativeToTip
            >
              <SVGInline
                svg={iconExclamationPoint}
                className={styles.invalidAddress}
              />
            </Tooltip>
          )}
          {!isIncentivizedTestnet ? (
            <button
              className={styles.shareAddressButton}
              onClick={onShareAddress}
            >
              <SVGInline svg={iconQR} className={styles.shareIcon} />
              <span className={styles.shareAddressLabel}>
                {shareAddressLabel}
              </span>
            </button>
          ) : null}
          {isIncentivizedTestnet ? (
            <CopyToClipboard
              text={address.id}
              // eslint-disable-next-line react/jsx-no-bind
              onCopy={onCopyAddress.bind(this, address.id)}
            >
              <span className={styles.copyAddress}>
                <SVGInline svg={iconCopy} className={styles.copyIcon} />
                <span className={styles.copyAddressLabel}>
                  {copyAddressLabel}
                </span>
              </span>
            </CopyToClipboard>
          ) : null}
        </div>
      </div>
    );
  }
}
