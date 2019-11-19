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

type Props = {
  address: WalletAddress,
  onShareAddress: Function,
  onCopyAddress: Function,
  shareAddressLabel: string,
  copyAddressLabel: string,
  invalidAddressTooltipLabel: string,
  isIncentivizedTestnet: boolean,
};

@observer
export default class Address extends Component<Props> {
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
    return (
      <div className={addressClasses}>
        <div className={styles.addressId} id={`address-${address.id}`}>
          {ellipsis(address.id, 30, 30)}
        </div>
        <div className={styles.addressActions}>
          {address.isInvalid && (
            <Tooltip
              skin={TooltipSkin}
              tip={invalidAddressTooltipLabel}
              className={styles.invalidAddressTooltip}
              themeOverrides={tooltipStyles}
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
