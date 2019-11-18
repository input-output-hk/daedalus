// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import CopyToClipboard from 'react-copy-to-clipboard';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './Address.scss';
import iconQR from '../../../assets/images/qr-code.inline.svg';
import iconExclamationPoint from '../../../assets/images/exclamation-point.inline.svg';
import WalletAddress from '../../../domains/WalletAddress';
import { ellipsis } from '../../../utils/strings';

type Props = {
  address: WalletAddress,
  onShareAddress: Function,
  isAddressValid: Function,
  shareAddressLabel: string,
  invalidAddressTooltipLabel: string,
  index: number,
};

@observer
export class Address extends Component<Props> {
  render() {
    const {
      address,
      onShareAddress,
      shareAddressLabel,
      isAddressValid,
      invalidAddressTooltipLabel,
      index,
    } = this.props;
    const addressClasses = classnames([
      `receiveAddress-${index + 1}`,
      styles.component,
      address.used ? styles.usedWalletAddress : null,
    ]);
    return (
      <div className={addressClasses}>
        <div className={styles.addressId} id={`address-${address.id}`}>
          {ellipsis(address.id, 30, 30)}
        </div>
        <div className={styles.addressActions}>
          <CopyToClipboard
            text={address.id}
            onCopy={() => onShareAddress(address.id)}
          >
            <span className={styles.copyAddress}>
              {!isAddressValid(index) && (
                <Tooltip
                  skin={TooltipSkin}
                  tip={invalidAddressTooltipLabel}
                  className={styles.invalidAddressTooltip}
                >
                  <SVGInline
                    svg={iconExclamationPoint}
                    className={styles.iconExclamationPoint}
                  />
                </Tooltip>
              )}
              <SVGInline svg={iconQR} className={styles.shareIcon} />
              <span className={styles.shareAddressLabel}>
                {shareAddressLabel}
              </span>
            </span>
          </CopyToClipboard>
        </div>
      </div>
    );
  }
}
