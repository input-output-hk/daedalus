// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import classNames from 'classnames';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import styles from './WalletSummaryAsset.scss';
import Asset from '../../assets/Asset';
import AssetAmount from '../../assets/AssetAmount';
import AssetContent from '../../assets/AssetContent';
import type { AssetToken } from '../../../api/assets/types';
import arrow from '../../../assets/images/collapse-arrow-small.inline.svg';

const messages = defineMessages({
  tokenSendButton: {
    id: 'wallet.summary.asset.tokenSendButton',
    defaultMessage: '!!!Send',
    description: 'Send button on Wallet summary assets page',
  },
  amountLabel: {
    id: 'wallet.summary.asset.amountLabel',
    defaultMessage: '!!!Amount',
    description: 'Amount label on Wallet summary assets page',
  },
  settingsButtonLabel: {
    id: 'wallet.summary.asset.settings.button.label',
    defaultMessage: '!!!Settings',
    description: 'Settings label on Wallet summary assets page',
  },
});

type Props = {
  asset: AssetToken,
  onOpenAssetSend: Function,
  onCopyAssetItem: Function,
  onAssetSettings: Function,
  anyAssetWasHovered: boolean,
  isLoading: boolean,
  assetSettingsDialogWasOpened: boolean,
};

type State = {
  isExpanded: boolean,
};

@observer
export default class WalletSummaryAsset extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isExpanded: false,
  };

  toggleIsExpanded = () => {
    this.setState((prevState) => ({
      isExpanded: !prevState.isExpanded,
    }));
  };

  render() {
    const { intl } = this.context;
    const {
      asset,
      onOpenAssetSend,
      onCopyAssetItem,
      onAssetSettings,
      isLoading,
      anyAssetWasHovered,
      assetSettingsDialogWasOpened,
    } = this.props;
    const { isExpanded } = this.state;
    const componentStyles = classNames(styles.component, {
      [styles.isExpanded]: isExpanded,
    });
    const arrowStyles = classNames(styles.arrow, {
      [styles.isExpanded]: isExpanded,
    });
    return (
      <div className={componentStyles}>
        <div className={styles.header} onClick={this.toggleIsExpanded}>
          <Asset
            asset={asset}
            onCopyAssetItem={onCopyAssetItem}
            metadataNameChars={get('name', asset.metadata, 0)}
            assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
            anyAssetWasHovered={anyAssetWasHovered}
            hidePopOver
            fullFingerprint
          />
          <AssetAmount
            amount={asset.quantity}
            metadata={asset.metadata}
            decimals={asset.decimals}
            isLoading={isLoading}
            className={styles.assetAmount}
            isShort
          />
          <SVGInline svg={arrow} className={arrowStyles} />
        </div>
        <div className={styles.content}>
          <AssetContent
            asset={asset}
            onCopyAssetItem={onCopyAssetItem}
            highlightFingerprint={false}
          />
          <div className={styles.footer}>
            <dl>
              <dt>{intl.formatMessage(messages.amountLabel)}</dt>
              <dd>
                {' '}
                <AssetAmount
                  amount={asset.quantity}
                  metadata={asset.metadata}
                  decimals={asset.decimals}
                  isLoading={isLoading}
                  className={styles.assetAmount}
                />
              </dd>
            </dl>
            <div className={styles.footerButtons}>
              <Button
                className={classNames(['flat', styles.button])}
                label={intl.formatMessage(messages.settingsButtonLabel)}
                onClick={() => onAssetSettings({ asset })}
              />
              <Button
                className={classNames([
                  'primary',
                  styles.button,
                  asset.quantity.isZero() ? styles.disabled : null,
                ])}
                onClick={() => onOpenAssetSend(asset)}
                label={intl.formatMessage(messages.tokenSendButton)}
              />
            </div>
          </div>
        </div>
      </div>
    );
  }
}
