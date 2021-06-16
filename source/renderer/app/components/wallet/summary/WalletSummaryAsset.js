// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import styles from './WalletSummaryAsset.scss';
import Asset from '../../assets/Asset';
import AssetAmount from '../../assets/AssetAmount';
import AssetContent from '../../assets/AssetContent';
import type { AssetToken } from '../../../api/assets/types';
import arrow from '../../../assets/images/collapse-arrow-small.inline.svg';
import warningIcon from '../../../assets/images/asset-token-warning-ic.inline.svg';

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
  settingsWarningPopOverAvailable: {
    id: 'assets.warning.available',
    defaultMessage:
      '!!!Recommended configuration for decimal places for this native token is available.',
    description: 'Asset settings recommended pop over content',
  },
  settingsWarningPopOverNotUsing: {
    id: 'assets.warning.notUsing',
    defaultMessage:
      '!!!You are not using the recommended decimal place configuration for this native token.',
    description: 'Asset settings recommended pop over content',
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

  renderHeader = () => {
    const {
      asset,
      onCopyAssetItem,
      isLoading,
      anyAssetWasHovered,
      assetSettingsDialogWasOpened,
    } = this.props;
    const { isExpanded } = this.state;
    const arrowStyles = classNames(styles.arrow, {
      [styles.isExpanded]: isExpanded,
    });
    return (
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
    );
  };

  renderFooter = () => {
    const { intl } = this.context;
    const { asset, isLoading } = this.props;
    return (
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
        {this.renderFooterButtons()}
      </div>
    );
  };

  renderFooterButtons = () => {
    const { intl } = this.context;
    const { asset, onOpenAssetSend, onAssetSettings } = this.props;
    const { recommendedDecimals, decimals } = asset;
    const hasWarning =
      typeof recommendedDecimals === 'number' &&
      decimals !== recommendedDecimals;
    let settingsButtonLabel = intl.formatMessage(messages.settingsButtonLabel);
    let warningPopOverMessage;
    if (hasWarning) {
      warningPopOverMessage =
        typeof decimals === 'number'
          ? messages.settingsWarningPopOverNotUsing
          : messages.settingsWarningPopOverAvailable;
      settingsButtonLabel = (
        <>
          {settingsButtonLabel}
          <SVGInline className={styles.warningIcon} svg={warningIcon} />
        </>
      );
    }
    const settingsButton = (
      <Button
        className={classNames(['flat', styles.button, styles.settingsButton])}
        label={settingsButtonLabel}
        onClick={() => onAssetSettings({ asset })}
      />
    );

    return (
      <div className={styles.footerButtons}>
        {hasWarning ? (
          <PopOver
            content={intl.formatMessage(warningPopOverMessage, {
              recommendedDecimals,
            })}
            className={styles.warningIconWrapper}
          >
            {settingsButton}
          </PopOver>
        ) : (
          settingsButton
        )}
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
    );
  };

  render() {
    const { asset, onCopyAssetItem } = this.props;
    const { isExpanded } = this.state;
    const componentStyles = classNames(styles.component, {
      [styles.isExpanded]: isExpanded,
    });
    return (
      <div className={componentStyles}>
        {this.renderHeader()}
        <div className={styles.content}>
          <AssetContent
            asset={asset}
            onCopyAssetItem={onCopyAssetItem}
            highlightFingerprint={false}
          />
          {this.renderFooter()}
        </div>
      </div>
    );
  }
}
