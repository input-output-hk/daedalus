// @flow
import React, { Component, Fragment as F } from 'react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import { observer } from 'mobx-react';
import styles from './AssetContent.scss';
import { hexToString } from '../../utils/strings';

import copyIcon from '../../assets/images/copy-asset.inline.svg';
import copyCheckmarkIcon from '../../assets/images/check-w.inline.svg';
import { ASSET_TOKEN_ID_COPY_FEEDBACK } from '../../config/timingConfig';
import type { Asset as AssetProps } from '../../api/assets/types';

const messages = defineMessages({
  fingerprintItem: {
    id: 'assets.assetToken.item.fingerprint',
    defaultMessage: '!!!Fingerprint',
    description: '"fingerprint" item.',
  },
  policyIdItem: {
    id: 'assets.assetToken.item.policyId',
    defaultMessage: '!!!Policy Id',
    description: '"policyId" item.',
  },
  assetNameItem: {
    id: 'assets.assetToken.item.assetName',
    defaultMessage: '!!!Asset name',
    description: '"assetName" item.',
  },
  nameItem: {
    id: 'assets.assetToken.item.name',
    defaultMessage: '!!!Name',
    description: '"name" item.',
  },
  tickerItem: {
    id: 'assets.assetToken.item.ticker',
    defaultMessage: '!!!Ticker',
    description: '"ticker" item.',
  },
  descriptionItem: {
    id: 'assets.assetToken.item.description',
    defaultMessage: '!!!Description',
    description: '"description" item.',
  },
  blank: {
    id: 'assets.assetToken.item.blank',
    defaultMessage: '!!!Blank',
    description: '"Blank" item value.',
  },
  settingsCogPopOver: {
    id: 'assets.assetToken.settings.cogPopOver',
    defaultMessage:
      '!!!You can configure the number of decimal places for this native token.',
    description: 'Asset settings pop over content',
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
  asset: AssetProps,
  onCopyAssetItem?: Function,
  highlightFingerprint?: boolean,
};

type State = {
  itemCopied: ?string,
};

@observer
export default class AssetContent extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    highlightFingerprint: true,
  };

  copyNotificationTimeout: TimeoutID;
  displayDelayTimeout: TimeoutID;

  state = {
    itemCopied: null,
  };

  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
  }

  componentWillUnmount() {
    this._isMounted = false;
  }

  handleCopyItem = (itemCopied: string, assetItem: string, value: string) => {
    const { onCopyAssetItem } = this.props;
    if (onCopyAssetItem) {
      onCopyAssetItem(assetItem, value);
    }
    clearTimeout(this.copyNotificationTimeout);
    this.setState({
      itemCopied,
    });
    this.copyNotificationTimeout = setTimeout(() => {
      if (this._isMounted) {
        this.setState({ itemCopied: null });
      }
    }, ASSET_TOKEN_ID_COPY_FEEDBACK);
  };

  renderAssetItem = (assetId: string, assetItem: string, value: string) => {
    const { itemCopied } = this.state;
    const icon = itemCopied === assetId ? copyCheckmarkIcon : copyIcon;
    const iconClassnames = classnames([
      styles.copyIcon,
      itemCopied === assetId ? styles.copiedIcon : null,
    ]);
    const onCopy = () => {
      this.handleCopyItem(assetId, assetItem, value);
    };
    return (
      <CopyToClipboard text={value} onCopy={onCopy}>
        <div className={styles.assetItem}>
          <div className={styles.value}>
            {value}
            <SVGInline svg={icon} className={iconClassnames} />
          </div>
          {assetId === 'assetName' && (
            <div className={styles.assetASCIIName}>
              (ASCII: {hexToString(value)})
            </div>
          )}
        </div>
      </CopyToClipboard>
    );
  };

  render() {
    const { intl } = this.context;
    const { asset, highlightFingerprint } = this.props;
    const { fingerprint, policyId, assetName, metadata } = asset;
    const { name, ticker, description } = metadata || {};
    const item = this.renderAssetItem;
    return (
      <div className={styles.component}>
        {highlightFingerprint && (
          <div className={styles.fingerprint}>
            {item(
              'fingerprint',
              intl.formatMessage(messages.fingerprintItem),
              fingerprint
            )}
          </div>
        )}
        <dl>
          {!highlightFingerprint && (
            <F>
              <dt>{intl.formatMessage(messages.fingerprintItem)}</dt>
              <dd>
                {item(
                  'fingerprint',
                  intl.formatMessage(messages.fingerprintItem),
                  fingerprint
                )}
              </dd>
            </F>
          )}
          {ticker && (
            <F>
              <dt>{intl.formatMessage(messages.tickerItem)}</dt>
              <dd>
                {item(
                  'ticker',
                  intl.formatMessage(messages.tickerItem),
                  ticker
                )}
              </dd>
            </F>
          )}
          {name && (
            <F>
              <dt>{intl.formatMessage(messages.nameItem)}</dt>
              <dd>
                {item('name', intl.formatMessage(messages.nameItem), name)}
              </dd>
            </F>
          )}
          {description && (
            <F>
              <dt>{intl.formatMessage(messages.descriptionItem)}</dt>
              <dd>
                {item(
                  'description',
                  intl.formatMessage(messages.descriptionItem),
                  description
                )}
              </dd>
            </F>
          )}
          <dt>{intl.formatMessage(messages.policyIdItem)}</dt>
          <dd>
            {item(
              'policyId',
              intl.formatMessage(messages.policyIdItem),
              policyId
            )}
          </dd>
          <dt>{intl.formatMessage(messages.assetNameItem)}</dt>
          <dd>
            {assetName ? (
              item(
                'assetName',
                intl.formatMessage(messages.assetNameItem),
                assetName
              )
            ) : (
              <span className={styles.blankValue}>
                {intl.formatMessage(messages.blank)}
              </span>
            )}
          </dd>
        </dl>
      </div>
    );
  }
}
