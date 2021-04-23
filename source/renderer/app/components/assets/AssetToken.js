// @flow
import React, { Component, Fragment as F } from 'react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { defineMessages, intlShape } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import { observer } from 'mobx-react';
import styles from './AssetToken.scss';
import { ellipsis, hexToString } from '../../utils/strings';
import type { WalletSummaryAsset } from '../../api/assets/types';
import copyIcon from '../../assets/images/copy-asset.inline.svg';
import settingsIcon from '../../assets/images/settings-asset-token-ic.inline.svg';
import copyCheckmarkIcon from '../../assets/images/check-w.inline.svg';
import {
  ASSET_TOKEN_ID_COPY_FEEDBACK,
  ASSET_TOKEN_DISPLAY_DELAY,
} from '../../config/timingConfig';

const messages = defineMessages({
  fingerprintItem: {
    id: 'widgets.assetToken.item.fingerprint',
    defaultMessage: '!!!Fingerprint',
    description: '"fingerprint" item.',
  },
  policyIdItem: {
    id: 'widgets.assetToken.item.policyId',
    defaultMessage: '!!!Policy Id',
    description: '"policyId" item.',
  },
  assetNameItem: {
    id: 'widgets.assetToken.item.assetName',
    defaultMessage: '!!!Asset name',
    description: '"assetName" item.',
  },
  nameItem: {
    id: 'widgets.assetToken.item.name',
    defaultMessage: '!!!Name',
    description: '"name" item.',
  },
  tickerItem: {
    id: 'widgets.assetToken.item.ticker',
    defaultMessage: '!!!Ticker',
    description: '"ticker" item.',
  },
  descriptionItem: {
    id: 'widgets.assetToken.item.description',
    defaultMessage: '!!!Description',
    description: '"description" item.',
  },
  blank: {
    id: 'widgets.assetToken.item.blank',
    defaultMessage: '!!!Blank',
    description: '"Blank" item value.',
  },
  settingsPopOver: {
    id: 'widgets.assetToken.settingsPopOver',
    defaultMessage: '!!!Asset settings',
    description: 'Asset settings pop over content',
  },
});

type Props = {
  asset: WalletSummaryAsset,
  small?: boolean,
  hidePopOver?: boolean,
  onCopyAssetItem?: Function,
  onClickSettings?: Function,
  className?: string,
  // In case it's not possible to calculate the container width
  // this props defines after how many characters the `metadata.name` text will cut off
  metadataNameChars?: number,
};

type State = {
  isPopOverVisible: boolean,
  itemCopied: ?string,
};

@observer
export default class AssetToken extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  copyNotificationTimeout: TimeoutID;
  displayDelayTimeout: TimeoutID;

  state = {
    isPopOverVisible: false,
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

  handleShowPopOver = () => {
    clearTimeout(this.displayDelayTimeout);
    this.displayDelayTimeout = setTimeout(() => {
      if (this._isMounted) {
        this.setState({
          isPopOverVisible: true,
        });
      }
    }, ASSET_TOKEN_DISPLAY_DELAY);
  };

  handleHidePopOver = () => {
    clearTimeout(this.displayDelayTimeout);
    this.displayDelayTimeout = setTimeout(() => {
      if (this._isMounted) {
        this.setState({
          isPopOverVisible: false,
        });
      }
    }, ASSET_TOKEN_DISPLAY_DELAY);
  };

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

  renderPill() {
    const { asset, metadataNameChars, small } = this.props;
    const { fingerprint, metadata } = asset;
    const { name } = metadata || {};
    const contentStyles = classnames([
      styles.pill,
      small ? styles.small : null,
    ]);
    return (
      <div className={contentStyles}>
        <div className={styles.fingerprint}>
          {ellipsis(fingerprint || '', 9, 4)}
        </div>
        {name && (
          <div className={styles.metadataName}>
            {metadataNameChars ? ellipsis(name, metadataNameChars) : name}
          </div>
        )}
      </div>
    );
  }

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

  renderPopOverContent() {
    const { intl } = this.context;
    const { asset } = this.props;
    const { fingerprint, policyId, assetName, metadata } = asset;
    const { name, ticker, description } = metadata || {};
    const item = this.renderAssetItem;
    return (
      <div className={styles.popOverContent}>
        <div className={styles.fingerprint}>
          {item(
            'fingerprint',
            intl.formatMessage(messages.fingerprintItem),
            fingerprint
          )}
        </div>
        <dl>
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

  renderPopOverContainer = () => {
    const pillContent = this.renderPill();
    const popOverContent = this.renderPopOverContent();
    const { isPopOverVisible } = this.state;
    return (
      <div
        className={styles.popOverContainer}
        onMouseEnter={this.handleShowPopOver}
        onMouseLeave={this.handleHidePopOver}
      >
        <PopOver
          themeVariables={{
            '--rp-pop-over-bg-color':
              'var(--theme-widgets-asset-token-background-color)',
            '--rp-pop-over-text-color': 'var(--theme-bordered-box-text-color)',
            '--rp-pop-over-border-color':
              'var(--theme-staking-stake-pool-tooltip-border-color)',
            '--rp-pop-over-border-width': '1px',
            '--rp-pop-over-border-style': 'solid',
            '--rp-pop-over-box-shadow':
              '0 5px 20px 0 var(--theme-widgets-asset-token-box-shadow)',
          }}
          contentClassName={styles.popOver}
          content={popOverContent}
          visible={isPopOverVisible}
          appendTo="parent"
          maxWidth={376}
          allowHTML
          interactive
        >
          {pillContent}
        </PopOver>
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const { hidePopOver, onClickSettings, asset, className } = this.props;
    const content = hidePopOver
      ? this.renderPill()
      : this.renderPopOverContainer();
    const onClickSettingsBind = () => onClickSettings && onClickSettings(asset);
    const componenClassnames = classnames([styles.component, className]);
    return (
      <div className={componenClassnames}>
        {content}
        {onClickSettings && (
          <PopOver content={intl.formatMessage(messages.settingsPopOver)}>
            <button
              className={styles.settingsButton}
              onClick={onClickSettingsBind}
            >
              <SVGInline svg={settingsIcon} />
            </button>
          </PopOver>
        )}
      </div>
    );
  }
}
