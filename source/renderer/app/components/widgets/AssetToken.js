// @flow
import React, { Component, Fragment as F } from 'react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { defineMessages, intlShape } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import { observer } from 'mobx-react';
import styles from './AssetToken.scss';
import { ellipsis } from '../../utils/strings';
import type { WalletSummaryAsset } from '../../api/assets/types';
import copyIcon from '../../assets/images/copy-asset.inline.svg';
import copyCheckmarkIcon from '../../assets/images/check-w.inline.svg';
import { ASSET_TOKEN_ID_COPY_FEEDBACK } from '../../config/timingConfig';

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
    defaultMessage: '!!!Asset Name',
    description: '"assetName" item.',
  },
  nameItem: {
    id: 'widgets.assetToken.item.name',
    defaultMessage: '!!!Name',
    description: '"name" item.',
  },
  acronymItem: {
    id: 'widgets.assetToken.item.acronym',
    defaultMessage: '!!!Acronym',
    description: '"acronym" item.',
  },
  descriptionItem: {
    id: 'widgets.assetToken.item.description',
    defaultMessage: '!!!Description',
    description: '"description" item.',
  },
});

type Props = {
  asset: WalletSummaryAsset,
  onCopyAssetItem: Function,
  hideTooltip?: boolean,
  // In case it's not possible to calculate the container width
  // this props defines after how many characters the text will cut off
  policyIdEllipsisLeft?: number,
};

type State = {
  isTooltipVisible: boolean,
  itemCopied: ?string,
};

@observer
export default class AssetToken extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  idCopyFeedbackTimeout: TimeoutID;

  state = {
    isTooltipVisible: false,
    itemCopied: null,
  };

  handleShowTooltip = () => {
    this.setState({
      isTooltipVisible: true,
    });
  };

  handleHideTooltip = () => {
    this.setState({
      isTooltipVisible: false,
    });
  };

  handleCopyItem = (itemCopied: string, assetItem: string, value: string) => {
    this.props.onCopyAssetItem(assetItem, value);
    clearTimeout(this.idCopyFeedbackTimeout);
    this.setState({
      itemCopied,
    });
    this.idCopyFeedbackTimeout = setTimeout(() => {
      this.setState({ itemCopied: null });
    }, ASSET_TOKEN_ID_COPY_FEEDBACK);
  };

  contentRender() {
    const { asset, policyIdEllipsisLeft } = this.props;
    const { fingerprint, policyId } = asset;
    return (
      <div className={styles.content}>
        <div className={styles.fingerprint}>
          {ellipsis(fingerprint || '', 9, 4)}
        </div>
        <div className={styles.policyId}>
          {policyIdEllipsisLeft
            ? ellipsis(policyId, policyIdEllipsisLeft)
            : policyId}
        </div>
      </div>
    );
  }

  assetItemRenderer = (
    assetId: string,
    assetItem: string,
    value: string,
    multiline?: boolean
  ) => {
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
          <em className={multiline ? styles.multiline : null}>{value}</em>
          <SVGInline svg={icon} className={iconClassnames} />
        </div>
      </CopyToClipboard>
    );
  };

  tooltipRender() {
    const { intl } = this.context;
    const { asset } = this.props;
    const { fingerprint, policyId, assetName, metadata } = asset;
    const { name, acronym, description } = metadata || {};
    const item = this.assetItemRenderer;
    return (
      <div className={styles.tooltipContent}>
        <div className={styles.fingerprint}>
          {item('fingerprint', 'fingerprint', fingerprint)}
        </div>
        <dl>
          <dt>{intl.formatMessage(messages.policyIdItem)}</dt>
          <dd>
            {item(
              'policyId',
              intl.formatMessage(messages.policyIdItem),
              policyId
            )}
          </dd>
          {assetName && (
            <F>
              <dt>{intl.formatMessage(messages.assetNameItem)}</dt>
              <dd>
                {item(
                  'assetName',
                  intl.formatMessage(messages.assetNameItem),
                  assetName
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
          {acronym && (
            <F>
              <dt>{intl.formatMessage(messages.acronymItem)}</dt>
              <dd>
                {item(
                  'acronym',
                  intl.formatMessage(messages.acronymItem),
                  acronym
                )}
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
                  description,
                  true
                )}
              </dd>
            </F>
          )}
        </dl>
      </div>
    );
  }

  render() {
    const { hideTooltip } = this.props;
    const { isTooltipVisible } = this.state;
    const children = this.contentRender();
    const tooltipContent = this.tooltipRender();
    if (hideTooltip) return children;
    return (
      <div
        className={styles.component}
        onMouseEnter={this.handleShowTooltip}
        onMouseLeave={this.handleHideTooltip}
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
          content={tooltipContent}
          isShowingOnHover={false}
          isVisible={isTooltipVisible}
          appendTo="parent"
          allowHTML
        >
          {children}
        </PopOver>
      </div>
    );
  }
}
