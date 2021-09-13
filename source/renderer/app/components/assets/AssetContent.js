// @flow
import React, { useState } from 'react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
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
  className?: string,
  intl: intlShape.isRequired,
  hasError?: boolean,
};

type ItemCopied = ?string;

const AssetContent = observer((props: Props) => {
  const [itemCopied, setItemCopied] = useState<ItemCopied>(null);

  let copyNotificationTimeout: TimeoutID;

  const handleCopyItem = (
    newItemCopied: string,
    assetItem: string,
    value: string
  ) => {
    const { onCopyAssetItem } = props;
    if (onCopyAssetItem) {
      onCopyAssetItem(assetItem, value);
    }
    clearTimeout(copyNotificationTimeout);
    setItemCopied(newItemCopied);
    copyNotificationTimeout = setTimeout(() => {
      setItemCopied(null);
    }, ASSET_TOKEN_ID_COPY_FEEDBACK);
  };

  const renderAssetItem = (
    assetId: string,
    assetItem: string,
    value: string
  ) => {
    const icon = itemCopied === assetId ? copyCheckmarkIcon : copyIcon;
    const iconClassnames = classnames([
      styles.copyIcon,
      itemCopied === assetId ? styles.copiedIcon : null,
    ]);
    const onCopy = () => {
      handleCopyItem(assetId, assetItem, value);
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

  const { asset, highlightFingerprint, className, intl, hasError } = props;
  const { fingerprint, policyId, assetName, metadata } = asset;
  const { name, ticker, description } = metadata || {};
  const componentStyles = classnames([
    styles.component,
    className,
    highlightFingerprint ? styles.highlightFingerprint : null,
    hasError ? styles.error : null,
  ]);
  return (
    <div className={componentStyles}>
      {highlightFingerprint && (
        <div className={styles.fingerprint}>
          {renderAssetItem(
            'fingerprint',
            intl.formatMessage(messages.fingerprintItem),
            fingerprint
          )}
        </div>
      )}
      <dl>
        {!highlightFingerprint && (
          <>
            <dt>{intl.formatMessage(messages.fingerprintItem)}</dt>
            <dd>
              {renderAssetItem(
                'fingerprint',
                intl.formatMessage(messages.fingerprintItem),
                fingerprint
              )}
            </dd>
          </>
        )}
        {ticker && (
          <>
            <dt>{intl.formatMessage(messages.tickerItem)}</dt>
            <dd>
              {renderAssetItem(
                'ticker',
                intl.formatMessage(messages.tickerItem),
                ticker
              )}
            </dd>
          </>
        )}
        {name && (
          <>
            <dt>{intl.formatMessage(messages.nameItem)}</dt>
            <dd>
              {renderAssetItem(
                'name',
                intl.formatMessage(messages.nameItem),
                name
              )}
            </dd>
          </>
        )}
        {description && (
          <>
            <dt>{intl.formatMessage(messages.descriptionItem)}</dt>
            <dd>
              {renderAssetItem(
                'description',
                intl.formatMessage(messages.descriptionItem),
                description
              )}
            </dd>
          </>
        )}
        <dt>{intl.formatMessage(messages.policyIdItem)}</dt>
        <dd>
          {renderAssetItem(
            'policyId',
            intl.formatMessage(messages.policyIdItem),
            policyId
          )}
        </dd>
        <dt>{intl.formatMessage(messages.assetNameItem)}</dt>
        <dd>
          {assetName ? (
            renderAssetItem(
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
});

export default injectIntl(AssetContent);
