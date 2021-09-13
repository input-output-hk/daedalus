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
  fingerprintrenderAssetParam: {
    id: 'assets.assetToken.param.fingerprint',
    defaultMessage: '!!!Fingerprint',
    description: '"fingerprint" param.',
  },
  policyIdrenderAssetParam: {
    id: 'assets.assetToken.param.policyId',
    defaultMessage: '!!!Policy Id',
    description: '"policyId" param.',
  },
  assetNamerenderAssetParam: {
    id: 'assets.assetToken.param.assetName',
    defaultMessage: '!!!Asset name',
    description: '"assetName" param.',
  },
  namerenderAssetParam: {
    id: 'assets.assetToken.param.name',
    defaultMessage: '!!!Name',
    description: '"name" param.',
  },
  tickerrenderAssetParam: {
    id: 'assets.assetToken.param.ticker',
    defaultMessage: '!!!Ticker',
    description: '"ticker" param.',
  },
  descriptionrenderAssetParam: {
    id: 'assets.assetToken.param.description',
    defaultMessage: '!!!Description',
    description: '"description" param.',
  },
  blank: {
    id: 'assets.assetToken.param.blank',
    defaultMessage: '!!!Blank',
    description: '"Blank" param value.',
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
  onCopyAssetParam?: Function,
  highlightFingerprint?: boolean,
  className?: string,
  intl: intlShape.isRequired,
  hasError?: boolean,
};

type ParamCopied = ?string;

const AssetContent = observer((props: Props) => {
  const [paramCopied, setParamCopied] = useState<ParamCopied>(null);

  let copyNotificationTimeout: TimeoutID;

  const handleCopyParam = (
    newParamCopied: string,
    param: string,
    fullValue: string
  ) => {
    const { onCopyAssetParam } = props;
    if (onCopyAssetParam) {
      onCopyAssetParam({ param, fullValue });
    }
    clearTimeout(copyNotificationTimeout);
    setParamCopied(newParamCopied);
    copyNotificationTimeout = setTimeout(() => {
      setParamCopied(null);
    }, ASSET_TOKEN_ID_COPY_FEEDBACK);
  };

  const renderAssetParam = (assetId: string, param: string, value: string) => {
    const icon = paramCopied === assetId ? copyCheckmarkIcon : copyIcon;
    const iconClassnames = classnames([
      styles.copyIcon,
      paramCopied === assetId ? styles.copiedIcon : null,
    ]);
    const onCopy = () => {
      handleCopyParam(assetId, param, value);
    };
    return (
      <CopyToClipboard text={value} onCopy={onCopy}>
        <div className={styles.assetParam}>
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
          {renderAssetParam(
            'fingerprint',
            intl.formatMessage(messages.fingerprintrenderAssetParam),
            fingerprint
          )}
        </div>
      )}
      <dl>
        {!highlightFingerprint && (
          <>
            <dt>{intl.formatMessage(messages.fingerprintrenderAssetParam)}</dt>
            <dd>
              {renderAssetParam(
                'fingerprint',
                intl.formatMessage(messages.fingerprintrenderAssetParam),
                fingerprint
              )}
            </dd>
          </>
        )}
        {ticker && (
          <>
            <dt>{intl.formatMessage(messages.tickerrenderAssetParam)}</dt>
            <dd>
              {renderAssetParam(
                'ticker',
                intl.formatMessage(messages.tickerrenderAssetParam),
                ticker
              )}
            </dd>
          </>
        )}
        {name && (
          <>
            <dt>{intl.formatMessage(messages.namerenderAssetParam)}</dt>
            <dd>
              {renderAssetParam(
                'name',
                intl.formatMessage(messages.namerenderAssetParam),
                name
              )}
            </dd>
          </>
        )}
        {description && (
          <>
            <dt>{intl.formatMessage(messages.descriptionrenderAssetParam)}</dt>
            <dd>
              {renderAssetParam(
                'description',
                intl.formatMessage(messages.descriptionrenderAssetParam),
                description
              )}
            </dd>
          </>
        )}
        <dt>{intl.formatMessage(messages.policyIdrenderAssetParam)}</dt>
        <dd>
          {renderAssetParam(
            'policyId',
            intl.formatMessage(messages.policyIdrenderAssetParam),
            policyId
          )}
        </dd>
        <dt>{intl.formatMessage(messages.assetNamerenderAssetParam)}</dt>
        <dd>
          {assetName ? (
            renderAssetParam(
              'assetName',
              intl.formatMessage(messages.assetNamerenderAssetParam),
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
