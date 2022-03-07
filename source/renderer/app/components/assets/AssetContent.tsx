import React, { useState } from 'react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AssetContent.scss' or its co... Remove this comment to see the full error message
import styles from './AssetContent.scss';
import { hexToString } from '../../utils/strings';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/copy-asset... Remove this comment to see the full error message
import copyIcon from '../../assets/images/copy-asset.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/check-w.in... Remove this comment to see the full error message
import copyCheckmarkIcon from '../../assets/images/check-w.inline.svg';
import { ASSET_TOKEN_ID_COPY_FEEDBACK } from '../../config/timingConfig';
import type { Asset as AssetProps } from '../../api/assets/types';

const messages = defineMessages({
  fingerprintAssetParam: {
    id: 'assets.assetToken.param.fingerprint',
    defaultMessage: '!!!Fingerprint',
    description: '"fingerprint" param.',
  },
  policyIdAssetParam: {
    id: 'assets.assetToken.param.policyId',
    defaultMessage: '!!!Policy Id',
    description: '"policyId" param.',
  },
  assetNameAssetParam: {
    id: 'assets.assetToken.param.assetName',
    defaultMessage: '!!!Asset name',
    description: '"assetName" param.',
  },
  nameAssetParam: {
    id: 'assets.assetToken.param.name',
    defaultMessage: '!!!Name',
    description: '"name" param.',
  },
  tickerAssetParam: {
    id: 'assets.assetToken.param.ticker',
    defaultMessage: '!!!Ticker',
    description: '"ticker" param.',
  },
  descriptionAssetParam: {
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
  asset: AssetProps;
  onCopyAssetParam?: (...args: Array<any>) => any;
  highlightFingerprint?: boolean;
  className?: string;
  intl: intlShape.isRequired;
  hasError?: boolean;
};
type ParamCopied = string | null | undefined;
const AssetContent = observer((props: Props) => {
  const [paramCopied, setParamCopied] = useState<ParamCopied>(null);
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'TimeoutID'.
  let copyNotificationTimeout: TimeoutID;

  const handleCopyParam = (
    newParamCopied: string,
    param: string,
    fullValue: string
  ) => {
    const { onCopyAssetParam } = props;

    if (onCopyAssetParam) {
      onCopyAssetParam({
        param,
        fullValue,
      });
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
            intl.formatMessage(messages.fingerprintAssetParam),
            fingerprint
          )}
        </div>
      )}
      <dl>
        {!highlightFingerprint && (
          <>
            <dt>{intl.formatMessage(messages.fingerprintAssetParam)}</dt>
            <dd>
              {renderAssetParam(
                'fingerprint',
                intl.formatMessage(messages.fingerprintAssetParam),
                fingerprint
              )}
            </dd>
          </>
        )}
        {ticker && (
          <>
            <dt>{intl.formatMessage(messages.tickerAssetParam)}</dt>
            <dd>
              {renderAssetParam(
                'ticker',
                intl.formatMessage(messages.tickerAssetParam),
                ticker
              )}
            </dd>
          </>
        )}
        {name && (
          <>
            <dt>{intl.formatMessage(messages.nameAssetParam)}</dt>
            <dd>
              {renderAssetParam(
                'name',
                intl.formatMessage(messages.nameAssetParam),
                name
              )}
            </dd>
          </>
        )}
        {description && (
          <>
            <dt>{intl.formatMessage(messages.descriptionAssetParam)}</dt>
            <dd>
              {renderAssetParam(
                'description',
                intl.formatMessage(messages.descriptionAssetParam),
                description
              )}
            </dd>
          </>
        )}
        <dt>{intl.formatMessage(messages.policyIdAssetParam)}</dt>
        <dd>
          {renderAssetParam(
            'policyId',
            intl.formatMessage(messages.policyIdAssetParam),
            policyId
          )}
        </dd>
        <dt>{intl.formatMessage(messages.assetNameAssetParam)}</dt>
        <dd>
          {assetName ? (
            renderAssetParam(
              'assetName',
              intl.formatMessage(messages.assetNameAssetParam),
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
