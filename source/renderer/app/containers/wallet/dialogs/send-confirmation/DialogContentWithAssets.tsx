import React, { Fragment } from 'react';
import compose from 'lodash/fp/compose';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { injectIntl, FormattedHTMLMessage } from 'react-intl';
import questionMarkIcon from '../../../../assets/images/question-mark.inline.svg';
import Asset from '../../../../components/assets/Asset';
import globalMessages from '../../../../i18n/global-messages';
import { DialogContentWithAssets as Props } from './types';
import { messages } from './messages';
import { getFormattedAssetAmount } from './helpers';
import styles from './DialogContentWithAssets.scss';

function Component({
  intl,
  amount,
  receiver,
  transactionFee,
  isHardwareWallet,
  selectedAssets,
  assetsAmounts,
  onCopyAssetParam,
}: Props) {
  const assetsSeparatorBasicHeight = 27;
  const assetsSeparatorCalculatedHeight = selectedAssets.length
    ? assetsSeparatorBasicHeight * selectedAssets.length * 2 - 18
    : assetsSeparatorBasicHeight;

  return (
    <div className={styles.root}>
      <div className={styles.addressToLabelWrapper}>
        <div className={styles.receiverRow}>
          <div className={styles.receiverRowItem}>
            <h2>{intl.formatMessage(messages.receiverLabel)}</h2>
            <div className={styles.receiverRowItemAddresses}>
              <p className={styles.addressTo}>{receiver}</p>
              <div className={styles.assetsWrapper}>
                <div
                  className={styles.assetsSeparator}
                  style={{
                    height: `${assetsSeparatorCalculatedHeight}px`,
                    top: `${assetsSeparatorCalculatedHeight + 5}px`,
                    marginTop: `-${assetsSeparatorCalculatedHeight + 5}px`,
                  }}
                />
                <div className={styles.assetsContainer}>
                  <h3>
                    <span>{intl.formatMessage(globalMessages.adaName)}</span>
                  </h3>
                  <div className={styles.amountFeesWrapper}>
                    <div className={styles.amount}>
                      {amount} {intl.formatMessage(globalMessages.adaUnit)}
                    </div>
                  </div>
                </div>
                {selectedAssets.map(
                  (asset, index) =>
                    asset?.uniqueId && (
                      <Fragment key={asset.uniqueId}>
                        <div className={styles.assetsContainer}>
                          <h3>
                            <span>
                              {intl.formatMessage(messages.assetLabel)}
                              &nbsp;#{index + 1}
                            </span>
                            <Asset
                              asset={asset}
                              onCopyAssetParam={onCopyAssetParam}
                              className={styles.assetToken}
                            />
                          </h3>
                          <div className={styles.amountFeesWrapper}>
                            <div className={styles.amount}>
                              {getFormattedAssetAmount(
                                asset,
                                Number(assetsAmounts[index])
                              )}
                            </div>
                          </div>
                        </div>
                        <div className={styles.assetsContainer}>
                          <div className={styles.unformattedAmountLine} />
                          <div className={styles.unformattedAmountLabel}>
                            {intl.formatMessage(
                              messages.unformattedAmountLabel
                            )}
                            <PopOver
                              content={
                                <div className="UnformattedAmountTooltip">
                                  <FormattedHTMLMessage
                                    {...messages[
                                      isHardwareWallet
                                        ? 'unformattedAmountMessageForHardwareWallets'
                                        : 'unformattedAmountMessageForSoftwareWallets'
                                    ]}
                                    tagName="div"
                                  />
                                </div>
                              }
                              key="tooltip"
                            >
                              <div className={styles.questionMark}>
                                <SVGInline svg={questionMarkIcon} />
                              </div>
                            </PopOver>
                            {':'}
                          </div>
                          <div className={styles.unformattedAmount}>
                            {assetsAmounts[index] || 0}
                          </div>
                        </div>
                      </Fragment>
                    )
                )}
              </div>
            </div>
          </div>
        </div>
      </div>

      <div className={styles.feesWrapper}>
        <div className={styles.feesLabel}>
          {intl.formatMessage(messages.feesLabel)}
        </div>
        <div className={styles.fees}>
          +{transactionFee}
          <span>&nbsp;{intl.formatMessage(globalMessages.adaUnit)}</span>
        </div>
      </div>
    </div>
  );
}

export const DialogContentWithAssets = compose(
  injectIntl,
  observer
)(Component) as React.ComponentType<Omit<Props, 'intl'>>;
