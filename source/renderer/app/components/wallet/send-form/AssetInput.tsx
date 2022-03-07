import React, { Component } from 'react';
import { observer } from 'mobx-react';
import type { Field } from 'mobx-react-form';
import { intlShape } from 'react-intl';
import { get } from 'lodash';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import AmountInputSkin from '../skins/AmountInputSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import closeIcon from '../../../assets/images/close-cross.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/remove.... Remove this comment to see the full error message
import removeIcon from '../../../assets/images/remove.inline.svg';
import type { NumberFormat } from '../../../../../common/types/number.types';
import { DiscreetTokenWalletAmount } from '../../../features/discreet-mode';
import Asset from '../../assets/Asset';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AssetInput.scss' or its corr... Remove this comment to see the full error message
import styles from './AssetInput.scss';
import messages from './messages';

type Props = {
  uniqueId: string;
  getAssetByUniqueId: (...args: Array<any>) => any;
  assetFields: Record<string, Field>;
  addFocusableField: (...args: Array<any>) => any;
  currentNumberFormat: NumberFormat;
  removeAssetRow: (...args: Array<any>) => any;
  handleSubmitOnEnter: (...args: Array<any>) => any;
  clearAssetFieldValue: (...args: Array<any>) => any;
  autoFocus: boolean;
};
const INPUT_FIELD_PADDING_DELTA = 10;

@observer
class AssetInput extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  rightContentRef: {
    current: null | HTMLDivElement;
  };

  constructor(props: Props) {
    super(props);
    this.rightContentRef = React.createRef();
  }

  hasAssetValue = (asset: Field) => {
    return get(asset, 'value', false);
  };
  generateInputFieldStyle = () => {
    const { current: rightContentDom } = this.rightContentRef;

    if (!rightContentDom) {
      return null;
    }

    const rightContentDomRect = rightContentDom.getBoundingClientRect();
    return {
      paddingRight: `${
        rightContentDomRect.width + INPUT_FIELD_PADDING_DELTA
      }px`,
    };
  };

  render() {
    const { intl } = this.context;
    const {
      uniqueId,
      getAssetByUniqueId,
      assetFields,
      addFocusableField,
      currentNumberFormat,
      removeAssetRow,
      handleSubmitOnEnter,
      clearAssetFieldValue,
      autoFocus,
    } = this.props;
    const asset = getAssetByUniqueId(uniqueId);

    if (!asset) {
      return false;
    }

    const { quantity, metadata, decimals } = asset;
    const ticker = get(metadata, 'ticker', null);
    const assetField = assetFields[uniqueId];
    const inputFieldStyle = this.generateInputFieldStyle();
    return (
      <div key={`receiver_asset_${uniqueId}`} className={styles.component}>
        <div className={styles.inputBlock}>
          {quantity.isPositive() && (
            <div className={styles.amountTokenTotal}>
              {intl.formatMessage(messages.ofLabel)}
              <span className={styles.amountValue}>
                <DiscreetTokenWalletAmount
                  amount={quantity}
                  metadata={metadata}
                  decimals={decimals}
                />
              </span>
            </div>
          )}
          <NumericInput
            {...assetField.bind()}
            ref={(field) => addFocusableField(field)}
            placeholder={
              decimals
                ? `0${currentNumberFormat.decimalSeparator}${'0'.repeat(
                    decimals
                  )}`
                : '0'
            }
            className={styles.assetItem}
            label={
              <Asset
                asset={asset}
                className={styles.assetToken}
                hidePopOver
                small
              />
            }
            bigNumberFormat={decimals ? currentNumberFormat : null}
            decimalPlaces={decimals}
            numberLocaleOptions={{
              minimumFractionDigits: decimals,
            }}
            onChange={(value) => {
              assetField.onChange(value);
            }}
            currency={ticker}
            value={assetField.value}
            error={assetField.error}
            skin={AmountInputSkin}
            style={inputFieldStyle}
            onKeyPress={(evt: React.KeyboardEvent<EventTarget>) => {
              if (decimals === 0) {
                const { charCode } = evt;

                if (charCode === 190 || charCode === 110 || charCode === 46) {
                  evt.persist();
                  evt.preventDefault();
                  evt.stopPropagation();
                }
              }

              handleSubmitOnEnter(evt);
            }}
            allowSigns={false}
            autoFocus={autoFocus}
          />
          <div className={styles.rightContent} ref={this.rightContentRef}>
            {this.hasAssetValue(assetField) && (
              <div className={styles.clearAssetContainer}>
                <PopOver
                  content={intl.formatMessage(messages.clearLabel)}
                  placement="top"
                >
                  <button
                    onClick={() => clearAssetFieldValue(assetField)}
                    className={styles.clearAssetButton}
                  >
                    <SVGInline
                      svg={closeIcon}
                      className={styles.clearReceiverIcon}
                    />
                  </button>
                </PopOver>
                <div className={styles.separator} />
              </div>
            )}
            {ticker ? (
              <>
                <span className={styles.divider} />
                <span className={styles.ticker}>{ticker}</span>
              </>
            ) : null}
          </div>
        </div>
        <div className={styles.removeAssetBlock}>
          <span
            className={classNames([styles.removeAssetButton, 'flat'])}
            onClick={() => removeAssetRow(uniqueId)}
            data-testid={`remove-asset-${uniqueId}`}
          >
            <SVGInline svg={removeIcon} className={styles.removeIcon} />
          </span>
        </div>
      </div>
    );
  }
}

export default AssetInput;
