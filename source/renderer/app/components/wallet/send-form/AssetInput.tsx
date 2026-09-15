import React, { Component } from 'react';
import { observer } from 'mobx-react';
import type { Field } from 'mobx-react-form';
import { intlShape } from 'react-intl';
import { get } from 'lodash';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import AmountInputSkin from '../skins/AmountInputSkin';
import removeIcon from '../../../assets/images/remove.inline.svg';
import type { NumberFormat } from '../../../../../common/types/number.types';
import { ellipsis } from '../../../utils/strings';
import { DiscreetTokenWalletAmount } from '../../../features/discreet-mode';
import Asset from '../../assets/Asset';
import { VerticalSeparator } from '../widgets/VerticalSeparator';
import { ClearButton } from '../widgets/ClearButton';
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
  /**
   * The decimal places this row is denominated in, snapshotted when the row was
   * added. Deliberately a prop and not a read of the asset: the asset's value
   * can change under an open field, and the whole point of the snapshot is that
   * the row does not move with it.
   */
  decimals: number | null | undefined;
  /**
   * Whether a resolution moved this asset's decimal places while the field held
   * an amount. The amount was cleared and the row says why.
   */
  hasDenominationChanged: boolean;
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
      decimals,
      hasDenominationChanged,
    } = this.props;
    const asset = getAssetByUniqueId(uniqueId);

    if (!asset) {
      return false;
    }

    // Everything in this row is drawn in the snapshotted denomination, the
    // balance beside the field included, so the amount a user compares against
    // is in the units the field is accepting.
    const { quantity, metadata } = asset;
    const ticker = get(metadata, 'ticker', null);
    // The unit the field is denominated in, for the label below it. A published
    // ticker where there is one, and otherwise the fingerprint in the same
    // spelling the pill above the field uses, so the two name the same thing.
    //
    // Never the decoded asset name: those bytes are chosen by whoever minted the
    // token and an asset whose name spells an existing ticker is free to exist,
    // which is the one confusion this label must not introduce.
    const unit = ticker || ellipsis(get(asset, 'fingerprint', '') || '', 9, 4);
    // A ledger quantity is an integer and decimal places are presentation
    // only, so a field whose decimal places are unknown, or known to be zero,
    // is denominated in raw units. A decimal separator typed into it means
    // nothing, and the submit path strips it rather than interpreting it, so
    // the field must not accept one in the first place.
    const areDecimalsKnown = decimals != null;
    const isInRawUnits = !areDecimalsKnown || decimals === 0;
    const assetField = assetFields[uniqueId];
    const inputFieldStyle = this.generateInputFieldStyle();
    // Computed from the same local the input's props are, so the label cannot
    // describe a denomination the field is not accepting.
    const unitLabel = areDecimalsKnown
      ? intl.formatMessage(messages.assetInputDecimalUnitsLabel, {
          unit,
          decimals,
        })
      : intl.formatMessage(messages.assetInputRawUnitsLabel, { unit });
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
            label={<Asset asset={asset} hidePopOver small />}
            data-testid={`assetInput:${uniqueId}`}
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
            onKeyPress={handleSubmitOnEnter}
            allowOnlyIntegers={isInRawUnits}
            allowSigns={false}
            autoFocus={autoFocus}
          />
          {hasDenominationChanged && (
            <div
              className={styles.denominationNotice}
              data-testid={`assetDenominationNotice:${uniqueId}`}
            >
              {intl.formatMessage(
                messages.assetInputDenominationChangedNotice,
                { unit }
              )}
            </div>
          )}
          <div
            className={styles.unitLabel}
            data-testid={`assetUnitLabel:${uniqueId}`}
          >
            {unitLabel}
          </div>
          <div className={styles.rightContent} ref={this.rightContentRef}>
            {this.hasAssetValue(assetField) && (
              <div className={styles.clearAssetContainer}>
                <ClearButton
                  label={intl.formatMessage(messages.clearLabel)}
                  onClick={() => clearAssetFieldValue(assetField)}
                />
              </div>
            )}
            {ticker ? (
              <>
                <VerticalSeparator />
                <span className={styles.ticker}>{ticker}</span>
              </>
            ) : null}
          </div>
        </div>
        <div className={styles.removeAssetBlock}>
          <span
            className={classNames([styles.removeAssetButton, 'flat'])}
            onClick={() => removeAssetRow(uniqueId)}
            data-testid={`removeAsset:${uniqueId}`}
          >
            <SVGInline svg={removeIcon} className={styles.removeIcon} />
          </span>
        </div>
      </div>
    );
  }
}

export default AssetInput;
