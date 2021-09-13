// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import type { Field } from 'mobx-react-form';
import { intlShape } from 'react-intl';
import { get, orderBy } from 'lodash';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import AmountInputSkin from '../skins/AmountInputSkin';
import AssetsDropdown from '../../widgets/forms/AssetsDropdown';
import closeIcon from '../../../assets/images/close-cross.inline.svg';
import { formattedTokenWalletAmount } from '../../../utils/formatters';
import type { NumberFormat } from '../../../../../common/types/number.types';
import type { AssetToken } from '../../../api/assets/types';
import styles from './AssetInput.scss';
import messages from './messages';

type Props = {
  uniqueId: string,
  index: number,
  getAssetByUniqueId: Function,
  availableAssets: Array<AssetToken>,
  assetFields: {
    [uniqueId: string]: Field,
  },
  assetsDropdown: {
    [uniqueId: string]: Field,
  },
  addFocusableField: Function,
  removeAssetButtonVisible: { [uniqueId: string]: boolean },
  showRemoveAssetButton: Function,
  hideRemoveAssetButton: Function,
  currentNumberFormat: NumberFormat,
  removeAssetRow: Function,
  handleSubmitOnEnter: Function,
  clearAssetFieldValue: Function,
  onChangeAsset: Function,
  autoFocus: boolean,
};

const INPUT_FIELD_PADDING_DELTA = 10;

@observer
export default class AssetInput extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  rightContentRef: { current: null | HTMLDivElement };

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
      index,
      getAssetByUniqueId,
      availableAssets,
      assetFields,
      assetsDropdown,
      addFocusableField,
      removeAssetButtonVisible,
      showRemoveAssetButton,
      hideRemoveAssetButton,
      currentNumberFormat,
      removeAssetRow,
      handleSubmitOnEnter,
      clearAssetFieldValue,
      onChangeAsset,
      autoFocus,
    } = this.props;
    const asset = getAssetByUniqueId(uniqueId);
    if (!asset) {
      return false;
    }

    const { quantity, metadata, decimals } = asset;
    const ticker = get(metadata, 'ticker', null);
    const sortedAssets = orderBy(
      [asset, ...availableAssets],
      'uniqueId',
      'asc'
    );
    const assetField = assetFields[uniqueId];
    const assetsDropdownField = assetsDropdown[uniqueId];
    const inputFieldStyle = this.generateInputFieldStyle();

    return (
      <div
        key={`receiver_asset_${uniqueId}`}
        onMouseOver={() => showRemoveAssetButton(uniqueId)}
        onMouseLeave={() => hideRemoveAssetButton(uniqueId)}
        onMouseEnter={() => showRemoveAssetButton(uniqueId)}
        onFocus={() => {
          // jsx-a11y/mouse-events-have-key-events
        }}
        className={styles.component}
      >
        {quantity.isPositive() && (
          <div className={styles.amountTokenTotal}>
            {intl.formatMessage(messages.ofLabel)}
            {` `}
            {formattedTokenWalletAmount(quantity, metadata, decimals)}
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
            <>
              {`${intl.formatMessage(messages.assetLabel)} #${index + 1}`}
              {removeAssetButtonVisible[uniqueId] && (
                <span
                  className={classNames([styles.removeAssetButton, 'flat'])}
                  onClick={() => removeAssetRow(uniqueId)}
                >
                  {intl.formatMessage(messages.removeLabel)}
                </span>
              )}
            </>
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
          onKeyPress={(evt: SyntheticKeyboardEvent<EventTarget>) => {
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
          <div className={styles.assetsDropdownWrapper}>
            <AssetsDropdown
              className={styles.assetsDropdown}
              {...assetsDropdownField.bind()}
              assets={sortedAssets}
              onChange={onChangeAsset}
              value={uniqueId}
              hasSearch
            />
          </div>
        </div>
      </div>
    );
  }
}
