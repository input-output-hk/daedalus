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
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import closeIcon from '../../../assets/images/close-cross.inline.svg';
import type { NumberFormat } from '../../../../../common/types/number.types';
import type { AssetToken } from '../../../api/assets/types';
import { DiscreetTokenWalletAmount } from '../../../features/discreet-mode';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AssetInput.scss' or its corr... Remove this comment to see the full error message
import styles from './AssetInput.scss';
import messages from './messages';

type Props = {
  uniqueId: string;
  index: number;
  getAssetByUniqueId: (...args: Array<any>) => any;
  availableAssets: Array<AssetToken>;
  assetFields: Record<string, Field>;
  assetsDropdown: Record<string, Field>;
  addFocusableField: (...args: Array<any>) => any;
  removeAssetButtonVisible: Record<string, boolean>;
  showRemoveAssetButton: (...args: Array<any>) => any;
  hideRemoveAssetButton: (...args: Array<any>) => any;
  currentNumberFormat: NumberFormat;
  removeAssetRow: (...args: Array<any>) => any;
  handleSubmitOnEnter: (...args: Array<any>) => any;
  clearAssetFieldValue: (...args: Array<any>) => any;
  onChangeAsset: (...args: Array<any>) => any;
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
            <DiscreetTokenWalletAmount
              amount={quantity}
              metadata={metadata}
              decimals={decimals}
            />
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

export default AssetInput;
