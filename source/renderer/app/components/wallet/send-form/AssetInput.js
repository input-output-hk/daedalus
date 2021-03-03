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
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import closeIcon from '../../../assets/images/close-cross.inline.svg';
import { formattedTokenWalletAmount } from '../../../utils/formatters';
import type { NumberFormat } from '../../../../../common/types/number.types';
import type { WalletSummaryAsset } from '../../../api/assets/types';
import styles from './AssetInput.scss';
import messages from './messages';

type Props = {
  fingerprint: string,
  index: number,
  getAssetByFingerprint: Function,
  availableAssets: Array<WalletSummaryAsset>,
  receiverFormField: {
    receiver: Field,
    adaAmount: Field,
    assetFields: {
      [fingerprint: string]: Field,
    },
    assetsDropdown: {
      [fingerprint: string]: Field,
    },
  },
  addFocusableField: Function,
  removeAssetButtonVisible: { [fingerprint: string]: boolean },
  showRemoveAssetButton: Function,
  hideRemoveAssetButton: Function,
  currentNumberFormat: NumberFormat,
  removeAssetRow: Function,
  handleSubmitOnEnter: Function,
  clearAssetFieldValue: Function,
  onChangeAsset: Function,
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
      fingerprint,
      index,
      getAssetByFingerprint,
      availableAssets,
      receiverFormField,
      addFocusableField,
      removeAssetButtonVisible,
      showRemoveAssetButton,
      hideRemoveAssetButton,
      currentNumberFormat,
      removeAssetRow,
      handleSubmitOnEnter,
      clearAssetFieldValue,
      onChangeAsset,
    } = this.props;
    const { assetFields, assetsDropdown } = receiverFormField;
    const asset = getAssetByFingerprint(fingerprint);
    if (!asset) {
      return false;
    }

    const { quantity, metadata } = asset;
    const acronym = get(metadata, 'acronym', null);
    const decimals = get(metadata, 'unit.decimals', 0);
    const sortedAssets = orderBy(
      [asset, ...availableAssets],
      'fingerprint',
      'asc'
    );
    const assetField = assetFields[fingerprint];
    const assetsDropdownField = assetsDropdown[fingerprint];
    const inputFieldStyle = this.generateInputFieldStyle();

    return (
      <div
        key={`receiver_asset_${fingerprint}`}
        onMouseOver={() => showRemoveAssetButton(fingerprint)}
        onMouseLeave={() => hideRemoveAssetButton(fingerprint)}
        onMouseEnter={() => showRemoveAssetButton(fingerprint)}
        onFocus={() => {
          // jsx-a11y/mouse-events-have-key-events
        }}
        className={styles.component}
      >
        {quantity.isPositive() && (
          <div className={styles.amountTokenTotal}>
            {intl.formatMessage(messages.ofLabel)}
            {` `}
            {formattedTokenWalletAmount(quantity, metadata)}
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
              {removeAssetButtonVisible[fingerprint] && (
                <span
                  className={classNames([styles.removeAssetButton, 'flat'])}
                  onClick={() => removeAssetRow(fingerprint)}
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
          currency={acronym}
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
            <WalletsDropdown
              className={styles.assetsDropdown}
              {...assetsDropdownField.bind()}
              assets={sortedAssets}
              onChange={(newFingerprint) => {
                if (newFingerprint !== fingerprint) {
                  onChangeAsset(fingerprint, newFingerprint);
                }
              }}
              syncingLabel={intl.formatMessage(messages.syncingWallet)}
              hasAssetsEnabled
              value={fingerprint}
              getStakePoolById={() => {}}
              errorPosition="bottom"
            />
          </div>
        </div>
      </div>
    );
  }
}
