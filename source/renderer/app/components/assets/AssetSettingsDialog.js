// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { range } from 'lodash';
import { observer } from 'mobx-react';
import { Select } from 'react-polymorph/lib/components/Select';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import AssetToken from './AssetToken';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import styles from './AssetSettingsDialog.scss';
import globalMessages from '../../i18n/global-messages';
import type { WalletSummaryAsset } from '../../api/assets/types';
import { formattedTokenWalletAmount } from '../../utils/formatters';
import warningIcon from '../../assets/images/asset-token-warning-ic.inline.svg';
import {
  DEFAULT_DECIMAL_PRECISION,
  MAX_DECIMAL_PRECISION,
} from '../../config/assetsConfig';

const messages = defineMessages({
  title: {
    id: 'assets.settings.dialog.title',
    defaultMessage: '!!!Native token settings',
    description: '"title" for the Asset settings dialog',
  },
  description: {
    id: 'assets.settings.dialog.description',
    defaultMessage:
      '!!!Updates made here will be applied in other wallets containing this token too.',
    description: '"description" for the Asset settings dialog',
  },
  formattedBalanceLabel: {
    id: 'assets.settings.dialog.formattedAmount.label',
    defaultMessage: '!!!Unformated amount',
    description: '"formattedBalanceLabel" for the Asset settings dialog',
  },
  unformattedBalanceLabel: {
    id: 'assets.settings.dialog.unformattedAmount.label',
    defaultMessage: '!!!Formated amount',
    description: '"unformattedBalanceLabel" for the Asset settings dialog',
  },
  decimalPrecisionLabel: {
    id: 'assets.settings.dialog.decimalPrecision.label',
    defaultMessage: '!!!Number of decimal places',
    description: '"decimalPrecisionLabel" for the Asset settings dialog',
  },
  recommended: {
    id: 'assets.settings.dialog.recommended',
    defaultMessage: '!!!(recommended)',
    description: '"recommended" for the Asset settings dialog',
  },
  default: {
    id: 'assets.settings.dialog.default',
    defaultMessage: '!!!(default)',
    description: '"default" for the Asset settings dialog',
  },
  warningPopOverAvailable: {
    id: 'assets.warning.available',
    defaultMessage:
      '!!!Recommended configuration for decimal places for this native token is available.',
    description: 'Asset settings recommended pop over content',
  },
  warningPopOverNotUsing: {
    id: 'assets.warning.notUsing',
    defaultMessage:
      '!!!You are not using the recommended decimal place configuration for this native token.',
    description: 'Asset settings recommended pop over content',
  },
});

type Props = {
  asset: WalletSummaryAsset,
  onSubmit: Function,
  onCancel: Function,
};

type State = {
  decimals: ?number,
};

@observer
export default class AssetSettingsDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);
    const { asset } = this.props;
    const { decimals } = asset;
    this.state = {
      decimals,
    };
  }

  onSetDecimalPrecision = (decimals: number) => {
    this.setState({ decimals });
  };

  optionRenderer = (props: { value: number }) => {
    const { value } = props;
    const { intl } = this.context;
    const { recommendedDecimals } = this.props.asset;
    let extraLabel;
    if (
      typeof recommendedDecimals === 'number' &&
      recommendedDecimals === value
    ) {
      extraLabel = messages.recommended;
    } else if (value === DEFAULT_DECIMAL_PRECISION) {
      extraLabel = messages.default;
    }
    if (extraLabel) {
      return (
        <div>
          {value} <i>{intl.formatMessage(extraLabel)}</i>
        </div>
      );
    }
    return value;
  };

  selectionRenderer = (props: { value: number }) => (
    <div className={styles.selection}>{this.optionRenderer(props)}</div>
  );

  render() {
    const { intl } = this.context;
    const { onCancel, onSubmit, asset } = this.props;
    const { recommendedDecimals } = asset;
    const { decimals } = this.state;
    const options = range(MAX_DECIMAL_PRECISION + 1).map((value) => ({
      value,
    }));
    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: onCancel,
      },
      {
        label: intl.formatMessage(globalMessages.save),
        primary: true,
        onClick: () => onSubmit(asset, decimals),
      },
    ];
    const hasWarning =
      typeof recommendedDecimals === 'number' &&
      decimals !== recommendedDecimals;
    let warningPopOverMessage;
    if (hasWarning) {
      warningPopOverMessage =
        typeof decimals === 'number'
          ? messages.warningPopOverNotUsing
          : messages.warningPopOverAvailable;
    }

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.title)}
        subtitle={<AssetToken asset={asset} className={styles.assetToken} />}
        actions={actions}
        closeOnOverlayClick
        onClose={onCancel}
        closeButton={<DialogCloseButton />}
        className1={styles.dialog}
      >
        <div>
          <div className={styles.description}>
            <FormattedHTMLMessage {...messages.description} />
          </div>
          <div className={styles.label}>
            {intl.formatMessage(messages.unformattedBalanceLabel)}
          </div>
          <p>{formattedTokenWalletAmount(asset.quantity, null, 0)}</p>
          <div className={styles.label}>
            {intl.formatMessage(messages.formattedBalanceLabel)}
          </div>
          <p>
            {formattedTokenWalletAmount(
              asset.quantity,
              asset.metadata,
              decimals
            )}
          </p>
          <Select
            options={options}
            value={decimals || DEFAULT_DECIMAL_PRECISION}
            className={styles.decimalsDropdown}
            label={
              <span className={styles.decimalsDropdownLabel}>
                {intl.formatMessage(messages.decimalPrecisionLabel)}
                {hasWarning && (
                  <PopOver content={intl.formatMessage(warningPopOverMessage)}>
                    <SVGInline
                      className={styles.warningIcon}
                      svg={warningIcon}
                    />
                  </PopOver>
                )}
              </span>
            }
            onChange={this.onSetDecimalPrecision}
            optionRenderer={this.optionRenderer}
            selectionRenderer={this.selectionRenderer}
          />
        </div>
      </Dialog>
    );
  }
}
