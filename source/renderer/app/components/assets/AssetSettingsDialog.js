// @flow
import React, { Component } from 'react';
import { range } from 'lodash';
import { observer } from 'mobx-react';
import { Select } from 'react-polymorph/lib/components/Select';
import { defineMessages, intlShape } from 'react-intl';
import AssetToken from './AssetToken';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import styles from './AssetSettingsDialog.scss';
import globalMessages from '../../i18n/global-messages';
import type { WalletSummaryAsset } from '../../api/assets/types';
import { formattedTokenWalletAmount } from '../../utils/formatters';
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
    id: 'assets.settings.dialog.formattedBalance.label',
    defaultMessage: '!!!Unformated balance',
    description: '"formattedBalanceLabel" for the Asset settings dialog',
  },
  unformattedBalanceLabel: {
    id: 'assets.settings.dialog.unformattedBalance.label',
    defaultMessage: '!!!Formated balance',
    description: '"unformattedBalanceLabel" for the Asset settings dialog',
  },
  decimalPrecisionLabel: {
    id: 'assets.settings.dialog.decimalPrecision.label',
    defaultMessage: '!!!Decimal Precision',
    description: '"decimalPrecisionLabel" for the Asset settings dialog',
  },
  recommended: {
    id: 'assets.settings.dialog.recommended',
    defaultMessage: '!!!(recommended)',
    description: '"recommended" for the Asset settings dialog',
  },
});

type Props = {
  asset: WalletSummaryAsset,
  onSubmit: Function,
  onCancel: Function,
  recommendedDecimals?: number,
};

type State = {
  decimals: number,
};

@observer
export default class AssetSettingsDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);
    const { asset, recommendedDecimals } = this.props;
    const decimals =
      typeof asset.decimals === 'number'
        ? asset.decimals
        : recommendedDecimals || DEFAULT_DECIMAL_PRECISION;
    this.state = {
      decimals,
    };
  }

  onSetDecimalPrecision = (decimals: number) => {
    this.setState({ decimals });
  };

  optionRenderer = ({ value }: { value: number }) => {
    const { intl } = this.context;
    const { recommendedDecimals } = this.props;
    if (recommendedDecimals && recommendedDecimals === value) {
      return (
        <div>
          {value}{' '}
          <em className={styles.recommended}>
            {intl.formatMessage(messages.recommended)}
          </em>
        </div>
      );
    }
    return value;
  };

  selectionRenderer = (props: { value: number }) => (
    <div className={styles.selection}>{this.optionRenderer(props)}</div>
  );

  renderTitle = () => {
    const { intl } = this.context;
    const { asset } = this.props;
    return (
      <>
        {intl.formatMessage(messages.title)}{' '}
        <div>
          <AssetToken asset={asset} hidePopOver />
        </div>
      </>
    );
  };

  render() {
    const { intl } = this.context;
    const { onCancel, onSubmit, asset } = this.props;
    const { decimals } = this.state;
    const title = this.renderTitle();
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
    return (
      <Dialog
        className={styles.component}
        title={title}
        actions={actions}
        closeOnOverlayClick
        onClose={onCancel}
        closeButton={<DialogCloseButton />}
      >
        <div>
          <p>{intl.formatMessage(messages.description)}</p>
          <div className={styles.label}>
            {intl.formatMessage(messages.unformattedBalanceLabel)}
          </div>
          <p>{formattedTokenWalletAmount(asset.quantity, asset.metadata, 0)}</p>
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
            value={decimals}
            className={styles.decimalsDropdown}
            label={intl.formatMessage(messages.decimalPrecisionLabel)}
            onChange={this.onSetDecimalPrecision}
            optionRenderer={this.optionRenderer}
            selectionRenderer={this.selectionRenderer}
          />
        </div>
      </Dialog>
    );
  }
}
