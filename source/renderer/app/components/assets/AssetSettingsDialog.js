// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { range } from 'lodash';
import { observer } from 'mobx-react';
import { Select } from 'react-polymorph/lib/components/Select';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { defineMessages, intlShape } from 'react-intl';
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
    defaultMessage: '!!!Number of decimal places',
    description: '"decimalPrecisionLabel" for the Asset settings dialog',
  },
  recommended: {
    id: 'assets.settings.dialog.recommended',
    defaultMessage: '!!!(recommended)',
    description: '"recommended" for the Asset settings dialog',
  },
  recommendedPopOver: {
    id: 'assets.settings.dialog.recommended.popOver',
    defaultMessage:
      '!!!Recommended configuration for decimal places for this native token is available.',
    description: '"recommendedPopOver" for the Asset settings dialog',
  },
});

type Props = {
  asset: WalletSummaryAsset,
  onSubmit: Function,
  onCancel: Function,
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
    const { asset } = this.props;
    const { decimals = DEFAULT_DECIMAL_PRECISION } = asset;
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
    const isRecommended =
      typeof recommendedDecimals === 'number' && recommendedDecimals === value;
    if (isRecommended) {
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
    const { recommendedDecimals } = asset;
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
    const hasRecommendedWarning =
      typeof recommendedDecimals === 'number' &&
      decimals !== recommendedDecimals;
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
            label={
              <>
                {intl.formatMessage(messages.decimalPrecisionLabel)}
                {hasRecommendedWarning && (
                  <PopOver
                    content={intl.formatMessage(messages.recommendedPopOver)}
                  >
                    <SVGInline
                      className={styles.warningIcon}
                      svg={warningIcon}
                    />
                  </PopOver>
                )}
              </>
            }
            onChange={this.onSetDecimalPrecision}
            optionRenderer={this.optionRenderer}
            selectionRenderer={this.selectionRenderer}
            error={
              hasRecommendedWarning &&
              intl.formatMessage(messages.recommendedPopOver)
            }
          />
        </div>
      </Dialog>
    );
  }
}
