// @flow
// TODO: Remove once the new wallet creation process is ready
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Select } from 'react-polymorph/lib/components/Select';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import styles from './AssetSettingsDialog.scss';
import globalMessages from '../../i18n/global-messages';

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
});

type Props = {
  onSubmit: Function,
  onCancel: Function,
  recommendedDecimalPrecision?: number,
};

type State = {
  decimalPrecision: ?number,
};

@observer
export default class AssetSettingsDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    decimalPrecision: null,
  };

  submit = () => {
    console.log('SUBMIT');
  };

  get decimalPrecisionValue() {
    return this.state.decimalPrecision;
  }

  render() {
    const { intl } = this.context;
    const { onCancel, onSubmit } = this.props;

    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: onCancel,
      },
      {
        label: intl.formatMessage(globalMessages.save),
        primary: true,
        onClick: onSubmit,
      },
    ];

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onCancel}
        closeButton={<DialogCloseButton />}
      >
        <div>
          <Select
            options={[]}
            value={this.decimalPrecisionValue}
            className={styles.decimalPrecisionDropdown}
            label={intl.formatMessage(messages.decimalPrecisionLabel)}
          />
        </div>
      </Dialog>
    );
  }
}
