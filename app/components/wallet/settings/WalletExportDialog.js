// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './WalletExportDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'wallet.export.dialog.headline',
    defaultMessage: '!!!Export',
    description: 'headline for "export paper wallet" dialog.'
  },
  fullTabTitle: {
    id: 'wallet.export.choices.tab.title.full',
    defaultMessage: '!!!Full',
    description: 'Tab title "Full" on wallet export dialog.'
  },
  readOnlyTabTitle: {
    id: 'wallet.export.choices.tab.title.readOnly',
    defaultMessage: '!!!Read-only',
    description: 'Tab title "Read-only" on wallet export dialog.'
  },
  exportButtonLabel: {
    id: 'wallet.export.submit.label',
    defaultMessage: '!!!Export wallet',
    description: 'Label for export wallet submit button.'
  }
});

type ExportType = 'full' | 'readOnly';

type WalletExportDialogState = {
  exportType: ExportType,
};

@observer
export default class WalletExportDialog extends Component {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  props: {
    onClose: Function,
  };

  state: WalletExportDialogState;

  constructor(props: any, children: any) {
    super(props, children);
    this.state = { exportType: 'full' };
  }

  setState(object: WalletExportDialogState) {
    super.setState(object);
  }

  onChangeExportType(exportType: ExportType) {
    this.setState({ exportType });
  }

  render() {
    const { intl } = this.context;
    const { onClose } = this.props;
    const { exportType } = this.state;
    const dialogClasses = classnames([
      styles.component,
      'WalletExportDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.exportButtonLabel),
        primary: true,
        onClick: () => console.log('submit'),
      }
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.choices}>
          <button
            className={exportType === 'full' ? styles.activeButton : ''}
            onClick={() => this.onChangeExportType('full')}
          >
            {intl.formatMessage(messages.fullTabTitle)}
          </button>
          <button
            className={exportType === 'readOnly' ? styles.activeButton : ''}
            onClick={() => this.onChangeExportType('readOnly')}
          >
            {intl.formatMessage(messages.readOnlyTabTitle)}
          </button>
        </div>

      </Dialog>
    );
  }

}
