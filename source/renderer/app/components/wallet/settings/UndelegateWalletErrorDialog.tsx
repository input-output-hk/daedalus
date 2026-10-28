import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape, FormattedHTMLMessage } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import ApiError from '../../../domains/ApiError';
import { messages as confirmationMessages } from './UndelegateWalletConfirmationDialog';

type Props = {
  error: LocalizableError | ApiError;
  onClose: () => void;
};

@observer
class UndelegateWalletErrorDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { error, onClose } = this.props;
    const actions = [
      {
        label: intl.formatMessage(globalMessages.close),
        onClick: onClose,
        primary: true,
      },
    ];
    return (
      <Dialog
        title={intl.formatMessage(confirmationMessages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <p>
          <FormattedHTMLMessage {...error} />
        </p>
      </Dialog>
    );
  }
}

export default UndelegateWalletErrorDialog;
