import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import globalMessages from '../../../i18n/global-messages';
import styles from './ToggleRTSFlagsDialog.scss';

const messages = defineMessages({
  enableRTSFlagsModeHeadline: {
    id: 'knownIssues.dialog.enableRtsFlagsMode.title',
    defaultMessage: '!!!Enable RTS flags (RAM management system)',
    description: 'Headline for the RTS flags dialog - when enabling',
  },
  enableRTSFlagsModeExplanation: {
    id: 'knownIssues.dialog.enableRtsFlagsMode.explanation',
    defaultMessage:
      '!!!When enabled, the Cardano node will attempt to reduce its RAM usage. The node will restart automatically to apply this change.',
    description: 'Main body of the dialog - when enabling',
  },
  enableRTSFlagsModeActionButton: {
    id: 'knownIssues.dialog.enableRtsFlagsMode.actionButton',
    defaultMessage: '!!!Enable',
    description: 'Enable RTS flags button label',
  },
  disableRTSFlagsModeHeadline: {
    id: 'knownIssues.dialog.disableRtsFlagsMode.title',
    defaultMessage: '!!!Disable RTS flags (RAM management system)',
    description: 'Headline for the RTS flags dialog - when disabling',
  },
  disableRTSFlagsModeExplanation: {
    id: 'knownIssues.dialog.disableRtsFlagsMode.explanation',
    defaultMessage:
      '!!!When disabled, the Cardano node will run in default mode. The node will restart automatically to apply this change.',
    description: 'Main body of the dialog - when disabling',
  },
  disableRTSFlagsModeActionButton: {
    id: 'knownIssues.dialog.disableRtsFlagsMode.actionButton',
    defaultMessage: '!!!Disable',
    description: 'Disable RTS flags button label',
  },
});
type Props = {
  onClose: () => void;
  onConfirm: () => void;
  isRTSFlagsModeEnabled: boolean;
};

@observer
class ToggleRTSFlagsDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { isRTSFlagsModeEnabled, onClose, onConfirm } = this.props;
    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: onClose,
      },
      {
        label: intl.formatMessage(
          isRTSFlagsModeEnabled
            ? messages.disableRTSFlagsModeActionButton
            : messages.enableRTSFlagsModeActionButton
        ),
        primary: true,
        onClick: onConfirm,
      },
    ];
    return (
      <Dialog
        className={styles.dialog}
        title={intl.formatMessage(
          isRTSFlagsModeEnabled
            ? messages.disableRTSFlagsModeHeadline
            : messages.enableRTSFlagsModeHeadline
        )}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <p>
          {intl.formatMessage(
            isRTSFlagsModeEnabled
              ? messages.disableRTSFlagsModeExplanation
              : messages.enableRTSFlagsModeExplanation
          )}
        </p>
      </Dialog>
    );
  }
}

export default ToggleRTSFlagsDialog;
