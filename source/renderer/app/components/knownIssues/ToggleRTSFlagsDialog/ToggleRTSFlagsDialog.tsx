import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Checkbox } from '@react-polymorph/components/Checkbox';
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
      '!!!When enabled, the Cardano node will attempt to reduce its RAM usage. You will need to restart Daedalus for this change to take effect.',
    description: 'Main body of the dialog - when enabling',
  },
  enableRTSFlagsModeActionButton: {
    id: 'knownIssues.dialog.enableRtsFlagsMode.actionButton',
    defaultMessage: '!!!Enable and quit',
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
      '!!!When disabled, the Cardano node will start in default mode. You will need to restart Daedalus for this change to take effect.',
    description: 'Main body of the dialog - when disabling',
  },
  disableRTSFlagsModeActionButton: {
    id: 'knownIssues.dialog.disableRtsFlagsMode.actionButton',
    defaultMessage: '!!!Disable and quit',
    description: 'Disable RTS flags button label',
  },
  manualRelaunchConfirmationCheckboxLabel: {
    id: 'knownIssues.dialog.toggleRtsFlagsMode.manualRelaunchConfirmationCheckboxLabel',
    defaultMessage:
      '!!!I understand that I will need to launch Daedalus manually',
    description: 'Manual relaunch confirmation checkbox label',
  },
});
type Props = {
  onClose: () => void;
  onConfirm: () => void;
  isRTSFlagsModeEnabled: boolean;
};
type State = {
  isConfirmationCheckboxChecked: boolean;
};

class ToggleRTSFlagsDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    isConfirmationCheckboxChecked: false,
  };
  handleCheckboxToggle = () => {
    this.setState((prevState) => ({
      isConfirmationCheckboxChecked: !prevState.isConfirmationCheckboxChecked,
    }));
  };

  render() {
    const { intl } = this.context;
    const { isRTSFlagsModeEnabled, onClose, onConfirm } = this.props;
    const { isConfirmationCheckboxChecked } = this.state;
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
        disabled: !isConfirmationCheckboxChecked,
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
        <Checkbox
          label={intl.formatMessage(
            messages.manualRelaunchConfirmationCheckboxLabel
          )}
          onChange={this.handleCheckboxToggle}
          checked={isConfirmationCheckboxChecked}
        />
      </Dialog>
    );
  }
}

export default observer(ToggleRTSFlagsDialog);
