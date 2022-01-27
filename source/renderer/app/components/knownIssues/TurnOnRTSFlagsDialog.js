// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import globalMessages from '../../i18n/global-messages';
import styles from './knownIssuesDialogStyles.scss';

const messages = defineMessages({
  enableRTSFlagsModeHeadline: {
    id: 'knownIssues.dialog.enableRtsFlagsMode.title',
    defaultMessage: 'Enable RTS flags (RAM management system)',
    description: 'Headline for the RTS flags dialog - when enabling',
  },
  enableRTSFlagsModeExplanation: {
    id: 'knownIssues.dialog.enableRtsFlagsMode.explanation',
    defaultMessage:
      'When enabled, the Cardano node will attempt to reduce its RAM usage. You will need to restart Daedalus for this change to take effect.',
    description: 'Main body of the dialog - when enabling',
  },
  enableRTSFlagsModeActionButton: {
    id: 'knownIssues.dialog.enableRtsFlagsMode.actionButton',
    defaultMessage: 'Enable and quit',
    description: 'Enable RTS flags button label',
  },
  disableRTSFlagsModeHeadline: {
    id: 'knownIssues.dialog.disableRtsFlagsMode.title',
    defaultMessage: 'Disable RTS flags (RAM management system)',
    description: 'Headline for the RTS flags dialog - when disabling',
  },
  disableRTSFlagsModeExplanation: {
    id: 'knownIssues.dialog.disableRtsFlagsMode.explanation',
    defaultMessage:
      'When disabled, the Cardano node will start in default mode. You will need to restart Daedalus for this change to take effect.',
    description: 'Main body of the dialog - when disabling',
  },
  disableRTSFlagsModeActionButton: {
    id: 'knownIssues.dialog.disableRtsFlagsMode.actionButton',
    defaultMessage: 'Disable and quit',
    description: 'Disable RTS flags button label',
  },
  manualRelaunchConfirmationCheckboxLabel: {
    id:
      'knownIssues.dialog.toggleRtsFlagsMode.manualRelaunchConfirmationCheckboxLabel',
    defaultMessage: 'I understand that I will need to launch Daedalus manually',
    description: 'Manual relaunch confirmation checkbox label',
  },
});

type Props = {
  onClose: () => void,
  onConfirm: () => void,
  rtsFlagsModeEnabled: boolean,
};

type State = {
  confirmationCheckboxChecked: boolean,
};

@observer
export default class TurnOnRTSFlagsDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    confirmationCheckboxChecked: false,
  };

  handleCheckboxToggle = () => {
    this.setState((prevState) => ({
      confirmationCheckboxChecked: !prevState.confirmationCheckboxChecked,
    }));
  };

  render() {
    const { intl } = this.context;
    const { rtsFlagsModeEnabled, onClose, onConfirm } = this.props;
    const { confirmationCheckboxChecked } = this.state;

    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: onClose,
      },
      {
        label: intl.formatMessage(
          rtsFlagsModeEnabled
            ? messages.enableRTSFlagsModeActionButton
            : messages.disableRTSFlagsModeActionButton
        ),
        primary: true,
        onClick: onConfirm,
        disabled: !confirmationCheckboxChecked,
      },
    ];

    return (
      <Dialog
        className={styles.dialog}
        title={intl.formatMessage(
          rtsFlagsModeEnabled
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
            rtsFlagsModeEnabled
              ? messages.enableRTSFlagsModeExplanation
              : messages.disableRTSFlagsModeExplanation
          )}
        </p>
        <Checkbox
          label={intl.formatMessage(
            messages.manualRelaunchConfirmationCheckboxLabel
          )}
          onChange={this.handleCheckboxToggle}
          checked={confirmationCheckboxChecked}
        />
      </Dialog>
    );
  }
}
