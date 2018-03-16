// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import globalMessages from '../../../i18n/global-messages';
import styles from './InstructionsDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.instructions.dialog.headline',
    defaultMessage: '!!!Create a paper wallet certificate',
    description: 'Headline for the "Paper wallet create certificate instructions dialog".'
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.instructions.dialog.subtitle',
    defaultMessage: '!!!Create a paper wallet certificate for offline storage of funds.',
    description: 'Subtitle for the "Paper wallet create certificate instructions dialog".'
  },
  instructionsListLabel: {
    id: 'paper.wallet.create.certificate.instructions.dialog.instructionsList.label',
    defaultMessage: '!!!Instructions:',
    description: 'Instructions list label for the "Paper wallet create certificate instructions dialog".'
  },
  instructionsListDefinition1: {
    id: 'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition1',
    defaultMessage: '!!!A printed certificate will include shielded recovery phrase in the form of 15 mnemonic words.',
    description: 'Wallet certificate create instructions dialog definition 1.',
  },
  instructionsListDefinition2: {
    id: 'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition2',
    defaultMessage: '!!!To restore your wallet at the later time, you will need a recovery phrase from your certificate and a password which will be chosen at the following step.',
    description: 'Wallet certificate create instructions dialog definition 2.',
  },
  instructionsListDefinition3: {
    id: 'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition3',
    defaultMessage: '!!!You will be able to send funds to your wallet using the address from the certificate.',
    description: 'Wallet certificate create instructions dialog definition 3.',
  },
  instructionsListDefinition4: {
    id: 'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition4',
    defaultMessage: '!!!Created wallet will not be kept in Daedalus. You will be able to check the balance on the address from the certificate using Cardano Explorer.',
    description: 'Wallet certificate create instructions dialog definition 4.',
  },
  instructionsListDefinition5: {
    id: 'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition5',
    defaultMessage: '!!!Store your certificate and password in a safe place. It is best not to keep them together.',
    description: 'Wallet certificate create instructions dialog definition 5.',
  },
});

type Props = {
  onContinue: Function,
  onClose: Function,
};

@observer
export default class InstructionsDialog extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, onContinue } = this.props;
    const dialogClasses = classnames([
      styles.component,
      'instructionsDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
        primary: true,
        onClick: onContinue,
      }
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >

        <div className={styles.instructionsContentWrapper}>
          <p className={styles.subtitle}>{intl.formatMessage(messages.subtitle)}</p>
          <div className={styles.instructionsList}>

            <p className={styles.instructionsListLabel}>
              {intl.formatMessage(messages.instructionsListLabel)}
            </p>

            <ul>
              <li>{intl.formatMessage(messages.instructionsListDefinition1)}</li>
              <li>{intl.formatMessage(messages.instructionsListDefinition2)}</li>
              <li>{intl.formatMessage(messages.instructionsListDefinition3)}</li>
              <li>{intl.formatMessage(messages.instructionsListDefinition4)}</li>
              <li>{intl.formatMessage(messages.instructionsListDefinition5)}</li>
            </ul>

          </div>
        </div>

      </Dialog>
    );
  }
}
