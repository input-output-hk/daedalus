// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/raw/CheckboxSkin';
import Dialog from '../../../widgets/Dialog';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import globalMessages from '../../../../i18n/global-messages';
import styles from './PaperWalletCreateCertificatePrintDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.print.dialog.headline',
    defaultMessage: '!!!Certificate generation complete',
    description: 'Headline for the "Paper wallet create certificate print dialog".'
  },
  printConfirmationLabel: {
    id: 'paper.wallet.create.certificate.print.dialog.printConfirmation',
    defaultMessage: '!!!Yes, paper wallet certificate successfully printed and everything is readable and scannable.',
    description: '"Paper wallet create certificate password choice dialog" print confirmation.'
  },
});


type State = {
  isPrintedCorrectly: boolean,
};

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
};

@observer
export default class PaperWalletCreateCertificatePrintDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isPrintedCorrectly: false,
  }

  render() {
    const { intl } = this.context;
    const { onClose, onBack, onContinue } = this.props;
    const { isPrintedCorrectly } = this.state;

    const dialogClasses = classnames([
      styles.component,
      'PaperWalletCreateCertificatePrintDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
        primary: true,
        disabled: !isPrintedCorrectly,
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
        backButton={<DialogBackButton onBack={onBack} />}
      >

        <div className={styles.printContentWrapper}>
          <p className={styles.subtitle}>Check your paper wallet certificate and verify that everything is correctly printed and readable. You can try scanning QR codes with QR scanner application on your mobile phone.</p>
          <div className={styles.content}>
            <Checkbox
              className={styles.checkbox}
              label={intl.formatMessage(messages.printConfirmationLabel)}
              onChange={this.onConfirmCorrectPrinting.bind(this)}
              checked={isPrintedCorrectly}
              skin={<SimpleCheckboxSkin />}
            />
          </div>
        </div>

      </Dialog>
    );
  }

  onConfirmCorrectPrinting = () => {
    this.setState({ isPrintedCorrectly: !this.state.isPrintedCorrectly });
  }
}
