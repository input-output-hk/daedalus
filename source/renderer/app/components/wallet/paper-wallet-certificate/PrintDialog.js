// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/raw/CheckboxSkin';
import Dialog from '../../widgets/Dialog';
import globalMessages from '../../../i18n/global-messages';
import styles from './PrintDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.print.dialog.headline',
    defaultMessage: '!!!Certificate generation complete',
    description: 'Headline for the "Paper wallet create certificate print dialog".'
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.print.dialog.subtitle',
    defaultMessage: '!!!Check your paper wallet certificate and verify that everything is correctly printed and readable. You can try scanning QR codes with QR scanner application on your mobile phone.',
    description: '"Paper wallet create certificate print dialog" subtitle.'
  },
  printConfirmationLabel: {
    id: 'paper.wallet.create.certificate.print.dialog.printConfirmation',
    defaultMessage: '!!!Yes, paper wallet certificate successfully printed and everything is readable and scannable.',
    description: '"Paper wallet create certificate print dialog" confirmation.'
  },
});

type State = {
  isPrintedCorrectly: boolean,
};

type Props = {
  onContinue: Function,
};

@observer
export default class PrintDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isPrintedCorrectly: false,
  };

  render() {
    const { intl } = this.context;
    const { onContinue } = this.props;
    const { isPrintedCorrectly } = this.state;

    const dialogClasses = classnames([
      styles.component,
      'printDialog',
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
      >

        <div className={styles.printContentWrapper}>
          <p className={styles.subtitle}>{intl.formatMessage(messages.subtitle)}</p>
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
  };
}
