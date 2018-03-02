// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../../widgets/Dialog';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import styles from './PaperWalletCreateCertificateTemplateChoiceDialog.scss';
import templateImage from '../../../../assets/images/paper-wallet-template-mock.png';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.templateChoice.dialog.headline',
    defaultMessage: '!!!Select color',
    description: 'Headline for "Paper wallet certificate create template choice dialog".'
  },
  printButtonLabe: {
    id: 'paper.wallet.create.certificate.templateChoice.dialog.button.printButtonLabe',
    defaultMessage: '!!!Print',
    description: '"Paper wallet create certificate template choice dialog" print button label.'
  },
});

type State = {
  selectedTemplate: string,
}

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
};

@observer
// eslint-disable-next-line
export default class PaperWalletCreateCertificateTemplateChoiceDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    selectedTemplate: '',
  }

  render() {
    const { intl } = this.context;
    const { onClose, onBack, onContinue } = this.props;
    const { selectedTemplate } = this.state;

    const dialogClasses = classnames([
      styles.component,
      'PaperWalletCreateCertificateTemplateChoiceDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.printButtonLabe),
        primary: true,
        disabled: !selectedTemplate,
        onClick: onContinue,
      }
    ];

    const templateBlueClasses = classnames([
      selectedTemplate === 'Blue' ? styles.active : styles.inactive,
      styles.templateWrapper,
    ]);

    const templateBrownClasses = classnames([
      selectedTemplate === 'Brown' ? styles.active : styles.inactive,
      styles.templateWrapper,
    ]);
    const templateGreenClasses = classnames([
      selectedTemplate === 'Green' ? styles.active : styles.inactive,
      styles.templateWrapper,
    ]);
    const templateRedClasses = classnames([
      selectedTemplate === 'Red' ? styles.active : styles.inactive,
      styles.templateWrapper,
    ]);
    const templateVioletClasses = classnames([
      selectedTemplate === 'Violet' ? styles.active : styles.inactive,
      styles.templateWrapper,
    ]);
    const templateYellowClasses = classnames([
      selectedTemplate === 'Yellow' ? styles.active : styles.inactive,
      styles.templateWrapper,
    ]);

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

        <div className={styles.templateChoiceContentWrapper}>

          <div className={styles.row}>
            <div
              className={templateBlueClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Blue')}
              aria-hidden
            >
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>Blue</div>
            </div>

            <div
              className={templateBrownClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Brown')}
              aria-hidden
            >
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>Brown</div>
            </div>

            <div
              className={templateGreenClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Green')}
              aria-hidden
            >
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>Green</div>
            </div>
          </div>

          <div className={classnames([styles.row, styles.nthRow])}>
            <div
              className={templateRedClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Red')}
              aria-hidden
            >
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>Red</div>
            </div>

            <div
              className={templateVioletClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Violet')}
              aria-hidden
            >
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>Violet</div>
            </div>

            <div
              className={templateYellowClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Yellow')}
              aria-hidden
            >
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>Yellow</div>
            </div>
          </div>

        </div>

      </Dialog>
    );
  }

  onSelectTemplate = (templateName: string) => {
    this.setState({ selectedTemplate: templateName });
  }
}
