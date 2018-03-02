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
export default class PaperWalletCreateCertificateTemplateChoiceDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props) {
    super(props);
    this.state = {
      selectedTemplate: null,
    }
  };

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
    ])
    const templateGreenClasses = classnames([
      selectedTemplate === 'Green' ? styles.active : styles.inactive,
      styles.templateWrapper,
    ])
    const templateRedClasses = classnames([
      selectedTemplate === 'Red' ? styles.active : styles.inactive,
      styles.templateWrapper,
    ])
    const templateVioletClasses = classnames([
      selectedTemplate === 'Violet' ? styles.active : styles.inactive,
      styles.templateWrapper,
    ])
    const templateYellowClasses = classnames([
      selectedTemplate === 'Yellow' ? styles.active : styles.inactive,
      styles.templateWrapper,
    ])

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

            <div className={templateBlueClasses} onClick={this.onSelectTemplate.bind(this, 'Blue')}>
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>
                Blue
              </div>
            </div>
            <div className={templateBrownClasses} onClick={this.onSelectTemplate.bind(this, 'Brown')}>
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>
                Brown
              </div>
            </div>
            <div className={templateGreenClasses} onClick={this.onSelectTemplate.bind(this, 'Green')}>
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>
                Green
              </div>
            </div>
          </div>

          <div className={styles.row}>
            <div className={templateRedClasses} onClick={this.onSelectTemplate.bind(this, 'Red')}>
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>
                Red
              </div>
            </div>
            <div className={templateVioletClasses} onClick={this.onSelectTemplate.bind(this, 'Violet')}>
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>
                Violet
              </div>
            </div>
            <div className={templateYellowClasses} onClick={this.onSelectTemplate.bind(this, 'Yellow')}>
              <img src={templateImage} role="presentation" />
              <div className={styles.templateTitle}>
                Yellow
              </div>
            </div>
          </div>
        </div>

      </Dialog>
    );
  }

  onSelectTemplate = (templateName) => {
    this.setState({ selectedTemplate: templateName });
  }
}
