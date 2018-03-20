// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../widgets/Dialog';
import styles from './TemplateChoiceDialog.scss';
import blueTemplateImage from '../../../assets/images/paper-wallet-certificate/template-blue.png';
import brownTemplateImage from '../../../assets/images/paper-wallet-certificate/template-brown.png';
import greenTemplateImage from '../../../assets/images/paper-wallet-certificate/template-green.png';
import redTemplateImage from '../../../assets/images/paper-wallet-certificate/template-red.png';
import violetTemplateImage from '../../../assets/images/paper-wallet-certificate/template-violet.png';
import yellowTemplateImage from '../../../assets/images/paper-wallet-certificate/template-yellow.png';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.templateChoice.dialog.headline',
    defaultMessage: '!!!Select color',
    description: 'Headline for "Paper wallet certificate create template choice dialog".'
  },
  printButtonLabel: {
    id: 'paper.wallet.create.certificate.templateChoice.dialog.button.printButtonLabel',
    defaultMessage: '!!!Print',
    description: '"Paper wallet create certificate template choice dialog" print button label.'
  },
});

type State = {
  selectedTemplate: ?string,
};

type Props = {
  onPrint: Function,
};

@observer
export default class TemplateChoiceDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    selectedTemplate: null,
  };

  render() {
    const { intl } = this.context;
    const { selectedTemplate } = this.state;

    const dialogClasses = classnames([
      styles.component,
      'templateChoiceDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.printButtonLabel),
        primary: true,
        disabled: !selectedTemplate,
        onClick: this.handleContinue.bind(this),
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
      >

        <div className={styles.templateChoiceContentWrapper}>

          <div className={styles.row}>
            <div
              className={templateBlueClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Blue')}
              aria-hidden
            >
              <img src={blueTemplateImage} role="presentation" />
              <div className={styles.templateTitle}>Blue</div>
            </div>

            <div
              className={templateBrownClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Brown')}
              aria-hidden
            >
              <img src={brownTemplateImage} role="presentation" />
              <div className={styles.templateTitle}>Brown</div>
            </div>

            <div
              className={templateGreenClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Green')}
              aria-hidden
            >
              <img src={greenTemplateImage} role="presentation" />
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
              <img src={redTemplateImage} role="presentation" />
              <div className={styles.templateTitle}>Red</div>
            </div>

            <div
              className={templateVioletClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Violet')}
              aria-hidden
            >
              <img src={violetTemplateImage} role="presentation" />
              <div className={styles.templateTitle}>Violet</div>
            </div>

            <div
              className={templateYellowClasses}
              role="presentation"
              onClick={this.onSelectTemplate.bind(this, 'Yellow')}
              aria-hidden
            >
              <img src={yellowTemplateImage} role="presentation" />
              <div className={styles.templateTitle}>Yellow</div>
            </div>
          </div>

        </div>

      </Dialog>
    );
  }

  onSelectTemplate = (templateName: string) => {
    this.setState({ selectedTemplate: templateName });
  };

  handleContinue = () => {
    this.props.onPrint({ selectedTemplate: this.state.selectedTemplate });
  };
}
