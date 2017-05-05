// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Button from 'react-toolbox/lib/button/Button';
import { defineMessages, intlShape } from 'react-intl';
import CheckboxWithLongLabel from '../../widgets/forms/CheckboxWithLongLabel';
import LocalizableError from '../../../i18n/LocalizableError';
import termsOfUseMessages from '../../../i18n/termsOfUse';
import styles from './TermsOfUseForm.scss';

const messages = defineMessages({
  checkboxLabel: {
    id: 'profile.termsOfUse.checkboxLabel',
    defaultMessage: '!!!I agree with terms of use',
    description: 'Label for the "I agree with terms of use" checkbox.'
  },
  submitLabel: {
    id: 'profile.termsOfUse.submitLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for the "Terms of use" form submit button.'
  },
});

@observer
export default class TermsOfUseForm extends Component {

  props: {
    onSubmit: Function,
    isSubmitting: boolean,
    error?: ?LocalizableError,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    areTermsOfUseAccepted: false,
  };

  toggleAcceptance() {
    this.setState({ areTermsOfUseAccepted: !this.state.areTermsOfUseAccepted });
  }

  submit = () => {
    this.props.onSubmit();
  };

  render() {
    const { intl } = this.context;
    const { isSubmitting, error } = this.props;
    const { areTermsOfUseAccepted } = this.state;

    return (
      <div className={styles.component}>
        <div className={styles.centeredBox}>

          <div className={styles.terms}>
            <h1>{intl.formatMessage(termsOfUseMessages.label)}</h1>
            <p>{intl.formatMessage(termsOfUseMessages.firstParagraph)}</p>
            <p>{intl.formatMessage(termsOfUseMessages.secondParagraph)}</p>
            <ol>
              <li>
                <h2>{intl.formatMessage(termsOfUseMessages.firstNumberedTitle)}</h2>
                <ul>
                  <li>
                    <strong>{intl.formatMessage(termsOfUseMessages.firstSubtitle)}</strong>
                    {intl.formatMessage(termsOfUseMessages.firstListText)}
                  </li>
                  <li>
                    <strong>{intl.formatMessage(termsOfUseMessages.secondSubtitle)}</strong>
                    {intl.formatMessage(termsOfUseMessages.secondListText)}
                  </li>
                </ul>
              </li>
              <li>
                <h2>{intl.formatMessage(termsOfUseMessages.secondNumberedTitle)}</h2>
                <ul>
                  <li>
                    <strong>{intl.formatMessage(termsOfUseMessages.thirdSubtitle)}</strong>
                    {intl.formatMessage(termsOfUseMessages.thirdListText)}
                  </li>
                </ul>
              </li>
            </ol>
            <p>{intl.formatMessage(termsOfUseMessages.lastParagraph)}</p>

            <CheckboxWithLongLabel
              className={styles.checkbox}
              label={intl.formatMessage(messages.checkboxLabel)}
              onChange={this.toggleAcceptance.bind(this)}
              checked={areTermsOfUseAccepted}
            />
          </div>

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

          <Button
            className={isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
            label={intl.formatMessage(messages.submitLabel)}
            onMouseUp={this.submit}
            primary
            disabled={!areTermsOfUseAccepted}
          />

        </div>
      </div>
    );
  }

}
