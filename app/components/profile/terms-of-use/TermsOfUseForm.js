// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Button from 'react-toolbox/lib/button/Button';
import ReactMarkdown from 'react-markdown';
import { defineMessages, intlShape } from 'react-intl';
import CheckboxWithLongLabel from '../../widgets/forms/CheckboxWithLongLabel';
import LocalizableError from '../../../i18n/LocalizableError';
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
    localizedTermsOfUse: string,
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
    const { isSubmitting, error, localizedTermsOfUse } = this.props;
    const { areTermsOfUseAccepted } = this.state;

    return (
      <div className={styles.component}>
        <div className={styles.centeredBox}>

          <div className={styles.terms}>

            <ReactMarkdown source={localizedTermsOfUse} />

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
