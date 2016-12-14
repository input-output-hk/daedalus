// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import MobxReactForm from 'mobx-react-form';
import Input from 'react-toolbox/lib/input/Input';
import Button from 'react-toolbox/lib/button/Button';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import logo from '../../assets/images/login-logo.svg';
import styles from './Login.scss';

const messages = defineMessages({
  emailHint: {
    id: 'login.form.email.hint',
    defaultMessage: '!!!E-mail',
    description: 'Hint for the login form email input.'
  },
  passwordHint: {
    id: 'login.form.password.hint',
    defaultMessage: '!!!Password',
    description: 'Hint for the login form password input.'
  },
  submitButtonLabel: {
    id: 'login.form.submit.label',
    defaultMessage: '!!!Login',
    description: 'Label for the login form submit button.'
  },
  invalidCredentials: {
    id: 'login.form.invalidCredentials',
    defaultMessage: '!!!E-mail or password is wrong.',
    description: 'Error shown when invalid login credentials are submitted.'
  },
});

@observer
export default class Login extends Component {

  static propTypes = {
    onSubmit: PropTypes.func.isRequired,
    onCreateAccount: PropTypes.func.isRequired,
    isSubmitting: PropTypes.bool,
    isInvalid: PropTypes.bool,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  validator = new MobxReactForm({
    options: {
      validateOnChange: false
    },
    fields: {
      email: {},
      password: {}
    }
  });

  submit() {
    this.validator.submit({
      onSuccess: (form) => {
        this.props.onSubmit(form.values());
      }
    });
  }

  render() {
    const { validator } = this;
    const { isSubmitting, isInvalid, onCreateAccount } = this.props;
    const { intl } = this.context;
    const email = validator.$('email');
    const password = validator.$('password');
    const componentClassNames = classnames([
      styles.component,
      isInvalid ? styles.invalidCredentials : null
    ]);
    return (
      <div className={componentClassNames}>
        <img className={styles.logo} src={logo} role="presentation" />
        <div className={styles.form}>
          <Input
            type="text"
            className="email"
            hint={intl.formatMessage(messages.emailHint)}
            value={email.value}
            onChange={email.onChange}
            onFocus={email.onFocus}
            onBlur={email.onBlur}
          />
          <Input
            type="text"
            className="password"
            hint={intl.formatMessage(messages.passwordHint)}
            value={password.value}
            onChange={password.onChange}
            onFocus={password.onFocus}
            onBlur={password.onBlur}
          />
          {isInvalid && (
            <div className={styles.formError}>
              {intl.formatMessage(messages.invalidCredentials)}
            </div>
          )}
          <Button
            className={isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
            label={intl.formatMessage(messages.submitButtonLabel)}
            onClick={this.submit.bind(this)}
          />
          <div className={styles.noAccountText}>
            Don’t have an account?&nbsp;
            <button className={styles.createAccountLink} onClick={onCreateAccount}>
              Create one
            </button>
            .
          </div>
        </div>
      </div>
    );
  }
}
