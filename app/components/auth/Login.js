// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import MobxReactForm from 'mobx-react-form';
import Input from 'react-toolbox/lib/input/Input';
import Button from 'react-toolbox/lib/button/Button';
import { defineMessages, intlShape } from 'react-intl';
import isEmail from 'validator/lib/isEmail';
import logo from '../../assets/images/login-logo.svg';
import styles from './Login.scss';

const messages = defineMessages({
  emailHint: {
    id: 'login.form.email.hint',
    defaultMessage: '!!!E-mail',
    description: 'Hint for the login form email input.'
  },
  emailInvalid: {
    id: 'login.form.email.invalid',
    defaultMessage: '!!!Please provide a valid E-mail address.',
    description: 'Error for invalid email addresses on the login form.'
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
  }
});

@observer
export default class Login extends Component {

  static propTypes = {
    isSubmitting: PropTypes.bool,
    onSubmit: PropTypes.func
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  validator = new MobxReactForm({
    options: {
      validateOnChange: false
    },
    fields: {
      email: {
        validate: [({ field }) => [isEmail(field.value), 'emailInvalid']]
      },
      password: {}
    }
  });

  submit() {
    this.validator.submit({
      onSuccess: (form) => {
        this.props.onSubmit(form.values());
      },
      onError: () => {
      }
    });
  }

  render() {
    const { validator } = this;
    const { isSubmitting } = this.props;
    const { intl } = this.context;
    const email = validator.$('email');
    const password = validator.$('password');
    const errors = {
      email: email.error ? intl.formatMessage(email.error) : null,
    };
    return (
      <div className={styles.component}>
        <img className={styles.logo} src={logo} role="presentation" />
        <div className={styles.form}>
          <Input
            type="text"
            className="email"
            hint={intl.formatMessage(messages.emailHint)}
            value={email.value}
            error={errors.email}
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
          <Button
            className={isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
            label={intl.formatMessage(messages.submitButtonLabel)}
            onMouseUp={this.submit.bind(this)}
          />
        </div>
      </div>
    );
  }
}
