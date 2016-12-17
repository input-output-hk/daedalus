// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import Login from '../../components/auth/Login';
import hashData from '../../util/hash-data';

@observer(['state', 'controller'])
export default class LoginPage extends Component {

  static propTypes = {
    state: PropTypes.shape({
      login: PropTypes.shape({
        isLoggingIn: PropTypes.bool.isRequired,
        errorLoading: PropTypes.string
      }).isRequired
    }).isRequired,
    controller: PropTypes.shape({
      user: PropTypes.shape({
        login: PropTypes.func.isRequired,
      })
    }),
  };

  handleLoginFormSubmit(values: Object) {
    const { email, password } = values;
    const passwordHash = hashData(password);
    this.props.controller.user.login({ email, passwordHash });
  }

  render() {
    const { isLoggingIn } = this.props.state.login;
    return (
      <CenteredLayout>
        <Login
          isSubmitting={isLoggingIn}
          onSubmit={this.handleLoginFormSubmit.bind(this)}
          onCreateAccount={() => console.log('create account')} // eslint-disable-line
        />
      </CenteredLayout>
    );
  }
}
