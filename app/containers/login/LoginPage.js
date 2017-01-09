// @flow
import React, { Component, PropTypes } from 'react';
import { inject, observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import Login from '../../components/auth/Login';
import hashData from '../../lib/hash-data';
import Request from '../../stores/lib/Request';

@inject('stores', 'actions') @observer
export default class LoginPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      user: PropTypes.shape({
        loginRequest: PropTypes.instanceOf(Request),
      }).isRequired
    }).isRequired,
    actions: PropTypes.shape({
      login: PropTypes.func.isRequired,
    }).isRequired,
  };

  handleLoginFormSubmit = (values: Object) => {
    const { email, password } = values;
    const passwordHash = hashData(password);
    this.props.actions.login({ email, passwordHash });
  };

  render() {
    const { loginRequest } = this.props.stores.user;
    return (
      <CenteredLayout>
        <Login
          isSubmitting={loginRequest.isExecuting}
          onSubmit={this.handleLoginFormSubmit}
          isInvalid={loginRequest.wasExecuted && loginRequest.result === false}
          onCreateAccount={() => console.log('create account')} // eslint-disable-line
        />
      </CenteredLayout>
    );
  }
}
