// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import Login from '../../components/auth/Login';

@observer(['state'])
export default class LoginPage extends Component {

  static propTypes = {
    state: PropTypes.shape({
      login: PropTypes.shape({
        isLoggingIn: PropTypes.bool.isRequired,
        errorLoading: PropTypes.string
      }).isRequired
    }).isRequired
  };

  render() {
    const { isLoggingIn } = this.props.state.login;
    return (
      <CenteredLayout>
        <Login
          isSubmitting={isLoggingIn}
          onSubmit={() => console.log('login')}
          onCreateAccount={() => console.log('create account')}
        />
      </CenteredLayout>
    );
  }
}
