// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import Login from '../../components/auth/Login';

@observer(['state', 'controller'])
export default class LoginPage extends Component {

  static propTypes = {
    state: PropTypes.shape({
      login: PropTypes.shape({
        isLoggedIn: PropTypes.bool.isRequired,
        isLoading: PropTypes.bool.isRequired,
        errorLoading: PropTypes.string
      }).isRequired
    }).isRequired
  };

  render() {
    return (
      <CenteredLayout>
        <Login />
      </CenteredLayout>
    );
  }
}
