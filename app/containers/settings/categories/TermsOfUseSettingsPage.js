// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import TermsOfUseSettings from '../../../components/settings/categories/TermsOfUseSettings';

@inject('state') @observer
export default class TermsOfUseSettingsPage extends Component {

  static propTypes = {
    state: PropTypes.shape({
      settings: PropTypes.shape({
        termsOfUse: PropTypes.string.isRequired
      }).isRequired,
      login: PropTypes.shape({
        isLoading: PropTypes.bool.isRequired
      }).isRequired
    }).isRequired
  };

  render() {
    const { login, settings } = this.props.state;
    if (login.isLoading) return <div>Loading</div>;
    return (
      <TermsOfUseSettings text={settings.termsOfUse} />
    );
  }

}
