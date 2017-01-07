// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import ProfileSettings from '../../../components/settings/categories/ProfileSettings';
import User from '../../../domain/User';

@inject('stores', 'controller') @observer
export default class ProfileSettingsPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      user: PropTypes.shape({
        active: PropTypes.instanceOf(User)
      }).isRequired,
    }).isRequired,
    controller: PropTypes.shape({
      user: PropTypes.shape({
        updateField: PropTypes.func.isRequired
      }).isRequired
    }).isRequired
  };

  render() {
    const { profile } = this.props.stores.user.active;
    const { controller } = this.props;
    return (
      <ProfileSettings
        profile={profile}
        onFieldValueChange={(field, name) => controller.user.updateField(field, name)}
      />
    );
  }

}
