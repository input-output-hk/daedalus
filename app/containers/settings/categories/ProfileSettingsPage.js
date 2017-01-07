// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import ProfileSettings from '../../../components/settings/categories/ProfileSettings';
import User from '../../../domain/User';

@inject('stores', 'actions') @observer
export default class ProfileSettingsPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      user: PropTypes.shape({
        active: PropTypes.instanceOf(User)
      }).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      updateProfileField: PropTypes.func.isRequired
    }).isRequired
  };

  render() {
    const { profile } = this.props.stores.user.active;
    const { actions } = this.props;
    return (
      <ProfileSettings
        profile={profile}
        onFieldValueChange={(field, value) => actions.updateProfileField({ field, value })}
      />
    );
  }

}
