// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import ProfileSettings from '../../../components/settings/categories/ProfileSettings';
import User from '../../../domain/User';

@inject('stores', 'actions') @observer
export default class ProfileSettingsPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      user: PropTypes.shape({
        active: PropTypes.instanceOf(User)
      }).isRequired,
      settings: PropTypes.shape({
        settingsFieldBeingEdited: PropTypes.string,
        isValidName: PropTypes.func.isRequired,
        isValidEmail: PropTypes.func.isRequired
      }).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      updateProfileField: PropTypes.func.isRequired,
      startEditingSettingsField: PropTypes.func.isRequired,
      stopEditingSettingsField: PropTypes.func.isRequired
    }).isRequired
  };

  render() {
    const { profile } = this.props.stores.user.active;
    const { actions } = this.props;
    const {
      settingsFieldBeingEdited,
      isValidName,
      isValidEmail,
    } = this.props.stores.settings;
    return (
      <ProfileSettings
        profile={profile}
        onFieldValueChange={(field, value) => actions.updateProfileField({ field, value })}
        onStartEditing={field => actions.startEditingSettingsField({ field })}
        onStopEditing={actions.stopEditingSettingsField}
        activeField={settingsFieldBeingEdited}
        nameValidator={name => isValidName(name)}
        emailValidator={email => isValidEmail(email)}
      />
    );
  }

}
