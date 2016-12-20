// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import Input from 'react-toolbox/lib/input/Input';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import FileUploadWidget from '../../widgets/FileUploadWidget';
import Profile from '../../../domain/Profile';
import styles from './ProfileSettings.scss';

const messages = defineMessages({
  name: {
    id: 'profile.settings.update.name.label',
    defaultMessage: '!!!Name',
    description: 'Label for the "Name" text input on the profile settings page.'
  },
  email: {
    id: 'profile.settings.update.email.label',
    defaultMessage: '!!!Email',
    description: 'Label for the "Email" text input on the profile settings page.'
  },
  phoneNumber: {
    id: 'profile.settings.update.phone.number.label',
    defaultMessage: '!!!Phone number',
    description: 'Label for the "Phone number" text input on the profile settings page.'
  },
  password: {
    id: 'profile.settings.update.password.label',
    defaultMessage: '!!!Password',
    description: 'Label for the "Password" text input on the profile settings page.'
  },
  language: {
    id: 'profile.settings.update.language.label',
    defaultMessage: '!!!Language',
    description: 'Label for the "Language" text input on the profile settings page.'
  },
  picture: {
    id: 'profile.settings.update.picture.label',
    defaultMessage: '!!!Picture',
    description: 'Label for the "Picture" upload control on the profile settings page.'
  },
  lastUpdated: {
    id: 'profile.settings.password.last.updated.label',
    defaultMessage: '!!!Last updated',
    description: '"Last updated" part of the message "Last updated X time ago" for password.'
  },
});

const languages = [
  { value: 'en-US', label: 'English' },
  { value: 'de-DE', label: 'German' },
  { value: 'hr-HR', label: 'Croatian' },
];

@observer
export default class ProfileSettings extends Component {

  static propTypes = {
    profile: PropTypes.instanceOf(Profile).isRequired,
    onFieldValueChange: PropTypes.func,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { profile, onFieldValueChange } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.nameEmailAndPicture}>
          <div className={styles.nameAndEmail}>
            <Input
              type="text"
              label={intl.formatMessage(messages.name)}
              value={profile.name}
              onChange={(value) => onFieldValueChange('name', value)}
            />
            <Input
              type="text"
              label={intl.formatMessage(messages.email)}
              value={profile.email}
              onChange={(value) => onFieldValueChange('email', value)}
            />
          </div>
          <div className={styles.picture}>
            <FileUploadWidget
              label={intl.formatMessage(messages.picture)}
            />
          </div>
        </div>
        <Input
          type="text"
          label={intl.formatMessage(messages.phoneNumber)}
          value={profile.phoneNumber}
          onChange={(value) => onFieldValueChange('phoneNumber', value)}
        />
        <Input
          type="text"
          label={intl.formatMessage(messages.password)}
          value={`${intl.formatMessage(messages.lastUpdated)} ${moment(profile.passwordUpdateDate).fromNow()}`}
        />
        <Dropdown
          className="language"
          label={intl.formatMessage(messages.language)}
          source={languages}
          value={profile.languageLocale}
          onChange={(value) => onFieldValueChange('languageLocale', value)}
        />
      </div>
    );
  }

}
