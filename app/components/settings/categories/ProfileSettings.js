// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import Input from 'react-toolbox/lib/input/Input';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import FileUploadWidget from '../../widgets/FileUploadWidget';
import UserProfile from '../../../domain/UserProfile';
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
    profile: PropTypes.instanceOf(UserProfile).isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { profile } = this.props;
    return (
      <div>
        <div className={styles.nameEmailAndPicture}>
          <div className={styles.nameAndEmail}>
            <Input
              type="text"
              label={intl.formatMessage(messages.name)}
              value={profile.name}
            />
            <Input
              type="text"
              label={intl.formatMessage(messages.email)}
              value={profile.email}
            />
          </div>
          <div className={styles.picture}>
            <FileUploadWidget
              label={intl.formatMessage(messages.picture)}
            />
          </div>
        </div>
        <div>
          <Input
            type="text"
            label={intl.formatMessage(messages.phoneNumber)}
            value={profile.phoneNumber}
          />
          <Input
            type="text"
            label={intl.formatMessage(messages.password)}
            value={`${intl.formatMessage(messages.lastUpdated)} ${moment(profile.passwordUpdateDate).fromNow()}`}
          />
          <Dropdown
            label={intl.formatMessage(messages.language)}
            source={languages}
            value={profile.languageLocale}
          />
        </div>
      </div>
    );
  }

}
