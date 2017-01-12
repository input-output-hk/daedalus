// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import Input from 'react-toolbox/lib/input/Input';
import DropUp from '../../widgets/forms/Dropup';
import classnames from 'classnames';
import FileUploadWidget from '../../widgets/FileUploadWidget';
import InlineEditingInput from '../../widgets/InlineEditingInput';
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
  invalidName: {
    id: 'profile.settings.errors.invalidName',
    defaultMessage: '!!!Please enter a valid name',
    description: 'Error message shown when invalid name was entered on profile settings.'
  },
  invalidEmail: {
    id: 'profile.settings.errors.invalidEmail',
    defaultMessage: '!!!Please enter a valid email',
    description: 'Error message shown when invalid email was entered on profile settings.'
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
    onFieldValueChange: PropTypes.func.isRequired,
    onStartEditing: PropTypes.func.isRequired,
    onStopEditing: PropTypes.func.isRequired,
    onCancelEditing: PropTypes.func.isRequired,
    nameValidator: PropTypes.func.isRequired,
    emailValidator: PropTypes.func.isRequired,
    activeField: PropTypes.string,
    isSubmitting: PropTypes.bool,
    isInvalid: PropTypes.bool,
    lastUpdatedFiled: PropTypes.string
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      profile,
      onFieldValueChange,
      onStartEditing,
      onStopEditing,
      activeField,
      nameValidator,
      emailValidator,
      isSubmitting,
      isInvalid,
      lastUpdatedFiled,
      onCancelEditing
    } = this.props;
    const componentClassNames = classnames([styles.component, 'profile']);
    return (
      <div className={componentClassNames}>
        <div className={styles.nameEmailAndPicture}>
          <div className={styles.nameAndEmail}>
            <InlineEditingInput
              inputFieldLabel={intl.formatMessage(messages.name)}
              inputFieldValue={profile.name}
              isActive={activeField === 'name'}
              onStartEditing={() => onStartEditing('name')}
              onStopEditing={onStopEditing}
              onCancelEditing={onCancelEditing}
              onSubmit={(value) => onFieldValueChange('name', value)}
              isValid={nameValidator}
              validationErrorMessage={intl.formatMessage(messages.invalidName)}
              successfullyUpdated={!isSubmitting && lastUpdatedFiled === 'name' && !isInvalid}
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
        <DropUp
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
