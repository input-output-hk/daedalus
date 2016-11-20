// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import Input from 'react-toolbox/lib/input/Input';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import FileUploadWidget from '../../widgets/FileUploadWidget';
import Profile from '../../../domain/Profile';
import styles from './ProfileSettings.scss';

const languages = [
  { value: 'en_US', label: 'English' },
  { value: 'de_DE', label: 'German' },
];

@observer
export default class ProfileSettings extends Component {

  static propTypes = {
    profile: PropTypes.instanceOf(Profile).isRequired
  };

  render() {
    const { profile } = this.props;
    return (
      <div>
        <div className={styles.nameEmailAndPicture}>
          <div className={styles.nameAndEmail}>
            <Input
              type="text"
              label="Name"
              value={profile.name}
            />
            <Input
              type="text"
              label="Email"
              value={profile.email}
            />
          </div>
          <div className={styles.picture}>
            <FileUploadWidget />
          </div>
        </div>
        <div>
          <Input
            type="text"
            label="Phone number"
            value={profile.phoneNumber}
          />
          <Input
            type="text"
            label="Password"
            value={moment(profile.passwordUpdateDate).fromNow()}
          />
          <Dropdown
            label="Language"
            source={languages}
            value={profile.languageLocale}
          />
        </div>
      </div>
    );
  }

}
