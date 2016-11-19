// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Input from 'react-toolbox/lib/input/Input';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import FileUploadWidget from '../../widgets/FileUploadWidget';
import styles from './ProfileSettings.scss';

const languages = [
  { value: 'EN', label: 'English' },
  { value: 'ES', label: 'Spanish' },
  { value: 'DE', label: 'German' },
  { value: 'HR', label: 'Croatian' },
];

@observer
export default class ProfileSettings extends Component {

  render() {
    return (
      <div>
        <div className={styles.nameEmailAndPicture}>
          <div className={styles.nameAndEmail}>
            <Input type="text" label="Name" />
            <Input type="text" label="Email" />
          </div>
          <div className={styles.picture}>
            <FileUploadWidget />
          </div>
        </div>
        <div>
          <Input type="text" label="Phone number" />
          <Input type="text" label="Password" />
          <Dropdown
            label="Language"
            source={languages}
            value="EN"
          />
        </div>
      </div>
    );
  }

}
