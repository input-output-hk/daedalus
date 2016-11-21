// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './FileUploadWidget.scss';

const messages = defineMessages({
  dropFileHere: {
    id: 'file.upload.widget.drop.file.here.label',
    defaultMessage: '!!!Drop file here',
    description: 'Label "Drop file here" on the file upload widget.'
  },
  orClickToUpload: {
    id: 'file.upload.widget.click.to.upload.label',
    defaultMessage: '!!!or click to upload',
    description: 'Label "or click to upload" on the file upload widget.'
  },
});

@observer
export default class FileUploadWidget extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { label } = this.props;
    return (
      <div>
        <div className={styles.label}>{label}</div>
        <div className={styles.uploadBox}>
          <div className={styles.instructions}>
            <div className={styles.title}>{intl.formatMessage(messages.dropFileHere)}</div>
            <div className={styles.subtitle}>{intl.formatMessage(messages.orClickToUpload)}</div>
          </div>
        </div>
      </div>
    );
  }

}
