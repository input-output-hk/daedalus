import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ImageUploadWidget.scss' or i... Remove this comment to see the full error message
import styles from './ImageUploadWidget.scss';

export const messages = defineMessages({
  dropFileHere: {
    id: 'ImageUploadWidget.dropFileHint',
    defaultMessage: '!!!Drop file here',
    description: 'Label "Drop file here" on the file upload widget.',
  },
  orClickToUpload: {
    id: 'ImageUploadWidget.clickToUploadLabel',
    defaultMessage: '!!!or click to upload',
    description: 'Label "or click to upload" on the file upload widget.',
  },
});
type Props = {
  label: string;
};

@observer
class ImageUploadWidget extends Component<Props> {
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
            <div className={styles.title}>
              {intl.formatMessage(messages.dropFileHere)}
            </div>
            <div className={styles.subtitle}>
              {intl.formatMessage(messages.orClickToUpload)}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default ImageUploadWidget;
