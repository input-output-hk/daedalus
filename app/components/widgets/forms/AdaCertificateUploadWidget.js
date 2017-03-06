// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Dropzone from 'react-dropzone';
import { defineMessages, intlShape } from 'react-intl';
import certificateNormalIcon from '../../../assets/images/cert-ic.svg';
import certificateLockedIcon from '../../../assets/images/cert-locked-ic.svg';
import certificateInvalidIcon from '../../../assets/images/cert-bad-ic.svg';
import closeCrossIcon from '../../../assets/images/close-cross-redemption.svg';
import styles from './AdaCertificateUploadWidget.scss';

const messages = defineMessages({
  dropFileHere: {
    id: 'ImageUploadWidget.dropFileHint',
    defaultMessage: '!!!Drop file here',
    description: 'Label "Drop file here" on the file upload widget.'
  },
  orClickToUpload: {
    id: 'ImageUploadWidget.clickToUploadLabel',
    defaultMessage: '!!!or click to upload',
    description: 'Label "or click to upload" on the file upload widget.'
  },
});

@observer
export default class AdaCertificateUploadWidget extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    onFileSelected: PropTypes.func.isRequired,
    onRemoveCertificate: PropTypes.func.isRequired,
    acceptedFileTypes: PropTypes.string,
    isCertificateEncrypted: PropTypes.bool,
    isCertificateSelected: PropTypes.bool,
    isCertificateInvalid: PropTypes.bool,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  onDrop = (files: [File]) => {
    this.props.onFileSelected(files[0]);
  };

  render() {
    const { intl } = this.context;
    const { label, acceptedFileTypes, isCertificateEncrypted,
      isCertificateSelected, onRemoveCertificate, isCertificateInvalid
    } = this.props;
    const certificateIcon = isCertificateEncrypted ? certificateLockedIcon :
      isCertificateInvalid ? certificateInvalidIcon : certificateNormalIcon;
    return (
      <div>
        <div className={styles.label}>{label}</div>
        <div className={styles.uploadBox}>
          {
            (() => {
              if (isCertificateSelected) {
                return (
                  <div className={styles.certificateUploaded}>
                    <button className={styles.removeFileButton} onClick={onRemoveCertificate}>
                      <img src={closeCrossIcon} className={styles.closeCrossIcon} role="presentation" />
                    </button>
                    <img src={certificateIcon} className={styles.certificateIcon} role="presentation" />
                  </div>
                );
              } else {
                return (
                  <Dropzone
                    className={styles.dropZone}
                    onDrop={this.onDrop}
                    multiple={false}
                    accept={acceptedFileTypes}
                  >
                    <div className={styles.instructions}>
                      <div className={styles.title}>{intl.formatMessage(messages.dropFileHere)}</div>
                      <div className={styles.subtitle}>{intl.formatMessage(messages.orClickToUpload)}</div>
                    </div>
                  </Dropzone>
                );
              }
            })()
          }
        </div>
      </div>
    );
  }

}
