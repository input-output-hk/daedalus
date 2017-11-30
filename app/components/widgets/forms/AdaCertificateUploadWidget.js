// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SvgInline from 'react-svg-inline';
import Dropzone from 'react-dropzone';
import { intlShape } from 'react-intl';
import certificateNormalIcon from '../../../assets/images/cert-ic.inline.svg';
import certificateLockedIcon from '../../../assets/images/cert-locked-ic.inline.svg';
import certificateInvalidIcon from '../../../assets/images/cert-bad-ic.inline.svg';
import closeCrossIcon from '../../../assets/images/close-cross.inline.svg';
import styles from './AdaCertificateUploadWidget.scss';
import { messages } from './ImageUploadWidget';

type Props = {
  label: string,
  onFileSelected: Function,
  onRemoveCertificate: Function,
  acceptedFileTypes: string,
  isCertificateEncrypted: boolean,
  isCertificateSelected: boolean,
  isCertificateInvalid: boolean,
};

@observer
export default class AdaCertificateUploadWidget extends Component<Props> {

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

    let certificateIcon;
    if (isCertificateEncrypted) {
      certificateIcon = certificateLockedIcon;
    } else {
      certificateIcon = isCertificateInvalid ? certificateInvalidIcon : certificateNormalIcon;
    }
    return (
      <div>
        <div className={styles.label}>{label}</div>
        <div className={styles.uploadBox}>
          {isCertificateSelected ? (
            <div className={styles.certificateUploaded}>
              <button className={styles.removeFileButton} onClick={onRemoveCertificate}>
                <SvgInline svg={closeCrossIcon} className={styles.closeCrossIcon} />
              </button>
              <SvgInline svg={certificateIcon} className={styles.certificateIcon} />
            </div>
          ) : (
            <Dropzone
              className={styles.dropZone}
              onDrop={this.onDrop}
              multiple={false}
              accept={acceptedFileTypes}
            >
              <div className={styles.instructions}>
                <div className={styles.title}>
                  {intl.formatMessage(messages.orClickToUpload)}
                </div>
              </div>
            </Dropzone>
          )}
        </div>
      </div>
    );
  }

}
