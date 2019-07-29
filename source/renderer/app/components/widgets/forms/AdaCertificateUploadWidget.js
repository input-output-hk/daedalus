// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { intlShape } from 'react-intl';
import { generateFileContent } from '../../../utils/fileContentGenerator';
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

  onOpen = () => {
    global.dialog.showOpenDialog(
      {
        filters: [
          {
            name: 'file-upload',
            extensions: this.props.acceptedFileTypes,
          },
        ],
        properties: ['openFile'],
      },
      async files => {
        if (!files) {
          return;
        }

        try {
          const filePath = files[0];
          const fileContent = await generateFileContent({ filePath });
          const {
            fileBuffer,
            fileLastModified,
            fileName,
            fileType,
          } = fileContent;
          const fileBlob = new Blob([fileBuffer]);
          const file = new File([fileBlob], fileName, {
            lastModified: fileLastModified,
            type: fileType,
          });
          this.props.onFileSelected(filePath, file);
        } catch (error) {} // eslint-disable-line
      }
    );
  };

  render() {
    const { intl } = this.context;
    const {
      label,
      isCertificateEncrypted,
      isCertificateSelected,
      onRemoveCertificate,
      isCertificateInvalid,
    } = this.props;

    let certificateIcon;
    if (isCertificateEncrypted) {
      certificateIcon = certificateLockedIcon;
    } else {
      certificateIcon = isCertificateInvalid
        ? certificateInvalidIcon
        : certificateNormalIcon;
    }
    return (
      <div>
        <div className={styles.label}>{label}</div>
        <div className={styles.uploadBox}>
          {isCertificateSelected ? (
            <div className={styles.certificateUploaded}>
              <button
                className={styles.removeFileButton}
                onClick={onRemoveCertificate}
              >
                <SVGInline
                  svg={closeCrossIcon}
                  className={styles.closeCrossIcon}
                />
              </button>
              <SVGInline
                svg={certificateIcon}
                className={styles.certificateIcon}
              />
            </div>
          ) : (
            <button className={styles.dropZone} onClick={this.onOpen}>
              <div className={styles.instructions}>
                <div className={styles.title}>
                  {intl.formatMessage(messages.orClickToUpload)}
                </div>
              </div>
            </button>
          )}
        </div>
      </div>
    );
  }
}
