import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import path from 'path';
import { showOpenDialogChannel } from '../../../ipc/show-file-dialog-channels';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/attach-... Remove this comment to see the full error message
import attachIcon from '../../../assets/images/attach-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './FileUploadWidget.scss' or it... Remove this comment to see the full error message
import styles from './FileUploadWidget.scss';
import type { FileDialogRequestParams } from '../../../../../common/types/file-dialog.types';

type Props = {
  label: string;
  placeholder: string;
  onFileSelected: (...args: Array<any>) => any;
  selectedFile: string;
  acceptedFileTypes: Array<string>;
};

@observer
class FileUploadWidget extends Component<Props> {
  onOpen = async () => {
    const params: FileDialogRequestParams = {
      filters: [
        {
          name: 'file-upload',
          extensions: this.props.acceptedFileTypes,
        },
      ],
      properties: ['openFile'],
    };
    const { filePaths } = await showOpenDialogChannel.send(params);

    if (!filePaths || filePaths.length === 0) {
      return;
    }

    const filePath = filePaths[0];
    this.props.onFileSelected(filePath);
  };
  onDragOver = () => false;
  onDragLeave = () => false;
  onDragEnd = () => false;
  onDrop = (e: any) => {
    e.preventDefault();
    const { files } = e.dataTransfer;

    if (!files || files.length === 0) {
      return;
    }

    const filePath = files[0].path;
    this.props.onFileSelected(filePath);
  };

  render() {
    const { label, placeholder, selectedFile } = this.props;
    const fileName = path.basename(selectedFile);
    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
        <button
          className={styles.dropZone}
          onClick={this.onOpen}
          onDragOver={this.onDragOver}
          onDragLeave={this.onDragLeave}
          onDragEnd={this.onDragEnd}
          onDrop={this.onDrop}
        >
          {selectedFile ? (
            <div className={styles.fileName}>{fileName}</div>
          ) : (
            <div className={styles.placeholder}>{placeholder}</div>
          )}
          <SVGInline svg={attachIcon} className={styles.attachIcon} />
        </button>
      </div>
    );
  }
}

export default FileUploadWidget;
