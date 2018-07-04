// @flow
import React, { Component } from 'react';
import { remote } from 'electron';
import { get } from 'lodash';
import { observer, inject } from 'mobx-react';
import BugReportDialog from '../../../components/profile/bug-report/BugReportDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { filenameWithTimestamp } from '../../../../../common/fileName';

const shell = require('electron').shell;

@inject('stores', 'actions') @observer
export default class BugReportDialogContainer extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  onSubmit = (values: {
    email: string, subject: string, problem: string, compressedLog: ?string
  }) => {
    this.props.actions.profile.sendBugReport.trigger(values);
  };

  onDownload = () => {
    const fileName = filenameWithTimestamp();
    const destination = remote.dialog.showSaveDialog({
      defaultPath: fileName,
    });
    if (destination) {
      this.props.actions.profile.downloadLogs.trigger({ fileName, destination, fresh: true });
    }
  };

  onSubmitManually = (link: string) => {
    shell.openExternal(`https://${link}`);
  };

  resetBugReportDialog = () => {
    this.props.actions.profile.resetBugReportDialog.trigger();
  };

  render() {
    const { actions, stores } = this.props;
    const { requestFreshCompressedLogs } = actions.profile;
    const {
      logFiles,
      compressedLog,
      compressedFileDownload,
      error,
    } = stores.profile;

    return (
      <BugReportDialog
        isDownloading={get(compressedFileDownload, 'inProgress', false)}
        logFiles={logFiles}
        compressedLog={compressedLog}
        error={error}
        onSubmit={this.onSubmit}
        onSubmitManually={this.onSubmitManually}
        onDownload={this.onDownload}
        onCancel={this.resetBugReportDialog}
        onRequestFreshCompressedLogs={(logs) => {
          requestFreshCompressedLogs.trigger({ logs });
        }}
      />
    );
  }
}
