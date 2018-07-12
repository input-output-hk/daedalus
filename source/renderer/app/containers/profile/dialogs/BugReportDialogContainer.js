// @flow
import React, { Component } from 'react';
import { remote } from 'electron';
import { get } from 'lodash';
import { observer, inject } from 'mobx-react';
import BugReportDialog from '../../../components/profile/bug-report/BugReportDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../../../common/fileName';

const shell = require('electron').shell;

@inject('stores', 'actions') @observer
export default class BugReportDialogContainer extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  onSubmit = (values: {
    email: string, subject: string, problem: string, compressedLogsFile: ?string
  }) => {
    this.props.actions.profile.sendBugReport.trigger(values);
  };

  onDownload = () => {
    const fileName = generateFileNameWithTimestamp();
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
    const { getLogs, getLogsAndCompress } = actions.profile;
    const {
      logFiles,
      compressedLogsFile,
      compressedLogsStatus,
      isSubmittingBugReport,
      error,
    } = stores.profile;

    return (
      <BugReportDialog
        isDownloading={get(compressedLogsStatus, 'isDownloading', false)}
        isSubmittingBugReport={isSubmittingBugReport}
        logFiles={logFiles}
        compressedLogsFile={compressedLogsFile}
        error={error}
        onSubmit={this.onSubmit}
        onSubmitManually={this.onSubmitManually}
        onDownload={this.onDownload}
        onCancel={this.resetBugReportDialog}
        onGetLogsAndCompress={(logs) => {
          getLogsAndCompress.trigger({ logs });
        }}
        onGetLogs={() => {
          getLogs.trigger();
        }}
      />
    );
  }
}
