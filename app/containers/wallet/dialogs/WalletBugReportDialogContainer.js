// @flow
import React, { Component } from 'react';
import { toJS } from 'mobx';
import { observer, inject } from 'mobx-react';
import WalletBugReportDialog from '../../../components/wallet/WalletBugReportDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletBugReportDialogContainer extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  onSubmit = (values: {
    email: string, subject: ?string, problem: ?string, files: Array<string>
  }) => {
    this.props.actions.profile.sendBugReport.trigger(values);
  };

  render() {
    const { actions, stores } = this.props;
    const { getLogs, compressLogs } = actions.profile;
    const {
      logFiles,
      compressedLogsFiles,
      isCompressing,
      sendBugReport,
      error,
    } = stores.profile;

    return (
      <WalletBugReportDialog
        logFiles={logFiles}
        compressedLogsFiles={toJS(compressedLogsFiles)}
        isCompressing={isCompressing}
        isSubmitting={sendBugReport.isExecuting}
        error={error}
        onSubmit={this.onSubmit}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
        }}
        onGetLogs={() => {
          getLogs.trigger();
        }}
        onCompressLogs={(logs) => {
          compressLogs.trigger({ logs });
        }}
      />
    );
  }
}
