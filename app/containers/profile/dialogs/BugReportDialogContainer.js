// @flow
import React, { Component } from 'react';
import { toJS } from 'mobx';
import { get } from 'lodash';
import { observer, inject } from 'mobx-react';
import BugReportDialog from '../../../components/profile/bug-report/BugReportDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class BugReportDialogContainer extends Component<InjectedProps> {

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
      compressedLogs,
      isCompressing,
      sendBugReport,
      error,
    } = stores.profile;

    const rawCompressedLogs = {
      files: toJS(get(compressedLogs, 'files', [])), // transform MobX array to JS array
      path: get(compressedLogs, 'path'),
    };

    return (
      <BugReportDialog
        logFiles={logFiles}
        compressedLogs={rawCompressedLogs}
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
