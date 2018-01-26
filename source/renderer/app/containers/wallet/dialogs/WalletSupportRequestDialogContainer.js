// @flow
import React, { Component } from 'react';
import { toJS } from 'mobx';
import { observer, inject } from 'mobx-react';
import WalletSupportRequestDialog from '../../../components/wallet/WalletSupportRequestDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletSupportRequestDialogContainer extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  onSubmit = (values: {
    email: string, subject: ?string, problem: ?string, files: Array<string>
  }) => {
    this.props.actions.profile.sendSupportRequest.trigger(values);
  };

  render() {
    const { actions, stores } = this.props;
    const { getLogs, compressLogs } = actions.profile;
    const {
      logFiles,
      compressedLogsFiles,
      isCompressing,
      sendSupportRequest,
      error,
    } = stores.profile;

    return (
      <WalletSupportRequestDialog
        logFiles={logFiles}
        compressedLogsFiles={toJS(compressedLogsFiles)}
        isCompressing={isCompressing}
        isSubmitting={sendSupportRequest.isExecuting}
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
