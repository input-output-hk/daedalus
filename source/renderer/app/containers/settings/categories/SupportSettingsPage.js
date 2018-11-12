// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SupportSettings from '../../../components/settings/categories/SupportSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';
import BugReportDialog from '../../../components/profile/bug-report/BugReportDialog';
import { generateFileNameWithTimestamp } from '../../../../../common/fileName';
import { openExternalUrlChannel } from '../../../ipc/open-external-url';

@inject('stores', 'actions') @observer
export default class SupportSettingsPage extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  handleExternalLinkClick = (event: MouseEvent, url: string) => {
    event.preventDefault();
    openExternalUrlChannel.send(url);
  };

  handleSupportRequestClick = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: BugReportDialog,
    });
  };

  handleDownloadLogs = () => {
    // TODO: Implement dialog with IPC channel
    // const fileName = generateFileNameWithTimestamp();
    // const destination = remote.dialog.showSaveDialog({
    //   defaultPath: fileName,
    // });
    // if (destination) {
    //   this.props.actions.profile.downloadLogs.trigger({ fileName, destination, fresh: true });
    // }
  };

  render() {
    return (
      <SupportSettings
        onExternalLinkClick={this.handleExternalLinkClick}
        onSupportRequestClick={this.handleSupportRequestClick}
        onDownloadLogs={this.handleDownloadLogs}
      />
    );
  }

}
