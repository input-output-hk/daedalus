// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { remote } from 'electron';
import SupportSettings from '../../../components/settings/categories/SupportSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';
import BugReportDialog from '../../../components/profile/bug-report/BugReportDialog';
import { generateFileNameWithTimestamp } from '../../../../../common/fileName';

const shell = require('electron').shell;

@inject('stores', 'actions') @observer
export default class SupportSettingsPage extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  handleExternalLinkClick = (event: MouseEvent) => {
    event.preventDefault();
    if (event.target.href) shell.openExternal(event.target.href);
  };

  handleSupportRequestClick = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: BugReportDialog,
    });
  };

  handleDownloadLogs = () => {
    const fileName = generateFileNameWithTimestamp();
    const destination = remote.dialog.showSaveDialog({
      defaultPath: fileName,
    });
    if (destination) {
      this.props.actions.profile.downloadLogs.trigger({ fileName, destination, fresh: true });
    }
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
