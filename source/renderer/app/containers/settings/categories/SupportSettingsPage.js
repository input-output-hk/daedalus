// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { remote } from "electron";
import SupportSettings from '../../../components/settings/categories/SupportSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';
import BugReportDialog from "../../../components/profile/bug-report/BugReportDialog";

const shell = require('electron').shell;

@inject('stores', 'actions') @observer
export default class SupportSettingsPage extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  handleExternalLinkClick = (event: MouseEvent) => {
    event.preventDefault();
    if (event.target.href) shell.openExternal(event.target.href);
  };

  handleSupportRequestClick = (event: MouseEvent) => {
    event.preventDefault();
    this.props.actions.dialogs.open.trigger({
      dialog: BugReportDialog
    });
  };

  handleDownloadLogs = (event: MouseEvent) => {
    event.preventDefault();
    const destination = remote.dialog.showSaveDialog({
      defaultPath: 'logs.zip',
    });
    if (destination) this.props.actions.profile.downloadLogs.trigger({ destination });
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
