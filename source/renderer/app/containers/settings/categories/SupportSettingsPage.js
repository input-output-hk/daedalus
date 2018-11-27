// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { remote } from 'electron';
import SupportSettings from '../../../components/settings/categories/SupportSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../../../common/fileName';

const shell = require('electron').shell;

@inject('stores', 'actions') @observer
export default class SupportSettingsPage extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  handleExternalLinkClick = (event: MouseEvent, url: string) => {
    event.preventDefault();
    shell.openExternal(url);
  };

  handleSupportRequestClick = () => {
    // this.props.actions.dialogs.open.trigger({
    //   dialog: BugReportDialog,
    // });
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
