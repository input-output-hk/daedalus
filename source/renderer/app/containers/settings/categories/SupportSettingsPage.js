// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { remote } from 'electron';
import SupportSettings from '../../../components/settings/categories/SupportSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../../../common/fileName';
import { getSupportUrl } from '../../../utils/network';

const messages = defineMessages({
  supportRequestLinkUrl: {
    id: 'settings.support.reportProblem.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/categories/360000877653-Daedalus-wallet-mainnet',
    description: '"Support request" link URL in the "Report a problem" section on the support settings page.',
  },
});

const shell = require('electron').shell;

@inject('stores', 'actions') @observer
export default class SupportSettingsPage extends Component<InjectedProps> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  handleExternalLinkClick = (
    event: MouseEvent | SyntheticEvent<HTMLButtonElement>, url: string
  ) => {
    event.preventDefault();
    shell.openExternal(url);
  };

  handleSupportRequestClick = async (event: SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const supportRequestLinkUrl = intl.formatMessage(messages.supportRequestLinkUrl);
    const locale = this.props.stores.profile.currentLocale;
    const supportUrl = await getSupportUrl(supportRequestLinkUrl, locale);
    this.handleExternalLinkClick(event, supportUrl);
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
