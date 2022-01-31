// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { generateSupportRequestLink } from '../../../../../common/utils/reporting';
import type { InjectedProps } from '../../../types/injectedPropsType';
import AnalyticsSettings from '../../../components/settings/categories/AnalyticsSettings';

const messages = defineMessages({
  supportRequestLinkUrl: {
    id: 'settings.support.reportProblem.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description:
      '"submit a support request" link URL in the "Report a problem" section on the support settings page.',
  },
});

@inject('stores', 'actions')
@observer
class AnalyticsSettingsPage extends Component<InjectedProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  handleSupportRequestClick = async (
    event: SyntheticEvent<HTMLButtonElement>
  ) => {
    event.preventDefault();
    event.stopPropagation();
    const { intl } = this.context;
    const supportRequestLinkUrl = intl.formatMessage(
      messages.supportRequestLinkUrl
    );
    const locale = this.props.stores.profile.currentLocale;
    const { environment } = this.props.stores.app;
    const supportUrl = generateSupportRequestLink(
      supportRequestLinkUrl,
      environment,
      locale
    );
    this.props.stores.app.openExternalLink(supportUrl);
  };

  handleDownloadLogs = () => {
    const { app } = this.props.actions;
    app.downloadLogs.trigger();
    app.setIsDownloadingLogs.trigger(true);
  };

  render() {
    const { stores } = this.props;

    return (
      // TODO
      <AnalyticsSettings
        onExternalLinkClick={stores.app.openExternalLink}
        onSupportRequestClick={this.handleSupportRequestClick}
        onDownloadLogs={this.handleDownloadLogs}
        disableDownloadLogs={
          this.props.stores.app.isDownloadNotificationVisible
        }
      />
    );
  }
}

export default AnalyticsSettingsPage