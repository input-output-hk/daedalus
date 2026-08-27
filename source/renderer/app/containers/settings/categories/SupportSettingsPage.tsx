import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SupportSettings from '../../../components/settings/categories/SupportSettings';
import {
  generateSupportRequestLink,
  getSupportUrl,
} from '../../../../../common/utils/reporting';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { AnalyticsAcceptanceStatus } from '../../../analytics';
import { ROUTES } from '../../../routes-config';

@inject('stores', 'actions')
@observer
class SupportSettingsPage extends Component<InjectedProps> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  handleChangeAnalyticsSettings = () => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.PROFILE.ANALYTICS,
    });
  };

  handleSupportRequestClick = async (
    event: React.SyntheticEvent<HTMLButtonElement>
  ) => {
    event.preventDefault();
    event.stopPropagation();
    const locale = this.props.stores.profile.currentLocale;
    const { environment } = this.props.stores.app;
    const supportUrl = generateSupportRequestLink(
      getSupportUrl(locale),
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
      <SupportSettings
        onExternalLinkClick={stores.app.openExternalLink}
        onSupportRequestClick={this.handleSupportRequestClick}
        onDownloadLogs={this.handleDownloadLogs}
        onChangeAnalyticsSettings={this.handleChangeAnalyticsSettings}
        disableDownloadLogs={
          this.props.stores.app.isDownloadNotificationVisible
        }
        analyticsAccepted={
          this.props.stores.profile.analyticsAcceptanceStatus ===
          AnalyticsAcceptanceStatus.ACCEPTED
        }
      />
    );
  }
}

export default SupportSettingsPage;
