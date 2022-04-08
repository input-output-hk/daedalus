import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { FormattedMessage, injectIntl, Intl } from 'react-intl';
import classNames from 'classnames';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import styles from './SupportSettings.scss';
import globalMessages from '../../../i18n/global-messages';
import AnalyticsForm from '../../profile/analytics/AnalyticsForm';
import { AnalyticsAcceptanceStatus } from '../../../analytics/types';
import { InjectedProps } from '../../../types/injectedPropsType';
import { messages } from './SupportSettings.messages';
import { Separator } from '../../widgets/separator/Separator';

interface SupportSettingsProps extends InjectedProps {
  intl: Intl;
  onExternalLinkClick: (...args: Array<any>) => any;
  onSupportRequestClick: (...args: Array<any>) => any;
  onDownloadLogs: (...args: Array<any>) => any;
  disableDownloadLogs: boolean;
}

interface SupportSettingsState {
  analyticsAccepted: boolean;
  pageViewEventSent: boolean;
}

@inject('stores', 'actions')
@observer
class SupportSettings extends Component<
  SupportSettingsProps,
  SupportSettingsState
> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  state = {
    analyticsAccepted: false,
    pageViewEventSent: false,
  };

  async componentDidMount() {
    const analyticsAcceptanceStatus = await this.props.stores.analytics.api.localStorage.getAnalyticsAcceptance();

    this.setState({
      analyticsAccepted:
        analyticsAcceptanceStatus === AnalyticsAcceptanceStatus.ACCEPTED,
    });
  }

  onAnalyticsAcceptanceChange = async (analyticsAccepted: boolean) => {
    await this.props.actions.profile.acceptAnalytics.trigger(
      analyticsAccepted
        ? AnalyticsAcceptanceStatus.ACCEPTED
        : AnalyticsAcceptanceStatus.REJECTED
    );
    await this.props.stores.analytics.resetAnalyticsClient();

    let { pageViewEventSent } = this.state;

    if (!pageViewEventSent && analyticsAccepted) {
      this.props.stores.analytics.analyticsClient.sendPageNavigationEvent(
        'Support Settings'
      );
      pageViewEventSent = true;
    }

    this.setState({
      analyticsAccepted,
      pageViewEventSent,
    });
  };

  render() {
    const {
      onExternalLinkClick,
      onSupportRequestClick,
      onDownloadLogs,
      disableDownloadLogs,
      intl,
    } = this.props;
    const faqLinkUrl = intl.formatMessage(globalMessages.faqLinkUrl);
    const faqLink = (
      <span className={styles.faqLink}>
        <Link
          className={styles.externalLink}
          onClick={(event) => onExternalLinkClick(faqLinkUrl, event)}
          label={intl.formatMessage(messages.faqLink)}
          skin={LinkSkin}
        />
      </span>
    );
    const downloadLogsClasses = classNames([
      styles.externalLink,
      disableDownloadLogs ? styles.disabled : null,
    ]);
    const stepsDownloadLogsLink = (
      <Link
        className={downloadLogsClasses}
        onClick={onDownloadLogs}
        hasIconAfter={false}
        label={intl.formatMessage(messages.stepsDownloadLogsLink)}
        skin={LinkSkin}
      />
    );
    const reportProblemLink = (
      <Link
        className={styles.externalLink}
        onClick={onSupportRequestClick}
        label={intl.formatMessage(messages.stepsReportProblemLink)}
        skin={LinkSkin}
      />
    );
    return (
      <>
        <div className={styles.supportGuide}>
          {/* Help and Support */}

          <h1>{intl.formatMessage(messages.faqTitle)}</h1>

          <p>
            <FormattedMessage
              {...messages.faqContent}
              values={{
                faqLink,
              }}
            />
          </p>

          {/* Steps for creating a support request: */}

          <h1>{intl.formatMessage(messages.stepsTitle)}</h1>

          <ol>
            <li>
              <h2>{intl.formatMessage(messages.stepsDownloadLogsTitle)}</h2>
              <p>
                <FormattedMessage
                  {...messages.stepsDownloadLogsDescription}
                  values={{
                    stepsDownloadLogsLink,
                  }}
                />
              </p>
            </li>
            <li>
              <h2>{intl.formatMessage(messages.stepsReportProblemTitle)}</h2>
              <p>
                <FormattedMessage
                  {...messages.stepsReportProblemDescription}
                  values={{
                    reportProblemLink,
                  }}
                />
              </p>
            </li>
          </ol>
        </div>
        <Separator />
        <AnalyticsForm
          intl={intl}
          onAnalyticsAcceptanceChange={this.onAnalyticsAcceptanceChange}
          analyticsAccepted={this.state.analyticsAccepted}
        />
      </>
    );
  }
}

export default injectIntl(SupportSettings);
