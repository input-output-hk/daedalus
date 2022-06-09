import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { FormattedMessage, injectIntl, Intl } from 'react-intl';
import classNames from 'classnames';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import styles from './SupportSettings.scss';
import globalMessages from '../../../i18n/global-messages';
import { InjectedProps } from '../../../types/injectedPropsType';
import { messages } from './SupportSettings.messages';
import { Separator } from '../../widgets/separator/Separator';

interface SupportSettingsProps {
  intl: Intl;
  onExternalLinkClick: (...args: Array<any>) => any;
  onSupportRequestClick: (...args: Array<any>) => any;
  onChangeAnalyticsSettings: () => void;
  onDownloadLogs: (...args: Array<any>) => any;
  disableDownloadLogs: boolean;
  analyticsAccepted: boolean;
}

@observer
class SupportSettings extends Component<SupportSettingsProps> {
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

    const changeAnalyticsSettingsLink = (
      <Link
        className={styles.changeAnalyticsSettingsLink}
        onClick={this.props.onChangeAnalyticsSettings}
        label={intl.formatMessage(messages.changeAnalyticsSettingsLink)}
        hasIconAfter={false}
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

        <h2 className={styles.analyticsSectionTitle}>
          {intl.formatMessage(messages.analyticsSectionTitle)}
        </h2>
        <p className={styles.analyticsSectionDescription}>
          <FormattedMessage
            {...(this.props.analyticsAccepted
              ? messages.analyticsAcceptedDescription
              : messages.analyticsDeclinedDescription)}
            values={{
              changeAnalyticsSettingsLink,
            }}
          />
        </p>
      </>
    );
  }
}

export default injectIntl(SupportSettings);
