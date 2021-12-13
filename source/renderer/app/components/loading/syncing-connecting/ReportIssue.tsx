import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ReportIssue.scss' or its cor... Remove this comment to see the full error message
import styles from './ReportIssue.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/link-ic... Remove this comment to see the full error message
import externalLinkIcon from '../../../assets/images/link-ic.inline.svg';

const messages = defineMessages({
  reportConnectingIssueText: {
    id: 'loading.screen.reportIssue.connecting.text',
    defaultMessage: '!!!Having trouble connecting to network?',
    description: 'Report connecting issue text on the loading screen.',
  },
  reportIssueButtonLabel: {
    id: 'loading.screen.reportIssue.buttonLabel',
    defaultMessage: '!!!Open support ticket',
    description: 'Open support ticket button label on the loading.',
  },
  readArticleButtonLabel: {
    id: 'loading.screen.readArticle.buttonLabel',
    defaultMessage: '!!!Read the article',
    description: 'Read the article button label on the loading.',
  },
  reportIssueDownloadLogsLinkLabel: {
    id: 'loading.screen.reportIssue.downloadLogsLinkLabel',
    defaultMessage: '!!!Download logs',
    description: 'Download logs button label on the loading.',
  },
  reportIssueButtonUrl: {
    id: 'loading.screen.reportIssue.reportIssueButtonUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description: 'Link to Open Support page',
  },
  connectivityIssueArticleUrl: {
    id: 'loading.screen.readIssueArticle.connectivityIssueArticleUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360010522913',
    description: 'Link to connectivity issue article page',
  },
});
type Props = {
  onIssueClick: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  onDownloadLogs: (...args: Array<any>) => any;
  disableDownloadLogs: boolean;
};
export default class ReportIssue extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onIssueClick,
      onOpenExternalLink,
      onDownloadLogs,
      disableDownloadLogs,
    } = this.props;
    const reportIssueButtonClasses = classNames([
      'primary',
      'reportIssueButton',
      styles.actionButton,
    ]);
    const readArticleButtonClasses = classNames([
      'primary',
      'readArticleButton',
      styles.actionButton,
    ]);
    const downloadLogsButtonClasses = classNames([
      styles.downloadLogsButton,
      disableDownloadLogs ? styles.disabled : null,
    ]);
    const readArticleButtonUrl = messages.connectivityIssueArticleUrl;
    return (
      <div className={styles.component}>
        <h1 className={styles.reportIssueText}>
          {intl.formatMessage(messages.reportConnectingIssueText)}
        </h1>
        <Button
          className={readArticleButtonClasses}
          label={
            <p>
              <SVGInline
                svg={externalLinkIcon}
                className={styles.externalLinkIcon}
              />
              {intl.formatMessage(messages.readArticleButtonLabel)}
            </p>
          }
          onClick={() =>
            onOpenExternalLink(intl.formatMessage(readArticleButtonUrl))
          }
          skin={ButtonSkin}
        />
        <Button
          className={reportIssueButtonClasses}
          label={
            <p>
              <SVGInline
                svg={externalLinkIcon}
                className={styles.externalLinkIcon}
              />
              {intl.formatMessage(messages.reportIssueButtonLabel)}
            </p>
          }
          onClick={() =>
            onIssueClick(intl.formatMessage(messages.reportIssueButtonUrl))
          }
          skin={ButtonSkin}
        />
        <br />
        <Link
          className={downloadLogsButtonClasses}
          onClick={!disableDownloadLogs ? onDownloadLogs : null}
          hasIconAfter={false}
          label={intl.formatMessage(messages.reportIssueDownloadLogsLinkLabel)}
          skin={LinkSkin}
        />
      </div>
    );
  }
}
