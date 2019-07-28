// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';

import styles from './ReportIssue.scss';
import externalLinkIcon from '../../../assets/images/link-ic.inline.svg';

const messages = defineMessages({
  reportConnectingIssueText: {
    id: 'loading.screen.reportIssue.connecting.text',
    defaultMessage: '!!!Having trouble connecting to network?',
    description: 'Report connecting issue text on the loading screen.',
  },
  reportSyncingIssueText: {
    id: 'loading.screen.reportIssue.syncing.text',
    defaultMessage: '!!!Having trouble syncing?',
    description: 'Report syncing issue text on the loading screen.',
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
  syncIssueArticleUrl: {
    id: 'loading.screen.readIssueArticle.syncIssueArticleUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360011536933',
    description: 'Link to sync issue article page',
  },
  connectivityIssueArticleUrl: {
    id: 'loading.screen.readIssueArticle.connectivityIssueArticleUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360010522913',
    description: 'Link to connectivity issue article page',
  },
});

type Props = {
  isConnected: boolean,
  onIssueClick: Function,
  onDownloadLogs: Function,
  disableDownloadLogs: boolean,
  isConnecting: boolean,
  isSyncing: boolean,
};

export default class ReportIssue extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      isConnected,
      onIssueClick,
      onDownloadLogs,
      disableDownloadLogs,
      isConnecting,
      isSyncing,
    } = this.props;

    const componentStyles = classNames([
      styles.component,
      isConnecting ? styles['is-connecting'] : null,
      isSyncing ? styles['is-syncing'] : null,
    ]);

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
      !isConnected ? styles.downloadLogsButtonConnecting : null,
    ]);

    const readArticleButtonUrl = isConnected
      ? messages.syncIssueArticleUrl
      : messages.connectivityIssueArticleUrl;

    return (
      <div className={componentStyles}>
        <h1 className={styles.reportIssueText}>
          {!isConnected
            ? intl.formatMessage(messages.reportConnectingIssueText)
            : intl.formatMessage(messages.reportSyncingIssueText)}
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
          onClick={() => onIssueClick(intl.formatMessage(readArticleButtonUrl))}
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
        <button
          className={downloadLogsButtonClasses}
          onClick={onDownloadLogs}
          disabled={disableDownloadLogs}
        >
          {intl.formatMessage(messages.reportIssueDownloadLogsLinkLabel)}
        </button>
      </div>
    );
  }
}
