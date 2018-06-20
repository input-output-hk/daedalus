// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './IssuesDetection.scss';

const messages = defineMessages({
  issuesTitle: {
    id: 'settings.support.issues.title',
    defaultMessage: '!!!Automatic detection of known issues',
    description: 'Title "Automatic detection of known issues" on the support settings page.',
  },
  issuesContentAnalyzing: {
    id: 'settings.support.issues.contentAnalyzing',
    defaultMessage: '!!!Analyzing logs for known issues...',
    description: 'Content for the "Automatic detection of known issues" section on the support settings page.',
  },
  issuesContentNotFound: {
    id: 'settings.support.issues.contentNotFound',
    defaultMessage: '!!!No known issues found.',
    description: 'Content for the "Automatic detection of known issues" section on the support settings page.',
  },
  issuesContentFound: {
    id: 'settings.support.issues.contentFound',
    defaultMessage: '!!!We have found a known issue after analyzing your logs. Please, use the FAQ with the solution for the issue you are experiencing. If you continue to experience the issue after completing steps from the instructions, please send a support request from the problem reporting section below',
    description: 'Content for the "Automatic detection of known issues" section on the support settings page.',
  },
  issuesFaqLink: {
    id: 'settings.support.issues.faqLink',
    defaultMessage: '!!!See solution in the FAQ',
    description: '"FAQ on Daedalus website" link in the ISSUES section on the support settings page',
  },
  faqLinkUrl: {
    id: 'settings.support.faq.faqLinkURL',
    defaultMessage: '!!!https://daedaluswallet.io/faq/',
    description: 'URL for the "FAQ on Daedalus website" link in the FAQ section on the support settings page',
  },
});

type Props = {
  isAnalyzingIssues: boolean,
  issuesDetected: Array<any>,
  onExternalLinkClick: Function,
};

export type Issue = {
  title: string
};

@observer
export default class SupportSettings extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      onExternalLinkClick,
      isAnalyzingIssues,
      issuesDetected,
    } = this.props;
    const { intl } = this.context;

    const issueFaqLink = (
      <a
        href={intl.formatMessage(messages.faqLinkUrl)}
        onClick={event => onExternalLinkClick(event)}
      >
        {intl.formatMessage(messages.issuesFaqLink)}
      </a>
    );

    let issuesTitle;
    if (isAnalyzingIssues) issuesTitle = intl.formatMessage(messages.issuesContentAnalyzing);
    else if (issuesDetected.length) issuesTitle = intl.formatMessage(messages.issuesContentFound);
    else issuesTitle = intl.formatMessage(messages.issuesContentNotFound);

    return (
      <div className={styles.component}>

        <h1>{intl.formatMessage(messages.issuesTitle)}</h1>

        {
          (isAnalyzingIssues) &&
          <span className={styles.spinning} />
        }

        <p>{ issuesTitle }</p>

        <ul>
          {
            (!isAnalyzingIssues && issuesDetected.length)
            ? (
                issuesDetected.map((issue: any, index) => (
                  <li key={index}>
                    {issue.title}
                    &nbsp;&rarr;&nbsp;
                    { issueFaqLink }
                  </li>
                ))
              )
            : false
          }
        </ul>

      </div>
    );
  }

}
