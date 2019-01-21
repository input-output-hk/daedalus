// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import styles from './BlockConsolidationStatus.scss';
import TopBar from '../layout/TopBar';

const messages = defineMessages({
  title: {
    id: 'blockConsolidationStatus.title',
    defaultMessage: '!!!Block consolidation status',
    description: 'Title of "Block consolidation status" page.'
  },
  description: {
    id: 'blockConsolidationStatus.description',
    defaultMessage: '!!!Block consolidation status',
    description: 'Description of "Block consolidation status" page.'
  },
  linkText: {
    id: 'blockConsolidationStatus.linkText',
    defaultMessage: '!!!Learn more about this process.',
    description: 'Link Text of "Block consolidation status" page.'
  },
  linkURL: {
    id: 'blockConsolidationStatus.linkURL',
    defaultMessage: '!!!https://cardanoexplorer.com/',
    description: 'Link URL of "Block consolidation status" page.'
  },
  consolidationDescription: {
    id: 'blockConsolidationStatus.consolidationDescription',
    defaultMessage: 'Uninterrupted consolidation usually takes less than 24 hours',
    description: 'Consolidation Description on "Block consolidation status" page.'
  }
});

type Props = {
  onExternalLinkClick: Function,
};

type State = {
};

@observer
export default class BlockConsolidationStatus extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {

    const { formatMessage } = this.context.intl;

    const { onExternalLinkClick } = this.props;

    return (
      <div className={styles.component}>
        <TopBar
          showSubMenuToggle={false}
        />
        <div className={styles.container}>
          <h1>{ formatMessage(messages.title) }</h1>
          <p className={styles.description}>
            <FormattedHTMLMessage {...messages.description} /> &nbsp;
            <a
              href={formatMessage(messages.linkURL)}
              onClick={onExternalLinkClick}
            >
              { formatMessage(messages.linkText) }
            </a>

          </p>

          <div className={styles.indicator}>
            <div className={styles.indicatorContainer}>
              <p className={styles.zeroEpoch}>0 epoch</p>
              <div className={styles.indicatorEpochsSynced} >
                <p>80% synced</p>
              </div>
              <div className={styles.indicatorEpochsConsolidated} >
                <p>75 epochs consolidated</p>
              </div>
              <div className={styles.indicatorEpochsDownloaded} >
                <p>93 epoch</p>
              </div>
              <p className={styles.fullEpoch}>95 epoch</p>
            </div>
          </div>

          <p className={styles.Description}>
            { formatMessage(messages.consolidationDescription) }
          </p>
        </div>
      </div>
    );
  }
}
