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

          <div className={styles.consolidationIndicator}>
            <div className={styles.consolidationIndicatorContainer}>
              <p className={styles.zeroEpoch}>0 epoch</p>
              <div className={styles.consolidationIndicatorEpochsSynced} >
                <p>93 epoch</p>
              </div>
              <div className={styles.consolidationIndicatorEpochsConsolidated} >
                <p>75 epochs consolidated</p>
              </div>
              <p className={styles.fullEpoch}>95 epoch</p>
            </div>
          </div>

          <p className={styles.consolidationDescription}>
            { formatMessage(messages.consolidationDescription) }
          </p>
        </div>
      </div>
    );
  }
}
