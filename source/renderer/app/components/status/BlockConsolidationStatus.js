// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './BlockConsolidationStatus.scss';
import TopBar from '../layout/TopBar';
import epochs from '../../assets/images/block-consolidation/epochs.png';

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
  },
  supportButton: {
    id: 'blockConsolidationStatus.supportButton',
    defaultMessage: '!!!Support',
    description: 'Support Button on "Block consolidation status" page.'
  },
  epochsConsolidatedOfTotal: {
    id: 'blockConsolidationStatus.epochsConsolidatedOfTotal',
    defaultMessage: '!!!<p><b>{consolidated}</b> <em>of</em> <b>{downloaded}</b> epochs consolidated</p>',
    description: 'Epochs Consolidated on "Block consolidation status" page.'
  },

});

type Props = {
  onExternalLinkClick: Function,
  epochsConsolidated: number,
  currentEpoch: number,
  currentEpoch: number,
  epochsSynced: number
};

type State = {
};

@observer
export default class BlockConsolidationStatus extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  getWidthOfEpochsConsolidated = (
    epochsConsolidated: number,
    currentEpoch: number,
  ) => `${epochsConsolidated * 100 / (currentEpoch - 2)}%`;

  render() {

    const {
      epochsConsolidated,
      currentEpoch,
      epochsSynced
    } = this.props;

    const { formatMessage } = this.context.intl;

    const { onExternalLinkClick } = this.props;

    return (
      <div className={styles.component}>
        <TopBar
          showSubMenuToggle={false}
          currentRoute=""
        />
        <div className={styles.container}>
          <h1>{ formatMessage(messages.title) }</h1>
          <p className={styles.description}>
            <FormattedHTMLMessage {...messages.description} /> &nbsp;
            <a
              href={formatMessage(messages.linkURL)}
              onClick={(event) => onExternalLinkClick(formatMessage(messages.linkURL), event)}
            >
              { formatMessage(messages.linkText) }
            </a>

          </p>

          <div className={styles.epochs} >
            <FormattedHTMLMessage
              {...messages.epochsConsolidatedOfTotal}
              values={{
                consolidated: epochsConsolidated,
                downloaded: currentEpoch
              }}
            />
            <img src={epochs} role="presentation" draggable="false" />
          </div>

          <div className={styles.indicator}>
            <div className={styles.indicatorContainer}>
              <p className={styles.zeroEpoch}>0 epoch</p>
              <div
                className={styles.indicatorEpochsSynced}
                style={{
                  width: `${epochsSynced}%`
                }}
              >
                <div
                  className={styles.indicatorEpochsBehind}
                >
                  <p>{ currentEpoch - 2 } epoch</p>
                </div>
                <p>{ epochsSynced }% synced</p>
              </div>
              <div
                className={styles.indicatorEpochsConsolidated}
                style={{
                  width: this.getWidthOfEpochsConsolidated(epochsConsolidated, currentEpoch)
                }}
              >
                <p>{ epochsConsolidated } epochs consolidated</p>
              </div>
              <p className={styles.fullEpoch}>{ currentEpoch } epoch</p>
            </div>
          </div>

          <p className={styles.consolidationDescription}>
            { formatMessage(messages.consolidationDescription) }
          </p>

          <Button
            label={formatMessage(messages.supportButton)}
            onClick={() => onExternalLinkClick(formatMessage(messages.linkURL))}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}
