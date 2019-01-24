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
    defaultMessage: '!!!Block storage consolidation status',
    description: 'Title of "Block consolidation status" page.'
  },
  description1: {
    id: 'blockConsolidationStatus.description1',
    defaultMessage: '!!!Block storage is being consolidated.',
    description: 'Description 1 of "Block consolidation status" page.'
  },
  description2: {
    id: 'blockConsolidationStatus.description2',
    defaultMessage: '!!!Blocks for the current epoch <b>({currentEpoch})</b> and the previous epoch <b>({currentEpochBehind})</b> are stored as one file per block. All previous epochs will be consolidated to two files per epoch.',
    description: 'Description 2 of "Block consolidation status" page.'
  },
  description3: {
    id: 'blockConsolidationStatus.description3',
    defaultMessage: '!!!This reduces the number of files and the amount of hard drive space required to store the blockchain on your machine.',
    description: 'Description 3 of "Block consolidation status" page.'
  },
  epochsConsolidatedOfTotal: {
    id: 'blockConsolidationStatus.epochsConsolidatedOfTotal',
    defaultMessage: '!!!<p><b>{consolidated}</b> <em>of</em> <b>{downloaded}</b> epochs consolidated</p>',
    description: 'Epochs Consolidated Of Total on "Block consolidation status" page.'
  },
  epoch: {
    id: 'blockConsolidationStatus.epoch',
    defaultMessage: '!!!epoch',
    description: 'Singular Epoch on "Block consolidation status" page.'
  },
  epochs: {
    id: 'blockConsolidationStatus.epochs',
    defaultMessage: '!!!epochs',
    description: 'Plural Epochs on "Block consolidation status" page.'
  },
  epochsConsolidated: {
    id: 'blockConsolidationStatus.epochsConsolidated',
    defaultMessage: '!!!epochs consolidated',
    description: 'Epochs consolidated on "Block consolidation status" page.'
  },
  synced: {
    id: 'blockConsolidationStatus.synced',
    defaultMessage: '!!!synced',
    description: 'synced on "Block consolidation status" page.'
  },
  supportButton: {
    id: 'blockConsolidationStatus.supportButton',
    defaultMessage: '!!!Support',
    description: 'Support Button on "Block consolidation status" page.'
  },
  supportButtonURL: {
    id: 'blockConsolidationStatus.supportButtonURL',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/articles/360016060314',
    description: 'URL of Support Button on "Block consolidation status" page.'
  },

});

type Props = {
  currentEpoch: number,
  epochsConsolidated: number,
  epochsSynced: number,
  onExternalLinkClick: Function,
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
  ) => epochsConsolidated * 100 / (currentEpoch - 2);

  getPositionOfEpochsConsolidated = (widthOfEpochsConsolidated: number) => {
    if (widthOfEpochsConsolidated > 40) {
      return {
        right: 8
      };
    }
    return {
      left: 0,
      textAlign: 'left',
    };
  };

  render() {

    const {
      currentEpoch,
      epochsConsolidated,
      epochsSynced,
      onExternalLinkClick
    } = this.props;

    const { formatMessage } = this.context.intl;

    const widthOfEpochsConsolidated =
      this.getWidthOfEpochsConsolidated(epochsConsolidated, currentEpoch);

    return (
      <div className={styles.component}>
        <TopBar
          showSubMenuToggle={false}
          currentRoute=""
        />
        <div className={styles.container}>
          <h1>{ formatMessage(messages.title) }</h1>
          <p className={styles.description}>
            { formatMessage(messages.description1) }
          </p>
          <p className={styles.description}>
            <FormattedHTMLMessage
              {...messages.description2}
              values={{
                currentEpoch,
                currentEpochBehind: currentEpoch - 2,
              }}
            />
          </p>
          <p className={styles.description}>
            { formatMessage(messages.description3) }
          </p>

          <div className={styles.epochs}>
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
              <p className={styles.zeroEpoch}>0 { formatMessage(messages.epoch) }</p>
              <div
                className={styles.indicatorEpochsBehind}
              >
                <p>{ currentEpoch - 2 } { formatMessage(messages.epoch) }</p>
              </div>
              <div
                className={styles.indicatorEpochsSynced}
                style={{
                  width: `${epochsSynced}%`
                }}
              >
                <p>{ epochsSynced }% { formatMessage(messages.synced) }</p>
              </div>
              <div className={styles.indicatorEpochsConsolidatedContainer}>
                <div
                  className={styles.indicatorEpochsConsolidated}
                  style={{
                    width: `${widthOfEpochsConsolidated}%`
                  }}
                >
                  <p
                    style={this.getPositionOfEpochsConsolidated(widthOfEpochsConsolidated)}
                  >
                    { epochsConsolidated } { formatMessage(messages.epochsConsolidated) }
                  </p>
                </div>
              </div>
              <p className={styles.fullEpoch}>
                { currentEpoch } { formatMessage(messages.epoch) }
              </p>
            </div>
          </div>

          <Button
            label={formatMessage(messages.supportButton)}
            onClick={() => onExternalLinkClick(formatMessage(messages.supportButtonURL))}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}
