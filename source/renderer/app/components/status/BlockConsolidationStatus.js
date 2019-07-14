// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import {
  defineMessages,
  intlShape,
  FormattedHTMLMessage,
  FormattedMessage,
} from 'react-intl';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../widgets/DialogCloseButton';
import styles from './BlockConsolidationStatus.scss';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';
import epochs from '../../assets/images/block-consolidation/epochs.inline.svg';

const messages = defineMessages({
  title: {
    id: 'blockConsolidationStatus.title',
    defaultMessage: '!!!Block storage consolidation status',
    description: 'Title of "Block consolidation status" page.',
  },
  description1: {
    id: 'blockConsolidationStatus.description1',
    defaultMessage: '!!!Block storage is being consolidated.',
    description: 'Description 1 of "Block consolidation status" page.',
  },
  description2: {
    id: 'blockConsolidationStatus.description2',
    defaultMessage:
      '!!!Blocks for the current epoch ({currentEpoch}) and the previous epoch ({currentEpochBehind}) are stored as one file per block. All previous epochs will be consolidated to two files per epoch.',
    description: 'Description 2 of "Block consolidation status" page.',
  },
  description3: {
    id: 'blockConsolidationStatus.description3',
    defaultMessage:
      '!!!This reduces the number of files and the amount of hard drive space required to store the blockchain on your machine.',
    description: 'Description 3 of "Block consolidation status" page.',
  },
  epochsConsolidatedOfTotal: {
    id: 'blockConsolidationStatus.epochsConsolidatedOfTotal',
    defaultMessage:
      '!!!<b>{epochsConsolidated}</b> <em>of</em> <b>{currentEpoch}</b><br />epochs consolidated',
    description:
      'Epochs Consolidated Of Total on "Block consolidation status" page.',
  },
  epoch: {
    id: 'blockConsolidationStatus.epoch',
    defaultMessage: '!!!epoch',
    description: 'Singular Epoch on "Block consolidation status" page.',
  },
  epochs: {
    id: 'blockConsolidationStatus.epochs',
    defaultMessage: '!!!epochs',
    description: 'Plural Epochs on "Block consolidation status" page.',
  },
  epochsConsolidated: {
    id: 'blockConsolidationStatus.epochsConsolidated',
    defaultMessage: '!!!epochs consolidated',
    description: 'Epochs consolidated on "Block consolidation status" page.',
  },
  synced: {
    id: 'blockConsolidationStatus.synced',
    defaultMessage: '!!!{epochsSynced}% blocks synced',
    description: 'synced on "Block consolidation status" page.',
  },
  learnMoreButton: {
    id: 'blockConsolidationStatus.learnMoreButton',
    defaultMessage: '!!!Learn more.',
    description: 'Learn more Button on "Block consolidation status" page.',
  },
  learnMoreButtonURL: {
    id: 'blockConsolidationStatus.learnMoreButtonURL',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360016060314',
    description:
      'URL of Learn more Button on "Block consolidation status" page.',
  },
});

type Props = {
  currentEpoch: number,
  epochsConsolidated: number,
  epochsSynced: number,
  onExternalLinkClick: Function,
  onClose: Function,
};

@observer
export default class BlockConsolidationStatus extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  getWidthOfEpochsConsolidated = (
    epochsConsolidated: number,
    currentEpoch: number
  ) => {
    const widthOfEpochsConsolidated =
      (epochsConsolidated * 100) / (currentEpoch - 2);
    return Math.min(widthOfEpochsConsolidated, 100);
  };

  getPositionOfEpochsConsolidated = (widthOfEpochsConsolidated: number) =>
    widthOfEpochsConsolidated > 32
      ? { right: 8 }
      : { left: 0, textAlign: 'left' };

  getPositionOfEpochsSynced = (widthOfEpochsSynced: number) =>
    widthOfEpochsSynced > 20 ? { right: 0 } : { left: 0 };

  render() {
    const {
      currentEpoch,
      epochsConsolidated,
      epochsSynced,
      onExternalLinkClick,
      onClose,
    } = this.props;
    const { formatMessage } = this.context.intl;
    const widthOfEpochsConsolidated = this.getWidthOfEpochsConsolidated(
      epochsConsolidated,
      currentEpoch
    );

    const description2Styles = classnames(styles.description, {
      [styles.descriptionNoCurrentEpoch]: !currentEpoch,
    });

    const epochsStyles = classnames(styles.epochs, {
      [styles.epochsNoCurrentEpoch]: !currentEpoch,
    });

    const indicatorContainerStyles = classnames(styles.indicatorContainer, {
      [styles.indicatorContainerNoCurrentEpochs]: !currentEpoch,
    });

    const currentEpochValue = currentEpoch > 0 ? <b>({currentEpoch})</b> : '';
    const currentEpochBehindValue =
      currentEpoch > 0 ? <b>({Math.max(currentEpoch - 1, 0)})</b> : '';

    return (
      <div className={styles.component}>
        <DialogCloseButton
          className={styles.closeButton}
          icon={closeCrossThin}
          onClose={onClose}
        />

        <div className={styles.container}>
          <div className={styles.content}>
            <h1>{formatMessage(messages.title)}</h1>
            <p className={styles.description}>
              {formatMessage(messages.description1)}
            </p>
            <p className={description2Styles}>
              <FormattedMessage
                {...messages.description2}
                values={{
                  currentEpoch: currentEpochValue,
                  currentEpochBehind: currentEpochBehindValue,
                }}
              />
            </p>
            <p className={styles.description}>
              {formatMessage(messages.description3)}
            </p>

            <div className={epochsStyles}>
              <p>
                <FormattedHTMLMessage
                  {...messages.epochsConsolidatedOfTotal}
                  values={{
                    epochsConsolidated,
                    currentEpoch,
                  }}
                />
              </p>
              <SVGInline svg={epochs} className={styles.epochsImage} />
            </div>

            <div className={styles.indicator}>
              <div className={indicatorContainerStyles}>
                {!!currentEpoch && (
                  <Fragment>
                    <p className={styles.zeroEpoch}>
                      {formatMessage(messages.epoch)} 0
                    </p>
                    <div className={styles.indicatorEpochsBehind}>
                      <p>
                        {formatMessage(messages.epoch)}{' '}
                        {Math.max(currentEpoch - 2, 0)}
                      </p>
                    </div>
                    {!!epochsSynced && (
                      <div
                        className={styles.indicatorEpochsSynced}
                        style={{ width: `${epochsSynced}%` }}
                      >
                        <p style={this.getPositionOfEpochsSynced(epochsSynced)}>
                          <FormattedMessage
                            {...messages.synced}
                            values={{ epochsSynced }}
                          />
                        </p>
                      </div>
                    )}
                    <div
                      className={styles.indicatorEpochsConsolidatedContainer}
                    >
                      <div
                        className={styles.indicatorEpochsConsolidated}
                        style={{ width: `${widthOfEpochsConsolidated}%` }}
                      >
                        <p
                          style={this.getPositionOfEpochsConsolidated(
                            widthOfEpochsConsolidated
                          )}
                        >
                          {epochsConsolidated}{' '}
                          {formatMessage(messages.epochsConsolidated)}
                        </p>
                      </div>
                    </div>
                    <p className={styles.fullEpoch}>
                      {formatMessage(messages.epoch)} {currentEpoch}
                    </p>
                  </Fragment>
                )}
              </div>
            </div>

            <Button
              className={styles.learnMoreButton}
              label={
                <p>
                  <SVGInline
                    svg={externalLinkIcon}
                    className={styles.externalLinkIcon}
                  />
                  {formatMessage(messages.learnMoreButton)}
                </p>
              }
              onClick={() =>
                onExternalLinkClick(formatMessage(messages.learnMoreButtonURL))
              }
              skin={ButtonSkin}
            />
          </div>
        </div>
      </div>
    );
  }
}
