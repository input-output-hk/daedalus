import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import BorderedBox from '../../widgets/BorderedBox';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import StakingEpochsCurrentEpochData from './StakingEpochsCurrentEpochData';
import StakingEpochsPreviousEpochData from './StakingEpochsPreviousEpochData';
import { SELECTED_EPOCH_OPTIONS, humanizeDurationToShort } from './helpers';
import type { EpochData } from '../../../api/staking/types';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingEpochs.scss' or its c... Remove this comment to see the full error message
import styles from './StakingEpochs.scss';

const messages = defineMessages({
  currentEpochHeading: {
    id: 'staking.epochs.currentHeading',
    defaultMessage: '!!!current epoch',
    description: 'Headline for the current epoch.',
  },
  previousEpochHeading: {
    id: 'staking.epochs.previousHeading',
    defaultMessage: '!!!previous epoch',
    description: 'Headline for the previous epoch.',
  },
});
type Props = {
  currentEpochName: string;
  currentEpochData: EpochData;
  currentEpochEndDateTime: string;
  currentEpochProgress: number;
  previousEpochName: string;
  previousEpochData: EpochData;
  isLoading: boolean;
};
type State = {
  selectedEpoch: string;
  duration: string;
};
const { CURRENT_EPOCH, PREVIOUS_EPOCH } = SELECTED_EPOCH_OPTIONS;

@observer
class StakingEpochs extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    isLoading: false,
  };

  constructor() {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1-2 arguments, but got 0.
    super();
    this.state = {
      selectedEpoch: CURRENT_EPOCH,
      duration: '',
    };
  }

  componentDidMount() {
    const { intl } = this.context;
    const { currentEpochEndDateTime } = this.props;
    this.setState({
      duration: humanizeDurationToShort(intl.locale, currentEpochEndDateTime),
    });
  }

  onSelectedEpochChange = (selectedEpoch: string) =>
    this.setState({
      selectedEpoch,
    });

  render() {
    const {
      currentEpochName,
      currentEpochData,
      currentEpochProgress,
      previousEpochName,
      previousEpochData,
      isLoading,
    } = this.props;
    const { selectedEpoch, duration } = this.state;
    const { intl } = this.context;
    const epochSelectOptions = [
      {
        label: `${currentEpochName} (${intl.formatMessage(
          messages.currentEpochHeading
        )})`,
        value: CURRENT_EPOCH,
      },
      {
        label: `${previousEpochName} (${intl.formatMessage(
          messages.previousEpochHeading
        )})`,
        value: PREVIOUS_EPOCH,
      },
    ];

    if (!currentEpochName) {
      return null;
    }

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.headerWrapper}>
            <Select
              className={styles.epochSelector}
              options={epochSelectOptions}
              value={selectedEpoch}
              onChange={this.onSelectedEpochChange}
              skin={SelectSkin}
              selectionRenderer={(option) => (
                <div className={styles.customSelectValue}>{option.label}</div>
              )}
              optionHeight={50}
            />
          </div>
          {selectedEpoch === CURRENT_EPOCH && (
            <div>
              <div className={styles.currentEpochProgressBar}>
                <div className={styles.progressBarContainer}>
                  <div
                    className={styles.progress}
                    style={{
                      width: `${currentEpochProgress}%`,
                    }}
                  >
                    <div
                      className={styles.overlapProgressLabel}
                      style={{
                        left: `${10000 / currentEpochProgress}%`,
                      }}
                    >
                      {duration}
                    </div>
                  </div>
                  <div className={styles.progressLabel}>{duration}</div>
                </div>
              </div>
              <StakingEpochsCurrentEpochData
                currentEpochData={currentEpochData}
                isLoading={isLoading}
              />
            </div>
          )}
          {selectedEpoch === PREVIOUS_EPOCH && (
            <StakingEpochsPreviousEpochData
              previousEpochData={previousEpochData}
              isLoading={isLoading}
            />
          )}
          {isLoading && (
            <div className={styles.loadingSpinnerWrapper}>
              <LoadingSpinner />
            </div>
          )}
        </BorderedBox>
      </div>
    );
  }
}

export default StakingEpochs;
