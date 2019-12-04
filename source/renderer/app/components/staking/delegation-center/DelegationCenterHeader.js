// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { get } from 'lodash';
import styles from './DelegationCenterHeader.scss';
import CountdownWidget from '../../widgets/CountdownWidget';
import type { NextEpoch, TipInfo } from '../../../api/network/types';
import delimeterIcon from '../../../assets/images/delimeter.inline.svg';
import delimeterSlashIcon from '../../../assets/images/delimeter-slash.inline.svg';
import { SLOTS_TOTAL } from '../../../config/epochsConfig';

const messages = defineMessages({
  epoch: {
    id: 'staking.delegationCenter.epoch',
    defaultMessage: '!!!Epoch',
    description: 'Headline for the Delegation center.',
  },
  currentSlot: {
    id: 'staking.delegationCenter.currentSlot',
    defaultMessage: '!!!Current slot',
    description: 'Headline for the Delegation center.',
  },
  totalSlots: {
    id: 'staking.delegationCenter.totalSlots',
    defaultMessage: '!!!Total slots',
    description: 'Headline for the Delegation center.',
  },
  headingLeft: {
    id: 'staking.delegationCenter.headingLeft',
    defaultMessage: '!!!Cardano epoch {nextEpochStart} starts in',
    description: 'Headline for the Delegation center.',
  },
  headingRight: {
    id: 'staking.delegationCenter.headingRight',
    defaultMessage: '!!!Current Cardano epoch',
    description: 'Headline for the Delegation center.',
  },
  description: {
    id: 'staking.delegationCenter.description',
    defaultMessage:
      'Changes to delegation preferences will take effect from the next epoch.',
    description: 'Delegation description for the Delegation center.',
  },
});

type Props = {
  networkTip: ?TipInfo,
  nextEpoch: ?NextEpoch,
};

@observer
export default class DelegationCenterHeader extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  generateCurrentEpochPanels = (
    epoch: number,
    slots: number,
    totalSlots: number
  ) => {
    const { intl } = this.context;
    const epochLabel = intl.formatMessage(messages.epoch);
    const currentSlotLabel = intl.formatMessage(messages.currentSlot);
    const totalSlotsLabel = intl.formatMessage(messages.totalSlots);
    const labels: Array<string> = [
      epochLabel,
      currentSlotLabel,
      totalSlotsLabel,
    ];

    const values = [epoch, slots, totalSlots];
    const keys = ['epoch', 'slots', 'totalSlots'];

    return labels.map<any>((
      label: string, // eslint-disable-line
      index: number
    ) => (
      <Fragment key={keys[index]}>
        {this.generateFieldPanel(labels, values, index)}
      </Fragment>
    ));
  };

  generateFieldPanel = (labels: any, values: any, index: number) => {
    const value = values[index];
    const includeSlashDelimeter = index === values.length - 2;
    const includeDotsDelimeter =
      !includeSlashDelimeter && index !== values.length - 1;
    const labelStr = labels[index];
    const valueStr = value.toString();
    let zeroValues = '';
    if (index === 1 && valueStr.length < values[index + 1].toString().length) {
      const zerosToAdd =
        parseInt(values[index + 1].toString().length, 10) -
        parseInt(valueStr.length, 10);
      switch (zerosToAdd) {
        case 1:
          zeroValues = '0';
          break;
        case 2:
          zeroValues = '00';
          break;
        case 3:
          zeroValues = '000';
          break;
        case 4:
          zeroValues = '0000';
          break;
        default:
          break;
      }
    }

    return (
      <div className={styles.fieldPanel}>
        <div className={styles.left}>
          <div className={styles.fieldLabel}>{labelStr}</div>
          <div className={styles.fieldValue}>
            {zeroValues && <span>{zeroValues}</span>}
            {valueStr}
          </div>
        </div>
        {includeDotsDelimeter && (
          <div className={styles.right}>
            <SVGInline svg={delimeterIcon} className={styles.delimeterIcon} />
          </div>
        )}
        {includeSlashDelimeter && (
          <div className={styles.right}>
            <SVGInline
              svg={delimeterSlashIcon}
              className={styles.delimeterSlashIcon}
            />
          </div>
        )}
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const { networkTip, nextEpoch } = this.props;
    const epoch = get(networkTip, 'epoch', '-');
    const nextEpochStart = get(nextEpoch, 'epochStart', '-');
    const slot = get(networkTip, 'slot', '-');
    const totalSlots = SLOTS_TOTAL;
    const headingFirst = intl.formatMessage(messages.headingRight);
    const headingSecond = intl.formatMessage(messages.headingLeft, {
      nextEpochStart,
    });
    const description = intl.formatMessage(messages.description);
    const fieldPanels = this.generateCurrentEpochPanels(
      epoch,
      slot,
      totalSlots
    );

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <div className={styles.mainContainer}>
            <div className={styles.countdownContainer}>
              <div className={styles.heading}>{headingFirst}</div>
              <div className={styles.epochsContainer}>
                <div className={styles.epochs}>{fieldPanels}</div>
              </div>
            </div>
            {nextEpochStart > 0 && (
              <div className={styles.countdownContainer}>
                <div className={styles.heading}>{headingSecond}</div>
                <CountdownWidget nextEpochStart={nextEpochStart} />
              </div>
            )}
          </div>
          <div className={styles.description}>{description}</div>
        </div>
      </div>
    );
  }
}
