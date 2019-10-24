// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { SIMPLE_DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import linkNewWindowIcon from '../../../assets/images/link-ic-colored.inline.svg';
import DonutRing from './DonutRing';
import styles from './DelegationCenterHeader.scss';
import { with2Decimals } from './helpers';
import type { NumberFormat } from '../../../../../common/types/number.types';

const messages = defineMessages({
  heading: {
    id: 'staking.delegationCenter.heading',
    defaultMessage: '!!!Delegation center',
    description: 'Headline for the Delegation center.',
  },
  descriptionFirstPart: {
    id: 'staking.delegationCenter.descriptionFirstPart',
    defaultMessage:
      '!!!You are currently delegating <b>{adaValue} ADA</b>. This is <b>{percentage}%</b> of ada in your wallets.',
    description: 'Delegation description-part1 for the Delegation center.',
  },
  descriptionSecondPart: {
    id: 'staking.delegationCenter.descriptionSecondPart',
    defaultMessage: '!!!Read',
    description: 'Delegation description-part2 for the Delegation center.',
  },
  descriptionThirdPart: {
    id: 'staking.delegationCenter.descriptionThirdPart',
    defaultMessage: '!!!this article',
    description: 'Delegation description-part3 for the Delegation center.',
  },
  descriptionFourthPart: {
    id: 'staking.delegationCenter.descriptionFourthPart',
    defaultMessage: '!!!to learn how to increase the delegated amount.',
    description: 'Delegation description-part4 for the Delegation center.',
  },
});

type Props = {
  adaValue: BigNumber,
  percentage: number,
  currentNumberFormatPretty: NumberFormat,
};

@observer
export default class DelegationCenterHeader extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { adaValue, percentage, currentNumberFormatPretty } = this.props;
    const heading = intl.formatMessage(messages.heading);
    const descriptionSecondPart = intl.formatMessage(
      messages.descriptionSecondPart
    );
    const descriptionThirdPart = intl.formatMessage(
      messages.descriptionThirdPart
    );
    const descriptionFourthPart = intl.formatMessage(
      messages.descriptionFourthPart
    );
    const percentageWith2Decimals = with2Decimals(percentage);

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <div className={styles.progressRing}>
            <DonutRing
              percentage={percentageWith2Decimals}
              sqSize={44}
              strokeWidth={8}
            />
          </div>
          <div className={styles.heading}>{heading}</div>
          <div className={styles.description}>
            <p>
              <FormattedHTMLMessage
                {...messages.descriptionFirstPart}
                values={{
                  adaValue: adaValue
                    .decimalPlaces(SIMPLE_DECIMAL_PLACES_IN_ADA)
                    .toFormat(currentNumberFormatPretty),
                  percentage: parseFloat(percentageWith2Decimals).toFixed(2),
                }}
              />
            </p>
            <p>
              <span>{descriptionSecondPart}</span>
              <span>
                <a href="/">
                  {descriptionThirdPart}
                  <SVGInline
                    svg={linkNewWindowIcon}
                    className={styles.linkNewWindowIcon}
                  />
                </a>
              </span>
              <span>{descriptionFourthPart}</span>
            </p>
          </div>
        </div>
      </div>
    );
  }
}
