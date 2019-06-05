// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import classnames from 'classnames';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './StakePoolTooltip.scss';
import { getHSLColor } from '../../../utils/colors';
import type { StakePoolProps } from '../../../api/staking/types';
import closeCross from '../../../assets/images/close-cross.inline.svg';

const messages = defineMessages({
  ranking: {
    id: 'staking.stakePools.tooltip.ranking',
    defaultMessage: '!!!Rank:',
    description: '"" for the Stake Pools Tooltip page.',
  },
  controlledStake: {
    id: 'staking.stakePools.tooltip.controlledStake',
    defaultMessage: '!!!Controlled stake:',
    description: '"Controlled stake" for the Stake Pools Tooltip page.',
  },
  profitMargin: {
    id: 'staking.stakePools.tooltip.profitMargin',
    defaultMessage: '!!!Profit margin:',
    description: '"Profit margin" for the Stake Pools Tooltip page.',
  },
  performance: {
    id: 'staking.stakePools.tooltip.performance',
    defaultMessage: '!!!Performance:',
    description: '"Performance" for the Stake Pools Tooltip page.',
  },
  retirement: {
    id: 'staking.stakePools.tooltip.retirement',
    defaultMessage: '!!!Retirement:',
    description: '"Retirement" for the Stake Pools Tooltip page.',
  },
  delegateButton: {
    id: 'staking.stakePools.tooltip.delegateButton',
    defaultMessage: '!!!Delegate to this pool',
    description:
      '"Delegate to this pool" Button for the Stake Pools Tooltip page.',
  },
});

type Props = {
  stakePool: StakePoolProps,
  ranking: number,
  onOpenExternalLink: Function,
  onClick: Function,
  currentTheme: string,
  flipHorizontal: boolean,
  flipVertical: boolean,
};

@observer
export default class StakePool extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  get color() {
    return getHSLColor(this.props.ranking);
  }

  render() {
    const { intl } = this.context;
    const {
      stakePool,
      ranking,
      onOpenExternalLink,
      onClick,
      currentTheme,
    } = this.props;

    const {
      id,
      name,
      description,
      url,
      controlledStake,
      profitMargin,
      performance,
      retirement,
      onOpenExternalLink,
      visible,
      onClick,
      currentTheme,
      flipHorizontal,
      flipVertical,
    } = this.props;

    const componentClassnames = classnames([
      styles.component,
      visible ? styles.visible : null,
      flipHorizontal ? styles.flipHorizontal : null,
      flipVertical ? styles.flipVertical : null,
    ]);

    const lighnessOffset = currentTheme === 'dark-blue' ? -20 : 0;

    return (
      <div className={styles.component}>
        <h3 className={styles.name}>{name}</h3>
        <button className={styles.closeButton} onClick={onClick}>
          <SVGInline svg={closeCross} />
        </button>
        <div className={styles.id}>
          {id} {flipVertical ? 'Y' : 'N'}
        </div>
        <div className={styles.description}>{description}</div>
        <button className={styles.url} onClick={() => onOpenExternalLink(url)}>
          {url}
        </button>
        <dl className={styles.table}>
          <dt>{intl.formatMessage(messages.ranking)}</dt>
          <dd className={styles.ranking}>
            <span
              style={{
                background: getHSLColor(ranking, { lighnessOffset }),
              }}
            >
              {parseFloat(ranking).toFixed(2)}
            </span>
          </dd>
          <dt>{intl.formatMessage(messages.controlledStake)}</dt>
          <dd className={styles.controlledStake}>
            <span
              style={{
                background: getHSLColor(controlledStake, { lighnessOffset }),
              }}
            >
              {controlledStake}%
            </span>
          </dd>
          <dt>{intl.formatMessage(messages.profitMargin)}</dt>
          <dd className={styles.profitMargin}>
            <span
              style={{
                background: getHSLColor(profitMargin, { lighnessOffset }),
              }}
            >
              {profitMargin}%
            </span>
          </dd>
          <dt>{intl.formatMessage(messages.performance)}</dt>
          <dd className={styles.performance}>
            <span
              style={{
                background: getHSLColor(performance, { lighnessOffset }),
              }}
            >
              {performance}%
            </span>
          </dd>
          {retirement && (
            <Fragment>
              <dt>{intl.formatMessage(messages.retirement)}</dt>
              <dd className={styles.retirement}>
                <span>{moment(retirement).fromNow(true)}</span>
              </dd>
            </Fragment>
          )}
        </dl>
        <Button
          className={styles.delegateButton}
          label={intl.formatMessage(messages.delegateButton)}
          onClick={() => {}}
          skin={ButtonSkin}
        />
      </div>
    );
  }
}
