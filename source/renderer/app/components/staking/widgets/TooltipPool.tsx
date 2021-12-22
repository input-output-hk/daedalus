import React, { createRef, Component } from 'react';
import type { ElementRef } from 'react';
import { observer } from 'mobx-react';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import CopyToClipboard from 'react-copy-to-clipboard';
import classnames from 'classnames';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TooltipPool.scss' or its cor... Remove this comment to see the full error message
import styles from './TooltipPool.scss';
import StakePool from '../../../domains/StakePool';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import closeCross from '../../../assets/images/close-cross.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/no-data... Remove this comment to see the full error message
import noDataDashSmallImage from '../../../assets/images/no-data-dash-small.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/questio... Remove this comment to see the full error message
import questionMarkIcon from '../../../assets/images/question-mark.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboa... Remove this comment to see the full error message
import copyIcon from '../../../assets/images/clipboard-small-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/check-w... Remove this comment to see the full error message
import copyCheckmarkIcon from '../../../assets/images/check-w.inline.svg';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import {
  formattedWalletAmount,
  toFixedUserFormat,
} from '../../../utils/formatters';
import { ellipsis } from '../../../utils/strings';
import { STAKE_POOL_ID_COPY_FEEDBACK } from '../../../config/timingConfig';
import {
  IS_RANKING_DATA_AVAILABLE,
  IS_SATURATION_DATA_AVAILABLE,
} from '../../../config/stakingConfig';

const messages = defineMessages({
  ranking: {
    id: 'staking.stakePools.tooltip.ranking',
    defaultMessage: '!!!Rank:',
    description: '"Rank" for the Stake Pools Tooltip page.',
  },
  rankingTooltip: {
    id: 'staking.stakePools.tooltip.rankingTooltip',
    defaultMessage:
      '!!!<p>A hierarchical ranking based on the potential rewards you will earn if you delegate the intended amount of stake to this pool, assuming that it reaches saturation.</p><p>*Stake pools with the potential rewards estimated at zero have the same ranking. Please set the stake slider to a higher value for more pools to get potential rewards estimated at more than zero.</p>',
    description: '"Rank" tooltip for the Stake Pools Tooltip page.',
  },
  relativeStake: {
    id: 'staking.stakePools.tooltip.relativeStake',
    defaultMessage: '!!!Live stake:',
    description: '"Live stake" for the Stake Pools Tooltip page.',
  },
  relativeStakeTooltip: {
    id: 'staking.stakePools.tooltip.relativeStakeTooltip',
    defaultMessage:
      '!!!Measures the amount of stake pledged by the pool plus the amount of stake currently delegated to the pool, versus the total amount in the system.',
    description: '"Live stake" tooltip for the Stake Pools Tooltip page.',
  },
  profitMargin: {
    id: 'staking.stakePools.tooltip.profitMargin',
    defaultMessage: '!!!Pool margin:',
    description: '"Pool margin" for the Stake Pools Tooltip page.',
  },
  profitMarginTooltip: {
    id: 'staking.stakePools.tooltip.profitMarginTooltip',
    defaultMessage:
      "!!!The pool's profit, defined as the rewards percentage kept by the pool from the stake that was delegated to it.",
    description: '"Pool margin" tooltip for the Stake Pools Tooltip page.',
  },
  costPerEpoch: {
    id: 'staking.stakePools.tooltip.costPerEpoch',
    defaultMessage: '!!!Cost per epoch:',
    description: '"Cost per epoch" for the Stake Pools Tooltip page.',
  },
  costPerEpochTooltip: {
    id: 'staking.stakePools.tooltip.costPerEpochTooltip',
    defaultMessage:
      '!!!Fixed operational costs that the stake pool retains from any rewards earned during each epoch.',
    description: '"Cost per epoch" tooltip for the Stake Pools Tooltip page.',
  },
  producedBlocks: {
    id: 'staking.stakePools.tooltip.producedBlocks',
    defaultMessage: '!!!Produced blocks:',
    description: '"Blocks" for the Stake Pools Tooltip page.',
  },
  producedBlocksTooltip: {
    id: 'staking.stakePools.tooltip.producedBlocksTooltip',
    defaultMessage:
      '!!!The total number of blocks the stake pool has produced.',
    description: '"Blocks" tooltip for the Stake Pools Tooltip page.',
  },
  potentialRewards: {
    id: 'staking.stakePools.tooltip.potentialRewards',
    defaultMessage: '!!!Potential rewards:',
    description: '"Rewards" for the Stake Pools Tooltip page.',
  },
  potentialRewardsTooltip: {
    id: 'staking.stakePools.tooltip.potentialRewardsTooltip',
    defaultMessage:
      "!!!An estimation of the potential rewards you will earn per epoch if you delegate the intended amount of stake. The system looks at the pool's parameters and historical performance data to calculate potential rewards, assuming that the pool reaches optimal saturation.",
    description: '"Rewards" tooltip for the Stake Pools Tooltip page.',
  },
  retirement: {
    id: 'staking.stakePools.tooltip.retirement',
    defaultMessage: '!!!Retirement in {retirementFromNow}',
    description: '"Retirement" for the Stake Pools Tooltip page.',
  },
  saturation: {
    id: 'staking.stakePools.tooltip.saturation',
    defaultMessage: '!!!Saturation:',
    description: '"Saturation" for the Stake Pools Tooltip page.',
  },
  saturationTooltip: {
    id: 'staking.stakePools.tooltip.saturationTooltip',
    defaultMessage:
      '!!!Saturation measures the stake in the pool and indicates the point at which rewards stop increasing with increases in stake. This capping mechanism encourages decentralization by discouraging users from delegating to oversaturated stake pools.',
    description: '"Saturation" tooltip for the Stake Pools Tooltip page.',
  },
  pledge: {
    id: 'staking.stakePools.tooltip.pledge',
    defaultMessage: '!!!Pledge:',
    description: '"Pledge" for the Stake Pools Tooltip page.',
  },
  pledgeTooltip: {
    id: 'staking.stakePools.tooltip.pledgeTooltip',
    defaultMessage:
      '!!!The amount of stake that a pool operator contributes to a pool. Pools with higher pledge amounts earn more rewards for themselves and their delegators. Pools that do not honor their pledge earn zero rewards and accrue low ranking.',
    description: '"Pledge" tooltip for the Stake Pools Tooltip page.',
  },
  delegateButton: {
    id: 'staking.stakePools.tooltip.delegateButton',
    defaultMessage: '!!!Delegate to this pool',
    description:
      '"Delegate to this pool" Button for the Stake Pools Tooltip page.',
  },
  copyIdTooltipLabel: {
    id: 'staking.stakePools.tooltip.copyIdTooltipLabel',
    defaultMessage: '!!!Copy the stake pool ID',
    description: 'copyId tooltip label',
  },
  copiedIdTooltipLabel: {
    id: 'staking.stakePools.tooltip.copiedIdTooltipLabel',
    defaultMessage: '!!!Copied',
    description: 'copyId tooltip label copied',
  },
  noDataDashTooltipLabel: {
    id: 'staking.stakePools.noDataDashTooltip',
    defaultMessage: '!!!Data not available yet',
    description: 'Data not available yet label',
  },
});
type Props = {
  stakePool: StakePool;
  currentTheme: string;
  onClose: () => void;
  onOpenExternalLink: (arg0: string) => void;
  onSelect?: () => void;
  showWithSelectButton?: boolean;
  color: string;
  containerClassName: string;
  numberOfRankedStakePools: number;
  isGridRewardsView?: boolean;
};
type State = {
  componentStyle: Record<string, any>;
  idCopyFeedback: boolean;
};

@observer
class TooltipPool extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'TimeoutID'.
  idCopyFeedbackTimeout: TimeoutID;
  rootRef: ElementRef<any> = createRef();
  state = {
    componentStyle: {},
    idCopyFeedback: false,
  };

  componentDidMount() {
    window.addEventListener('keydown', this.handleInputKeyDown);
  }

  componentWillUnmount() {
    window.removeEventListener('keydown', this.handleInputKeyDown);
  }

  handleInputKeyDown = (event: KeyboardEvent) => {
    if (event.key === 'Escape') {
      this.props.onClose();
    }
  };
  onCopyId = () => {
    clearTimeout(this.idCopyFeedbackTimeout);
    this.setState({
      idCopyFeedback: true,
    });
    this.idCopyFeedbackTimeout = setTimeout(() => {
      this.setState({
        idCopyFeedback: false,
      });
    }, STAKE_POOL_ID_COPY_FEEDBACK);
  };
  onIdMouseOut = () => {
    this.setState({
      idCopyFeedback: false,
    });
  };

  get isGreyColor() {
    return !IS_RANKING_DATA_AVAILABLE;
  }

  renderDescriptionFields = () => {
    const { intl } = this.context;
    const {
      currentTheme,
      stakePool,
      numberOfRankedStakePools,
      isGridRewardsView,
    } = this.props;
    const {
      ranking,
      relativeStake,
      producedBlocks,
      potentialRewards,
      cost,
      profitMargin,
      saturation,
      pledge,
    } = stakePool;
    const darken = currentTheme === 'dark-blue' ? 1 : 0;
    const alpha = 0.3;
    const saturationBarClassnames = classnames([
      styles.saturationBar,
      styles[getSaturationColor(saturation)],
    ]);
    const fields = [
      {
        key: 'saturation',
        value: (
          <div className={styles.saturationValue}>
            <span>
              <span className={saturationBarClassnames}>
                <span
                  style={{
                    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
                    width: `${parseFloat(saturation).toFixed(2)}%`,
                  }}
                />
              </span>
              {`${toFixedUserFormat(saturation, 2)}%`}
            </span>
          </div>
        ),
      },
      {
        key: 'ranking',
        value: (
          <div className={styles.ranking}>
            {IS_RANKING_DATA_AVAILABLE ? (
              <span
                style={{
                  background: getColorFromRange(ranking, {
                    darken,
                    alpha,
                    numberOfItems: numberOfRankedStakePools,
                  }),
                }}
              >
                {potentialRewards.isZero && !potentialRewards.isZero() ? (
                  ranking
                ) : (
                  <>
                    {numberOfRankedStakePools + 1}
                    <span className={styles.asterisk}>*</span>
                  </>
                )}
              </span>
            ) : (
              <div className={styles.noDataDash}>
                <SVGInline svg={noDataDashSmallImage} />
              </div>
            )}
          </div>
        ),
      },
      {
        key: 'relativeStake',
        value: (
          <div className={styles.defaultColor}>
            <span className={styles.defaultColorContent}>{`${toFixedUserFormat(
              // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BigNumber' is not assignable to ... Remove this comment to see the full error message
              relativeStake,
              2
            )}%`}</span>
          </div>
        ),
      },
      {
        key: 'profitMargin',
        value: (
          <div>
            <span
              style={{
                background: getColorFromRange(profitMargin, {
                  darken,
                  alpha,
                }),
              }}
            >
              {`${toFixedUserFormat(profitMargin, 2)}%`}
            </span>
          </div>
        ),
      },
      {
        key: 'pledge',
        value: (
          <div className={styles.defaultColor}>
            <span className={styles.defaultColorContent}>
              {formattedWalletAmount(pledge, true, false)}
            </span>
          </div>
        ),
      },
      {
        key: 'costPerEpoch',
        value: (
          <div className={styles.costValue}>
            <span
              style={{
                background: getColorFromRange(profitMargin, {
                  darken,
                  alpha,
                }),
              }}
            >
              {`${formattedWalletAmount(cost, true, false)}`}
            </span>
          </div>
        ),
      },
      {
        key: 'producedBlocks',
        value: (
          <div className={styles.defaultColor}>
            <span className={styles.defaultColorContent}>
              {toFixedUserFormat(producedBlocks, 0)}
            </span>
          </div>
        ),
      },
      {
        key: 'potentialRewards',
        value: (
          <div className={styles.defaultColor}>
            {isGridRewardsView &&
            potentialRewards.isZero &&
            potentialRewards.isZero() ? (
              <div className={styles.noDataDash}>
                <SVGInline svg={noDataDashSmallImage} />
              </div>
            ) : (
              <span className={styles.defaultColorContent}>
                {formattedWalletAmount(potentialRewards)}
              </span>
            )}
          </div>
        ),
      },
    ];
    return (
      <div className={styles.table}>
        {fields.map((field: { key: string; value: any }) => {
          const labelPart = (
            <div className={styles[`${field.key}Label`]}>
              <div className={styles.labelContainer}>
                <div className={styles.fieldLabel}>
                  {intl.formatMessage(messages[field.key])}
                </div>
                <PopOver
                  offset={[0, 10]}
                  key={field.key}
                  content={
                    <div className={styles.tooltipWithHTMLContent}>
                      <FormattedHTMLMessage
                        {...messages[`${field.key}Tooltip`]}
                      />
                    </div>
                  }
                >
                  <div className={styles.questionMark}>
                    <SVGInline svg={questionMarkIcon} />
                  </div>
                </PopOver>
              </div>
            </div>
          );

          if (field.key === 'saturation' && !IS_SATURATION_DATA_AVAILABLE) {
            return null;
          }

          return (
            <div key={field.key} className={styles.dRow}>
              {labelPart}
              {field.value}
            </div>
          );
        })}
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const {
      stakePool,
      onClose,
      onOpenExternalLink,
      onSelect,
      showWithSelectButton,
    } = this.props;
    const { componentStyle, idCopyFeedback } = this.state;
    const { id, name, description, ticker, homepage, retiring } = stakePool;
    const retirementFromNow = retiring
      ? moment(retiring).locale(intl.locale).fromNow(true)
      : '';
    const idCopyIcon = idCopyFeedback ? copyCheckmarkIcon : copyIcon;
    const hoverContentClassnames = classnames([
      styles.hoverContent,
      idCopyFeedback ? styles.checkIcon : styles.copyIcon,
    ]);
    const colorBandClassnames = classnames([
      styles.colorBand,
      this.isGreyColor ? styles.greyColorBand : null,
    ]);
    const colorBandStyle = this.isGreyColor
      ? {}
      : {
          background: this.props.color,
        };
    const stakePoolAddressHoverCopy = idCopyFeedback
      ? intl.formatMessage(messages.copiedIdTooltipLabel)
      : intl.formatMessage(messages.copyIdTooltipLabel);
    return (
      <div
        className={styles.component}
        style={componentStyle}
        // @ts-ignore ts-migrate(2322) FIXME: Type 'unknown' is not assignable to type 'LegacyRe... Remove this comment to see the full error message
        ref={this.rootRef}
      >
        <div className={colorBandClassnames} style={colorBandStyle} />
        <div className={styles.container}>
          <h3 className={styles.name}>{name}</h3>
          <button
            className={styles.closeButton}
            onClick={(e) => {
              e.stopPropagation();
              onClose();
            }}
          >
            <SVGInline svg={closeCross} />
          </button>
          <div className={styles.ticker}>{ticker}</div>
          {retiring && (
            <div className={styles.retirement}>
              <FormattedMessage
                {...messages.retirement}
                values={{
                  retirementFromNow,
                }}
              />
            </div>
          )}
          <PopOver
            key="id"
            content={
              <div className={styles.tooltipWithHTMLContent}>
                {stakePoolAddressHoverCopy}
              </div>
            }
            hideOnClick={false}
          >
            <div
              className={styles.id}
              onMouseOut={this.onIdMouseOut}
              onBlur={() => {}}
            >
              <p className={styles.ellipsisContent}>{ellipsis(id, 18, 18)}</p>
              <CopyToClipboard text={id} onCopy={this.onCopyId}>
                <div className={hoverContentClassnames}>
                  <p className={styles.hoverContentBackground}>
                    {id} <SVGInline svg={idCopyIcon} />
                  </p>
                </div>
              </CopyToClipboard>
            </div>
          </PopOver>
          <div className={styles.description}>{description}</div>
          <Link
            onClick={() => onOpenExternalLink(homepage)}
            className={styles.homepage}
            label={homepage}
            skin={LinkSkin}
          />
          {this.renderDescriptionFields()}
        </div>
        {onSelect && showWithSelectButton && (
          <Button
            label={intl.formatMessage(messages.delegateButton)}
            onClick={() => onSelect && onSelect()}
            skin={ButtonSkin}
          />
        )}
      </div>
    );
  }
}

export default TooltipPool;
