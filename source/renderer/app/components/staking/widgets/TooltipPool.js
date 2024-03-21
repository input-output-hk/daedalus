'use strict';
// @ts-nocheck
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const Button_1 = require('@react-polymorph/components/Button');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_copy_to_clipboard_1 = __importDefault(
  require('react-copy-to-clipboard')
);
const classnames_1 = __importDefault(require('classnames'));
const moment_1 = __importDefault(require('moment'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const TooltipPool_scss_1 = __importDefault(require('./TooltipPool.scss'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
const close_cross_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/no-data... Remove this comment to see the full error message
const no_data_dash_small_inline_svg_1 = __importDefault(
  require('../../../assets/images/no-data-dash-small.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/questio... Remove this comment to see the full error message
const question_mark_inline_svg_1 = __importDefault(
  require('../../../assets/images/question-mark.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboa... Remove this comment to see the full error message
const clipboard_small_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/clipboard-small-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/check-w... Remove this comment to see the full error message
const check_w_inline_svg_1 = __importDefault(
  require('../../../assets/images/check-w.inline.svg')
);
const colors_1 = require('../../../utils/colors');
const formatters_1 = require('../../../utils/formatters');
const strings_1 = require('../../../utils/strings');
const timingConfig_1 = require('../../../config/timingConfig');
const stakingConfig_1 = require('../../../config/stakingConfig');
const messages = (0, react_intl_1.defineMessages)({
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
let TooltipPool = class TooltipPool extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'TimeoutID'.
  idCopyFeedbackTimeout;
  rootRef = (0, react_1.createRef)();
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
  handleInputKeyDown = (event) => {
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
    }, timingConfig_1.STAKE_POOL_ID_COPY_FEEDBACK);
  };
  onIdMouseOut = () => {
    this.setState({
      idCopyFeedback: false,
    });
  };
  get isGreyColor() {
    return !stakingConfig_1.IS_RANKING_DATA_AVAILABLE;
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
    const saturationBarClassnames = (0, classnames_1.default)([
      TooltipPool_scss_1.default.saturationBar,
      TooltipPool_scss_1.default[(0, colors_1.getSaturationColor)(saturation)],
    ]);
    const fields = [
      {
        key: 'saturation',
        value: react_1.default.createElement(
          'div',
          { className: TooltipPool_scss_1.default.saturationValue },
          react_1.default.createElement(
            'span',
            null,
            react_1.default.createElement(
              'span',
              { className: saturationBarClassnames },
              react_1.default.createElement('span', {
                style: {
                  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
                  width: `${parseFloat(saturation).toFixed(2)}%`,
                },
              })
            ),
            `${(0, formatters_1.toFixedUserFormat)(saturation, 2)}%`
          )
        ),
      },
      {
        key: 'ranking',
        value: react_1.default.createElement(
          'div',
          { className: TooltipPool_scss_1.default.ranking },
          stakingConfig_1.IS_RANKING_DATA_AVAILABLE
            ? react_1.default.createElement(
                'span',
                {
                  style: {
                    background: (0, colors_1.getColorFromRange)(ranking, {
                      darken,
                      alpha,
                      numberOfItems: numberOfRankedStakePools,
                    }),
                  },
                },
                potentialRewards.isZero && !potentialRewards.isZero()
                  ? ranking
                  : react_1.default.createElement(
                      react_1.default.Fragment,
                      null,
                      numberOfRankedStakePools + 1,
                      react_1.default.createElement(
                        'span',
                        { className: TooltipPool_scss_1.default.asterisk },
                        '*'
                      )
                    )
              )
            : react_1.default.createElement(
                'div',
                { className: TooltipPool_scss_1.default.noDataDash },
                react_1.default.createElement(react_svg_inline_1.default, {
                  svg: no_data_dash_small_inline_svg_1.default,
                })
              )
        ),
      },
      {
        key: 'relativeStake',
        value: react_1.default.createElement(
          'div',
          null,
          react_1.default.createElement(
            'span',
            { className: TooltipPool_scss_1.default.defaultColorContent },
            `${(0, formatters_1.toFixedUserFormat)(
              // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BigNumber' is not assignable to ... Remove this comment to see the full error message
              relativeStake,
              2
            )}%`
          )
        ),
      },
      {
        key: 'profitMargin',
        value: react_1.default.createElement(
          'div',
          null,
          react_1.default.createElement(
            'span',
            {
              style: {
                background: (0, colors_1.getColorFromRange)(profitMargin, {
                  darken,
                  alpha,
                }),
              },
            },
            `${(0, formatters_1.toFixedUserFormat)(profitMargin, 2)}%`
          )
        ),
      },
      {
        key: 'pledge',
        value: react_1.default.createElement(
          'div',
          null,
          react_1.default.createElement(
            'span',
            { className: TooltipPool_scss_1.default.defaultColorContent },
            (0, formatters_1.formattedWalletAmount)(pledge, true, false)
          )
        ),
      },
      {
        key: 'costPerEpoch',
        value: react_1.default.createElement(
          'div',
          { className: TooltipPool_scss_1.default.costValue },
          react_1.default.createElement(
            'span',
            {
              style: {
                background: (0, colors_1.getColorFromRange)(profitMargin, {
                  darken,
                  alpha,
                }),
              },
            },
            `${(0, formatters_1.formattedWalletAmount)(cost, true, false)}`
          )
        ),
      },
      {
        key: 'producedBlocks',
        value: react_1.default.createElement(
          'div',
          null,
          react_1.default.createElement(
            'span',
            { className: TooltipPool_scss_1.default.defaultColorContent },
            (0, formatters_1.toFixedUserFormat)(producedBlocks, 0)
          )
        ),
      },
      {
        key: 'potentialRewards',
        value: react_1.default.createElement(
          'div',
          null,
          isGridRewardsView &&
            potentialRewards.isZero &&
            potentialRewards.isZero()
            ? react_1.default.createElement(
                'div',
                { className: TooltipPool_scss_1.default.noDataDash },
                react_1.default.createElement(react_svg_inline_1.default, {
                  svg: no_data_dash_small_inline_svg_1.default,
                })
              )
            : react_1.default.createElement(
                'span',
                { className: TooltipPool_scss_1.default.defaultColorContent },
                (0, formatters_1.formattedWalletAmount)(potentialRewards)
              )
        ),
      },
    ];
    return react_1.default.createElement(
      'div',
      { className: TooltipPool_scss_1.default.table },
      fields.map((field) => {
        const labelPart = react_1.default.createElement(
          'div',
          { className: TooltipPool_scss_1.default[`${field.key}Label`] },
          react_1.default.createElement(
            'div',
            { className: TooltipPool_scss_1.default.labelContainer },
            react_1.default.createElement(
              'div',
              { className: TooltipPool_scss_1.default.fieldLabel },
              intl.formatMessage(messages[field.key])
            ),
            react_1.default.createElement(
              PopOver_1.PopOver,
              {
                offset: [0, 10],
                key: field.key,
                content: react_1.default.createElement(
                  'div',
                  {
                    className:
                      TooltipPool_scss_1.default.tooltipWithHtmlContent,
                  },
                  react_1.default.createElement(
                    react_intl_1.FormattedHTMLMessage,
                    { ...messages[`${field.key}Tooltip`] }
                  )
                ),
              },
              react_1.default.createElement(
                'div',
                { className: TooltipPool_scss_1.default.questionMark },
                react_1.default.createElement(react_svg_inline_1.default, {
                  svg: question_mark_inline_svg_1.default,
                })
              )
            )
          )
        );
        if (
          field.key === 'saturation' &&
          !stakingConfig_1.IS_SATURATION_DATA_AVAILABLE
        ) {
          return null;
        }
        return react_1.default.createElement(
          'div',
          { key: field.key, className: TooltipPool_scss_1.default.dRow },
          labelPart,
          field.value
        );
      })
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
      ? (0, moment_1.default)(retiring).locale(intl.locale).fromNow(true)
      : '';
    const idCopyIcon = idCopyFeedback
      ? check_w_inline_svg_1.default
      : clipboard_small_ic_inline_svg_1.default;
    const hoverContentClassnames = (0, classnames_1.default)([
      TooltipPool_scss_1.default.hoverContent,
      idCopyFeedback
        ? TooltipPool_scss_1.default.checkIcon
        : TooltipPool_scss_1.default.copyIcon,
    ]);
    const colorBandClassnames = (0, classnames_1.default)([
      TooltipPool_scss_1.default.colorBand,
      this.isGreyColor ? TooltipPool_scss_1.default.greyColorBand : null,
    ]);
    const colorBandStyle = this.isGreyColor
      ? {}
      : {
          background: this.props.color,
        };
    const stakePoolAddressHoverCopy = idCopyFeedback
      ? intl.formatMessage(messages.copiedIdTooltipLabel)
      : intl.formatMessage(messages.copyIdTooltipLabel);
    return react_1.default.createElement(
      'div',
      {
        className: TooltipPool_scss_1.default.component,
        style: componentStyle,
        // @ts-ignore ts-migrate(2322) FIXME: Type 'unknown' is not assignable to type 'LegacyRe... Remove this comment to see the full error message
        ref: this.rootRef,
      },
      react_1.default.createElement('div', {
        className: colorBandClassnames,
        style: colorBandStyle,
      }),
      react_1.default.createElement(
        'div',
        { className: TooltipPool_scss_1.default.container },
        react_1.default.createElement(
          'h3',
          { className: TooltipPool_scss_1.default.name },
          name
        ),
        react_1.default.createElement(
          'button',
          {
            className: TooltipPool_scss_1.default.closeButton,
            onClick: (e) => {
              e.stopPropagation();
              onClose();
            },
          },
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: close_cross_inline_svg_1.default,
          })
        ),
        react_1.default.createElement(
          'div',
          { className: TooltipPool_scss_1.default.ticker },
          ticker
        ),
        retiring &&
          react_1.default.createElement(
            'div',
            { className: TooltipPool_scss_1.default.retirement },
            react_1.default.createElement(react_intl_1.FormattedMessage, {
              ...messages.retirement,
              values: {
                retirementFromNow,
              },
            })
          ),
        react_1.default.createElement(
          PopOver_1.PopOver,
          {
            key: 'id',
            content: react_1.default.createElement(
              'div',
              { className: TooltipPool_scss_1.default.tooltipWithHtmlContent },
              stakePoolAddressHoverCopy
            ),
            hideOnClick: false,
          },
          react_1.default.createElement(
            'div',
            {
              className: TooltipPool_scss_1.default.id,
              onMouseOut: this.onIdMouseOut,
              onBlur: () => {},
            },
            react_1.default.createElement(
              'p',
              { className: TooltipPool_scss_1.default.ellipsisContent },
              (0, strings_1.ellipsis)(id, 18, 18)
            ),
            react_1.default.createElement(
              react_copy_to_clipboard_1.default,
              { text: id, onCopy: this.onCopyId },
              react_1.default.createElement(
                'div',
                { className: hoverContentClassnames },
                react_1.default.createElement(
                  'p',
                  {
                    className:
                      TooltipPool_scss_1.default.hoverContentBackground,
                  },
                  id,
                  ' ',
                  react_1.default.createElement(react_svg_inline_1.default, {
                    svg: idCopyIcon,
                  })
                )
              )
            )
          )
        ),
        react_1.default.createElement(
          'div',
          { className: TooltipPool_scss_1.default.description },
          description
        ),
        react_1.default.createElement(Link_1.Link, {
          onClick: () => onOpenExternalLink(homepage),
          className: TooltipPool_scss_1.default.homepage,
          label: homepage,
          skin: LinkSkin_1.LinkSkin,
        }),
        this.renderDescriptionFields()
      ),
      onSelect &&
        showWithSelectButton &&
        react_1.default.createElement(Button_1.Button, {
          label: intl.formatMessage(messages.delegateButton),
          onClick: () => onSelect && onSelect(),
          skin: ButtonSkin_1.ButtonSkin,
        })
    );
  }
};
TooltipPool = __decorate([mobx_react_1.observer], TooltipPool);
exports.default = TooltipPool;
//# sourceMappingURL=TooltipPool.js.map
