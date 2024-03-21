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
exports.useCreateColumns = void 0;
const react_1 = __importStar(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const moment_1 = __importDefault(require('moment'));
const react_intl_1 = require('react-intl');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const PoolPopOver_1 = require('../../widgets/PoolPopOver');
const StakePoolsTable_scss_1 = __importDefault(
  require('../StakePoolsTable.scss')
);
const colors_1 = require('../../../../utils/colors');
const formatters_1 = require('../../../../utils/formatters');
const StakePoolsTable_messages_1 = require('../StakePoolsTable.messages');
const useCreateColumns = ({
  numberOfRankedStakePools,
  intl,
  currentTheme,
  onOpenExternalLink,
  onSelect,
  containerClassName,
  showWithSelectButton,
}) =>
  (0, react_1.useMemo)(
    () => [
      {
        id: 'ranking',
        Header: react_1.default.createElement(
          PopOver_1.PopOver,
          {
            key: 'ranking',
            placement: 'bottom',
            content: react_1.default.createElement(
              'div',
              {
                className:
                  StakePoolsTable_scss_1.default.tooltipWithHtmlContent,
              },
              react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
                ...StakePoolsTable_messages_1.messages.tableHeaderRankTooltip,
              })
            ),
          },
          intl.formatMessage(
            StakePoolsTable_messages_1.messages.tableHeaderRank
          )
        ),
        accessor: 'ranking',
        Cell: ({ row }) => {
          const { potentialRewards, ranking } = row.original;
          const memberRewards = new bignumber_js_1.default(potentialRewards);
          return react_1.default.createElement(
            react_1.default.Fragment,
            null,
            !memberRewards.isZero()
              ? ranking
              : react_1.default.createElement(
                  react_1.default.Fragment,
                  null,
                  numberOfRankedStakePools + 1,
                  react_1.default.createElement(
                    'span',
                    { className: StakePoolsTable_scss_1.default.asterisk },
                    '*'
                  )
                )
          );
        },
      },
      {
        id: 'ticker',
        Header: intl.formatMessage(
          StakePoolsTable_messages_1.messages.tableHeaderTicker
        ),
        accessor: 'ticker',
        Cell: ({ row }) => {
          const stakePool = row.original;
          const color = (0, colors_1.getColorFromRange)(
            stakePool.ranking,
            numberOfRankedStakePools
          );
          return react_1.default.createElement(
            PoolPopOver_1.PoolPopOver,
            {
              color: color,
              currentTheme: currentTheme,
              onOpenExternalLink: onOpenExternalLink,
              onSelect: onSelect,
              stakePool: stakePool,
              containerClassName: containerClassName,
              numberOfRankedStakePools: numberOfRankedStakePools,
              showWithSelectButton: showWithSelectButton,
            },
            react_1.default.createElement(
              'span',
              {
                className: StakePoolsTable_scss_1.default.ticker,
                role: 'presentation',
              },
              stakePool.ticker
            )
          );
        },
      },
      {
        id: 'saturation',
        Header: react_1.default.createElement(
          PopOver_1.PopOver,
          {
            key: 'saturation',
            placement: 'bottom',
            content: intl.formatMessage(
              StakePoolsTable_messages_1.messages.tableHeaderSaturationTooltip
            ),
          },
          intl.formatMessage(
            StakePoolsTable_messages_1.messages.tableHeaderSaturation
          )
        ),
        accessor: 'saturation',
        Cell: ({ row }) => {
          const { saturation } = row.original;
          const progressBarContentClassnames = (0, classnames_1.default)([
            StakePoolsTable_scss_1.default.progressBarContent,
            StakePoolsTable_scss_1.default[
              (0, colors_1.getSaturationColor)(saturation)
            ],
          ]);
          return react_1.default.createElement(
            'div',
            { className: StakePoolsTable_scss_1.default.saturation },
            react_1.default.createElement(
              'div',
              { className: StakePoolsTable_scss_1.default.progressBar },
              react_1.default.createElement(
                'div',
                {
                  className:
                    StakePoolsTable_scss_1.default.progressBarContainer,
                },
                react_1.default.createElement('div', {
                  className: progressBarContentClassnames,
                  style: {
                    width: `${saturation.toFixed(2)}%`,
                  },
                })
              )
            ),
            react_1.default.createElement(
              'div',
              { className: StakePoolsTable_scss_1.default.saturationLabel },
              `${(0, formatters_1.toFixedUserFormat)(saturation, 2)}%`
            )
          );
        },
      },
      {
        id: 'cost',
        Header: react_1.default.createElement(
          PopOver_1.PopOver,
          {
            key: 'cost',
            placement: 'bottom',
            content: intl.formatMessage(
              StakePoolsTable_messages_1.messages.tableHeaderCostTooltip
            ),
          },
          intl.formatMessage(
            StakePoolsTable_messages_1.messages.tableHeaderCost
          )
        ),
        accessor: 'cost',
        Cell: ({ value }) => {
          const cost = new bignumber_js_1.default(value);
          const costValue = (0, formatters_1.formattedWalletAmount)(
            cost,
            false,
            false
          );
          return costValue;
        },
      },
      {
        id: 'profitMargin',
        Header: react_1.default.createElement(
          PopOver_1.PopOver,
          {
            key: 'profitMargin',
            placement: 'bottom',
            content: intl.formatMessage(
              StakePoolsTable_messages_1.messages.tableHeaderMarginTooltip
            ),
          },
          intl.formatMessage(
            StakePoolsTable_messages_1.messages.tableHeaderMargin
          )
        ),
        accessor: 'profitMargin',
        Cell: ({ value }) => {
          return `${(0, formatters_1.toFixedUserFormat)(value, 2)}%`;
        },
      },
      {
        id: 'producedBlocks',
        Header: react_1.default.createElement(
          PopOver_1.PopOver,
          {
            key: 'producedBlocks',
            placement: 'bottom',
            content: intl.formatMessage(
              StakePoolsTable_messages_1.messages
                .tableHeaderProducedBlocksTooltip
            ),
          },
          intl.formatMessage(
            StakePoolsTable_messages_1.messages.tableHeaderProducedBlocks
          )
        ),
        accessor: 'producedBlocks',
        Cell: ({ value }) => {
          return (0, formatters_1.toFixedUserFormat)(value, 0);
        },
      },
      {
        id: 'nonMyopicMemberRewards',
        Header: react_1.default.createElement(
          PopOver_1.PopOver,
          {
            key: 'nonMyopicMemberRewards',
            placement: 'bottom',
            content: intl.formatMessage(
              StakePoolsTable_messages_1.messages
                .tableHeaderPotentialRewardsTooltip
            ),
          },
          intl.formatMessage(
            StakePoolsTable_messages_1.messages.tableHeaderPotentialRewards
          )
        ),
        accessor: 'nonMyopicMemberRewards',
        Cell: ({ row }) => {
          const stakePool = row.original;
          const memberRewards = new bignumber_js_1.default(
            stakePool.potentialRewards
          );
          const potentialRewards = (0, formatters_1.formattedWalletAmount)(
            memberRewards
          );
          return potentialRewards;
        },
      },
      {
        id: 'pledge',
        Header: react_1.default.createElement(
          PopOver_1.PopOver,
          {
            key: 'pledge',
            placement: 'bottom',
            content: intl.formatMessage(
              StakePoolsTable_messages_1.messages.tableHeaderPledgeTooltip
            ),
          },
          intl.formatMessage(
            StakePoolsTable_messages_1.messages.tableHeaderPledge
          )
        ),
        accessor: 'pledge',
        Cell: ({ row }) => {
          const stakePool = row.original;
          const pledge = new bignumber_js_1.default(stakePool.pledge);
          const pledgeValue = (0, formatters_1.formattedWalletAmount)(
            pledge,
            false,
            false
          );
          return pledgeValue;
        },
      },
      {
        id: 'retiring',
        Header: intl.formatMessage(
          StakePoolsTable_messages_1.messages.tableHeaderRetiring
        ),
        accessor: 'retiring',
        Cell: ({ row }) => {
          const stakePool = row.original;
          const retirement =
            stakePool.retiring &&
            (0, moment_1.default)(stakePool.retiring)
              .locale(intl.locale)
              .fromNow(true);
          return react_1.default.createElement(
            react_1.default.Fragment,
            null,
            retirement
              ? react_1.default.createElement(
                  'span',
                  { className: StakePoolsTable_scss_1.default.retiring },
                  retirement
                )
              : '-'
          );
        },
      },
    ],
    [numberOfRankedStakePools]
  );
exports.useCreateColumns = useCreateColumns;
//# sourceMappingURL=useCreateColumns.js.map
