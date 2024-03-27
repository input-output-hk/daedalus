'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.discreetRewardsAmount = exports.getFormattedRewardAmount = void 0;
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/question... Remove this comment to see the full error message
const question_mark_inline_svg_1 = __importDefault(
  require('../../../assets/images/question-mark.inline.svg')
);
const features_1 = require('../../../features');
const formatters_1 = require('../../../utils/formatters');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSummaryHeaderRewards.s... Remove this comment to see the full error message
const WalletSummaryHeaderRewards_scss_1 = __importDefault(
  require('./WalletSummaryHeaderRewards.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  rewards: {
    id: 'wallet.summary.header.rewardsSummary',
    defaultMessage: '!!!{total} rewards earned, {unspent} unspent rewards',
    description: 'Headline for the Decentralisation notification.',
  },
  unspendableTooltip: {
    id: 'wallet.summary.header.unspendableTooltip',
    defaultMessage: `!!!All the ada in this wallet is in the rewards account.
      Since transaction fees cannot be paid with rewards, please send 2 or
      more ada to this wallet to cover transaction fees.`,
    description:
      'Tooltip describing that rewards are unspendable on the Wallet summary header',
  },
});
function getFormattedRewardAmount(amount) {
  return amount.isGreaterThan(0) && amount.isLessThan(0.1)
    ? '< 0.1 ADA'
    : (0, formatters_1.formattedWalletAmount)(amount, true, false, 'ADA', 1);
}
exports.getFormattedRewardAmount = getFormattedRewardAmount;
function discreetRewardsAmount(isRestoring = false) {
  return (isDiscreetMode, symbol, value) => {
    if (isRestoring) {
      return '–';
    }
    if (!isDiscreetMode) {
      return getFormattedRewardAmount(value);
    }
    return `${symbol} ADA`;
  };
}
exports.discreetRewardsAmount = discreetRewardsAmount;
function WalletSummaryHeaderRewards(props) {
  const discreetModeFeature = (0, features_1.useDiscreetModeFeature)();
  const { isRestoring, total, unspent, walletAmount } = props;
  return react_1.default.createElement(
    'div',
    { className: WalletSummaryHeaderRewards_scss_1.default.component },
    react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
      ...messages.rewards,
      values: {
        total: discreetModeFeature.discreetValue({
          replacer: discreetRewardsAmount(isRestoring),
          value: total,
        }),
        unspent: discreetModeFeature.discreetValue({
          replacer: discreetRewardsAmount(isRestoring),
          value: unspent,
        }),
      },
    }),
    walletAmount.isGreaterThan(0) &&
      walletAmount.isEqualTo(unspent) &&
      react_1.default.createElement(
        PopOver_1.PopOver,
        {
          offset: [0, 10],
          content: react_1.default.createElement(
            react_intl_1.FormattedMessage,
            { ...messages.unspendableTooltip }
          ),
        },
        react_1.default.createElement(
          'span',
          { className: WalletSummaryHeaderRewards_scss_1.default.questionMark },
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: question_mark_inline_svg_1.default,
          })
        )
      )
  );
}
exports.default = (0, mobx_react_1.observer)(WalletSummaryHeaderRewards);
//# sourceMappingURL=WalletSummaryHeaderRewards.js.map
