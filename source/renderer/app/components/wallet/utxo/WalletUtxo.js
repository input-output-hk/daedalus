'use strict';
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
exports.messages = void 0;
const react_1 = __importStar(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const recharts_1 = require('recharts');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const WalletUtxoTick_1 = __importDefault(require('./WalletUtxoTick'));
const WalletUtxoTooltip_1 = __importDefault(require('./WalletUtxoTooltip'));
const WalletUtxoCursor_1 = __importDefault(require('./WalletUtxoCursor'));
const numbersConfig_1 = require('../../../config/numbersConfig');
const WalletUtxo_scss_1 = __importDefault(require('./WalletUtxo.scss'));
const WalletUtxoDescription_1 = require('./WalletUtxoDescription');
exports.messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.settings.utxos.title',
    defaultMessage: '!!!Wallet UTXO distribution',
    description: 'Title for the "Wallet Utxos" screen.',
  },
  description: {
    id: 'wallet.settings.utxos.description',
    defaultMessage:
      '!!!This wallet contains <b>{formattedWalletAmount} ADA</b> on <b>{walletUtxosAmount} UTXOs</b> (unspent transaction outputs). Examine the histogram below to see the distribution of UTXOs with different amounts of ada.',
    description: 'Description for the "Wallet Utxos" screen.',
  },
  emptyWallet: {
    id: 'wallet.settings.utxos.emptyWallet',
    defaultMessage:
      '!!!This wallet is empty so it does not contain any UTXOs (unspent transaction outputs).',
    description: 'Empty wallet description for the "Wallet Utxos" screen.',
  },
  findOutMoreLink: {
    id: 'wallet.settings.utxos.findOutMoreLink',
    defaultMessage: '!!!Find out more',
    description: '"Find out more" link on the "Wallet Utxos" screen.',
  },
  findOutMoreLinkUrl: {
    id: 'wallet.settings.utxos.findOutMoreLinkUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360034118013',
    description: '"Find out more" link URL on the "Wallet Utxos" screen.',
  },
  labelX: {
    id: 'wallet.settings.utxos.labelX',
    defaultMessage: '!!!amount',
    description: 'Label X for the "Wallet Utxos" screen.',
  },
  labelY: {
    id: 'wallet.settings.utxos.labelY',
    defaultMessage: '!!!Nº UTXO',
    description: 'Label Y for the "Wallet Utxos" screen.',
  },
  pendingTransactions: {
    id: 'wallet.settings.utxos.pendingTransactions',
    defaultMessage:
      '!!!<b>Pending transactions</b> may affect the accuracy of data presented here. <br /> You have <b>{pendingTxnsCount}</b> pending transaction{txnsPlural}.',
    description:
      'Number of pending transactions for the "Wallet Utxos" screen.',
  },
});
class WalletUtxo extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    isHoveringChart: false,
  };
  shouldComponentUpdate() {
    return !this.state.isHoveringChart;
  }
  renderPendingTxns = (pendingTxnsCount) => {
    const txnsPlural = (!pendingTxnsCount || pendingTxnsCount > 1) && 's';
    return react_1.default.createElement(
      'div',
      { className: WalletUtxo_scss_1.default.pendingTxnsWrapper },
      react_1.default.createElement(
        'div',
        null,
        react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...exports.messages.pendingTransactions,
            values: {
              pendingTxnsCount,
              txnsPlural,
            },
          })
        )
      )
    );
  };
  render() {
    const { intl } = this.context;
    const {
      isLoadingInitialUtxoData,
      walletAmount,
      walletUtxosAmount,
      chartData,
      onExternalLinkClick,
      pendingTxnsCount,
    } = this.props;
    const formattedWalletAmount = walletAmount.toFormat(
      numbersConfig_1.DECIMAL_PLACES_IN_ADA
    );
    const isEmpty = walletUtxosAmount === 0;
    const componentStyles = (0, classnames_1.default)([
      WalletUtxo_scss_1.default.component,
      isEmpty ? WalletUtxo_scss_1.default.isEmpty : null,
    ]);
    const findOutMoreLinkUrl = intl.formatMessage(
      exports.messages.findOutMoreLinkUrl
    );
    const findOutMoreLink = react_1.default.createElement(Link_1.Link, {
      className: WalletUtxo_scss_1.default.externalLink,
      onClick: (event) => onExternalLinkClick(findOutMoreLinkUrl, event),
      label: intl.formatMessage(exports.messages.findOutMoreLink),
      skin: LinkSkin_1.LinkSkin,
    });
    const emptyOrLoadingState = isLoadingInitialUtxoData
      ? react_1.default.createElement(LoadingSpinner_1.default, null)
      : react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(exports.messages.emptyWallet)
        );
    return react_1.default.createElement(
      'div',
      { className: componentStyles },
      react_1.default.createElement(
        BorderedBox_1.default,
        { className: WalletUtxo_scss_1.default.borderedBox },
        react_1.default.createElement(
          'div',
          {
            className: WalletUtxo_scss_1.default.container,
            onMouseEnter: () =>
              this.setState({
                isHoveringChart: true,
              }),
            onMouseLeave: () =>
              this.setState({
                isHoveringChart: false,
              }),
          },
          react_1.default.createElement(
            'h1',
            null,
            intl.formatMessage(exports.messages.title)
          ),
          !isLoadingInitialUtxoData && !isEmpty
            ? react_1.default.createElement(
                react_1.Fragment,
                null,
                react_1.default.createElement(
                  'p',
                  null,
                  react_1.default.createElement(
                    WalletUtxoDescription_1.WalletUtxoDescription,
                    {
                      description: exports.messages.description,
                      formattedWalletAmount: formattedWalletAmount,
                      walletUtxosAmount: walletUtxosAmount,
                    }
                  ),
                  ' ',
                  findOutMoreLink
                ),
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      WalletUtxo_scss_1.default.responsiveContainerWrapper,
                  },
                  react_1.default.createElement(
                    recharts_1.ResponsiveContainer,
                    {
                      height: 280,
                      className: WalletUtxo_scss_1.default.responsiveContainer,
                    },
                    react_1.default.createElement(
                      recharts_1.BarChart,
                      { data: chartData, barSize: 23 },
                      react_1.default.createElement(recharts_1.CartesianGrid, {
                        horizontal: false,
                        vertical: false,
                        y: -10,
                        height: 255,
                        fill: 'transparent',
                      }),
                      react_1.default.createElement(
                        recharts_1.XAxis,
                        {
                          dataKey: 'walletAmount',
                          interval: 0,
                          axisLine: false,
                          tickLine: false,
                          tick: (props) =>
                            react_1.default.createElement(
                              WalletUtxoTick_1.default,
                              { ...props, textAnchor: 'start', vertical: true }
                            ),
                          y: 0,
                        },
                        react_1.default.createElement(recharts_1.Label, {
                          value: intl.formatMessage(exports.messages.labelX),
                          offset: 20,
                          position: 'insideBottomRight',
                          className: WalletUtxo_scss_1.default.xAxisLabel,
                        })
                      ),
                      react_1.default.createElement(
                        recharts_1.YAxis,
                        {
                          dataKey: 'walletUtxosAmount',
                          axisLine: false,
                          tickLine: false,
                          allowDecimals: false,
                          domain: [0, 'dataMax'],
                          tick: (props) =>
                            react_1.default.createElement(
                              WalletUtxoTick_1.default,
                              { ...props, textAnchor: 'end' }
                            ),
                        },
                        react_1.default.createElement(recharts_1.Label, {
                          value: intl.formatMessage(exports.messages.labelY),
                          offset: 0,
                          position: 'insideTopLeft',
                          className: WalletUtxo_scss_1.default.yAxisLabel,
                        })
                      ),
                      react_1.default.createElement(recharts_1.Tooltip, {
                        cursor: react_1.default.createElement(
                          WalletUtxoCursor_1.default,
                          { offsetWidth: 28 }
                        ),
                        isAnimationActive: false,
                        content: react_1.default.createElement(
                          WalletUtxoTooltip_1.default,
                          null
                        ),
                      }),
                      react_1.default.createElement(recharts_1.Bar, {
                        dataKey: 'walletUtxosAmount',
                        className: WalletUtxo_scss_1.default.bar,
                      })
                    )
                  )
                ),
                this.renderPendingTxns(pendingTxnsCount)
              )
            : emptyOrLoadingState
        )
      )
    );
  }
}
exports.default = WalletUtxo;
//# sourceMappingURL=WalletUtxo.js.map
