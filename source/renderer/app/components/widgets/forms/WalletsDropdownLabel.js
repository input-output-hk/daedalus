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
const react_1 = __importStar(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const colors_1 = require('../../../utils/colors');
const WalletsDropdownLabel_scss_1 = __importDefault(
  require('./WalletsDropdownLabel.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/hardware... Remove this comment to see the full error message
const connect_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/hardware-wallet/connect-ic.inline.svg')
);
class WalletsDropdownLabel extends react_1.Component {
  renderTicker = () => {
    const { wallet, getStakePoolById, numberOfStakePools } = this.props;
    const {
      delegatedStakePoolId,
      lastDelegatedStakePoolId,
      pendingDelegations,
    } = wallet;
    const hasPendingDelegations =
      pendingDelegations && pendingDelegations.length > 0;
    let currentStakePoolId = delegatedStakePoolId;
    if (hasPendingDelegations) {
      currentStakePoolId = lastDelegatedStakePoolId;
    }
    const delegatedStakePool = currentStakePoolId
      ? getStakePoolById(currentStakePoolId)
      : null;
    if (!numberOfStakePools || !delegatedStakePool) {
      return null;
    }
    const { ranking, ticker } = delegatedStakePool;
    const color = (0, colors_1.getColorFromRange)(ranking, numberOfStakePools);
    return react_1.default.createElement(
      'div',
      {
        style: {
          color,
        },
        className: WalletsDropdownLabel_scss_1.default.ticker,
      },
      '[',
      ticker,
      ']'
    );
  };
  render() {
    const { wallet } = this.props;
    const { name, isHardwareWallet } = wallet;
    const ticker = this.renderTicker();
    return react_1.default.createElement(
      'div',
      { className: WalletsDropdownLabel_scss_1.default.component },
      ticker,
      react_1.default.createElement(
        'div',
        { className: WalletsDropdownLabel_scss_1.default.walletName },
        name,
        isHardwareWallet &&
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: connect_ic_inline_svg_1.default,
            className: WalletsDropdownLabel_scss_1.default.hardwareWalletsIcon,
          })
      )
    );
  }
}
exports.default = WalletsDropdownLabel;
//# sourceMappingURL=WalletsDropdownLabel.js.map
