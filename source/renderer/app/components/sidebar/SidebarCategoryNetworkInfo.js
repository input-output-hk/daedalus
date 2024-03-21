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
const react_intl_1 = require('react-intl');
const SidebarCategoryNetworkInfo_scss_1 = __importDefault(
  require('./SidebarCategoryNetworkInfo.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  mainnet: {
    id: 'test.environment.mainnetLabel',
    defaultMessage: '!!!Mainnet vx',
    description: 'Label for mainnet network with version.',
  },
  flight: {
    id: 'test.environment.daedalusFlightLabel',
    defaultMessage: '!!!Cardano mainnet - Daedalus Flight',
    description: 'Label for Daedalus Flight with version.',
  },
  testnet: {
    id: 'test.environment.testnetLabel',
    defaultMessage: '!!!Testnet vx',
    description: 'Label for testnet with version.',
  },
  staging: {
    id: 'test.environment.stagingLabel',
    defaultMessage: '!!!Staging vx',
    description: 'Label for staging network with version.',
  },
  shelley_qa: {
    id: 'test.environment.shelleyQaLabel',
    defaultMessage: '!!!Shelley QA',
    description: 'Label for shelley_qa with version.',
  },
  alonzo_purple: {
    id: 'test.environment.alonzoPurpleLabel',
    defaultMessage: '!!!Alonzo Purple',
    description: 'Label for alonzo_purple with version.',
  },
  vasil_dev: {
    id: 'test.environment.vasilDevLabel',
    defaultMessage: '!!!Vasil-Dev',
    description: 'Label for vasil_dev with version.',
  },
  preprod: {
    id: 'test.environment.preprodLabel',
    defaultMessage: '!!!Pre-Prod',
    description: 'Label for preprod with version.',
  },
  preview: {
    id: 'test.environment.previewLabel',
    defaultMessage: '!!!Preview',
    description: 'Label for preview with version.',
  },
  selfnode: {
    id: 'test.environment.selfnodeLabel',
    defaultMessage: '!!!Selfnode vx',
    description: 'Label for selfnode with version.',
  },
  development: {
    id: 'test.environment.developmentLabel',
    defaultMessage: '!!!Development vx',
    description: 'Label for development with version.',
  },
});
class SidebarCategoryNetworkInfo extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { isFlight } = global;
    const { network } = this.props;
    const { intl } = this.context;
    const label = messages[isFlight ? 'flight' : network];
    return react_1.default.createElement(
      'div',
      { className: SidebarCategoryNetworkInfo_scss_1.default.component },
      intl.formatMessage(label)
    );
  }
}
exports.default = SidebarCategoryNetworkInfo;
//# sourceMappingURL=SidebarCategoryNetworkInfo.js.map
