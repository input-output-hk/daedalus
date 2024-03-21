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
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const CountdownWidget_1 = __importDefault(
  require('../../widgets/CountdownWidget')
);
const StakingCountdown_scss_1 = __importDefault(
  require('./StakingCountdown.scss')
);
const ButtonLink_1 = __importDefault(require('../../widgets/ButtonLink'));
const messages = (0, react_intl_1.defineMessages)({
  heading: {
    id: 'staking.delegationCountdown.heading',
    defaultMessage: '!!!Shelley upgrade',
    description: 'Headline for the Decentralisation notification.',
  },
  description: {
    id: 'staking.delegationCountdown.description',
    defaultMessage:
      '!!!Cardano will soon start transitioning from a federated to a decentralized system. The first step is the activation of the Shelley upgrade. Once the upgrade is complete, stake pools will start registering and users will be able to delegate their wallets. Two epochs (10 days) later, stake pools will begin producing blocks and users could start earning rewards from delegating their stakes. The first rewards, where due, will be distributed two more epochs later (10 days).',
    description: 'Info for the Decentralisation notification.',
  },
  timeLeftDesc: {
    id: 'staking.delegationCountdown.timeLeftDesc',
    defaultMessage: '!!!Rewards begin in',
    description: 'Description for the Decentralisation notification.',
  },
  buttonLabel: {
    id: 'staking.delegationCountdown.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the Decentralisation notification.',
  },
});
let StakingCountdown = class StakingCountdown extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const {
      redirectToStakingInfo,
      startDateTime,
      onLearnMoreClick,
    } = this.props;
    const { intl } = this.context;
    const heading = intl.formatMessage(messages.heading);
    const description = intl.formatMessage(messages.description);
    const timeLeftDesc = intl.formatMessage(messages.timeLeftDesc);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const showLearnMoreButton = false;
    return react_1.default.createElement(
      'div',
      { className: StakingCountdown_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: StakingCountdown_scss_1.default.mainContent },
        react_1.default.createElement(
          'div',
          { className: StakingCountdown_scss_1.default.heading },
          heading
        ),
        react_1.default.createElement(
          'div',
          { className: StakingCountdown_scss_1.default.description },
          description
        ),
        react_1.default.createElement(
          'div',
          { className: StakingCountdown_scss_1.default.timeLeftDesc },
          timeLeftDesc
        ),
        react_1.default.createElement(CountdownWidget_1.default, {
          startDateTime: startDateTime,
          redirectOnEnd: redirectToStakingInfo,
        }),
        showLearnMoreButton &&
          react_1.default.createElement(
            ButtonLink_1.default,
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            {
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              className: StakingCountdown_scss_1.default.learnMoreButton,
              onClick: onLearnMoreClick,
              skin: ButtonSkin_1.ButtonSkin,
              label: buttonLabel,
              linkProps: {
                className: StakingCountdown_scss_1.default.externalLinkIcon,
              },
            }
          )
      )
    );
  }
};
StakingCountdown = __decorate([mobx_react_1.observer], StakingCountdown);
exports.default = StakingCountdown;
//# sourceMappingURL=StakingCountdown.js.map
