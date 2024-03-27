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
const ButtonLink_1 = __importDefault(require('../../widgets/ButtonLink'));
const StakingInfoCountdown_scss_1 = __importDefault(
  require('./StakingInfoCountdown.scss')
);
const FullyDecentralizedEffect_1 = __importDefault(
  require('../../widgets/FullyDecentralizedEffect')
);
const CountdownWidget_1 = __importDefault(
  require('../../widgets/CountdownWidget')
);
const messages = (0, react_intl_1.defineMessages)({
  heading: {
    id: 'staking.infoCountdown.heading',
    defaultMessage: '!!!Alonzo upgrade',
    description: 'Headline for the "Staking Info" page screen.',
  },
  descriptionBefore: {
    id: 'staking.infoCountdown.description.before',
    defaultMessage:
      '!!!The ‘Alonzo’ protocol upgrade will bring highly-anticipated new smart contract capabilities to Cardano, by integrating Plutus scripts onto the blockchain. This important milestone will open up a whole new world of smart contracts, DeFi capabilities, and dApp development on Cardano.',
    description: 'Info description for the "Staking Info" page screen.',
  },
  descriptionAfter: {
    id: 'staking.infoCountdown.description.after',
    defaultMessage:
      '!!!The ‘Alonzo’ protocol upgrade is now live on Cardano, enabling highly-anticipated new smart contract capabilities, by integrating Plutus scripts onto the blockchain. This important milestone opens up a whole new world of smart contracts, DeFi capabilities, and dApp development on Cardano.',
    description: 'Info description for the "Staking Info" page screen.',
  },
  countdownTitle: {
    id: 'staking.infoCountdown.countdownTitle',
    defaultMessage: '!!!Alonzo upgrade in',
    description: 'Countdown Title for the "Staking Info" page screen.',
  },
  buttonLabel: {
    id: 'staking.infoCountdown.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the "Staking Info" page screen.',
  },
  learnMoreLinkUrl: {
    id: 'staking.infoCountdown.learnMore.linkUrl',
    defaultMessage:
      '!!!https://iohk.io/en/blog/posts/2021/04/08/smart-contracts-%E2%80%93-here-we-come/',
    description: '"Learn more" link URL in the "Staking Info" screen.',
  },
});
let StakingInfoCountdown = class StakingInfoCountdown extends react_1.Component {
  static defaultProps = {
    percentage: 0,
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  componentDidMount() {
    this.checkIfShouldSetStakingInfoWasOpen();
  }
  componentDidUpdate() {
    this.checkIfShouldSetStakingInfoWasOpen();
  }
  checkIfShouldSetStakingInfoWasOpen = () => {
    const {
      onSetStakingInfoWasOpen,
      isAlonzoActivated,
      stakingInfoWasOpen,
    } = this.props;
    if (isAlonzoActivated && !stakingInfoWasOpen) {
      onSetStakingInfoWasOpen();
    }
  };
  render() {
    const { intl } = this.context;
    const { startDateTime, onLearnMoreClick, isAlonzoActivated } = this.props;
    const heading = intl.formatMessage(messages.heading);
    const description = isAlonzoActivated
      ? messages.descriptionAfter
      : messages.descriptionBefore;
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    return react_1.default.createElement(
      'div',
      { className: StakingInfoCountdown_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: StakingInfoCountdown_scss_1.default.mainContent },
        react_1.default.createElement(
          'div',
          { className: StakingInfoCountdown_scss_1.default.content },
          react_1.default.createElement(
            'div',
            { className: StakingInfoCountdown_scss_1.default.heading },
            heading
          ),
          react_1.default.createElement(
            'div',
            { className: StakingInfoCountdown_scss_1.default.description },
            intl.formatMessage(description)
          ),
          react_1.default.createElement(
            'div',
            { className: StakingInfoCountdown_scss_1.default.countdownTitle },
            intl.formatMessage(messages.countdownTitle)
          ),
          react_1.default.createElement(CountdownWidget_1.default, {
            startDateTime: isAlonzoActivated ? '0' : startDateTime,
            format: 'DD-HH-mm-ss',
          }),
          react_1.default.createElement(
            ButtonLink_1.default,
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            {
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              className: StakingInfoCountdown_scss_1.default.learnMoreButton,
              onClick: () =>
                onLearnMoreClick(intl.formatMessage(messages.learnMoreLinkUrl)),
              skin: ButtonSkin_1.ButtonSkin,
              label: buttonLabel,
              linkProps: {
                className: StakingInfoCountdown_scss_1.default.externalLinkIcon,
              },
            }
          )
        ),
        react_1.default.createElement(FullyDecentralizedEffect_1.default, {
          isActive: isAlonzoActivated,
        })
      )
    );
  }
};
StakingInfoCountdown = __decorate(
  [mobx_react_1.observer],
  StakingInfoCountdown
);
exports.default = StakingInfoCountdown;
//# sourceMappingURL=StakingInfoCountdown.js.map
