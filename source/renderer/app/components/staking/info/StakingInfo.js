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
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
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
const StakingInfo_scss_1 = __importDefault(require('./StakingInfo.scss'));
const messages = (0, react_intl_1.defineMessages)({
  heading: {
    id: 'staking.info.heading',
    defaultMessage: '!!!Cardano is transitioning into a decentralized system',
    description: 'Headline for the Decentralization progress notification.',
  },
  description: {
    id: 'staking.info.description',
    defaultMessage:
      '!!!Cardano is transitioning from a federated system operated by its creators to a decentralized system operated by a community of stake pool operators. During this transition, blocks will be produced both by the federated nodes and by stake pools. The percentage of blocks produced by stake pools will increase every epoch until block production in the Cardano network becomes fully decentralized.',
    description:
      'Info description for the Decentralization progress notification.',
  },
  percentage: {
    id: 'staking.info.percentage',
    defaultMessage:
      '!!!Currently, {percentage}% of the blocks are produced by the stake pools.',
    description:
      'Percentage info description for the Decentralization progress notification.',
  },
  buttonLabel: {
    id: 'staking.info.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the Decentralization progress notification.',
  },
  learnMoreLinkUrl: {
    id: 'staking.info.learnMore.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc',
    description: '"Learn more" link URL in the staking info page',
  },
});
let StakingInfo = class StakingInfo extends react_1.Component {
  static defaultProps = {
    percentage: 0,
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  progressRef;
  constructor(props) {
    super(props);
    this.progressRef = react_1.default.createRef();
    this.state = {
      progressLabelClassName: StakingInfo_scss_1.default.progressLabelWhite,
    };
  }
  componentDidMount() {
    this.handleProgressLabelClassName();
  }
  componentDidUpdate(prevProps) {
    const { percentage: prevPercentage } = prevProps;
    const { percentage: currentPercentage } = this.props;
    if (prevPercentage !== currentPercentage) {
      this.handleProgressLabelClassName();
    }
  }
  handleProgressLabelClassName = () => {
    const { current: progressComponent } = this.progressRef;
    const progressLabelClassName =
      progressComponent.clientWidth >= 50
        ? StakingInfo_scss_1.default.progressLabelWhite
        : StakingInfo_scss_1.default.progressLabel;
    this.setState({
      progressLabelClassName,
    });
  };
  render() {
    const { intl } = this.context;
    const { percentage, onLearnMoreClick } = this.props;
    const { progressLabelClassName } = this.state;
    const heading = intl.formatMessage(messages.heading);
    const description = intl.formatMessage(messages.description);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const showLearnMoreButton = false;
    return react_1.default.createElement(
      'div',
      { className: StakingInfo_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: StakingInfo_scss_1.default.mainContent },
        react_1.default.createElement(
          'div',
          { className: StakingInfo_scss_1.default.heading },
          heading
        ),
        react_1.default.createElement(
          'div',
          { className: StakingInfo_scss_1.default.description },
          description
        ),
        react_1.default.createElement(
          'div',
          { className: StakingInfo_scss_1.default.description },
          react_1.default.createElement(react_intl_1.FormattedMessage, {
            ...messages.percentage,
            values: {
              percentage,
            },
          })
        ),
        react_1.default.createElement(
          'div',
          { className: StakingInfo_scss_1.default.progressBar },
          react_1.default.createElement(
            'div',
            { className: StakingInfo_scss_1.default.progressBarContainer },
            react_1.default.createElement(
              'div',
              {
                className: StakingInfo_scss_1.default.progress,
                ref: this.progressRef,
                style: {
                  width: `${percentage}%`,
                },
              },
              react_1.default.createElement(
                'div',
                { className: progressLabelClassName },
                percentage,
                '%'
              )
            )
          )
        ),
        showLearnMoreButton &&
          react_1.default.createElement(
            ButtonLink_1.default,
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            {
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              className: StakingInfo_scss_1.default.learnMoreButton,
              onClick: () =>
                onLearnMoreClick(intl.formatMessage(messages.learnMoreLinkUrl)),
              skin: ButtonSkin_1.ButtonSkin,
              label: buttonLabel,
              linkProps: {
                className: StakingInfo_scss_1.default.externalLinkIcon,
              },
            }
          )
      )
    );
  }
};
StakingInfo = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object])],
  StakingInfo
);
exports.default = StakingInfo;
//# sourceMappingURL=StakingInfo.js.map
