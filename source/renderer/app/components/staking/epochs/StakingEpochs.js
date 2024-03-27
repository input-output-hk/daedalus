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
const Select_1 = require('@react-polymorph/components/Select');
const SelectSkin_1 = require('@react-polymorph/skins/simple/SelectSkin');
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const StakingEpochsCurrentEpochData_1 = __importDefault(
  require('./StakingEpochsCurrentEpochData')
);
const StakingEpochsPreviousEpochData_1 = __importDefault(
  require('./StakingEpochsPreviousEpochData')
);
const helpers_1 = require('./helpers');
const StakingEpochs_scss_1 = __importDefault(require('./StakingEpochs.scss'));
const messages = (0, react_intl_1.defineMessages)({
  currentEpochHeading: {
    id: 'staking.epochs.currentHeading',
    defaultMessage: '!!!current epoch',
    description: 'Headline for the current epoch.',
  },
  previousEpochHeading: {
    id: 'staking.epochs.previousHeading',
    defaultMessage: '!!!previous epoch',
    description: 'Headline for the previous epoch.',
  },
});
const { CURRENT_EPOCH, PREVIOUS_EPOCH } = helpers_1.SELECTED_EPOCH_OPTIONS;
let StakingEpochs = class StakingEpochs extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    isLoading: false,
  };
  constructor() {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1-2 arguments, but got 0.
    super();
    this.state = {
      selectedEpoch: CURRENT_EPOCH,
      duration: '',
    };
  }
  componentDidMount() {
    const { intl } = this.context;
    const { currentEpochEndDateTime } = this.props;
    this.setState({
      duration: (0, helpers_1.humanizeDurationToShort)(
        intl.locale,
        currentEpochEndDateTime
      ),
    });
  }
  onSelectedEpochChange = (selectedEpoch) =>
    this.setState({
      selectedEpoch,
    });
  render() {
    const {
      currentEpochName,
      currentEpochData,
      currentEpochProgress,
      previousEpochName,
      previousEpochData,
      isLoading,
    } = this.props;
    const { selectedEpoch, duration } = this.state;
    const { intl } = this.context;
    const epochSelectOptions = [
      {
        label: `${currentEpochName} (${intl.formatMessage(
          messages.currentEpochHeading
        )})`,
        value: CURRENT_EPOCH,
      },
      {
        label: `${previousEpochName} (${intl.formatMessage(
          messages.previousEpochHeading
        )})`,
        value: PREVIOUS_EPOCH,
      },
    ];
    if (!currentEpochName) {
      return null;
    }
    return react_1.default.createElement(
      'div',
      { className: StakingEpochs_scss_1.default.component },
      react_1.default.createElement(
        BorderedBox_1.default,
        null,
        react_1.default.createElement(
          'div',
          { className: StakingEpochs_scss_1.default.headerWrapper },
          react_1.default.createElement(Select_1.Select, {
            className: StakingEpochs_scss_1.default.epochSelector,
            options: epochSelectOptions,
            value: selectedEpoch,
            onChange: this.onSelectedEpochChange,
            skin: SelectSkin_1.SelectSkin,
            selectionRenderer: (option) =>
              react_1.default.createElement(
                'div',
                { className: StakingEpochs_scss_1.default.customSelectValue },
                option.label
              ),
            optionHeight: 50,
          })
        ),
        selectedEpoch === CURRENT_EPOCH &&
          react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(
              'div',
              {
                className: StakingEpochs_scss_1.default.currentEpochProgressBar,
              },
              react_1.default.createElement(
                'div',
                {
                  className: StakingEpochs_scss_1.default.progressBarContainer,
                },
                react_1.default.createElement(
                  'div',
                  {
                    className: StakingEpochs_scss_1.default.progress,
                    style: {
                      width: `${currentEpochProgress}%`,
                    },
                  },
                  react_1.default.createElement(
                    'div',
                    {
                      className:
                        StakingEpochs_scss_1.default.overlapProgressLabel,
                      style: {
                        left: `${10000 / currentEpochProgress}%`,
                      },
                    },
                    duration
                  )
                ),
                react_1.default.createElement(
                  'div',
                  { className: StakingEpochs_scss_1.default.progressLabel },
                  duration
                )
              )
            ),
            react_1.default.createElement(
              StakingEpochsCurrentEpochData_1.default,
              { currentEpochData: currentEpochData, isLoading: isLoading }
            )
          ),
        selectedEpoch === PREVIOUS_EPOCH &&
          react_1.default.createElement(
            StakingEpochsPreviousEpochData_1.default,
            { previousEpochData: previousEpochData, isLoading: isLoading }
          ),
        isLoading &&
          react_1.default.createElement(
            'div',
            { className: StakingEpochs_scss_1.default.loadingSpinnerWrapper },
            react_1.default.createElement(LoadingSpinner_1.default, null)
          )
      )
    );
  }
};
StakingEpochs = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [])],
  StakingEpochs
);
exports.default = StakingEpochs;
//# sourceMappingURL=StakingEpochs.js.map
