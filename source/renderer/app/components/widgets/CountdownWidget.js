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
const moment_1 = __importDefault(require('moment'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const CountdownWidget_scss_1 = __importDefault(
  require('./CountdownWidget.scss')
);
const delimeter_inline_svg_1 = __importDefault(
  require('../../assets/images/delimeter.inline.svg')
);
const spinner_inline_svg_1 = __importDefault(
  require('../../assets/images/spinner.inline.svg')
);
const global_messages_1 = __importDefault(
  require('../../i18n/global-messages')
);
const TIME_LEFT_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds;
const COLUMNS = {
  YYYY: 'years',
  MM: 'months',
  DD: 'days',
  HH: 'hours',
  mm: 'minutes',
  ss: 'seconds',
};
let CountdownWidget = class CountdownWidget extends react_1.Component {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  intervalHandler = null;
  state = {
    timeLeft: null,
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  componentDidMount() {
    this.startTimer();
  }
  componentWillUnmount() {
    this.stopTimer();
  }
  startTimer = () => {
    this.updateTimeLeft();
    this.intervalHandler = setInterval(
      () => this.updateTimeLeft(),
      TIME_LEFT_INTERVAL
    );
  };
  stopTimer = () => {
    if (this.intervalHandler) {
      clearInterval(this.intervalHandler);
      this.intervalHandler = null;
    }
  };
  updateTimeLeft = () => {
    const { redirectOnEnd, startDateTime } = this.props;
    if (startDateTime) {
      const timeLeft = Math.max(
        0,
        new Date(startDateTime).getTime() - new Date().getTime()
      );
      this.setState({
        timeLeft,
      });
      if (timeLeft === 0 && redirectOnEnd) {
        redirectOnEnd();
      }
    }
  };
  generateFieldPanel = (labels, values, index) => {
    const value = values[index];
    const includeDelimeter = index !== values.length - 1;
    const labelStr = labels[index];
    const { format } = this.props;
    const shouldBeHidden =
      values.slice(0, index).reduce((acc, val) => acc + val, 0) === 0 &&
      value === 0 &&
      !format;
    if (shouldBeHidden) {
      return null;
    }
    const valueStr = value.toString();
    const zeroValue = valueStr.length === 1 ? '0' : '';
    const isZeroValue =
      valueStr === '0' &&
      values.slice(0, index).reduce((acc, val) => acc + val, 0) === 0;
    return react_1.default.createElement(
      'div',
      { className: CountdownWidget_scss_1.default.fieldPanel },
      react_1.default.createElement(
        'div',
        { className: CountdownWidget_scss_1.default.left },
        react_1.default.createElement(
          'div',
          { className: CountdownWidget_scss_1.default.fieldLabel },
          labelStr
        ),
        react_1.default.createElement(
          'div',
          { className: CountdownWidget_scss_1.default.fieldValue },
          (isZeroValue || zeroValue) &&
            react_1.default.createElement('span', null, zeroValue),
          valueStr
        )
      ),
      includeDelimeter &&
        react_1.default.createElement(
          'div',
          { className: CountdownWidget_scss_1.default.right },
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: delimeter_inline_svg_1.default,
            className: CountdownWidget_scss_1.default.delimeterIcon,
          })
        )
    );
  };
  generateCountdownPanels = () => {
    const { intl } = this.context;
    const { timeLeft } = this.state;
    const { format } = this.props;
    const duration = moment_1.default.duration(timeLeft, 'milliseconds');
    const yearsLabel = intl.formatMessage(global_messages_1.default.years);
    const monthsLabel = intl.formatMessage(global_messages_1.default.months);
    const daysLabel = intl.formatMessage(global_messages_1.default.days);
    const hoursLabel = intl.formatMessage(global_messages_1.default.hours);
    const minutesLabel = intl.formatMessage(global_messages_1.default.minutes);
    const secondsLabel = intl.formatMessage(global_messages_1.default.seconds);
    const labels = format
      ? format
          .split('-')
          .map((item) =>
            intl.formatMessage(global_messages_1.default[COLUMNS[item]])
          )
      : [
          yearsLabel,
          monthsLabel,
          daysLabel,
          hoursLabel,
          minutesLabel,
          secondsLabel,
        ];
    const years = duration.years();
    const months = duration.months();
    const days = duration.days();
    const hours = duration.hours();
    const minutes = duration.minutes();
    const seconds = duration.seconds();
    const values = format
      ? format.split('-').map((item) => duration[COLUMNS[item]]())
      : [years, months, days, hours, minutes, seconds];
    const keys = format
      ? format.split('-').map((item) => COLUMNS[item])
      : ['years', 'months', 'days', 'hours', 'minutes', 'seconds'];
    return labels.map((label, index) =>
      react_1.default.createElement(
        react_1.Fragment,
        { key: keys[index] },
        this.generateFieldPanel(labels, values, index)
      )
    );
  };
  render() {
    const { startDateTime } = this.props;
    const fieldPanels = this.generateCountdownPanels();
    const showSpinner = startDateTime === null;
    return react_1.default.createElement(
      'div',
      { className: CountdownWidget_scss_1.default.timeLeftContainer },
      react_1.default.createElement(
        'div',
        { className: CountdownWidget_scss_1.default.timeLeft },
        showSpinner
          ? react_1.default.createElement(react_svg_inline_1.default, {
              svg: spinner_inline_svg_1.default,
              className: CountdownWidget_scss_1.default.spinnerIcon,
            })
          : fieldPanels
      )
    );
  }
};
CountdownWidget = __decorate([mobx_react_1.observer], CountdownWidget);
exports.default = CountdownWidget;
//# sourceMappingURL=CountdownWidget.js.map
