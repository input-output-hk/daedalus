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
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const ProgressBarLarge_scss_1 = __importDefault(
  require('./ProgressBarLarge.scss')
);
let ProgressBarLarge = class ProgressBarLarge extends react_1.Component {
  static defaultProps = {
    progress: 0,
  };
  render() {
    const {
      progress,
      showProgressLabel,
      leftLabel,
      rightLabel1,
      rightLabel2,
      isDarkMode,
      loading,
    } = this.props;
    const isComplete = progress >= 100;
    const progressStyles = (0, classnames_1.default)([
      ProgressBarLarge_scss_1.default.progress,
      isComplete ? ProgressBarLarge_scss_1.default.isComplete : null,
      isDarkMode
        ? ProgressBarLarge_scss_1.default.progressDarkMode
        : ProgressBarLarge_scss_1.default.progressLightMode,
      loading ? ProgressBarLarge_scss_1.default.loading : null,
    ]);
    const progressBarContainerStyles = (0, classnames_1.default)([
      ProgressBarLarge_scss_1.default.progressBarContainer,
      loading ? ProgressBarLarge_scss_1.default.loading : null,
    ]);
    return react_1.default.createElement(
      'div',
      { className: ProgressBarLarge_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: ProgressBarLarge_scss_1.default.content },
        react_1.default.createElement('p', null, leftLabel),
        react_1.default.createElement(
          'p',
          { className: ProgressBarLarge_scss_1.default.rightLabel },
          react_1.default.createElement('b', null, rightLabel1),
          ' ',
          rightLabel2
        )
      ),
      react_1.default.createElement(
        'div',
        { className: progressBarContainerStyles },
        !loading &&
          react_1.default.createElement(
            'div',
            {
              className: progressStyles,
              style: {
                width: `${progress}%`,
              },
            },
            showProgressLabel &&
              react_1.default.createElement(
                'div',
                { className: ProgressBarLarge_scss_1.default.progressLabel },
                progress,
                '%'
              )
          )
      )
    );
  }
};
ProgressBarLarge = __decorate([mobx_react_1.observer], ProgressBarLarge);
exports.default = ProgressBarLarge;
//# sourceMappingURL=ProgressBarLarge.js.map
