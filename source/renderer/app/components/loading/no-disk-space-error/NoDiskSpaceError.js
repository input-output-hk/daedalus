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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/attenti... Remove this comment to see the full error message
const attention_big_light_inline_svg_1 = __importDefault(
  require('../../../assets/images/attention-big-light.inline.svg')
);
const NoDiskSpaceError_scss_1 = __importDefault(
  require('./NoDiskSpaceError.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  overlayContent: {
    id: 'noDiskSpace.error.overlayContent',
    defaultMessage:
      '!!!<b>Daedalus requires at least {diskSpaceRequired} of hard drive space to operate. Your computer is missing {diskSpaceMissing} of available space. Please delete some files to increase available hard drive space to continue using Daedalus. </b><br /><br />It is recommended to have at least 15% of hard drive space available ({diskSpaceRecommended} in your case) for normal and stable operation of the operating system and installed programs. We strongly recommend that you free up at least that amount of space from your hard drive.',
    description: 'Content of No disk space overlay',
  },
  overlayTitle: {
    id: 'noDiskSpace.error.overlayTitle',
    defaultMessage: '!!!Daedalus requires more hard drive space',
    description: 'Title of No disk space overlay',
  },
});
let NoDiskSpaceError = class NoDiskSpaceError extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      diskSpaceRequired,
      diskSpaceMissing,
      diskSpaceRecommended,
    } = this.props;
    return react_1.default.createElement(
      'div',
      { className: NoDiskSpaceError_scss_1.default.component },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: attention_big_light_inline_svg_1.default,
        className: NoDiskSpaceError_scss_1.default.icon,
      }),
      react_1.default.createElement(
        'div',
        null,
        react_1.default.createElement(
          'h1',
          null,
          intl.formatMessage(messages.overlayTitle)
        ),
        react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.overlayContent,
            values: {
              diskSpaceRequired,
              diskSpaceMissing,
              diskSpaceRecommended,
            },
          })
        )
      )
    );
  }
};
NoDiskSpaceError = __decorate([mobx_react_1.observer], NoDiskSpaceError);
exports.default = NoDiskSpaceError;
//# sourceMappingURL=NoDiskSpaceError.js.map
