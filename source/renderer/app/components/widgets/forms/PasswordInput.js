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
exports.PasswordInput = void 0;
const classnames_1 = __importDefault(require('classnames'));
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const PasswordInput_1 = require('@react-polymorph/components/PasswordInput');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const PasswordInput_scss_1 = __importDefault(require('./PasswordInput.scss'));
class PasswordInput extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { className, ...pwInputProps } = this.props;
    return react_1.default.createElement(
      'div',
      {
        className: (0, classnames_1.default)([
          PasswordInput_scss_1.default.root,
          className,
        ]),
      },
      react_1.default.createElement(PasswordInput_1.PasswordInput, {
        ...pwInputProps,
        passwordFeedbacks: {
          insecure: intl.formatMessage(
            global_messages_1.default.invalidSpendingPassword
          ),
          weak: intl.formatMessage(
            global_messages_1.default.weakSpendingPassword
          ),
          strong: intl.formatMessage(
            global_messages_1.default.strongSpendingPassword
          ),
          noMatch: intl.formatMessage(
            global_messages_1.default.invalidRepeatPassword
          ),
        },
      })
    );
  }
}
exports.PasswordInput = PasswordInput;
//# sourceMappingURL=PasswordInput.js.map
