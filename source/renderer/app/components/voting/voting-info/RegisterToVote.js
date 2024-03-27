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
const Button_1 = require('@react-polymorph/components/Button');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const RegisterToVote_messages_1 = require('./RegisterToVote.messages');
const VotingInfo_messages_1 = require('./VotingInfo.messages');
const RegisterToVote_scss_1 = __importDefault(require('./RegisterToVote.scss'));
function RegisterToVote({ intl, onRegisterToVoteClick }) {
  const [step1, setStep1] = (0, react_1.useState)(false);
  const [step2, setStep2] = (0, react_1.useState)(false);
  const canRegister = step1 && step2;
  return react_1.default.createElement(
    'div',
    { className: RegisterToVote_scss_1.default.root },
    react_1.default.createElement(
      'div',
      { className: RegisterToVote_scss_1.default.title },
      intl.formatMessage(VotingInfo_messages_1.messages.registerToVoteHeader)
    ),
    react_1.default.createElement(
      'div',
      { className: RegisterToVote_scss_1.default.votingInstructions },
      intl.formatMessage(RegisterToVote_messages_1.messages.votingInstructions)
    ),
    react_1.default.createElement(
      'div',
      { className: RegisterToVote_scss_1.default.stepsTitle },
      intl.formatMessage(RegisterToVote_messages_1.messages.stepsTitle)
    ),
    react_1.default.createElement(
      'div',
      { className: RegisterToVote_scss_1.default.step },
      react_1.default.createElement(Checkbox_1.Checkbox, {
        checked: step1,
        onChange: setStep1,
        label: intl.formatMessage(
          RegisterToVote_messages_1.messages.step1CheckBoxLabel
        ),
      })
    ),
    react_1.default.createElement(
      'div',
      { className: RegisterToVote_scss_1.default.step },
      react_1.default.createElement(Checkbox_1.Checkbox, {
        checked: step2,
        label: intl.formatMessage(
          RegisterToVote_messages_1.messages.step2CheckBoxLabel
        ),
        onChange: setStep2,
      })
    ),
    react_1.default.createElement(Button_1.Button, {
      className: RegisterToVote_scss_1.default.button,
      onClick: onRegisterToVoteClick,
      label: intl.formatMessage(RegisterToVote_messages_1.messages.buttonLabel),
      disabled: !canRegister,
    })
  );
}
exports.default = (0, react_intl_1.injectIntl)(RegisterToVote);
//# sourceMappingURL=RegisterToVote.js.map
