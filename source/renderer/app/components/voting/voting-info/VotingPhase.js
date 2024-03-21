'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const formatters_1 = require('../../../utils/formatters');
const VotingPhase_messages_1 = require('./VotingPhase.messages');
const VotingInfo_messages_1 = require('./VotingInfo.messages');
const CurrentPhase_scss_1 = __importDefault(require('./CurrentPhase.scss'));
const VotingInfo_scss_1 = __importDefault(require('./VotingInfo.scss'));
function VotingPhase({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  fundInfo,
  intl,
}) {
  const mappedFormats = (0, formatters_1.mapToLongDateTimeFormat)({
    currentLocale,
    currentDateFormat,
    currentTimeFormat,
  });
  const startDate = (0, formatters_1.formattedDateTime)(
    fundInfo.current.startTime,
    {
      currentLocale,
      currentDateFormat: mappedFormats.currentDateFormat,
    }
  );
  const endDate = (0, formatters_1.formattedDateTime)(
    fundInfo.current.endTime,
    {
      currentLocale,
      currentDateFormat: mappedFormats.currentDateFormat,
    }
  );
  return react_1.default.createElement(
    'section',
    { className: CurrentPhase_scss_1.default.root },
    react_1.default.createElement(
      'h1',
      { className: CurrentPhase_scss_1.default.fundName },
      intl.formatMessage(VotingInfo_messages_1.messages.fundName, {
        votingFundNumber: fundInfo.current.number,
      })
    ),
    react_1.default.createElement(
      'div',
      { className: CurrentPhase_scss_1.default.block },
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.label },
        intl.formatMessage(VotingPhase_messages_1.messages.dateLabel)
      ),
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.value },
        startDate,
        ' \u2013 ',
        endDate
      )
    ),
    react_1.default.createElement('hr', {
      className: VotingInfo_scss_1.default.separator,
    }),
    react_1.default.createElement(
      'div',
      { className: CurrentPhase_scss_1.default.block },
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.value },
        intl.formatMessage(VotingPhase_messages_1.messages.instruction)
      )
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(VotingPhase)
);
//# sourceMappingURL=VotingPhase.js.map
