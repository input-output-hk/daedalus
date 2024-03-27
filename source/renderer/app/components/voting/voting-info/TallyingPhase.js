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
const CurrentPhase_scss_1 = __importDefault(require('./CurrentPhase.scss'));
const TallyingPhase_messages_1 = require('./TallyingPhase.messages');
const VotingInfo_messages_1 = require('./VotingInfo.messages');
function TallyingPhase({
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
  const endDated = (0, formatters_1.formattedDateTime)(
    fundInfo.current.endTime,
    {
      currentLocale,
      currentDateFormat: mappedFormats.currentDateFormat,
    }
  );
  const resultsDate = (0, formatters_1.formattedDateTime)(
    fundInfo.current.resultsTime,
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
        intl.formatMessage(TallyingPhase_messages_1.messages.endDateLabel)
      ),
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.value },
        endDated
      )
    ),
    react_1.default.createElement(
      'div',
      { className: CurrentPhase_scss_1.default.block },
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.label },
        intl.formatMessage(TallyingPhase_messages_1.messages.resultsLabel)
      ),
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.value },
        resultsDate
      )
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(TallyingPhase)
);
//# sourceMappingURL=TallyingPhase.js.map
