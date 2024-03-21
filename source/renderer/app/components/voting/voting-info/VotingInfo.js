'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const VotingInfo_scss_1 = __importDefault(require('./VotingInfo.scss'));
const ResultsPhase_1 = __importDefault(require('./ResultsPhase'));
const SnapshotPhase_1 = __importDefault(require('./SnapshotPhase'));
const VotingPhase_1 = __importDefault(require('./VotingPhase'));
const TallyingPhase_1 = __importDefault(require('./TallyingPhase'));
const Headline_1 = __importDefault(require('./Headline'));
const AppStore_1 = __importDefault(require('./AppStore'));
const RegisterToVote_1 = __importDefault(require('./RegisterToVote'));
const ApiError_1 = __importDefault(require('./ApiError'));
const VotingStore_1 = require('../../../stores/VotingStore');
const phaseToComponentMap = {
  [VotingStore_1.FundPhase.SNAPSHOT]: SnapshotPhase_1.default,
  [VotingStore_1.FundPhase.VOTING]: VotingPhase_1.default,
  [VotingStore_1.FundPhase.TALLYING]: TallyingPhase_1.default,
  [VotingStore_1.FundPhase.RESULTS]: ResultsPhase_1.default,
};
function VotingInfo({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  fundPhase,
  fundInfo,
  onRegisterToVoteClick,
  onExternalLinkClick,
}) {
  const PhaseComponent = phaseToComponentMap[fundPhase];
  return react_1.default.createElement(
    'div',
    { className: VotingInfo_scss_1.default.component },
    react_1.default.createElement(
      BorderedBox_1.default,
      null,
      react_1.default.createElement(Headline_1.default, {
        onExternalLinkClick: onExternalLinkClick,
      }),
      react_1.default.createElement('hr', {
        className: VotingInfo_scss_1.default.separator,
      }),
      react_1.default.createElement(
        'div',
        { className: VotingInfo_scss_1.default.bottomContent },
        fundPhase === null &&
          react_1.default.createElement(ApiError_1.default, null),
        fundPhase &&
          react_1.default.createElement(
            react_1.default.Fragment,
            null,
            react_1.default.createElement(
              'div',
              { className: VotingInfo_scss_1.default.leftContent },
              react_1.default.createElement(PhaseComponent, {
                fundInfo: fundInfo,
                currentLocale: currentLocale,
                currentDateFormat: currentDateFormat,
                currentTimeFormat: currentTimeFormat,
                onExternalLinkClick: onExternalLinkClick,
              }),
              react_1.default.createElement(
                'div',
                { className: VotingInfo_scss_1.default.appStoreSpacing },
                react_1.default.createElement(AppStore_1.default, {
                  onAppleStoreLinkClick: onExternalLinkClick,
                  onAndroidStoreLinkClick: onExternalLinkClick,
                })
              )
            ),
            react_1.default.createElement(
              'div',
              { className: VotingInfo_scss_1.default.rightContent },
              react_1.default.createElement(RegisterToVote_1.default, {
                onRegisterToVoteClick: onRegisterToVoteClick,
              })
            )
          )
      )
    )
  );
}
exports.default = (0, mobx_react_1.observer)(VotingInfo);
//# sourceMappingURL=VotingInfo.js.map
