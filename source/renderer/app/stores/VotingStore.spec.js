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
Object.defineProperty(exports, '__esModule', { value: true });
const VotingStore_1 = __importStar(require('./VotingStore'));
const analytics_1 = require('../analytics');
const mockFundInfo = {
  current: {
    startTime: new Date('Jan 20, 2022, 11:00 UTC'),
    endTime: new Date('Feb 3, 2022, 11:00 UTC'),
    resultsTime: new Date('Feb 10, 2022'),
    registrationSnapshotTime: new Date('Jan 6, 2022, 11:00 UTC'),
  },
};
describe('VotingStore', () => {
  const api = {
    ada: jest.fn(),
  };
  const actions = jest.fn();
  const cases = [
    [undefined, null],
    [
      new Date(mockFundInfo.current.registrationSnapshotTime.getTime() - 60000),
      VotingStore_1.FundPhase.SNAPSHOT,
    ],
    [
      mockFundInfo.current.registrationSnapshotTime,
      VotingStore_1.FundPhase.SNAPSHOT,
    ],
    [
      new Date(mockFundInfo.current.startTime.getTime() - 60000),
      VotingStore_1.FundPhase.SNAPSHOT,
    ],
    [mockFundInfo.current.startTime, VotingStore_1.FundPhase.VOTING],
    [
      new Date(mockFundInfo.current.endTime.getTime() - 60000),
      VotingStore_1.FundPhase.VOTING,
    ],
    [mockFundInfo.current.endTime, VotingStore_1.FundPhase.TALLYING],
    [
      new Date(mockFundInfo.current.resultsTime.getTime() - 60000),
      VotingStore_1.FundPhase.TALLYING,
    ],
    [mockFundInfo.current.resultsTime, VotingStore_1.FundPhase.RESULTS],
  ];
  const votingStore = new VotingStore_1.default(
    api,
    actions,
    analytics_1.noopAnalyticsTracker
  );
  beforeAll(() => {
    votingStore.catalystFund = mockFundInfo;
  });
  test.each(cases)(
    `should have correct fund phase for date %s - %s phase`,
    (date, expected) => {
      votingStore._checkFundPhase(date);
      expect(votingStore.fundPhase).toEqual(expected);
    }
  );
});
//# sourceMappingURL=VotingStore.spec.js.map
