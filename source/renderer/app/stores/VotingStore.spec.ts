import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import VotingStore, { FundPhase } from './VotingStore';
import type { CatalystFund } from '../api/voting/types';
import { noopAnalyticsTracker } from '../analytics';

const mockFundInfo = {
  current: {
    startTime: new Date('Jan 20, 2022, 11:00 UTC'),
    endTime: new Date('Feb 3, 2022, 11:00 UTC'),
    resultsTime: new Date('Feb 10, 2022'),
    registrationSnapshotTime: new Date('Jan 6, 2022, 11:00 UTC'),
  },
};

describe('VotingStore', () => {
  const api: Api = {
    ada: jest.fn(),
  } as any;
  const actions: ActionsMap = jest.fn() as any;

  const cases = [
    [undefined, null],
    [
      new Date(mockFundInfo.current.registrationSnapshotTime.getTime() - 60000),
      FundPhase.SNAPSHOT,
    ],
    [mockFundInfo.current.registrationSnapshotTime, FundPhase.SNAPSHOT],
    [
      new Date(mockFundInfo.current.startTime.getTime() - 60000),
      FundPhase.SNAPSHOT,
    ],
    [mockFundInfo.current.startTime, FundPhase.VOTING],
    [
      new Date(mockFundInfo.current.endTime.getTime() - 60000),
      FundPhase.VOTING,
    ],
    [mockFundInfo.current.endTime, FundPhase.TALLYING],
    [
      new Date(mockFundInfo.current.resultsTime.getTime() - 60000),
      FundPhase.TALLYING,
    ],
    [mockFundInfo.current.resultsTime, FundPhase.RESULTS],
  ];
  const votingStore = new VotingStore(api, actions, noopAnalyticsTracker);

  beforeAll(() => {
    votingStore.catalystFund = mockFundInfo as CatalystFund;
  });

  test.each(cases)(
    `should have correct fund phase for date %s - %s phase`,
    (date: Date, expected: FundPhase) => {
      votingStore._checkFundPhase(date);
      expect(votingStore.fundPhase).toEqual(expected);
    }
  );
});
