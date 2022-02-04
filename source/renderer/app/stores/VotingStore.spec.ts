import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import VotingStore, { FundPhases } from './VotingStore';
import type { CatalystFund } from '../api/voting/types';

const mockFundInfo: CatalystFund = {
  fundNumber: 7,
  nextFundNumber: 8,
  fundEndTime: new Date('Feb 3, 2022, 11:00 UTC'),
  fundStartTime: new Date('Jan 20, 2022, 11:00 UTC'),
  fundResults: new Date('Feb 10, 2022'),
  nextRegistrationSnapshotTime: new Date('Apr 7, 2022, 11:00 UTC'),
  registrationSnapshotTime: new Date('Jan 6, 2022, 11:00 UTC'),
  nextFundStartTime: new Date('Jan 6, 2022, 11:00 UTC'),
};

describe('VotingStore', () => {
  const api: Api = {
    ada: jest.fn(),
  } as any;
  const actions: ActionsMap = jest.fn() as any;
  const cases = [
    [
      new Date(mockFundInfo.registrationSnapshotTime.getTime() - 60000),
      FundPhases.SNAPSHOT,
    ],
    [mockFundInfo.registrationSnapshotTime, FundPhases.SNAPSHOT],
    [
      new Date(mockFundInfo.fundStartTime.getTime() - 60000),
      FundPhases.SNAPSHOT,
    ],
    [mockFundInfo.fundStartTime, FundPhases.VOTING],
    [new Date(mockFundInfo.fundEndTime.getTime() - 60000), FundPhases.VOTING],
    [mockFundInfo.fundEndTime, FundPhases.TALLYING],
    [new Date(mockFundInfo.fundResults.getTime() - 60000), FundPhases.TALLYING],
    [mockFundInfo.fundResults, FundPhases.RESULTS],
  ];
  const votingStore = new VotingStore(api, actions);

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
