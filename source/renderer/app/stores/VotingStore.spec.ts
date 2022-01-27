import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import VotingStore, { FundPhases } from './VotingStore';
import {
  VOTING_CAST_END_DATE,
  VOTING_CAST_START_DATE,
  VOTING_RESULTS_DATE,
  VOTING_SNAPSHOT_DATE,
} from '../config/votingConfig';

describe('VotingStore', () => {
  const api: Api = {
    ada: jest.fn(),
  } as any;
  const actions: ActionsMap = jest.fn() as any;
  const cases = [
    [new Date(VOTING_SNAPSHOT_DATE.getTime() - 60000), FundPhases.SNAPSHOT],
    [VOTING_SNAPSHOT_DATE, FundPhases.SNAPSHOT],
    [new Date(VOTING_CAST_START_DATE.getTime() - 60000), FundPhases.SNAPSHOT],
    [VOTING_CAST_START_DATE, FundPhases.VOTING],
    [new Date(VOTING_CAST_END_DATE.getTime() - 60000), FundPhases.VOTING],
    [VOTING_CAST_END_DATE, FundPhases.TALLYING],
    [new Date(VOTING_RESULTS_DATE.getTime() - 60000), FundPhases.TALLYING],
    [VOTING_RESULTS_DATE, FundPhases.RESULTS],
  ];
  test.each(cases)(
    `should have correct fund phase for date %s - %s phase`,
    (date, expected) => {
      const votingStore = new VotingStore(api, actions);

      votingStore._checkFundPhase(date);

      expect(votingStore.fundPhase).toEqual(expected);
    }
  );
});
