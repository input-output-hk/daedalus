/**
 * @jest-environment node
 */
import { AnchorFetchErrorType } from '../../common/types/governance.types';

const mockChannels: Array<{ onRequest: jest.Mock }> = [];

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => {
    const channel = { onRequest: jest.fn() };
    mockChannels.push(channel);
    return channel;
  }),
}));

jest.mock('../governance/AnchorVerificationService', () => ({
  resolveVerifiedAnchor: jest.fn(),
}));

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));

const { resolveVerifiedAnchor } = jest.requireMock(
  '../governance/AnchorVerificationService'
);
const { logger } = jest.requireMock('../utils/logging');

const loadHandler = () => {
  mockChannels.length = 0;
  jest.isolateModules(() => {
    const {
      handleGovernanceAnchorRequests,
    } = require('./governanceAnchorChannel');
    handleGovernanceAnchorRequests();
  });
  return mockChannels[0].onRequest.mock.calls[0][0];
};

const anchor = {
  url: 'https://anchor.example.org/drep.jsonld',
  hash: '9e8cb2b0f4c2ddbd9dea316b44680d8a989743868aeb40c1e6959982452f38e1',
};

describe('governanceAnchorChannel', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('returns the verification result unchanged', async () => {
    const verified = {
      status: 'verified',
      content: { givenName: 'Daedalus Test DRep' },
      host: 'anchor.example.org',
      fetchedAt: 1,
    };
    resolveVerifiedAnchor.mockResolvedValue(verified);
    const handler = loadHandler();
    await expect(handler(anchor)).resolves.toEqual(verified);
    expect(resolveVerifiedAnchor).toHaveBeenCalledWith(anchor);
  });

  it('resolves as unavailable when the verification service throws', async () => {
    resolveVerifiedAnchor.mockRejectedValue(new Error('boom'));
    const handler = loadHandler();
    await expect(handler(anchor)).resolves.toEqual({
      status: 'unavailable',
      reason: AnchorFetchErrorType.InvalidRequest,
    });
    const payloads = JSON.stringify([
      logger.debug.mock.calls,
      logger.info.mock.calls,
      logger.error.mock.calls,
      logger.warn.mock.calls,
    ]);
    expect(payloads).not.toContain('boom');
  });
});
