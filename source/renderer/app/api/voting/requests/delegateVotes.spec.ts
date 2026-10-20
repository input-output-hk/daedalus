import { delegateVotes } from './delegateVotes';
import { request } from '../../utils/request';

jest.mock('../../utils/request', () => ({ request: jest.fn() }));

const requestMock = request as jest.MockedFunction<typeof request>;

describe('upstream governance request contract', () => {
  afterEach(() => jest.clearAllMocks());

  it.each([
    'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy',
    'abstain',
    'no_confidence',
  ])(
    'preserves the vote bytes for %s and sends only the password body',
    async (dRepId) => {
      const config = {
        hostname: 'localhost',
        port: 8090,
        ca: new Uint8Array(),
        cert: new Uint8Array(),
        key: new Uint8Array(),
      };
      await delegateVotes(config, {
        walletId: 'a'.repeat(40),
        dRepId,
        passphrase: 'collected-password',
      });
      expect(requestMock).toHaveBeenCalledWith(
        {
          ...config,
          method: 'PUT',
          path: `/v2/dreps/${dRepId}/wallets/${'a'.repeat(40)}`,
        },
        {},
        { passphrase: 'collected-password' }
      );
    }
  );
});
