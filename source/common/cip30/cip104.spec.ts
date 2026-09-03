import { encodeCip104AccountPub } from './cip104';

const NEWM_ACCOUNT_XPUB =
  'acct_xvk10yq2v72lq0h7lnhkw308uy23fjq384zufvyesh6mlklnpmv048xs8arze4nws0xfp8h87d7jdxwgm5dsr7l0qruedrtcdudjlnxls3sgsdluv';
const RAW_ACCOUNT_XPUB =
  '7900a6795f03efefcef6745e7e11514c8113d45c4b09985f5bfdbf30ed8fa9cd03f462cd66e83cc909ee7f37d2699c8dd1b01fbef00f9968d786f1b2fccdf846';

describe('CIP-104 account public key encoding', () => {
  it('encodes the newm-chain account xpub payload as a CBOR byte string', () => {
    expect(encodeCip104AccountPub(NEWM_ACCOUNT_XPUB)).toBe(
      `5840${RAW_ACCOUNT_XPUB}`
    );
  });

  it.each([NEWM_ACCOUNT_XPUB.replace('acct_xvk', 'xpub'), 'acct_xvk1invalid'])(
    'rejects an invalid backend account key',
    (value) => {
      expect(() => encodeCip104AccountPub(value)).toThrow();
    }
  );
});
