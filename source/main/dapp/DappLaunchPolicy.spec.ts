import { DAPP_POLICY_REVISION, DappLaunchPolicy } from './DappLaunchPolicy';

const enabled = {
  revision: DAPP_POLICY_REVISION,
  globalEnabled: true,
  preferredCatalogEnabled: true,
  diagnosticsEnabled: true,
  cip104Revision: 3,
  cip142Revision: 2,
};

describe('DappLaunchPolicy', () => {
  it.each([
    undefined,
    null,
    {},
    { ...enabled, revision: 2 },
    { ...enabled, globalEnabled: 'true' },
  ])('fails closed for malformed launcher policy %#', (value) => {
    const policy = new DappLaunchPolicy(value);
    expect(policy.allows('preferred')).toBe(false);
    expect(policy.allows('diagnostics')).toBe(false);
    expect(policy.extensionRevision(104)).toBe(0);
    expect(policy.extensionRevision(142)).toBe(0);
  });

  it('enforces global, preferred, and diagnostics switches independently', () => {
    expect(
      new DappLaunchPolicy({ ...enabled, globalEnabled: false }).allows(
        'preferred'
      )
    ).toBe(false);
    expect(
      new DappLaunchPolicy({
        ...enabled,
        preferredCatalogEnabled: false,
      }).allows('diagnostics')
    ).toBe(true);
    expect(
      new DappLaunchPolicy({ ...enabled, diagnosticsEnabled: false }).allows(
        'preferred'
      )
    ).toBe(true);
  });

  it('exposes immutable revisioned CIP activation values', () => {
    const policy = new DappLaunchPolicy(enabled);
    expect(policy.extensionRevision(104)).toBe(3);
    expect(policy.extensionRevision(142)).toBe(2);
    expect(Object.isFrozen(policy.config)).toBe(true);
  });
});
