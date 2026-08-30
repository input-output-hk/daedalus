import {
  DAPP_POLICY_REVISION,
  DappLaunchPolicy,
} from '../dapp/DappLaunchPolicy';
import { CapabilityContext, CapabilityService } from './CapabilityService';
import { ExtensionRegistry } from './ExtensionRegistry';
import { Negotiator } from './Negotiator';
import {
  BASE_DESCRIPTOR,
  EXTENSION_DESCRIPTORS,
  ExtensionDescriptor,
} from './extensions';

const policy = (cip104Revision = 0, cip142Revision = 0) =>
  new DappLaunchPolicy({
    revision: DAPP_POLICY_REVISION,
    globalEnabled: true,
    preferredCatalogEnabled: true,
    diagnosticsEnabled: true,
    cip104Revision,
    cip142Revision,
  });

const context = (
  overrides: Partial<CapabilityContext> = {}
): CapabilityContext => ({
  walletKind: 'shelley-software',
  backendApiVersion: 1,
  backendExtensions: [95, 103, 104],
  networkSupported: true,
  policy: policy(),
  ...overrides,
});

const replace = (
  cip: number,
  changes: Partial<ExtensionDescriptor>
): readonly ExtensionDescriptor[] =>
  EXTENSION_DESCRIPTORS.map((descriptor) =>
    descriptor.cip === cip ? { ...descriptor, ...changes } : descriptor
  );

const thrownBy = (call: () => unknown): unknown => {
  try {
    call();
  } catch (error) {
    return error;
  }
  throw new Error('Expected call to throw');
};

describe('CIP-30 extension engine', () => {
  it('rejects invalid descriptors at startup', () => {
    expect(
      () =>
        new ExtensionRegistry(
          BASE_DESCRIPTOR,
          replace(103, {
            methods: EXTENSION_DESCRIPTORS[0].methods,
          })
        )
    ).toThrow('Duplicate CIP-30 method');

    expect(
      () =>
        new ExtensionRegistry(
          BASE_DESCRIPTOR,
          replace(103, {
            dependencies: [95],
          }).map((descriptor) =>
            descriptor.cip === 95
              ? { ...descriptor, dependencies: [103] }
              : descriptor
          )
        )
    ).toThrow('dependency cycle');

    expect(
      () =>
        new ExtensionRegistry(
          BASE_DESCRIPTOR,
          EXTENSION_DESCRIPTORS,
          (method) => method !== 'api.cip142.getNetworkMagic'
        )
    ).toThrow('Invalid CIP-30 method descriptor');

    expect(
      () =>
        new ExtensionRegistry(
          BASE_DESCRIPTOR,
          replace(142, {
            methods: [
              {
                path: 'api.cip142.getNetworkMagic',
                scopes: undefined,
              } as never,
            ],
          })
        )
    ).toThrow('Invalid CIP-30 method descriptor');

    expect(
      () =>
        new ExtensionRegistry(
          BASE_DESCRIPTOR,
          replace(142, {
            baseOverrides: EXTENSION_DESCRIPTORS[0].methods.slice(0, 1),
          })
        )
    ).toThrow('Undeclared base override');
  });

  it('keeps known, supported, and enabled states distinct', () => {
    const registry = new ExtensionRegistry();
    const capabilities = new CapabilityService(registry);
    const negotiator = new Negotiator(registry, capabilities);

    expect(registry.isKnown(142)).toBe(true);
    expect(registry.isKnown(8)).toBe(false);
    expect(registry.isKnown(106)).toBe(false);
    expect(registry.isKnown(141)).toBe(false);
    expect(capabilities.isSupported(142, context())).toBe(false);
    expect(capabilities.isEnabled(142, [])).toBe(false);

    const supported = context({ policy: policy(0, 1) });
    expect(capabilities.isSupported(142, supported)).toBe(true);
    expect(negotiator.negotiate({}, supported).enabledExtensions).toEqual([]);
    expect(
      negotiator.negotiate({ extensions: [{ cip: 142 }] }, supported)
        .enabledExtensions
    ).toEqual([{ cip: 142 }]);
  });

  it('composes CIP-103 with the effective CIP-95 base signer', () => {
    const registry = new ExtensionRegistry();
    const capabilities = new CapabilityService(registry);
    const negotiated = new Negotiator(registry, capabilities).negotiate(
      { extensions: [{ cip: 103 }, { cip: 95 }, { cip: 103 }] },
      context()
    );
    const enabled = negotiated.enabledExtensions.map(({ cip }) => cip);

    expect(enabled).toEqual([95, 103]);
    expect(registry.resolve('api.signTx', enabled)?.override).toBe(95);
    expect(
      registry.compositionTarget('api.cip103.signTxs', enabled)?.override
    ).toBe(95);
  });

  it('rechecks Proposed policy and exact hardware evidence at invocation', () => {
    const registry = new ExtensionRegistry();
    const capabilities = new CapabilityService(registry);
    const negotiator = new Negotiator(registry, capabilities);
    const enabledPolicy = context({ policy: policy(1, 1) });

    expect(negotiator.supported(enabledPolicy)).toEqual([
      { cip: 95 },
      { cip: 103 },
      { cip: 142 },
    ]);
    expect(capabilities.isSupported(104, enabledPolicy)).toBe(false);
    expect(
      negotiator.negotiate(
        { extensions: [{ cip: 104 }, { cip: 142 }] },
        enabledPolicy
      ).enabledExtensions
    ).toEqual([{ cip: 142 }]);
    expect(
      thrownBy(() =>
        capabilities.requireInvocation(
          'api.cip104.getAccountPub',
          [104],
          enabledPolicy
        )
      )
    ).toEqual({ code: -3, info: 'Refused' });
    expect(
      capabilities.requireInvocation(
        'api.cip142.getNetworkMagic',
        [142],
        enabledPolicy
      ).extension
    ).toBe(142);
    expect(
      thrownBy(() =>
        capabilities.requireInvocation(
          'api.cip142.getNetworkMagic',
          [142],
          context()
        )
      )
    ).toEqual({ code: -3, info: 'Refused' });

    expect(
      capabilities.isSupported(
        95,
        context({ walletKind: 'ledger', device: undefined })
      )
    ).toBe(false);
    expect(
      capabilities.isSupported(
        95,
        context({
          walletKind: 'ledger',
          device: {
            matrixRevision: 'task-006-matrix-2026-08-14',
            rowId: 'ledger:nanoSP:8.0.0:signData',
            vendor: 'ledger',
            model: 'nanoSP',
            appVersion: '8.0.0',
            certifiedExtensions: [95],
            physicalCertified: true,
            packagedEnabled: true,
          },
        })
      )
    ).toBe(true);
  });

  it('validates enable options before consulting capabilities', () => {
    const registry = new ExtensionRegistry();
    const capabilities = new CapabilityService(registry);
    const spy = jest.spyOn(capabilities, 'isSupported');

    expect(
      thrownBy(() =>
        new Negotiator(registry, capabilities).negotiate(
          { extensions: [{ cip: '142' }] },
          context()
        )
      )
    ).toEqual({ code: -1, info: 'Invalid request' });
    expect(spy).not.toHaveBeenCalled();
  });
});
