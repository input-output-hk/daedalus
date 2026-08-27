import cbor from 'cbor';

import { parseDappCip30GatewayRequest } from '../../common/cip30/schemas';
import wireFixtures from '../../common/cip30/contracts/fixtures/wire-fixtures.json';
import * as transactionContext from '../../common/cardano/transactionContext';
import type {
  ContextOutput,
  TransactionContextSnapshot,
} from '../../common/cardano/transactionContext';
import { CapabilityService } from './CapabilityService';
import { Cip30DispatchRejection, Dispatcher } from './Dispatcher';
import type { Cip30DispatchAuthority } from './Dispatcher';
import { ExtensionRegistry } from './ExtensionRegistry';
import { SessionStore } from './SessionStore';
import { DappLaunchPolicy } from '../dapp/DappLaunchPolicy';

jest.mock('../../common/cardano/transactionContext', () => ({
  ...(jest.requireActual('../../common/cardano/transactionContext') as object),
  reconcileTransactionContext: jest.fn(),
}));

const address = wireFixtures.addresses.find(
  ({ name }) => name === 'mainnet-enterprise-matching-drep'
)!;
const input = [Buffer.alloc(32, 1), 0];
const output = cbor.encodeCanonical([Buffer.from(address.raw, 'hex'), 5]);
const contextOutput: ContextOutput = {
  outpoint: { transactionId: '01'.repeat(32), index: 0 },
  sourceCbor: output.toString('hex'),
  inputCbor: cbor.encodeCanonical(input).toString('hex'),
  canonicalCbor: output.toString('hex'),
  unspentCbor: cbor
    .encodeCanonical([input, cbor.decodeFirstSync(output)])
    .toString('hex'),
  provenance: ['node'],
  roles: ['wallet_snapshot'],
  walletMember: true,
  pendingState: 'none',
};
const network = {
  networkId: 1 as const,
  networkMagic: 764824073,
  genesisHash: '11'.repeat(32),
};
const snapshot = ({
  walletId: 'wallet',
  network,
  outputs: [contextOutput],
  maxCollateralInputs: 3,
} as unknown) as TransactionContextSnapshot;
const authority: Cip30DispatchAuthority = {
  guestWebContentsId: 9,
  documentGeneration: 2,
  origin: 'https://dapp.test',
  launch: {
    kind: 'catalog',
    catalogEntryId: 'dex',
    catalogEntryIdentity: 'identity',
  },
  walletId: 'wallet',
  routeEpoch: 4,
  network,
};
const capabilityContext = {
  walletKind: 'shelley-software' as const,
  backendApiVersion: 1,
  backendExtensions: [95, 103],
  networkSupported: true,
  policy: new DappLaunchPolicy({
    revision: 1,
    globalEnabled: true,
    preferredCatalogEnabled: true,
    diagnosticsEnabled: true,
    cip104Revision: 0,
    cip142Revision: 0,
  }),
};

const create = (
  enabledExtensions: number[] = [],
  elevated = enabledExtensions.includes(95)
) => {
  (transactionContext.reconcileTransactionContext as jest.Mock).mockReturnValue(
    snapshot
  );
  const registry = new ExtensionRegistry();
  const capabilities = new CapabilityService(registry);
  const sessions = new SessionStore();
  sessions.create({
    guestWebContentsId: authority.guestWebContentsId,
    documentGeneration: authority.documentGeneration,
    dappId: 'dex',
    origin: authority.origin,
    connectionId: 'connection',
    walletId: authority.walletId,
    routeEpoch: authority.routeEpoch,
    networkId: network.networkId,
    networkMagic: network.networkMagic,
    networkGenesis: network.genesisHash,
    enabledExtensions,
    grantedScopes: [
      'connection',
      'read',
      'transaction-signing',
      'data-signing',
      'transaction-submission',
      ...(elevated ? (['governance-key-disclosure'] as const) : []),
    ],
  });
  const execute = jest.fn(async (operation) => {
    if (operation === 'context')
      return {
        status: 'fulfilled' as const,
        operation: 'context' as const,
        value: {},
      };
    if (operation === 'cip95-key-state')
      return {
        status: 'fulfilled' as const,
        operation: 'cip95-key-state' as const,
        value: {
          drep_public_key: '33'.repeat(32),
          registered_stake_public_keys: ['44'.repeat(32)],
          unregistered_stake_public_keys: ['55'.repeat(32)],
        },
      };
    return {
      status: 'fulfilled' as const,
      operation: 'addresses' as const,
      value: {
        walletId: 'wallet',
        network,
        used: [address.raw],
        unused: [address.raw],
        change: address.raw,
        reward: [
          wireFixtures.addresses.find(
            ({ name }) => name === 'mainnet-reward-key'
          )!.raw,
        ],
      },
    };
  });
  return { dispatcher: new Dispatcher(capabilities, sessions), execute };
};

describe('CIP-30 Dispatcher', () => {
  afterEach(() =>
    (transactionContext.reconcileTransactionContext as jest.Mock).mockReset()
  );
  afterEach(() => jest.restoreAllMocks());

  it('returns task-305 exact UTxO, balance, and collateral encodings', async () => {
    const { dispatcher, execute } = create();
    await expect(
      dispatcher.dispatch(
        parseDappCip30GatewayRequest({ method: 'api.getUtxos', args: [] }),
        authority,
        capabilityContext,
        execute
      )
    ).resolves.toEqual([contextOutput.unspentCbor]);
    await expect(
      dispatcher.dispatch(
        parseDappCip30GatewayRequest({ method: 'api.getBalance', args: [] }),
        authority,
        capabilityContext,
        execute
      )
    ).resolves.toBe('05');
    await expect(
      dispatcher.dispatch(
        parseDappCip30GatewayRequest({
          method: 'api.getCollateral',
          args: [{ amount: '01' }],
        }),
        authority,
        capabilityContext,
        execute
      )
    ).resolves.toEqual([contextOutput.unspentCbor]);
  });

  it('normalizes address results and returns typed pagination failure', async () => {
    const { dispatcher, execute } = create();
    await expect(
      dispatcher.dispatch(
        parseDappCip30GatewayRequest({
          method: 'api.getUsedAddresses',
          args: [{ page: 0, limit: 1 }],
        }),
        authority,
        capabilityContext,
        execute
      )
    ).resolves.toEqual([address.raw]);

    const error = await dispatcher
      .dispatch(
        parseDappCip30GatewayRequest({
          method: 'api.getUsedAddresses',
          args: [{ page: 2, limit: 1 }],
        }),
        authority,
        capabilityContext,
        execute
      )
      .catch((value) => value);
    expect(error).toBeInstanceOf(Cip30DispatchRejection);
    expect((error as Cip30DispatchRejection).rejection).toEqual({
      type: 'paginate-error',
      value: { maxSize: 1 },
    });
  });

  it('returns exact CIP-95 keys only with negotiated disclosure authority', async () => {
    const enabled = create([95]);
    for (const [method, expected] of [
      ['api.cip95.getPubDRepKey', '33'.repeat(32)],
      ['api.cip95.getRegisteredPubStakeKeys', ['44'.repeat(32)]],
      ['api.cip95.getUnregisteredPubStakeKeys', ['55'.repeat(32)]],
    ] as const)
      await expect(
        enabled.dispatcher.dispatch(
          parseDappCip30GatewayRequest({ method, args: [] }),
          authority,
          capabilityContext,
          enabled.execute
        )
      ).resolves.toEqual(expected);
    expect(enabled.execute).toHaveBeenCalledTimes(3);
    expect(enabled.execute).toHaveBeenCalledWith('cip95-key-state');

    for (const fixture of [create(), create([95], false)]) {
      const error = await fixture.dispatcher
        .dispatch(
          parseDappCip30GatewayRequest({
            method: 'api.cip95.getPubDRepKey',
            args: [],
          }),
          authority,
          capabilityContext,
          fixture.execute
        )
        .catch((value) => value);
      expect(error).toBeInstanceOf(Cip30DispatchRejection);
      expect((error as Cip30DispatchRejection).rejection).toEqual({
        type: 'api-error',
        value: { code: -3, info: 'Refused' },
      });
      expect(fixture.execute).not.toHaveBeenCalled();
    }
  });

  it('returns configured CIP-142 magic only for negotiated policy-enabled sessions', async () => {
    const enabledContext = {
      ...capabilityContext,
      policy: new DappLaunchPolicy({
        revision: 1,
        globalEnabled: true,
        preferredCatalogEnabled: true,
        diagnosticsEnabled: true,
        cip104Revision: 0,
        cip142Revision: 1,
      }),
    };
    const enabled = create([142]);
    await expect(
      enabled.dispatcher.dispatch(
        parseDappCip30GatewayRequest({
          method: 'api.cip142.getNetworkMagic',
          args: [],
        }),
        authority,
        enabledContext,
        enabled.execute
      )
    ).resolves.toBe(764824073);
    await expect(
      enabled.dispatcher.dispatch(
        parseDappCip30GatewayRequest({
          method: 'api.getNetworkId',
          args: [],
        }),
        authority,
        enabledContext,
        enabled.execute
      )
    ).resolves.toBe(1);
    expect(enabled.execute).not.toHaveBeenCalled();

    for (const [extensions, context] of [
      [[142], capabilityContext],
      [[], enabledContext],
    ] as const) {
      const fixture = create([...extensions]);
      const error = await fixture.dispatcher
        .dispatch(
          parseDappCip30GatewayRequest({
            method: 'api.cip142.getNetworkMagic',
            args: [],
          }),
          authority,
          context,
          fixture.execute
        )
        .catch((value) => value);
      expect(error).toBeInstanceOf(Cip30DispatchRejection);
      expect((error as Cip30DispatchRejection).rejection).toMatchObject({
        type: 'api-error',
        value: { code: -3 },
      });
      expect(fixture.execute).not.toHaveBeenCalled();
    }
  });

  it('rejects unnegotiated methods before executor access', async () => {
    const { dispatcher, execute } = create();
    const error = await dispatcher
      .dispatch(
        parseDappCip30GatewayRequest({
          method: 'api.cip95.getPubDRepKey',
          args: [],
        }),
        authority,
        capabilityContext,
        execute
      )
      .catch((value) => value);
    expect(error).toBeInstanceOf(Cip30DispatchRejection);
    expect((error as Cip30DispatchRejection).rejection).toMatchObject({
      type: 'api-error',
      value: { code: -3 },
    });
    expect(execute).not.toHaveBeenCalled();
  });
});
