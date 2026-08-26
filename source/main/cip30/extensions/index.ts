import { DappCip30Method } from '../../../common/cip30/wire';
import { DappScope } from '../../../common/types/dapp.types';

export type MethodDescriptor = Readonly<{
  path: DappCip30Method;
  scopes: readonly DappScope[];
  composesWith?: DappCip30Method;
}>;

export type BaseDescriptor = Readonly<{
  methods: readonly MethodDescriptor[];
}>;

export type ExtensionDescriptor = Readonly<{
  cip: number;
  status: 'active' | 'proposed';
  namespace: string;
  dependencies: readonly number[];
  incompatibleWith: readonly number[];
  scopes: readonly DappScope[];
  methods: readonly MethodDescriptor[];
  baseOverrides: readonly MethodDescriptor[];
  availability: 'available' | 'disabled';
  requiresBackend: boolean;
  requiresDevice: boolean;
  policyRevision?: 1;
}>;

const method = (
  path: DappCip30Method,
  ...scopes: DappScope[]
): MethodDescriptor => ({ path, scopes });

export const BASE_DESCRIPTOR: BaseDescriptor = Object.freeze({
  methods: [
    method('provider.isEnabled'),
    method('provider.enable', 'connection'),
    method('api.getExtensions', 'connection'),
    method('api.getNetworkId', 'read'),
    method('api.getUtxos', 'read'),
    method('api.getCollateral', 'read'),
    method('api.getBalance', 'read'),
    method('api.getUsedAddresses', 'read'),
    method('api.getUnusedAddresses', 'read'),
    method('api.getChangeAddress', 'read'),
    method('api.getRewardAddresses', 'read'),
    method('api.signTx', 'transaction-signing'),
    method('api.signData', 'data-signing'),
    method('api.submitTx', 'transaction-submission'),
  ],
});

export const EXTENSION_DESCRIPTORS: readonly ExtensionDescriptor[] = Object.freeze(
  [
    {
      cip: 95,
      status: 'active',
      namespace: 'cip95',
      dependencies: [],
      incompatibleWith: [],
      scopes: ['governance-key-disclosure'],
      methods: [
        method('api.cip95.getPubDRepKey', 'governance-key-disclosure'),
        method(
          'api.cip95.getRegisteredPubStakeKeys',
          'governance-key-disclosure'
        ),
        method(
          'api.cip95.getUnregisteredPubStakeKeys',
          'governance-key-disclosure'
        ),
        method('api.cip95.signData', 'governance-data-signing'),
      ],
      baseOverrides: [method('api.signTx', 'governance-transaction-signing')],
      availability: 'available',
      requiresBackend: true,
      requiresDevice: true,
    },
    {
      cip: 103,
      status: 'active',
      namespace: 'cip103',
      dependencies: [],
      incompatibleWith: [],
      scopes: [],
      methods: [
        {
          ...method('api.cip103.signTxs', 'transaction-signing'),
          composesWith: 'api.signTx',
        },
        method('api.cip103.submitTxs', 'transaction-submission'),
      ],
      baseOverrides: [],
      availability: 'available',
      requiresBackend: true,
      requiresDevice: true,
    },
    {
      cip: 104,
      status: 'proposed',
      namespace: 'cip104',
      dependencies: [],
      incompatibleWith: [],
      scopes: ['account-public-key-disclosure'],
      methods: [
        method('api.cip104.getAccountPub', 'account-public-key-disclosure'),
      ],
      baseOverrides: [],
      availability: 'disabled',
      requiresBackend: true,
      requiresDevice: true,
      policyRevision: 1,
    },
    {
      cip: 142,
      status: 'proposed',
      namespace: 'cip142',
      dependencies: [],
      incompatibleWith: [],
      scopes: [],
      methods: [method('api.cip142.getNetworkMagic', 'read')],
      baseOverrides: [],
      availability: 'available',
      requiresBackend: false,
      requiresDevice: false,
      policyRevision: 1,
    },
  ]
);
