import fs from 'fs';
import path from 'path';
import { randomUUID } from 'crypto';
import { ipcMain } from 'electron';
import type { IpcMainInvokeEvent } from 'electron';
import {
  createDappCip30FulfilledEnvelope,
  createDappCip30RejectedEnvelope,
  parseDappCip30GatewayRequest,
} from '../../common/cip30/schemas';
import {
  Cip8AddressNotPKError,
  Cip8Error,
  createCip8DataSignReview,
  prepareCip8Request,
  verifyCip8BackendResponse,
} from '../../common/cardano/cip8';
import type { ApiError, DappCip30Rejection } from '../../common/cip30/errors';
import type {
  Cip30WalletCapabilities,
  Cip30WalletNetwork,
  Cip30WalletOperation,
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../common/cip30/executor';
import { DAPP_CIP30_GATEWAY_CHANNEL } from '../../common/cip30/wire';
import type {
  DappCip30GatewayRequest,
  DappCip30Method,
} from '../../common/cip30/wire';
import type {
  DappCapability,
  DappGrant,
  DappScope,
} from '../../common/types/dapp.types';
import {
  dappLaunchPolicy,
  launcherConfig,
  stateDirectoryPath,
} from '../config';
import type { DappGuestAuthority } from '../dapp/DappBrowserManager';
import type { DappRouteLease } from '../dapp/DappRouteLease';
import {
  authenticateDappGuest,
  getCurrentDappRouteLease,
  setDappBrokerLifecycleRevoker,
} from '../ipc/dappBrowser';
import { consentCoordinator } from '../ipc/dappConsent';
import { executeCip30WalletRequest } from '../ipc/cip30Wallet';
import { CapabilityContext, CapabilityService } from './CapabilityService';
import { ConsentCoordinator } from './ConsentCoordinator';
import { Dispatcher, Cip30DispatchRejection } from './Dispatcher';
import type { Cip30DispatchAuthority } from './Dispatcher';
import { ExtensionRegistry } from './ExtensionRegistry';
import { GrantRepository } from './GrantRepository';
import { Negotiator } from './Negotiator';
import { SessionStore } from './SessionStore';
import { setCip30SessionRevoker } from './runtime';

export const CARDANO_WALLET_SOURCE_REVISION =
  '0cbd4618f5b3ac76bcee52c57a7cd6067a87408e';

const IMPLEMENTED_METHODS = new Set<DappCip30Method>([
  'api.getExtensions',
  'api.getNetworkId',
  'api.cip142.getNetworkMagic',
  'api.cip95.getPubDRepKey',
  'api.cip95.getRegisteredPubStakeKeys',
  'api.cip95.getUnregisteredPubStakeKeys',
  'api.getUtxos',
  'api.getCollateral',
  'api.getBalance',
  'api.getUsedAddresses',
  'api.getUnusedAddresses',
  'api.getChangeAddress',
  'api.getRewardAddresses',
]);
const PERSISTED_SCOPES = new Set<DappScope>([
  'connection',
  'read',
  'governance-key-disclosure',
  'account-public-key-disclosure',
]);
const LIVE_BASE_SCOPES: readonly DappScope[] = [
  'connection',
  'read',
  'transaction-signing',
  'data-signing',
  'transaction-submission',
];

const dataSignRejection = (
  code: 1 | 2 | 3,
  info: string
): DappCip30Rejection => ({
  type: 'data-sign-error',
  value: { code, info },
});

const invalidRequestRejection = (): DappCip30Rejection => ({
  type: 'api-error',
  value: { code: -1, info: 'Invalid request' },
});
const refusal = (): DappCip30Rejection => ({
  type: 'api-error',
  value: { code: -3, info: 'Refused' },
});
const internal = (): DappCip30Rejection => ({
  type: 'api-error',
  value: { code: -2, info: 'Internal error' },
});
const accountChange = (): DappCip30Rejection => ({
  type: 'api-error',
  value: { code: -4, info: 'Account changed' },
});

const isApiError = (value: unknown): value is ApiError => {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return false;
  const error = value as Record<string, unknown>;
  return (
    Object.keys(error).sort().join(',') === 'code,info' &&
    [-1, -2, -3, -4].includes(error.code as number) &&
    typeof error.info === 'string'
  );
};
const isRejection = (value: unknown): value is DappCip30Rejection =>
  !!value &&
  typeof value === 'object' &&
  typeof (value as { type?: unknown }).type === 'string' &&
  Object.prototype.hasOwnProperty.call(value, 'value');

export type Cip30BrokerBinding = Readonly<{
  guest: DappGuestAuthority;
  lease: DappRouteLease;
  authority: Cip30DispatchAuthority;
}>;

export type Cip30BrokerOptions = Readonly<{
  authenticate: (event: IpcMainInvokeEvent) => DappGuestAuthority | null;
  currentLease: () => DappRouteLease | null;
  executeWallet: (request: Cip30WalletRequest) => Promise<Cip30WalletResponse>;
  consent: ConsentCoordinator;
  grants: GrantRepository;
  sessions: SessionStore;
  registry: ExtensionRegistry;
  capabilities: CapabilityService;
  negotiator: Negotiator;
  dispatcher: Dispatcher;
  network: Cip30WalletNetwork;
  networkName: string;
  sourceRevision: string;
  now?: () => Date;
  connectionId?: () => string;
}>;

export class Cip30Broker {
  private readonly options: Cip30BrokerOptions;

  constructor(options: Cip30BrokerOptions) {
    this.options = options;
  }

  revoke(): void {
    this.options.sessions.revokeAll();
  }

  private binding(event: IpcMainInvokeEvent): Cip30BrokerBinding | null {
    const guest = this.options.authenticate(event);
    const lease = this.options.currentLease();
    if (
      !guest ||
      !lease ||
      lease.networkGenesis !== this.options.network.genesisHash
    )
      return null;
    return Object.freeze({
      guest,
      lease,
      authority: Object.freeze({
        guestWebContentsId: guest.guestWebContentsId,
        documentGeneration: guest.documentGeneration,
        origin: guest.origin,
        launch: guest.launch,
        walletId: lease.walletId,
        routeEpoch: lease.routeEpoch,
        network: Object.freeze({
          networkId: this.options.network.networkId,
          networkMagic: this.options.network.networkMagic,
          genesisHash: this.options.network.genesisHash,
        }),
      }),
    });
  }

  private assertCurrent(binding: Cip30BrokerBinding): void {
    const lease = this.options.currentLease();
    if (
      !binding.guest.isCurrent() ||
      !lease ||
      lease.walletId !== binding.lease.walletId ||
      lease.routeEpoch !== binding.lease.routeEpoch ||
      lease.networkGenesis !== binding.lease.networkGenesis
    )
      throw accountChange();
  }

  private request(
    binding: Cip30BrokerBinding,
    operation: Exclude<Cip30WalletOperation, 'sign-data'>
  ): Cip30WalletRequest {
    return Object.freeze({
      operation,
      walletId: binding.authority.walletId,
      network: binding.authority.network,
      sourceRevision: this.options.sourceRevision,
    });
  }

  private async executeWallet(
    binding: Cip30BrokerBinding,
    operation: Exclude<Cip30WalletOperation, 'sign-data'>
  ): Promise<Cip30WalletResponse> {
    this.assertCurrent(binding);
    const response = await this.options.executeWallet(
      this.request(binding, operation)
    );
    this.assertCurrent(binding);
    if (response.status === 'rejected') {
      if (response.reason === 'account-change') throw accountChange();
      throw internal();
    }
    return response;
  }

  private async capabilityEvidence(
    binding: Cip30BrokerBinding
  ): Promise<{
    evidence: Cip30WalletCapabilities;
    context: CapabilityContext;
  }> {
    const response = await this.executeWallet(binding, 'capabilities');
    if (
      response.status !== 'fulfilled' ||
      response.operation !== 'capabilities'
    )
      throw internal();
    const evidence = response.value;
    return {
      evidence,
      context: Object.freeze({
        walletKind: evidence.walletKind,
        backendApiVersion: evidence.backendApiVersion,
        backendExtensions: evidence.backendExtensions,
        networkSupported: true,
        policy: dappLaunchPolicy,
      }),
    };
  }

  private grantIdentity(binding: Cip30BrokerBinding) {
    return {
      origin: binding.authority.origin,
      walletId: binding.authority.walletId,
      networkGenesis: binding.authority.network.genesisHash,
      launch: binding.authority.launch,
    } as const;
  }

  private async enable(
    request: DappCip30GatewayRequest<'provider.enable'>,
    binding: Cip30BrokerBinding
  ): Promise<object> {
    const { evidence, context } = await this.capabilityEvidence(binding);
    if (!this.options.capabilities.isBaseSupported(context)) throw refusal();
    const negotiation = this.options.negotiator.negotiate(
      request.args[0],
      context
    );
    const enabledExtensions = negotiation.enabledExtensions.map(
      ({ cip }) => cip
    );
    const persistentScopes = new Set<DappScope>(['connection', 'read']);
    const liveScopes = new Set<DappScope>(LIVE_BASE_SCOPES);
    enabledExtensions.forEach((cip) => {
      const descriptor = this.options.registry.get(cip);
      descriptor?.scopes.forEach((scope) => {
        liveScopes.add(scope);
        if (PERSISTED_SCOPES.has(scope)) persistentScopes.add(scope);
      });
      descriptor?.methods.forEach(({ scopes }) =>
        scopes.forEach((scope) => liveScopes.add(scope))
      );
      descriptor?.baseOverrides.forEach(({ scopes }) =>
        scopes.forEach((scope) => liveScopes.add(scope))
      );
    });

    const identity = this.grantIdentity(binding);
    let grant = this.options.grants.find({
      ...identity,
      scopes: [...persistentScopes],
      extensions: enabledExtensions,
    });
    if (!grant) {
      const value: Omit<DappGrant, 'schemaVersion'> = {
        ...identity,
        networkMagic: binding.authority.network.networkMagic,
        readScopes: Object.freeze([...persistentScopes]),
        enabledExtensionScopes: Object.freeze(enabledExtensions),
        grantedAt: (this.options.now ?? (() => new Date()))().toISOString(),
      };
      grant = await this.options.consent.request({
        identity: {
          guestWebContentsId: binding.authority.guestWebContentsId,
          documentGeneration: binding.authority.documentGeneration,
          origin: binding.authority.origin,
          connectionId: '',
          walletId: binding.authority.walletId,
          routeEpoch: binding.authority.routeEpoch,
          networkGenesis: binding.authority.network.genesisHash,
        },
        presentation: {
          kind: persistentScopes.size > 2 ? 'key-disclosure' : 'connection',
          origin: binding.authority.origin,
          walletName: evidence.walletName,
          networkName: this.options.networkName,
          scopes: [...persistentScopes],
          extensions: enabledExtensions,
        },
        payload: value,
        declined: refusal(),
        execute: async (payload) => {
          this.assertCurrent(binding);
          const latest = await this.capabilityEvidence(binding);
          const latestExtensions = this.options.negotiator
            .negotiate(request.args[0], latest.context)
            .enabledExtensions.map(({ cip }) => cip);
          if (
            latestExtensions.length !== enabledExtensions.length ||
            latestExtensions.some(
              (cip, index) => cip !== enabledExtensions[index]
            )
          )
            throw refusal();
          return this.options.grants.put(
            payload as Omit<DappGrant, 'schemaVersion'>
          );
        },
      });
    }
    this.assertCurrent(binding);

    const capability: DappCapability = {
      guestWebContentsId: binding.authority.guestWebContentsId,
      documentGeneration: binding.authority.documentGeneration,
      ...(binding.authority.launch.kind === 'catalog'
        ? { dappId: binding.authority.launch.catalogEntryId }
        : {}),
      origin: binding.authority.origin,
      connectionId: (this.options.connectionId ?? randomUUID)(),
      walletId: binding.authority.walletId,
      routeEpoch: binding.authority.routeEpoch,
      networkId: binding.authority.network.networkId,
      networkMagic: binding.authority.network.networkMagic,
      networkGenesis: binding.authority.network.genesisHash,
      enabledExtensions: Object.freeze(enabledExtensions),
      grantedScopes: Object.freeze([...liveScopes]),
    };
    this.options.sessions.create(capability);
    return {};
  }

  private async signData(
    request: DappCip30GatewayRequest<'api.signData'>,
    binding: Cip30BrokerBinding
  ) {
    const { evidence, context } = await this.capabilityEvidence(binding);
    const capability = this.options.dispatcher.requireCapability(
      request.method,
      binding.authority,
      context
    );
    if (evidence.walletKind !== 'shelley-software')
      throw dataSignRejection(1, 'Proof generation failed');

    let expected: ReturnType<typeof prepareCip8Request>;
    try {
      expected = prepareCip8Request(request.args[0], request.args[1], {
        networkId: binding.authority.network.networkId,
      });
    } catch (error) {
      if (error instanceof Cip8AddressNotPKError)
        throw dataSignRejection(2, 'Address is not a public key');
      throw invalidRequestRejection();
    }
    const review = createCip8DataSignReview(expected);
    const result = await this.options.consent.request({
      identity: {
        guestWebContentsId: binding.authority.guestWebContentsId,
        documentGeneration: binding.authority.documentGeneration,
        origin: binding.authority.origin,
        connectionId: capability.connectionId,
        walletId: binding.authority.walletId,
        routeEpoch: binding.authority.routeEpoch,
        networkGenesis: binding.authority.network.genesisHash,
      },
      presentation: {
        kind: 'data-sign',
        origin: binding.authority.origin,
        walletName: evidence.walletName,
        networkName: this.options.networkName,
        scopes: ['data-signing'],
        extensions: capability.enabledExtensions,
        review,
      },
      payload: { address: review.address, payload: review.payload },
      declined: dataSignRejection(3, 'User declined'),
      execute: async (_payload, signal, passphrase) => {
        if (signal.aborted || !passphrase)
          throw dataSignRejection(1, 'Proof generation failed');
        this.assertCurrent(binding);
        const latest = await this.capabilityEvidence(binding);
        if (latest.evidence.walletKind !== 'shelley-software')
          throw dataSignRejection(1, 'Proof generation failed');
        this.options.dispatcher.requireCapability(
          request.method,
          binding.authority,
          latest.context
        );
        const response = await this.options.executeWallet({
          operation: 'sign-data',
          walletId: binding.authority.walletId,
          network: binding.authority.network,
          sourceRevision: this.options.sourceRevision,
          address: review.address,
          payload: review.payload,
          passphrase,
        });
        this.assertCurrent(binding);
        if (response.status === 'rejected') {
          if (response.reason === 'account-change') throw accountChange();
          if (response.reason === 'address-not-pk')
            throw dataSignRejection(2, 'Address is not a public key');
          if (response.reason === 'proof-generation')
            throw dataSignRejection(1, 'Proof generation failed');
          throw internal();
        }
        if (response.operation !== 'sign-data') throw internal();
        try {
          return verifyCip8BackendResponse(expected, response.value);
        } catch (error) {
          if (error instanceof Cip8Error) throw internal();
          throw error;
        }
      },
    });
    this.assertCurrent(binding);
    return result;
  }

  private isEnabled(binding: Cip30BrokerBinding): boolean {
    return (
      this.options.grants.find({
        ...this.grantIdentity(binding),
        scopes: ['connection', 'read'],
      }) !== undefined
    );
  }

  handle = async (
    event: IpcMainInvokeEvent,
    value: unknown
  ): Promise<unknown> => {
    const binding = this.binding(event);
    if (!binding) return { status: 'rejected', rejection: refusal() };

    let request: DappCip30GatewayRequest | undefined;
    try {
      request = parseDappCip30GatewayRequest(value);
      if (request.method === 'provider.isEnabled') {
        return createDappCip30FulfilledEnvelope(
          request.method,
          this.isEnabled(binding)
        );
      }
      if (request.method === 'provider.enable') {
        const result = await this.enable(request, binding);
        this.assertCurrent(binding);
        return createDappCip30FulfilledEnvelope(request.method, result);
      }
      if (request.method === 'api.signData') {
        const result = await this.signData(request, binding);
        return createDappCip30FulfilledEnvelope(request.method, result);
      }
      if (!IMPLEMENTED_METHODS.has(request.method)) throw refusal();

      const { context } = await this.capabilityEvidence(binding);
      const result = await this.options.dispatcher.dispatch(
        request,
        binding.authority,
        context,
        (operation) => this.executeWallet(binding, operation)
      );
      this.assertCurrent(binding);
      return createDappCip30FulfilledEnvelope(request.method, result);
    } catch (error) {
      let rejection: DappCip30Rejection;
      if (error instanceof Cip30DispatchRejection) {
        rejection = error.rejection;
      } else if (isRejection(error)) {
        rejection = error;
      } else if (isApiError(error)) {
        rejection = { type: 'api-error', value: error };
      } else {
        rejection = internal();
      }
      return request
        ? createDappCip30RejectedEnvelope(request.method, rejection)
        : { status: 'rejected', rejection };
    }
  };
}

export const parseConfiguredNetwork = (
  value: unknown,
  cluster: string,
  genesisHash: string
): Cip30WalletNetwork => {
  const genesis =
    value && typeof value === 'object' && !Array.isArray(value)
      ? (value as Record<string, unknown>)
      : {};
  const protocol =
    genesis.protocolConsts &&
    typeof genesis.protocolConsts === 'object' &&
    !Array.isArray(genesis.protocolConsts)
      ? (genesis.protocolConsts as Record<string, unknown>)
      : {};
  const magic = genesis.networkMagic ?? protocol.protocolMagic;
  if (
    !Number.isSafeInteger(magic) ||
    Number(magic) < 0 ||
    Number(magic) > 0xffffffff ||
    !/^[0-9a-f]{64}$/u.test(genesisHash)
  )
    throw new Error('Invalid configured network identity');
  return Object.freeze({
    networkId: cluster === 'mainnet' ? 1 : 0,
    networkMagic: Number(magic),
    genesisHash,
  });
};

const readNetwork = (): Cip30WalletNetwork =>
  parseConfiguredNetwork(
    JSON.parse(
      fs.readFileSync(launcherConfig.nodeConfig.network.genesisFile, 'utf8')
    ),
    launcherConfig.cluster,
    launcherConfig.nodeConfig.network.genesisHash
  );

const registry = new ExtensionRegistry();
const capabilities = new CapabilityService(registry);
const sessions = new SessionStore();
const negotiator = new Negotiator(registry, capabilities);
const dispatcher = new Dispatcher(capabilities, sessions);
let broker: Cip30Broker | undefined;
let registered = false;

export const handleCip30BrokerRequests = (): void => {
  if (registered) return;
  broker = new Cip30Broker({
    authenticate: authenticateDappGuest,
    currentLease: getCurrentDappRouteLease,
    executeWallet: executeCip30WalletRequest,
    consent: consentCoordinator,
    grants: new GrantRepository(
      path.join(stateDirectoryPath, 'dapp-grants.json')
    ),
    sessions,
    registry,
    capabilities,
    negotiator,
    dispatcher,
    network: readNetwork(),
    networkName: launcherConfig.cluster,
    sourceRevision: CARDANO_WALLET_SOURCE_REVISION,
  });
  setDappBrokerLifecycleRevoker(() => broker?.revoke());
  setCip30SessionRevoker(() => broker?.revoke());
  ipcMain.handle(DAPP_CIP30_GATEWAY_CHANNEL, broker.handle);
  registered = true;
};
