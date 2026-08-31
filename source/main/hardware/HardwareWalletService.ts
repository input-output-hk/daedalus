import TransportNodeHid, {
  getDevices,
} from '@ledgerhq/hw-transport-node-hid-noevents';
import AppAda, {
  AddressType,
  MessageAddressFieldType,
  utils,
} from '@cardano-foundation/ledgerjs-hw-app-cardano';
import type { DeviceOwnedAddress } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { str_to_path } from '@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address';
import TrezorConnect, {
  DEVICE,
  DEVICE_EVENT,
  DeviceUniquePath,
  Features,
  Success,
  TRANSPORT,
  TRANSPORT_EVENT,
  UI,
  UI_EVENT,
  Unsuccessful,
} from '@trezor/connect';
import { find, get, includes, last } from 'lodash';
import { derivePublic as deriveChildXpub } from 'cardano-crypto.js';
import { blake2b } from 'blakejs';
import type {
  CardanoSignedMessage,
  CardanoSignedTxData,
} from '@trezor/connect';
import { verifyHardwareTransactionWitnesses } from '../../common/cardano/witnessSet';
import { decodeConwayTransaction } from '../../common/cardano/transaction';
import { parseConwayTransactionEnvelope } from '../../common/cardano/transactionEnvelope';
import { toExactLedgerSignTransactionRequest } from '../../common/hardware/ledgerTransaction';
import { toExactTrezorSignTransactionRequest } from '../../common/hardware/trezorTransaction';
import { serializeCip8 } from '../../common/cardano/cip8';
import type { Cip8ExpectedRequest } from '../../common/cardano/cip8Request';
import {
  deviceDetection,
  waitForDevice,
} from '../ipc/hardwareWallets/ledger/deviceDetection';
import { logger } from '../utils/logging';
import {
  HardwareExactTransaction,
  HardwareMessageAddress,
  HardwareMessageRequest,
  HardwareTransactionWitnessResponse,
  HardwareWalletTransportDeviceRequest,
  LedgerDevicePayload,
  LedgerSignTransactionResponse,
  TransportDevice,
  HARDWARE_CONNECTOR_MATRIX_REVISION,
  hardwareConnectorRowId,
} from '../../common/types/hardware-wallets.types';

import { HardwareWalletChannels } from '../ipc/createHardwareWalletIPCChannels';
import {
  consumeIpcResponse,
  currentWindowSender,
} from '../ipc/lib/currentWindowSender';
import { Device } from '../ipc/hardwareWallets/ledger/deviceDetection/types';
import { DeviceDetectionPayload } from '../ipc/hardwareWallets/ledger/deviceDetection/deviceDetection';
import { initTrezorConnect, reinitTrezorConnect } from '../trezor/connection';
import { dappLaunchPolicy } from '../config';

type LedgerConnection = {
  device: Device;
  transport: TransportNodeHid;
  AdaConnection: AppAda;
};

export type LedgerServiceDependencies = {
  open: (path: string) => Promise<TransportNodeHid>;
  list: () => Promise<string[]>;
  getDevices: typeof getDevices;
  detect: typeof deviceDetection;
  wait: typeof waitForDevice;
  createApp: (transport: TransportNodeHid) => AppAda;
};

const ledgerDefaults: LedgerServiceDependencies = {
  open: (path) => TransportNodeHid.open(path),
  list: () => TransportNodeHid.list(),
  getDevices,
  detect: deviceDetection,
  wait: waitForDevice,
  createApp: (transport) => new AppAda(transport),
};

const decodeHex = (value: string): Buffer => {
  if (!/^(?:[0-9a-fA-F]{2})+$/.test(value)) throw new Error('Invalid hex');
  return Buffer.from(value, 'hex');
};

const expectedHardwareMessage = (
  request: HardwareMessageRequest
): Cip8ExpectedRequest => ({
  address: request.address.value,
  credentialKind: request.credentialKind,
  credential: decodeHex(request.credential),
  protectedAddress: decodeHex(request.protectedAddress),
  payload: request.payload.length
    ? decodeHex(request.payload)
    : Buffer.alloc(0),
});
const ledgerMessageAddress = (
  address: Extract<HardwareMessageAddress, { kind: 'address' }>
): DeviceOwnedAddress => {
  const paymentPath = address.paymentPath && [...address.paymentPath];
  const stakePath = address.stakePath && [...address.stakePath];
  switch (address.addressType) {
    case 0:
      if (!paymentPath || (!stakePath && !address.stakeKeyHash))
        throw new Error('Missing Ledger base address binding');
      return {
        type: AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
        params: {
          spendingPath: paymentPath,
          ...(stakePath
            ? { stakingPath: stakePath }
            : { stakingKeyHashHex: address.stakeKeyHash }),
        },
      };
    case 2:
      if (!paymentPath || !address.stakeScriptHash)
        throw new Error('Missing Ledger base address binding');
      return {
        type: AddressType.BASE_PAYMENT_KEY_STAKE_SCRIPT,
        params: {
          spendingPath: paymentPath,
          stakingScriptHashHex: address.stakeScriptHash,
        },
      };
    case 4:
      if (!paymentPath || !address.pointer)
        throw new Error('Missing Ledger pointer address binding');
      return {
        type: AddressType.POINTER_KEY,
        params: {
          spendingPath: paymentPath,
          stakingBlockchainPointer: address.pointer,
        },
      };
    case 6:
      if (!paymentPath) throw new Error('Missing Ledger payment path');
      return {
        type: AddressType.ENTERPRISE_KEY,
        params: { spendingPath: paymentPath },
      };
    case 14:
      if (!stakePath) throw new Error('Missing Ledger stake path');
      return {
        type: AddressType.REWARD_KEY,
        params: { stakingPath: stakePath },
      };
    default:
      throw new Error('Unsupported Ledger message address');
  }
};

const trezorMessageAddress = (
  address: Extract<HardwareMessageAddress, { kind: 'address' }>
) => ({
  addressType: address.addressType,
  ...(address.paymentPath ? { path: [...address.paymentPath] } : {}),
  ...(address.stakePath ? { stakingPath: [...address.stakePath] } : {}),
  ...(address.stakeKeyHash ? { stakingKeyHash: address.stakeKeyHash } : {}),
  ...(address.stakeScriptHash
    ? { stakingScriptHash: address.stakeScriptHash }
    : {}),
  ...(address.pointer ? { certificatePointer: address.pointer } : {}),
});

const verifiedHardwareMessage = (
  request: HardwareMessageRequest,
  publicKeyHex: string,
  signatureHex: string,
  addressFieldHex: string
) => {
  if (
    !/^[0-9a-f]{64}$/u.test(publicKeyHex) ||
    !/^[0-9a-f]{128}$/u.test(signatureHex) ||
    addressFieldHex !== request.address.value
  )
    throw new Error('Hardware wallet returned invalid message proof');
  return serializeCip8(expectedHardwareMessage(request), {
    publicKey: Buffer.from(publicKeyHex, 'hex'),
    signature: Buffer.from(signatureHex, 'hex'),
  });
};

export type HardwareWalletOperation = 'signTx' | 'signData';
export type HardwareWalletOperationErrorCode =
  | 'APIError.InternalError'
  | 'TxSignError.ProofGeneration'
  | 'TxSignError.UserDeclined'
  | 'DataSignError.ProofGeneration'
  | 'DataSignError.UserDeclined';

type OperationInvalidation = 'host' | 'device';

const operationErrorCode = (
  operation: HardwareWalletOperation,
  outcome: 'internal' | 'proof-generation' | 'user-declined'
): HardwareWalletOperationErrorCode => {
  if (outcome === 'internal') return 'APIError.InternalError';
  if (operation === 'signTx')
    return outcome === 'user-declined'
      ? 'TxSignError.UserDeclined'
      : 'TxSignError.ProofGeneration';
  return outcome === 'user-declined'
    ? 'DataSignError.UserDeclined'
    : 'DataSignError.ProofGeneration';
};

const vendorErrorCode = (error: unknown): string | number | undefined => {
  if (!error || typeof error !== 'object') return undefined;
  const code = (error as { code?: unknown }).code;
  return typeof code === 'string' || typeof code === 'number'
    ? code
    : undefined;
};

const ledgerRefusalCodes = new Set([0x6e09, 0x6985]);
const ledgerProofCodes = new Set([
  'DEVICE_NOT_CONNECTED',
  'DEVICE_PATH_CHANGED',
  'NO_DEVICE_PATHS',
]);
const ledgerTransportErrors = new Set([
  'DisconnectedDevice',
  'DisconnectedDeviceDuringOperation',
  'TransportError',
  'TransportStatusError',
]);
const trezorRefusalCodes = new Set([
  'Failure_ActionCancelled',
  'Failure_PinCancelled',
]);

export class HardwareWalletOperationError extends Error {
  constructor(
    public readonly operation: HardwareWalletOperation,
    public readonly code: HardwareWalletOperationErrorCode
  ) {
    super(code);
    this.name = 'HardwareWalletOperationError';
  }
}

export class HardwareWalletOperationCancelled extends Error {
  constructor(
    public readonly reason: OperationInvalidation | 'stale' = 'stale'
  ) {
    super('Hardware wallet operation cancelled');
    this.name = 'HardwareWalletOperationCancelled';
  }
}
const restoreExactTransaction = (
  exact: HardwareExactTransaction
): HardwareExactTransaction => {
  const transaction = decodeConwayTransaction(
    parseConwayTransactionEnvelope(Buffer.from(exact.transaction.envelope.cbor))
  );
  if (
    transaction.transactionId !== exact.bodyHash ||
    exact.transaction.transactionId !== exact.bodyHash
  )
    throw new Error('Hardware transaction body mismatch');
  return Object.freeze({ ...exact, transaction });
};

export class HardwareWalletService {
  private devicesMemo: Record<string, LedgerConnection> = {};
  private generations = new Map<string, number>();
  private activeLedgerOperations = new Map<string, number>();
  private ledgerInvalidations = new Map<string, OperationInvalidation>();
  private trezorGeneration = 0;
  private activeTrezorGenerations = new Map<number, number>();
  private trezorInvalidations = new Map<number, OperationInvalidation>();
  private detectorUnsubscribe: (() => void) | null = null;
  private disposed = false;

  constructor(
    private readonly ledger: LedgerServiceDependencies = ledgerDefaults
  ) {}

  private generation = (path: string): number =>
    this.generations.get(path) || 0;

  private invalidate = (path: string, reason: OperationInvalidation): void => {
    const generation = this.generation(path);
    const key = `${path}:${generation}`;
    if (this.activeLedgerOperations.has(key))
      this.ledgerInvalidations.set(key, reason);
    this.generations.set(path, generation + 1);
  };

  private notifyLedger = async (
    event: DeviceDetectionPayload,
    channel: HardwareWalletChannels['getHardwareWalletConnectionChannel']
  ): Promise<void> => {
    try {
      if (this.disposed) return;
      const connectionChanged = event.type === 'add' || event.type === 'remove';
      if (!connectionChanged) return;

      const { device, deviceModel } = event;
      const walletData: LedgerDevicePayload = {
        disconnected: event.type === 'remove',
        deviceType: 'ledger',
        deviceId: null,
        deviceModel: deviceModel.id,
        deviceName: deviceModel.productName,
        path: device.path,
        product: device.product,
      };

      if (event.type === 'add') {
        if (this.devicesMemo[device.path]) return;
        const generation = this.generation(device.path);
        const transport = await this.ledger.open(device.path);
        if (this.disposed || generation !== this.generation(device.path)) {
          await transport.close();
          return;
        }
        this.devicesMemo[device.path] = {
          device,
          transport,
          AdaConnection: this.ledger.createApp(transport),
        };
      } else {
        await this.cancelLedgerOperation(device.path, 'device');
      }

      consumeIpcResponse(
        channel.send(walletData, currentWindowSender.sender),
        'GET_HARDWARE_WALLET_CONNECTION_CHANNEL'
      );
    } catch (error) {
      logger.error('[HW-DEBUG] Ledger connection event failed', {
        error: String(error),
      });
    }
  };

  private operationError = (
    operation: HardwareWalletOperation,
    outcome: 'internal' | 'proof-generation' | 'user-declined'
  ): HardwareWalletOperationError =>
    new HardwareWalletOperationError(
      operation,
      operationErrorCode(operation, outcome)
    );

  private normalizeOperationError = (
    operation: HardwareWalletOperation,
    vendor: 'ledger' | 'trezor',
    error: unknown
  ): HardwareWalletOperationError => {
    if (error instanceof HardwareWalletOperationError) return error;
    if (error instanceof HardwareWalletOperationCancelled) {
      let outcome: 'internal' | 'proof-generation' | 'user-declined' =
        'internal';
      if (error.reason === 'host') outcome = 'user-declined';
      if (error.reason === 'device') outcome = 'proof-generation';
      return this.operationError(operation, outcome);
    }

    const code = vendorErrorCode(error);
    if (
      (vendor === 'ledger' &&
        typeof code === 'number' &&
        ledgerRefusalCodes.has(code)) ||
      (vendor === 'trezor' &&
        typeof code === 'string' &&
        trezorRefusalCodes.has(code))
    )
      return this.operationError(operation, 'user-declined');

    const name =
      error && typeof error === 'object'
        ? (error as { name?: unknown }).name
        : undefined;
    if (
      (vendor === 'ledger' && typeof code === 'number') ||
      (vendor === 'ledger' &&
        typeof code === 'string' &&
        ledgerProofCodes.has(code)) ||
      (vendor === 'ledger' &&
        typeof name === 'string' &&
        ledgerTransportErrors.has(name)) ||
      (vendor === 'trezor' &&
        typeof code === 'string' &&
        /^(Failure|Device|Transport)_/u.test(code))
    )
      return this.operationError(operation, 'proof-generation');

    return this.operationError(operation, 'internal');
  };

  private withNormalizedOperation = async <T>(
    operation: HardwareWalletOperation,
    vendor: 'ledger' | 'trezor',
    execute: () => Promise<T>
  ): Promise<T> => {
    try {
      return await execute();
    } catch (error) {
      throw this.normalizeOperationError(operation, vendor, error);
    }
  };

  private assertCurrentLedgerOperation = (
    path: string,
    record: LedgerConnection,
    generation: number
  ): void => {
    if (
      this.disposed ||
      generation !== this.generation(path) ||
      this.devicesMemo[path] !== record
    ) {
      const key = `${path}:${generation}`;
      throw new HardwareWalletOperationCancelled(
        this.ledgerInvalidations.get(key) || 'stale'
      );
    }
  };

  public withLedgerOperation = async <T>(
    path: string,
    execute: (connection: AppAda) => Promise<T>
  ): Promise<T> => {
    const record = this.devicesMemo[path];
    if (!record)
      throw Object.assign(new Error('Ledger device not connected'), {
        code: 'DEVICE_NOT_CONNECTED',
      });
    const generation = this.generation(path);
    const key = `${path}:${generation}`;
    this.activeLedgerOperations.set(
      key,
      (this.activeLedgerOperations.get(key) || 0) + 1
    );
    try {
      const result = await execute(record.AdaConnection);
      this.assertCurrentLedgerOperation(path, record, generation);
      return result;
    } catch (error) {
      this.assertCurrentLedgerOperation(path, record, generation);
      throw error;
    } finally {
      const remaining = (this.activeLedgerOperations.get(key) || 1) - 1;
      if (remaining) {
        this.activeLedgerOperations.set(key, remaining);
      } else {
        this.activeLedgerOperations.delete(key);
        this.ledgerInvalidations.delete(key);
      }
    }
  };

  private assertCurrentTrezorOperation = (generation: number): void => {
    if (this.disposed || generation !== this.trezorGeneration)
      throw new HardwareWalletOperationCancelled(
        this.trezorInvalidations.get(generation) || 'stale'
      );
  };

  private withTrezorOperation = async <T>(
    execute: () => Promise<T>
  ): Promise<T> => {
    const generation = this.trezorGeneration;
    this.activeTrezorGenerations.set(
      generation,
      (this.activeTrezorGenerations.get(generation) || 0) + 1
    );
    try {
      const result = await execute();
      this.assertCurrentTrezorOperation(generation);
      return result;
    } catch (error) {
      this.assertCurrentTrezorOperation(generation);
      throw error;
    } finally {
      const remaining = (this.activeTrezorGenerations.get(generation) || 1) - 1;
      if (remaining) {
        this.activeTrezorGenerations.set(generation, remaining);
      } else {
        this.activeTrezorGenerations.delete(generation);
        this.trezorInvalidations.delete(generation);
      }
    }
  };

  public signExactLedgerTransaction = (
    devicePath: string,
    exact: HardwareExactTransaction
  ): Promise<string> =>
    this.withNormalizedOperation('signTx', 'ledger', async () => {
      let request: ReturnType<typeof toExactLedgerSignTransactionRequest>;
      try {
        request = toExactLedgerSignTransactionRequest(exact);
      } catch {
        throw this.operationError('signTx', 'proof-generation');
      }
      const expected = exact.signers
        .filter(({ keyHash }) =>
          exact.witnesses.requestedDeviceKeyHashes.includes(keyHash)
        )
        .map(({ path, keyHash }) => ({
          path: path.join('/'),
          keyHash,
        }));
      return this.withLedgerOperation(devicePath, async (connection) => {
        const signed = await connection.signTransaction(request);
        if (
          signed.txHashHex !== exact.bodyHash ||
          !/^[0-9a-f]{64}$/u.test(signed.txHashHex)
        )
          throw this.operationError('signTx', 'proof-generation');
        const seen = new Set<string>();
        const witnesses: HardwareTransactionWitnessResponse['witnesses'][number][] = [];
        for (const witness of signed.witnesses) {
          const path = witness.path.join('/');
          const expectedWitness = expected.find(
            (candidate) => candidate.path === path
          );
          if (!expectedWitness || seen.has(path))
            throw this.operationError('signTx', 'proof-generation');
          seen.add(path);
          if (!/^[0-9a-f]{128}$/u.test(witness.witnessSignatureHex))
            throw this.operationError('signTx', 'proof-generation');
          const key = await connection.getExtendedPublicKey({
            path: witness.path,
          });
          if (!/^[0-9a-f]{64}$/u.test(key.publicKeyHex))
            throw this.operationError('signTx', 'proof-generation');
          const keyHash = Buffer.from(
            blake2b(Buffer.from(key.publicKeyHex, 'hex'), undefined, 28)
          ).toString('hex');
          if (keyHash !== expectedWitness.keyHash)
            throw this.operationError('signTx', 'proof-generation');
          witnesses.push({
            publicKey: key.publicKeyHex,
            signature: witness.witnessSignatureHex,
          });
        }
        if (seen.size !== expected.length)
          throw this.operationError('signTx', 'proof-generation');
        try {
          return verifyHardwareTransactionWitnesses(exact, {
            bodyHash: signed.txHashHex,
            witnesses,
          });
        } catch {
          throw this.operationError('signTx', 'proof-generation');
        }
      });
    });

  public signExactTrezorTransaction = (
    exact: HardwareExactTransaction
  ): Promise<string> =>
    this.withNormalizedOperation('signTx', 'trezor', async () => {
      let request: ReturnType<typeof toExactTrezorSignTransactionRequest>;
      try {
        request = toExactTrezorSignTransactionRequest(exact);
      } catch {
        throw this.operationError('signTx', 'proof-generation');
      }
      return this.withTrezorOperation(async () => {
        const result = await TrezorConnect.cardanoSignTransaction(request);
        if (!result.success) {
          const code = vendorErrorCode(result.payload);
          if (typeof code !== 'string')
            throw this.operationError('signTx', 'internal');
          throw this.operationError(
            'signTx',
            trezorRefusalCodes.has(code) ? 'user-declined' : 'proof-generation'
          );
        }
        const payload = result.payload as CardanoSignedTxData;
        if (
          !payload ||
          payload.auxiliaryDataSupplement !== undefined ||
          payload.hash !== exact.bodyHash ||
          !/^[0-9a-f]{64}$/u.test(payload.hash) ||
          !Array.isArray(payload.witnesses)
        )
          throw this.operationError('signTx', 'proof-generation');
        const witnesses: HardwareTransactionWitnessResponse['witnesses'][number][] = payload.witnesses.map(
          ({ type, pubKey, signature, chainCode }) => {
            if (
              chainCode !== undefined ||
              type !== 1 ||
              !/^[0-9a-f]{64}$/u.test(pubKey) ||
              !/^[0-9a-f]{128}$/u.test(signature)
            )
              throw this.operationError('signTx', 'proof-generation');
            return { publicKey: pubKey, signature };
          }
        );
        try {
          return verifyHardwareTransactionWitnesses(exact, {
            bodyHash: payload.hash,
            witnesses,
          });
        } catch {
          throw this.operationError('signTx', 'proof-generation');
        }
      });
    });

  public signLedgerMessage = (
    devicePath: string,
    request: HardwareMessageRequest
  ) =>
    this.withNormalizedOperation('signData', 'ledger', () =>
      this.withLedgerOperation(devicePath, async (connection) => {
        const signed = await connection.signMessage(
          request.address.kind === 'key_hash'
            ? {
                messageHex: request.payload,
                signingPath: [...request.path],
                hashPayload: false,
                preferHexDisplay: false,
                addressFieldType: MessageAddressFieldType.KEY_HASH,
              }
            : {
                messageHex: request.payload,
                signingPath: [...request.path],
                hashPayload: false,
                preferHexDisplay: false,
                addressFieldType: MessageAddressFieldType.ADDRESS,
                address: ledgerMessageAddress(request.address),
                network: {
                  networkId: request.network.networkId,
                  protocolMagic: request.network.networkMagic,
                },
              }
        );
        try {
          return verifiedHardwareMessage(
            request,
            signed.signingPublicKeyHex,
            signed.signatureHex,
            signed.addressFieldHex
          );
        } catch {
          throw this.operationError('signData', 'proof-generation');
        }
      })
    );

  public signTrezorMessage = (request: HardwareMessageRequest) =>
    this.withNormalizedOperation('signData', 'trezor', () =>
      this.withTrezorOperation(async () => {
        const result = await TrezorConnect.cardanoSignMessage({
          path: [...request.path],
          payload: request.payload,
          preferHexDisplay: false,
          networkId: request.network.networkId,
          protocolMagic: request.network.networkMagic,
          ...(request.address.kind === 'address'
            ? { addressParameters: trezorMessageAddress(request.address) }
            : {}),
        });
        if (!result.success) {
          const code = vendorErrorCode(result.payload);
          if (typeof code !== 'string')
            throw this.operationError('signData', 'internal');
          throw this.operationError(
            'signData',
            trezorRefusalCodes.has(code) ? 'user-declined' : 'proof-generation'
          );
        }
        const signed = result.payload as CardanoSignedMessage;
        if (
          !signed ||
          signed.payload !== request.payload ||
          signed.headers?.protected?.[1] !== -8 ||
          signed.headers.protected.address !== request.address.value ||
          signed.headers.unprotected?.hashed !== false ||
          signed.headers.unprotected.version !== 1
        )
          throw this.operationError('signData', 'proof-generation');
        try {
          return verifiedHardwareMessage(
            request,
            signed.pubKey,
            signed.signature,
            signed.headers.protected.address
          );
        } catch {
          throw this.operationError('signData', 'proof-generation');
        }
      })
    );

  public cancelLedgerOperation = async (
    path?: string,
    reason: OperationInvalidation = 'host'
  ): Promise<void> => {
    const paths = path ? [path] : Object.keys(this.devicesMemo);
    await Promise.all(
      paths.map(async (devicePath) => {
        this.invalidate(devicePath, reason);
        const record = this.devicesMemo[devicePath];
        delete this.devicesMemo[devicePath];
        if (record) {
          try {
            await record.transport.close();
          } catch (error) {
            logger.debug('[HW-DEBUG] Ledger transport close failed', {
              error: String(error),
            });
          }
        }
      })
    );
  };

  public cancelTrezorOperation = (
    reason: OperationInvalidation = 'host'
  ): void => {
    const generation = this.trezorGeneration;
    if (this.activeTrezorGenerations.has(generation))
      this.trezorInvalidations.set(generation, reason);
    this.trezorGeneration += 1;
    TrezorConnect.cancel('Method_Cancel');
  };

  public dispose = async (): Promise<void> => {
    if (this.disposed) return;
    this.disposed = true;
    this.detectorUnsubscribe?.();
    this.detectorUnsubscribe = null;
    TrezorConnect.removeAllListeners();
    this.cancelTrezorOperation();
    await this.cancelLedgerOperation();
  };

  public register = async ({
    getHardwareWalletTransportChannel,
    getExtendedPublicKeyChannel,
    getCardanoAdaAppChannel,
    getHardwareWalletConnectionChannel,
    signTransactionLedgerChannel,
    signTransactionTrezorChannel,
    signExactHardwareTransactionChannel,
    signExactHardwareMessageChannel,
    resetTrezorActionChannel,
    handleInitTrezorConnectChannel,
    handleInitLedgerConnectChannel,
    deriveXpubChannel,
    deriveAddressChannel,
    showAddressChannel,
    waitForLedgerDevicesToConnectChannel,
  }: HardwareWalletChannels): Promise<void> => {
    const resetTrezorListeners = () => {
      // Remove all listeners if exist - e.g. on app refresh
      TrezorConnect.removeAllListeners();
      // Initialize new device listeners
      TrezorConnect.on(UI_EVENT, (event) => {
        logger.info('[TREZOR-CONNECT] Received UI_EVENT: ' + event.type);

        if (event.type === UI.REQUEST_PASSPHRASE) {
          // ui-request_passphrase
          if (event.payload && event.payload.device) {
            TrezorConnect.uiResponse({
              type: UI.RECEIVE_PASSPHRASE,
              payload: {
                save: true,
                value: '',
                passphraseOnDevice: true,
              },
            });

            logger.info(
              '[TREZOR-CONNECT] Called TrezorConnect.uiResponse - requested to provide passphrase on device'
            );
          }
        }
      });
      TrezorConnect.on(TRANSPORT_EVENT, (event) => {
        if (event.type === TRANSPORT.ERROR) {
          const isNoDevicePollingNoise =
            get(event, ['payload', 'apiType']) === 'usb' &&
            get(event, ['payload', 'error']) === 'Network request failed';

          logger[isNoDevicePollingNoise ? 'debug' : 'info'](
            '[TREZOR-CONNECT] Received TRANSPORT_EVENT: transport-error',
            event.payload
          );
          if (!isNoDevicePollingNoise) this.cancelTrezorOperation('device');

          // Send Transport error to Renderer
          consumeIpcResponse(
            getHardwareWalletConnectionChannel.send(
              {
                deviceType: 'trezor',
                error: {
                  payload: event.payload,
                },
              },
              currentWindowSender.sender
            ),
            'GET_HARDWARE_WALLET_CONNECTION_CHANNEL'
          );
        }
      });
      TrezorConnect.on(DEVICE_EVENT, (event) => {
        logger.info('[TREZOR-CONNECT] Received DEVICE_EVENT: ' + event.type);

        const connectionChanged =
          event.type === DEVICE.CONNECT ||
          event.type === DEVICE.DISCONNECT ||
          event.type === DEVICE.CHANGED;
        const isAcquired = get(event, ['payload', 'type'], '') === 'acquired';
        const deviceError = get(event, ['payload', 'error']);
        if (event.type === DEVICE.DISCONNECT)
          this.cancelTrezorOperation('device');

        if (deviceError) {
          throw new Error(deviceError);
        }

        if (connectionChanged && isAcquired) {
          consumeIpcResponse(
            getHardwareWalletConnectionChannel.send(
              {
                disconnected: event.type === DEVICE.DISCONNECT,
                deviceType: 'trezor',
                deviceId: event.payload.id,
                // 123456ABCDEF
                deviceModel: event.payload.features.model,
                // e.g. T
                deviceName: event.payload.label,
                // e.g. Test Name
                path: event.payload.path,
                eventType: event.type,
              },
              currentWindowSender.sender
            ),
            'GET_HARDWARE_WALLET_CONNECTION_CHANNEL'
          );
        }
      });
    };

    waitForLedgerDevicesToConnectChannel.onRequest(async () => {
      logger.info('[HW-DEBUG] waitForLedgerDevicesToConnectChannel::waiting');
      const { device, deviceModel } = await this.ledger.wait();
      logger.info('[HW-DEBUG] waitForLedgerDevicesToConnectChannel::found');
      return {
        disconnected: false,
        deviceType: 'ledger',
        deviceId: null,
        // Available only when Cardano APP opened
        deviceModel: deviceModel.id,
        // e.g. nanoS
        deviceName: deviceModel.productName,
        // e.g. Test Name
        path: device.path,
        product: device.product,
      };
    });

    getHardwareWalletTransportChannel.onRequest(
      async (request: HardwareWalletTransportDeviceRequest) => {
        const { isTrezor, devicePath } = request;
        logger.info('[HW-DEBUG] getHardwareWalletTransportChannel', {
          devicePath,
        });
        // Connected Trezor device info
        let deviceFeatures: Unsuccessful | Success<Features>;

        if (isTrezor) {
          logger.info('[HW-DEBUG] getHardwareWalletTransportChannel::TREZOR ');

          try {
            deviceFeatures = await TrezorConnect.getFeatures({
              device: {
                path: devicePath as DeviceUniquePath,
              },
            });

            logger.info('[TREZOR-CONNECT] Called TrezorConnect.getFeatures()');

            if (deviceFeatures && deviceFeatures.success) {
              logger.info('[HW-DEBUG] Trezor connect success');

              const {
                major_version: majorVersion,
                minor_version: minorVersion,
                patch_version: patchVersion,
                device_id: deviceId,
                model,
                label,
              } = deviceFeatures.payload;
              const firmwareVersion = `${majorVersion}.${minorVersion}.${patchVersion}`;
              return Promise.resolve({
                deviceId,
                deviceType: 'trezor',
                deviceModel: model,
                // e.g. "1" or "T"
                deviceName: label,
                path: devicePath,
                firmwareVersion,
              } as TransportDevice);
            }

            throw deviceFeatures.payload; // Error is in payload
          } catch (e) {
            logger.info(
              '[HW-DEBUG] Trezor connect error: ',
              e.message || 'no message'
            );
            throw e;
          }
        }

        try {
          logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER');
          const transportList = await this.ledger.list();
          let hw;
          let lastConnectedPath;
          logger.info(
            `[HW-DEBUG] getHardwareWalletTransportChannel::transportList=${JSON.stringify(
              transportList
            )}`
          );

          const openTransportLayer = async (
            pathToOpen: string,
            device: Device
          ) => {
            await this.cancelLedgerOperation(pathToOpen);
            const generation = this.generation(pathToOpen);
            const transport = await this.ledger.open(pathToOpen);
            if (this.disposed || generation !== this.generation(pathToOpen)) {
              await transport.close();
              throw new HardwareWalletOperationCancelled();
            }
            hw = transport;
            lastConnectedPath = pathToOpen;
            this.devicesMemo[pathToOpen] = {
              device,
              transport,
              AdaConnection: this.ledger.createApp(transport),
            };
          };

          if (transportList && !transportList.length) {
            // Establish connection with last device
            try {
              logger.info('[HW-DEBUG] INIT NEW transport');

              const { device } = await this.ledger.wait();

              await openTransportLayer(device.path, device);
            } catch (e) {
              logger.info('[HW-DEBUG] INIT NEW transport - ERROR');
              throw e;
            }
          } else if (!devicePath || !this.devicesMemo[devicePath]) {
            // Use first like native usb nodeHID
            lastConnectedPath = transportList[0]; // eslint-disable-line
            logger.info('[HW-DEBUG] USE First transport', {
              lastConnectedPath,
            });

            if (this.devicesMemo[lastConnectedPath]) {
              await openTransportLayer(
                lastConnectedPath,
                this.devicesMemo[lastConnectedPath].device
              );
            } else {
              throw new Error('Device not connected!');
            }
          } else {
            logger.info('[HW-DEBUG] USE CURRENT CONNECTION');
            hw = this.devicesMemo[devicePath].transport;
          }

          const { deviceModel } = hw;

          if (deviceModel) {
            const { id, productName } = deviceModel;
            const ledgerData: TransportDevice = {
              deviceId: null,
              // @TODO - to be defined
              deviceType: 'ledger',
              deviceModel: id,
              // e.g. nanoS
              deviceName: productName,
              // e.g. Ledger Nano S
              path: lastConnectedPath || devicePath,
            };

            logger.info(
              '[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case RESPONSE',
              { ledgerData }
            );

            return Promise.resolve(ledgerData);
          }

          throw new Error('Missing device info');
        } catch (error) {
          logger.info('[HW-DEBUG] ERROR on getHardwareWalletTransportChannel');
          throw error;
        }
      }
    );

    handleInitTrezorConnectChannel.onRequest(async () => {
      logger.info('[HW-DEBUG] INIT TREZOR');
      await initTrezorConnect();
      resetTrezorListeners();
    });

    handleInitLedgerConnectChannel.onRequest(async () => {
      logger.info('[HW-DEBUG] INIT LEDGER');
      if (this.detectorUnsubscribe) return;
      try {
        const notify = (payload: DeviceDetectionPayload) => {
          this.notifyLedger(payload, getHardwareWalletConnectionChannel);
        };
        this.detectorUnsubscribe = this.ledger.detect(notify, notify);
        logger.info('[HW-DEBUG] Ledger device listener started');
      } catch (error) {
        logger.info('[HW-DEBUG] Ledger device listener failed', {
          error: String(error),
        });
        this.detectorUnsubscribe = null;
      }
    });
    deriveXpubChannel.onRequest(async (params) => {
      const { parentXpubHex, lastIndex, derivationScheme } = params;
      const parentXpub = decodeHex(parentXpubHex);

      try {
        const xpub = deriveChildXpub(parentXpub, lastIndex, derivationScheme);
        return utils.buf_to_hex(xpub);
      } catch (e) {
        throw e;
      }
    });
    deriveAddressChannel.onRequest(async (params) => {
      const {
        addressType,
        spendingPathStr,
        stakingPathStr,
        devicePath,
        isTrezor,
        networkId,
        protocolMagic,
      } = params;
      const spendingPath = str_to_path(spendingPathStr);
      const stakingPath = stakingPathStr ? str_to_path(stakingPathStr) : null;

      logger.info('[HW-DEBUG] DERIVE ADDRESS');

      if (isTrezor) {
        logger.info(
          '[TREZOR-CONNECT] Called TrezorConnect.cardanoGetAddress()'
        );

        const result = await TrezorConnect.cardanoGetAddress({
          showOnTrezor: true,
          addressParameters: {
            addressType,
            path: `m/${spendingPathStr}`,
            stakingPath: stakingPathStr ? `m/${stakingPathStr}` : null,
          },
          protocolMagic,
          networkId,
        });

        if (result.success === false) {
          logger.error(
            '[TREZOR-CONNECT] TrezorConnect.cardanoGetAddress() failed',
            result.payload
          );

          throw new Error('TrezorConnect.cardanoGetAddress() failed');
        }

        return result.payload.address;
      }

      if (!devicePath) throw new Error('Ledger device not connected');
      const { addressHex } = await this.withLedgerOperation(
        devicePath,
        (connection) =>
          connection.deriveAddress({
            network: {
              networkId,
              protocolMagic,
            },
            address: {
              type: addressType,
              params: {
                spendingPath,
                stakingPath,
              },
            },
          })
      );
      return utils.bech32_encodeAddress(decodeHex(addressHex));
    });
    showAddressChannel.onRequest(async (params) => {
      const {
        addressType,
        spendingPathStr,
        stakingPathStr,
        devicePath,
        isTrezor,
        networkId,
        protocolMagic,
      } = params;
      const spendingPath = str_to_path(spendingPathStr);
      const stakingPath = stakingPathStr ? str_to_path(stakingPathStr) : null;

      logger.info('[HW-DEBUG] SHOW ADDRESS');
      if (isTrezor) {
        throw new Error('Address verification not supported on Trezor devices');
      }
      if (!devicePath) throw new Error('Ledger device not connected');
      await this.withLedgerOperation(devicePath, (connection) =>
        connection.showAddress({
          network: {
            networkId,
            protocolMagic,
          },
          address: {
            type: addressType,
            params: {
              spendingPath,
              stakingPath,
            },
          },
        })
      );
    });
    getCardanoAdaAppChannel.onRequest(async (request) => {
      const { path, product } = request;

      try {
        if (!this.devicesMemo[path]) {
          const deviceList = this.ledger.getDevices();
          const device =
            find(deviceList, ['product', product]) ||
            find(deviceList, ['path', path]);

          logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: Path not found', {
            product,
            deviceList,
            oldPath: path,
          });

          if (!device) {
            logger.info('[HW-DEBUG] Device not instantiated');
            // eslint-disable-next-line
            throw {
              code: 'DEVICE_NOT_CONNECTED',
            };
          }

          const newTransport = await this.ledger.open(device.path);
          const newDeviceConnection = this.ledger.createApp(newTransport);

          logger.info(
            '[HW-DEBUG] getCardanoAdaAppChannel::Use new device path',
            {
              product,
              device,
              newPath: device.path,
              oldPath: path,
            }
          );

          this.devicesMemo[device.path] = {
            device,
            transport: newTransport,
            AdaConnection: newDeviceConnection,
          };

          if (device.path !== path) {
            // eslint-disable-next-line
            throw {
              code: 'DEVICE_PATH_CHANGED',
              path: device.path,
            };
          }
        }

        if (!path || !this.devicesMemo[path]) {
          logger.info('[HW-DEBUG] Device not instantiated');
          // eslint-disable-next-line
          throw {
            code: 'DEVICE_NOT_CONNECTED',
          };
        }

        logger.info('[HW-DEBUG] GET CARDANO APP');
        const { version } = await this.withLedgerOperation(path, (connection) =>
          connection.getVersion()
        );

        logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: appVersion');
        const { minor, major, patch } = version;
        return {
          minor: String(minor),
          major: String(major),
          patch: String(patch),
          deviceId: '',
        };
      } catch (error) {
        const errorCode = error.code || '';
        const errorName = error.name || 'UknownErrorName';
        const errorMessage = error.message || 'UknownErrorMessage';
        const isDeviceDisconnected = errorCode === 'DEVICE_NOT_CONNECTED';
        const isDisconnectError =
          errorName === 'DisconnectedDevice' ||
          errorMessage === 'Cannot write to hid device';
        //  errorMessage.toLowerCase().includes('cannot open device with path') ||
        //  errorMessage.toLowerCase().includes('cannot write to hid device') ||
        //  errorMessage.toLowerCase().includes('cannot write to closed device');
        logger.info('[HW-DEBUG] ERROR in Cardano App', {
          path,
          errorName,
          errorMessage,
          isDisconnectError,
          isDeviceDisconnected,
        });

        if (path && !isDeviceDisconnected && isDisconnectError) {
          const oldPath = path;
          const deviceMemo = this.devicesMemo[oldPath];
          const devicePaths: string[] = await this.ledger.list();
          const hasPathChanged = !includes(devicePaths, oldPath);
          const newPath = hasPathChanged ? last(devicePaths) : oldPath;

          if (hasPathChanged) {
            logger.info(
              `[HW-DEBUG] Device path changed from ${oldPath} to ${newPath}`
            );
          }

          if (!newPath) {
            logger.info(
              '[HW-DEBUG] ERROR in Cardano App (Device paths list is empty)',
              {
                devicePaths,
                oldPath,
                newPath,
                deviceList: this.ledger.getDevices(),
              }
            );
            // eslint-disable-next-line
            throw {
              code: 'NO_DEVICE_PATHS',
              errorCode,
              errorName,
            };
          }

          if (!deviceMemo) throw error;
          const { device: oldDevice } = deviceMemo;
          await this.cancelLedgerOperation(oldPath);
          const newTransport = await this.ledger.open(newPath);
          const newDeviceConnection = this.ledger.createApp(newTransport);
          const deviceList = this.ledger.getDevices();
          const newDevice = find(deviceList, ['path', newPath]);
          if (!newDevice) {
            await newTransport.close();
            throw error;
          }
          const hasDeviceChanged = newDevice.productId !== oldDevice.productId;
          logger.info(
            '[HW-DEBUG] ERROR in Cardano App (Re-establish Connection)',
            {
              hasPathChanged,
              hasDeviceChanged,
              oldPath: oldPath || 'UNKNOWN_PATH',
              newPath: newPath || 'UNKNOWN_PATH',
              oldDevice: oldDevice || 'NOT_FOUND',
              newDevice: newDevice || 'NOT_FOUND',
            }
          );
          // Update devicesMemo
          this.devicesMemo[newPath] = {
            device: newDevice,
            transport: newTransport,
            AdaConnection: newDeviceConnection,
          };

          if (hasPathChanged) {
            // eslint-disable-next-line
            throw {
              code: 'DEVICE_PATH_CHANGED',
              path: newPath,
            };
          }
        }

        throw error;
      }
    });
    getExtendedPublicKeyChannel.onRequest(async (params) => {
      // Params example:
      // { path: "1852'/1815'/0'", isTrezor: false, devicePath: null }

      logger.info('[HW-DEBUG] getExtendedPublicKeyChannel');
      const { path, isTrezor, devicePath } = params;

      try {
        if (isTrezor) {
          // We re-initialize the Trezor Connect session to give the user the chance to provide
          // a different passphrase, in case they want to switch to a different
          // hidden wallet or just if they provided a wrong one.
          await reinitTrezorConnect();
          resetTrezorListeners();

          logger.info('[TREZOR-CONNECT] Calling TrezorConnect.getFeatures()');
          const deviceFeatures = await TrezorConnect.getFeatures();

          if (deviceFeatures.success) {
            logger.info(
              '[TREZOR-CONNECT] Calling TrezorConnect.cardanoGetPublicKey()'
            );
            const extendedPublicKeyResponse = await TrezorConnect.cardanoGetPublicKey(
              {
                path: `m/${path}`,
                showOnTrezor: true,
              }
            );

            if (!extendedPublicKeyResponse.success) {
              throw extendedPublicKeyResponse.payload;
            }

            const extendedPublicKey = get(extendedPublicKeyResponse, [
              'payload',
              'node',
            ]);

            return Promise.resolve({
              publicKeyHex: extendedPublicKey.public_key,
              chainCodeHex: extendedPublicKey.chain_code,
            });
          }

          throw new Error('Trezor device not connected');
        }

        if (!devicePath) throw new Error('Ledger device not connected');
        logger.info('[HW-DEBUG] EXPORT KEY');
        return this.withLedgerOperation(devicePath, async (connection) => {
          const extendedPublicKey = await connection.getExtendedPublicKey({
            path: str_to_path(path),
          });
          const deviceSerial = await connection.getSerial();
          return {
            publicKeyHex: extendedPublicKey.publicKeyHex,
            chainCodeHex: extendedPublicKey.chainCodeHex,
            deviceId: deviceSerial.serialHex,
          };
        });
      } catch (error) {
        logger.info('[HW-DEBUG] EXPORT KEY ERROR');
        throw error;
      }
    });
    // @TODO - validityIntervalStart is not working with Cardano App 2.1.0
    signTransactionLedgerChannel.onRequest(async (params) => {
      const {
        inputs,
        outputs,
        protocolMagic,
        fee,
        ttl,
        networkId,
        certificates,
        withdrawals,
        auxiliaryData,
        devicePath,
        signingMode,
        additionalWitnessPaths,
      } = params;

      logger.info('[HW-DEBUG] SIGN Ledger transaction');
      if (!devicePath) throw new Error('Device not connected!');
      return this.withLedgerOperation(devicePath, async (connection) => {
        // The trusted UI still sends the legacy reduced request; task-602 replaces it.
        const request = ({
          signingMode,
          additionalWitnessPaths,
          tx: {
            network: {
              networkId,
              protocolMagic,
            },
            inputs,
            outputs,
            fee,
            ttl,
            certificates,
            withdrawals,
            auxiliaryData,
          },
        } as unknown) as Parameters<AppAda['signTransaction']>[0];
        return ((await connection.signTransaction(
          request
        )) as unknown) as LedgerSignTransactionResponse;
      });
    });

    signTransactionTrezorChannel.onRequest((dataToSign) => {
      logger.info(
        '[TREZOR-CONNECT] Calling TrezorConnect.cardanoSignTransaction()'
      );

      return this.withTrezorOperation(() =>
        TrezorConnect.cardanoSignTransaction(dataToSign)
      );
    });

    signExactHardwareTransactionChannel.onRequest(
      async ({ vendor, ledgerPath, exact }) => {
        const capability = exact.capability;
        const artifactId =
          vendor === 'ledger'
            ? 'ledger-8.0.0-candidate'
            : 'trezor-connect-9.7.2';
        if (
          vendor !== capability.vendor ||
          capability.matrixRevision !== HARDWARE_CONNECTOR_MATRIX_REVISION ||
          capability.artifactId !== artifactId ||
          capability.rowId !== `${vendor}-signTx` ||
          !capability.staticallyRepresentable ||
          !capability.staticGatesPassed ||
          !capability.physicalCertified ||
          !capability.productEnabled ||
          !dappLaunchPolicy.hardwareConnectorEnabled(capability.rowId)
        )
          throw new Error('Hardware exact transaction is not enabled');
        const restored = restoreExactTransaction(exact);
        if (vendor === 'ledger') {
          if (!ledgerPath) throw new Error('Ledger device not connected');
          return this.signExactLedgerTransaction(ledgerPath, restored);
        }
        if (ledgerPath !== undefined)
          throw new Error('Trezor must not receive a Ledger path');
        return this.signExactTrezorTransaction(restored);
      }
    );

    signExactHardwareMessageChannel.onRequest(
      async ({ vendor, ledgerPath, capability, message }) => {
        const version =
          capability.vendor === 'ledger'
            ? capability.appVersion
            : capability.firmwareVersion;
        if (
          vendor !== capability.vendor ||
          capability.matrixRevision !== HARDWARE_CONNECTOR_MATRIX_REVISION ||
          capability.rowId !==
            hardwareConnectorRowId(
              capability.vendor,
              capability.model,
              version || ''
            ) ||
          !capability.physicalCertified ||
          !capability.packagedEnabled ||
          !dappLaunchPolicy.hardwareConnectorEnabled(capability.rowId)
        )
          throw new Error('Hardware connector is not enabled');
        if (vendor === 'ledger') {
          if (!ledgerPath) throw new Error('Ledger device not connected');
          return this.signLedgerMessage(ledgerPath, message);
        }
        if (ledgerPath !== undefined)
          throw new Error('Trezor must not receive a Ledger path');
        return this.signTrezorMessage(message);
      }
    );

    resetTrezorActionChannel.onRequest(async () => {
      logger.info('[TREZOR-CONNECT] Called TrezorConnect.cancel()');
      this.cancelTrezorOperation();
    });
  };
}

export const hardwareWalletService = new HardwareWalletService();
