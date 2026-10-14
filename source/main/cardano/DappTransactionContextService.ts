import {
  ContextExpectation,
  DappNetwork,
  reconcileTransactionContext,
  TransactionContextSnapshot,
} from '../../common/cardano/transactionContext';

export type DappContextBinding = Readonly<{
  walletId: string;
  network: DappNetwork;
  generation: number;
}>;
export type DappContextTransport = (
  _path: string,
  _body: Readonly<{
    revision: 1;
    network: Readonly<{
      network_id: 0 | 1;
      network_magic: number;
      genesis_hash: string;
    }>;
    transactions: readonly string[];
  }>
) => Promise<unknown>;

export type DappContextFailure =
  | 'invalid_request'
  | 'context_conflict'
  | 'account_changed'
  | 'context_unavailable'
  | 'internal_error';

export class DappTransactionContextServiceError extends Error {
  public constructor(public readonly failure: DappContextFailure) {
    super(failure);
    this.name = 'DappTransactionContextServiceError';
  }
}

const sameNetwork = (left: DappNetwork, right: DappNetwork): boolean =>
  left.networkId === right.networkId &&
  left.networkMagic === right.networkMagic &&
  left.genesisHash === right.genesisHash;

const assertBinding = (
  actual: DappContextBinding,
  expected: DappContextBinding
): void => {
  if (
    actual.walletId !== expected.walletId ||
    actual.generation !== expected.generation ||
    !sameNetwork(actual.network, expected.network)
  )
    throw new DappTransactionContextServiceError('account_changed');
};

const backendFailure = (error: unknown): DappTransactionContextServiceError => {
  if (error instanceof DappTransactionContextServiceError) return error;
  const candidate = error as {
    response?: { status?: unknown; data?: unknown };
  };
  const status = candidate?.response?.status;
  const data = candidate?.response?.data as { code?: unknown } | undefined;
  const code = data?.code;
  if (status === 400 && code === 'dapp_invalid_request')
    return new DappTransactionContextServiceError('invalid_request');
  if (status === 400 && code === 'dapp_context_conflict')
    return new DappTransactionContextServiceError('context_conflict');
  if (status === 409 && code === 'dapp_account_changed')
    return new DappTransactionContextServiceError('account_changed');
  if (status === 503 && code === 'dapp_context_unavailable')
    return new DappTransactionContextServiceError('context_unavailable');
  return new DappTransactionContextServiceError('internal_error');
};

export class DappTransactionContextService {
  private readonly transport: DappContextTransport;
  private readonly currentBinding: () => DappContextBinding;

  public constructor(
    transport: DappContextTransport,
    currentBinding: () => DappContextBinding
  ) {
    this.transport = transport;
    this.currentBinding = currentBinding;
  }

  public async capture(
    expected: DappContextBinding,
    transactions: readonly string[]
  ): Promise<TransactionContextSnapshot> {
    assertBinding(this.currentBinding(), expected);
    const expectation: ContextExpectation = {
      walletId: expected.walletId,
      network: expected.network,
      transactions,
    };
    let response: unknown;
    try {
      response = await this.transport(
        `/v2/wallets/${encodeURIComponent(
          expected.walletId
        )}/transaction-context`,
        {
          revision: 1,
          network: {
            network_id: expected.network.networkId,
            network_magic: expected.network.networkMagic,
            genesis_hash: expected.network.genesisHash,
          },
          transactions,
        }
      );
    } catch (error) {
      throw backendFailure(error);
    }
    assertBinding(this.currentBinding(), expected);
    try {
      return reconcileTransactionContext(response, expectation);
    } catch (_error) {
      throw new DappTransactionContextServiceError('internal_error');
    }
  }
}
