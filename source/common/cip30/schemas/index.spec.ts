import {
  createDappCip30FulfilledEnvelope,
  createDappCip30RejectedEnvelope,
  parseDappApprovalDecision,
  parseDappCip30GatewayRequest,
  parseDappCip30ResultEnvelope,
} from '.';

const invalidRequest = { code: -1, info: 'Invalid request' };
const request = (method: string, args: unknown[] = []) => ({ method, args });
const expectInvalid = (callback: () => unknown): void => {
  let caught: unknown;
  try {
    callback();
  } catch (error) {
    caught = error;
  }
  expect(caught).toEqual(invalidRequest);
};

describe('CIP-30 runtime schemas', () => {
  it('rejects unknown methods, properties, accessors, and inherited input', () => {
    const accessor = { method: 'api.getNetworkId', args: [] } as Record<
      string,
      unknown
    >;
    Object.defineProperty(accessor, 'extra', {
      enumerable: true,
      get: () => 1,
    });
    const inherited = Object.create({ extra: true });
    Object.assign(inherited, request('api.getNetworkId'));
    const getter = jest.fn(() => undefined);
    const accessorArgs: unknown[] = [];
    Object.defineProperty(accessorArgs, '0', {
      enumerable: true,
      get: getter,
    });
    accessorArgs.length = 1;

    [
      request('api.unknown'),
      { ...request('api.getNetworkId'), extra: true },
      accessor,
      inherited,
      request('api.getCollateral', [{ amount: '00', extra: true }]),
      request('api.getNetworkId', [undefined]),
      request('api.getNetworkId', accessorArgs),
    ].forEach((value) =>
      expectInvalid(() => parseDappCip30GatewayRequest(value))
    );
    expect(getter).not.toHaveBeenCalled();
  });

  it('accepts optional undefined but rejects unsupported JavaScript values', () => {
    expect(
      parseDappCip30GatewayRequest(request('provider.enable', [undefined]))
    ).toEqual(request('provider.enable', [undefined]));

    [
      NaN,
      Infinity,
      () => undefined,
      Symbol('value'),
      BigInt(1),
    ].forEach((value) =>
      expectInvalid(() =>
        parseDappCip30GatewayRequest(
          request('provider.enable', [{ extensions: [{ cip: value }] }])
        )
      )
    );
  });

  it('enforces decoded-byte, batch, and pagination boundaries', () => {
    const maximum = '00'.repeat(65536);
    expect(() =>
      parseDappCip30GatewayRequest(request('api.signTx', [maximum]))
    ).not.toThrow();
    expectInvalid(() =>
      parseDappCip30GatewayRequest(request('api.signTx', [`${maximum}00`]))
    );

    expect(() =>
      parseDappCip30GatewayRequest(
        request('api.cip103.submitTxs', [Array(50).fill('00')])
      )
    ).not.toThrow();
    expectInvalid(() =>
      parseDappCip30GatewayRequest(
        request('api.cip103.submitTxs', [Array(51).fill('00')])
      )
    );

    expect(() =>
      parseDappCip30GatewayRequest(
        request('api.getUsedAddresses', [{ page: 0, limit: 100 }])
      )
    ).not.toThrow();
    expectInvalid(() =>
      parseDappCip30GatewayRequest(
        request('api.getUsedAddresses', [{ page: 0, limit: 101 }])
      )
    );
  });

  it('validates method-specific fulfilled and rejected envelopes', () => {
    expect(createDappCip30FulfilledEnvelope('api.getNetworkId', 1)).toEqual({
      status: 'fulfilled',
      value: 1,
    });
    expect(() =>
      createDappCip30FulfilledEnvelope('api.getNetworkId', '1')
    ).toThrow('Invalid CIP-30 result value');
    expect(() =>
      createDappCip30RejectedEnvelope('api.getNetworkId', {
        type: 'tx-sign-error',
        value: { code: 2, info: 'declined' },
      })
    ).toThrow('Invalid CIP-30 rejection for method');

    expect(
      parseDappCip30ResultEnvelope('api.cip103.submitTxs', {
        status: 'rejected',
        rejection: {
          type: 'cip103-submit-error',
          value: [
            'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
            { code: 2, info: 'failed' },
          ],
        },
      })
    ).toEqual(expect.objectContaining({ status: 'rejected' }));
  });

  it('accepts only request identity and decision from trusted approval', () => {
    expect(
      parseDappApprovalDecision({ requestId: 'request', approved: true })
    ).toEqual({ requestId: 'request', approved: true });
    expectInvalid(() =>
      parseDappApprovalDecision({
        requestId: 'request',
        approved: true,
        args: ['replacement'],
      })
    );
  });
});
