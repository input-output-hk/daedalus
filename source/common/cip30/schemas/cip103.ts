import type { TransactionSignatureRequest } from '../../types/cip103.types';
import { parseDappCip30GatewayRequest } from './index';

export type ValidatedTransactionSignatureRequest = Readonly<{
  cbor: string;
  partialSign: boolean;
}>;

export const parseCip103SignRequest = (
  value: unknown
): readonly ValidatedTransactionSignatureRequest[] => {
  const request = parseDappCip30GatewayRequest({
    method: 'api.cip103.signTxs',
    args: [value],
  });
  const [items] = request.args as [TransactionSignatureRequest[]];
  return Object.freeze(
    items.map(({ cbor, partialSign }) =>
      Object.freeze({ cbor, partialSign: partialSign ?? false })
    )
  );
};

export const parseCip103SubmitRequest = (value: unknown): readonly string[] => {
  const request = parseDappCip30GatewayRequest({
    method: 'api.cip103.submitTxs',
    args: [value],
  });
  const [items] = request.args as [string[]];
  return Object.freeze(items.slice());
};
