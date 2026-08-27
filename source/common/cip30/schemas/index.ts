import Ajv, { ValidateFunction } from 'ajv';
import contractManifest from '../contracts/contract-manifest.json';
import commonSchema from '../contracts/schemas/common.schema.json';
import envelopeSchema from '../contracts/schemas/envelope.schema.json';
import errorsSchema from '../contracts/schemas/errors.schema.json';
import { DappCip30Rejection, invalidRequest } from '../errors';
import {
  DAPP_CIP30_METHODS,
  DappApprovalDecision,
  DappCip30GatewayRequest,
  DappCip30Method,
  DappCip30ResultEnvelope,
} from '../wire';

const SCHEMA_BASE = 'https://daedaluswallet.io/schemas/cip30/';

type ArgumentContract = {
  required: boolean;
  undefinedMeansOmitted: boolean;
  schemaRef?: string;
  schema?: object;
  decodedByteMaximum?: number;
  decodedByteMaximumPerItem?: number;
};

type MethodContract = {
  path: DappCip30Method;
  minArgs: number;
  maxArgs: number;
  args: ArgumentContract[];
  successSchemaRef?: string;
  successSchema?: object;
  rejections: string[];
};

const methods = contractManifest.methods as MethodContract[];
const methodContracts = new Map(methods.map((method) => [method.path, method]));

if (
  methods.length !== DAPP_CIP30_METHODS.length ||
  methods.some((method, index) => method.path !== DAPP_CIP30_METHODS[index])
) {
  throw new Error('CIP-30 method contract does not match the frozen manifest');
}

const ajv = new Ajv({ schemaId: 'auto' });
ajv.addSchema(commonSchema);
ajv.addSchema(errorsSchema);
ajv.addSchema(envelopeSchema);

const absoluteRef = (ref: string): string =>
  ref.startsWith('https://') ? ref : `${SCHEMA_BASE}${ref}`;
const compile = (schemaRef?: string, schema?: object): ValidateFunction =>
  ajv.compile(
    schemaRef ? { $ref: absoluteRef(schemaRef) } : (schema as object)
  );

const argumentValidators = new Map(
  methods.map((method) => [
    method.path,
    method.args.map((argument) => compile(argument.schemaRef, argument.schema)),
  ])
);
const resultValidators = new Map(
  methods.map((method) => [
    method.path,
    compile(method.successSchemaRef, method.successSchema),
  ])
);
const validateEnvelope = compile('envelope.json#/definitions/resultEnvelope');

const rejectionTypeByContract: Record<string, DappCip30Rejection['type']> = {
  apiError: 'api-error',
  paginateError: 'paginate-error',
  txSignError: 'tx-sign-error',
  dataSignError: 'data-sign-error',
  txSendError: 'tx-send-error',
  cip103SubmitError: 'cip103-submit-error',
};

function isPlainArray(value: unknown[], allowUndefined = false): boolean {
  if (Object.getOwnPropertySymbols(value).length !== 0) return false;
  const descriptors = Object.getOwnPropertyDescriptors(value);
  const names = Object.keys(descriptors).filter((name) => name !== 'length');
  return (
    names.length === value.length &&
    names.every((name, index) => {
      const descriptor = descriptors[name];
      return (
        name === String(index) &&
        descriptor.enumerable === true &&
        Object.prototype.hasOwnProperty.call(descriptor, 'value') &&
        ((allowUndefined && descriptor.value === undefined) ||
          isPlainData(descriptor.value))
      );
    })
  );
}

function isPlainData(value: unknown): boolean {
  if (
    value === null ||
    typeof value === 'string' ||
    typeof value === 'boolean'
  ) {
    return true;
  }
  if (typeof value === 'number') return Number.isFinite(value);
  if (typeof value !== 'object') return false;
  if (Array.isArray(value)) return isPlainArray(value);
  if (Object.getOwnPropertySymbols(value).length !== 0) return false;

  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) return false;
  return Object.values(Object.getOwnPropertyDescriptors(value)).every(
    (descriptor) =>
      descriptor.enumerable === true &&
      Object.prototype.hasOwnProperty.call(descriptor, 'value') &&
      isPlainData(descriptor.value)
  );
}

const ownData = (value: unknown, keys: string[]): value is object => {
  if (value === null || typeof value !== 'object' || Array.isArray(value)) {
    return false;
  }
  if (Object.getPrototypeOf(value) !== Object.prototype) return false;
  if (Object.getOwnPropertySymbols(value).length !== 0) return false;
  const descriptors = Object.getOwnPropertyDescriptors(value);
  return (
    Object.keys(descriptors).length === keys.length &&
    keys.every(
      (key) =>
        descriptors[key]?.enumerable === true &&
        Object.prototype.hasOwnProperty.call(descriptors[key], 'value')
    )
  );
};

const isMethod = (value: unknown): value is DappCip30Method =>
  typeof value === 'string' && methodContracts.has(value as DappCip30Method);

export const hasDappCip30MethodSchema = (method: DappCip30Method): boolean =>
  methodContracts.has(method);

const withinDecodedLimit = (
  value: unknown,
  maximum: number | undefined
): boolean =>
  maximum === undefined ||
  (typeof value === 'string' && value.length / 2 <= maximum);

export const parseDappCip30GatewayRequest = (
  value: unknown
): DappCip30GatewayRequest => {
  if (!ownData(value, ['method', 'args'])) throw invalidRequest();
  const { method, args } = value as { method: unknown; args: unknown };
  if (!isMethod(method) || !Array.isArray(args) || !isPlainArray(args, true)) {
    throw invalidRequest();
  }

  const contract = methodContracts.get(method) as MethodContract;
  if (args.length < contract.minArgs || args.length > contract.maxArgs) {
    throw invalidRequest();
  }

  const validators = argumentValidators.get(method) as ValidateFunction[];
  contract.args.forEach((argument, index) => {
    const argumentValue = args[index];
    if (argumentValue === undefined) {
      if (index < args.length && !argument.undefinedMeansOmitted) {
        throw invalidRequest();
      }
      if (argument.required) throw invalidRequest();
      return;
    }
    if (!isPlainData(argumentValue) || !validators[index](argumentValue)) {
      throw invalidRequest();
    }
    if (!withinDecodedLimit(argumentValue, argument.decodedByteMaximum)) {
      throw invalidRequest();
    }
    if (argument.decodedByteMaximumPerItem !== undefined) {
      const items = argumentValue as Array<string | { cbor: string }>;
      if (
        items.some(
          (item) =>
            !withinDecodedLimit(
              typeof item === 'string' ? item : item.cbor,
              argument.decodedByteMaximumPerItem
            )
        )
      ) {
        throw invalidRequest();
      }
    }
  });

  return { method, args } as DappCip30GatewayRequest;
};

export const parseDappCip30ResultEnvelope = <M extends DappCip30Method>(
  method: M,
  value: unknown
): DappCip30ResultEnvelope => {
  if (!isPlainData(value) || !validateEnvelope(value)) {
    throw new Error('Invalid CIP-30 result envelope');
  }
  const envelope = value as DappCip30ResultEnvelope;
  const contract = methodContracts.get(method) as MethodContract;
  if (envelope.status === 'fulfilled') {
    if (!(resultValidators.get(method) as ValidateFunction)(envelope.value)) {
      throw new Error('Invalid CIP-30 result value');
    }
  } else {
    const allowed = contract.rejections.map(
      (rejection) => rejectionTypeByContract[rejection]
    );
    if (!allowed.includes(envelope.rejection.type)) {
      throw new Error('Invalid CIP-30 rejection for method');
    }
  }
  return envelope;
};

export const createDappCip30FulfilledEnvelope = <M extends DappCip30Method>(
  method: M,
  value: unknown
): DappCip30ResultEnvelope =>
  parseDappCip30ResultEnvelope(method, { status: 'fulfilled', value });

export const createDappCip30RejectedEnvelope = <M extends DappCip30Method>(
  method: M,
  rejection: DappCip30Rejection
): DappCip30ResultEnvelope =>
  parseDappCip30ResultEnvelope(method, { status: 'rejected', rejection });

export const parseDappApprovalDecision = (
  value: unknown
): DappApprovalDecision => {
  const hasPassphrase =
    !!value &&
    typeof value === 'object' &&
    Object.prototype.hasOwnProperty.call(value, 'passphrase');
  if (
    !ownData(
      value,
      hasPassphrase
        ? ['requestId', 'approved', 'passphrase']
        : ['requestId', 'approved']
    )
  )
    throw invalidRequest();
  const { requestId, approved, passphrase } = value as DappApprovalDecision;
  if (
    typeof requestId !== 'string' ||
    requestId.length === 0 ||
    typeof approved !== 'boolean' ||
    (hasPassphrase &&
      (!approved || typeof passphrase !== 'string' || passphrase.length === 0))
  )
    throw invalidRequest();
  return {
    requestId,
    approved,
    ...(hasPassphrase ? { passphrase } : {}),
  };
};
