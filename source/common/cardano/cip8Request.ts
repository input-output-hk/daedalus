import { normalizeCip30Address } from './cip30Serialization';

const MAX_PAYLOAD_BYTES = 65_536;
export const cip8HexBytes = /^(?:[0-9a-f]{2})*$/;
const hash28 = /^[0-9a-f]{56}$/;

export class Cip8Error extends Error {
  public constructor() {
    super('Invalid CIP-8 data signature');
    this.name = 'Cip8Error';
  }
}

export class Cip8AddressNotPKError extends Cip8Error {
  public constructor() {
    super();
    this.name = 'Cip8AddressNotPKError';
  }
}

export const invalidCip8 = (): never => {
  throw new Cip8Error();
};

export const decodeCip8Hex = (value: unknown, allowEmpty = false): Buffer => {
  if (
    typeof value !== 'string' ||
    !cip8HexBytes.test(value) ||
    (!allowEmpty && value.length === 0)
  )
    return invalidCip8();
  return Buffer.from(value, 'hex');
};

export type Cip8CredentialKind = 'payment' | 'stake' | 'drep';

export type Cip8ExpectedRequest = Readonly<{
  address: string;
  credentialKind: Cip8CredentialKind;
  credential: Buffer;
  protectedAddress: Buffer;
  payload: Buffer;
}>;

export type PrepareCip8RequestOptions = Readonly<{
  networkId: 0 | 1;
  drepCredential?: string;
}>;

export const prepareCip8Request = (
  address: string,
  payload: string,
  { networkId, drepCredential }: PrepareCip8RequestOptions
): Cip8ExpectedRequest => {
  try {
    const payloadBytes = decodeCip8Hex(payload, true);
    if (payloadBytes.length > MAX_PAYLOAD_BYTES) return invalidCip8();
    if (drepCredential !== undefined && !hash28.test(drepCredential))
      return invalidCip8();

    if (hash28.test(address)) {
      if (drepCredential === undefined || address !== drepCredential)
        return invalidCip8();
      const credential = Buffer.from(address, 'hex');
      return {
        address,
        credentialKind: 'drep',
        credential,
        protectedAddress: Buffer.from(credential),
        payload: payloadBytes,
      };
    }

    const normalized = normalizeCip30Address(address, networkId);
    const addressBytes = Buffer.from(normalized, 'hex');
    const type = addressBytes[0] >> 4;
    const credential = addressBytes.subarray(1, 29);
    if (credential.length !== 28) return invalidCip8();

    let credentialKind: Cip8CredentialKind;
    let protectedAddress = addressBytes;
    if (type === 14) {
      credentialKind = 'stake';
    } else if (type === 0 || type === 2 || type === 4 || type === 6) {
      credentialKind = 'payment';
      if (
        type === 6 &&
        drepCredential !== undefined &&
        credential.toString('hex') === drepCredential
      ) {
        credentialKind = 'drep';
        protectedAddress = credential;
      }
    } else if ([1, 3, 5, 7, 15].includes(type)) {
      throw new Cip8AddressNotPKError();
    } else {
      return invalidCip8();
    }

    return {
      address: normalized,
      credentialKind,
      credential: Buffer.from(credential),
      protectedAddress: Buffer.from(protectedAddress),
      payload: payloadBytes,
    };
  } catch (error) {
    if (error instanceof Cip8Error) throw error;
    throw new Cip8Error();
  }
};
