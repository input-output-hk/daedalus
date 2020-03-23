//@flow
import * as bech32 from 'bech32';
import { Address } from 'cardano-js';
import { AddressGroup } from 'cardano-js/dist/Address/AddressGroup';

const BYRON_BECH32_PUBKEY_LENGTH = 32 // in bytes
const BECH32_DECODE_LIMIT = 128;

const ChainSettings = {
  mainnet: 'mainnet',
  testnet: 'testnet'
};

const ByronBech32AddressKind = {
  spendingAddress: 0,
  scriptAddress: 1,
  redeemAddress: 2,
  stakingAddress: 3,
  groupedAddress: 4, // @TODO - check needed
}

const ByronBech32AddressName = {
  spending: 'spending',
  script: 'script',
  redeem: 'redeem',
  staking: 'staking',
  grouped: 'grouped', // @TODO - check needed
}

type ChainSettingsType = 'mainnet' | 'testnet';
type DecodeByronBech32AddressType = {
  kind: number,
  network: string,
};

const introspectAddress = (address: string) => {
  try {
    const decodedAddress = bech32.decode(address, BECH32_DECODE_LIMIT);
    const bytes = Buffer.from(bech32.fromWords(decodedAddress.words))
    const byronBech32Address = decodeByronBech32Address(bytes);

    if (byronBech32Address) {
      return { ...byronBech32Address, kind: translateByronBech32AddressKind(byronBech32Address.kind) }
    }
  } catch (error) {
    throw new Error(`${address} is not a valid Byron Address`)
  }
}

// @TODO - check needed
const translateByronBech32AddressKind = (kind: ByronBech32AddressKind) => {
  switch (kind) {
    case 0 :
      return ByronBech32AddressName.spending
    case 1 :
      return ByronBech32AddressName.script
    case 2 :
      return ByronBech32AddressName.redeem
    case 3 :
      return ByronBech32AddressName.staking
    case 4 :
      return ByronBech32AddressName.grouped
    default :
      throw new Error('Unrecognised ByronBech32AddressKind')
  }
}

const decodeByronBech32Address = (bytes : Buffer) : (DecodeByronBech32AddressType | null) => {
  const kind = bytes[0] & 0b01111111;
  const network = (bytes[0] & 0b10000000) ? ChainSettings.testnet : ChainSettings.mainnet;
  // TODO - check needed
  // switch (kind) {
  //   case ByronBech32AddressKind.spending:
  //     if (bytes.byteLength !== (1 + BYRON_BECH32_PUBKEY_LENGTH)) {
  //       return null
  //     }
  //     break
  //
  //   case ByronBech32AddressKind.script:
  //     if (bytes.byteLength !== (1 + 2 * BYRON_BECH32_PUBKEY_LENGTH)) {
  //       return null
  //     }
  //     break
  //
  //   case ByronBech32AddressKind.redeem:
  //     if (bytes.byteLength !== (1 + BYRON_BECH32_PUBKEY_LENGTH)) {
  //       return null
  //     }
  //     break
  //
  //   case ByronBech32AddressKind.staking:
  //     if (bytes.byteLength !== (1 + BYRON_BECH32_PUBKEY_LENGTH)) {
  //       return null
  //     }
  //     break
  //
  //   case ByronBech32AddressKind.staking:
  //     if (bytes.grouped !== (1 + 2 * BYRON_BECH32_PUBKEY_LENGTH)) {
  //       return null
  //     }
  //     break
  //
  //   default:
  //     return null
  // }
  return { kind, network };
}

export const isValidByronAddress = (address: string, expectedNetwork: ChainSettingsType) => {
  // Try to validate Byron "Bech32" address
  try {
    const decodedAddress = bech32.decode(address, BECH32_DECODE_LIMIT);
    const bytes = Buffer.from(bech32.fromWords(decodedAddress.words))
    const { kind, network } = introspectAddress(address);

    if (!Object.values(ByronBech32AddressName).includes(kind)) return false;
    return network === expectedNetwork;
  } catch (error) {
    if (error.message.substring(0, 17) !== 'Mixed-case string') throw error
  }

  // Try to validate Byron "Base58" address
  try {
    return Address.Util.isAddress(address, expectedNetwork, AddressGroup.byron);
  } catch (error) {
    return error;
  }
}