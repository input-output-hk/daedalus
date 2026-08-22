import { getAddressNetwork, isAddressForNetwork } from './addressNetwork';

const MAINNET_ADDRESS =
  'addr1qyr53s0h929lksqp5v8rhlveu4skwp8ugdz87ghaswu95v6q9mncexq3sz7phzf5x4yuez5ljkhfauj6puptdtp86ekq8ndej2';
const TESTNET_ADDRESS =
  'addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgsrgxksj';

describe('getAddressNetwork', () => {
  it('reads mainnet and testnet payment addresses from their prefix', () => {
    expect(getAddressNetwork(MAINNET_ADDRESS)).toBe('mainnet');
    expect(getAddressNetwork(TESTNET_ADDRESS)).toBe('testnet');
  });

  it('reads stake addresses too', () => {
    expect(getAddressNetwork('stake1uyehkck0lajq8gldxgp')).toBe('mainnet');
    expect(getAddressNetwork('stake_test1uqevw2xnsc0')).toBe('testnet');
  });

  it('does not mistake a testnet address for a mainnet one', () => {
    // Every testnet prefix begins with its mainnet prefix, so a naive
    // startsWith on 'addr1' in the wrong order would match both.
    expect(getAddressNetwork(TESTNET_ADDRESS)).not.toBe('mainnet');
  });

  it('tolerates case and surrounding whitespace', () => {
    expect(getAddressNetwork(`  ${MAINNET_ADDRESS.toUpperCase()}  `)).toBe(
      'mainnet'
    );
  });

  it('says unknown rather than guessing', () => {
    expect(getAddressNetwork('')).toBe('unknown');
    expect(getAddressNetwork('not an address')).toBe('unknown');
    expect(getAddressNetwork('DdzFFzCqrht_byron_era_address')).toBe('unknown');
    // The value is DRep-supplied, so the guard is for real input rather than
    // for the type system.
    expect(getAddressNetwork(null as unknown as string)).toBe('unknown');
  });
});

describe('isAddressForNetwork', () => {
  it('matches an address to the network it belongs to', () => {
    expect(isAddressForNetwork(MAINNET_ADDRESS, 'mainnet')).toBe(true);
    expect(isAddressForNetwork(TESTNET_ADDRESS, 'preprod')).toBe(true);
    expect(isAddressForNetwork(TESTNET_ADDRESS, 'preview')).toBe(true);
  });

  it('flags the mismatch in both directions', () => {
    expect(isAddressForNetwork(TESTNET_ADDRESS, 'mainnet')).toBe(false);
    expect(isAddressForNetwork(MAINNET_ADDRESS, 'preprod')).toBe(false);
  });

  it('treats the flight build as mainnet', () => {
    expect(isAddressForNetwork(MAINNET_ADDRESS, 'mainnet_flight')).toBe(true);
    expect(isAddressForNetwork(TESTNET_ADDRESS, 'mainnet_flight')).toBe(false);
  });

  it('treats every other network as a testnet', () => {
    expect(isAddressForNetwork(TESTNET_ADDRESS, 'selfnode')).toBe(true);
    expect(isAddressForNetwork(MAINNET_ADDRESS, 'development')).toBe(false);
  });

  it('answers null when there is nothing to compare', () => {
    // An unrecognised prefix is not evidence of a mismatch. Warning on it would
    // put a notice under every address the day a new prefix appears.
    expect(isAddressForNetwork('who knows', 'mainnet')).toBeNull();
    expect(isAddressForNetwork(MAINNET_ADDRESS, null)).toBeNull();
    expect(isAddressForNetwork(MAINNET_ADDRESS, undefined)).toBeNull();
  });
});
