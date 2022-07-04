import Wallet from '../../domains/Wallet';

export const getEventNameFromWallet = (wallet: Wallet) =>
  wallet.isHardwareWallet ? 'Hardware wallet' : 'Software wallet';
