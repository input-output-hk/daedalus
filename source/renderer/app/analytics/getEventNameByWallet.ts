import Wallet from '../domains/Wallet';

export const getEventNameByWallet = (wallet: Wallet) =>
  wallet.isHardwareWallet ? 'Hardware wallet' : 'Software wallet';
