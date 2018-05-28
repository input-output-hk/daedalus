// @flow
import { generateMnemonic } from '../../utils/crypto';
import type { EtcRecoveryPassphrase } from './types';

export const getEtcAccountRecoveryPhrase = (): EtcRecoveryPassphrase => (
  generateMnemonic().split(' ')
);
