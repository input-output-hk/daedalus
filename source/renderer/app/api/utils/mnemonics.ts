import {
  unscramblePaperWalletMnemonic,
  scramblePaperWalletMnemonic,
  generateMnemonic,
} from '../../utils/crypto';
import { PAPER_WALLET_WRITTEN_WORDS_COUNT } from '../../config/cryptoConfig';

type MnemonicsParams = {
  passphrase: string;
  // 9-word mnemonic
  scrambledInput: string; // 18-word scrambled mnemonic
};
export const unscrambleMnemonics = ({
  passphrase,
  scrambledInput,
}: MnemonicsParams): Array<string> =>
  unscramblePaperWalletMnemonic(passphrase, scrambledInput);
export const scrambleMnemonics = ({
  passphrase,
  scrambledInput,
}: MnemonicsParams): Array<string> =>
  scramblePaperWalletMnemonic(passphrase, scrambledInput);
export const generateAccountMnemonics = (
  numberOfWords: number
): Array<string> => generateMnemonic(numberOfWords).split(' ');
export const generateAdditionalMnemonics = (): Array<string> =>
  generateMnemonic(PAPER_WALLET_WRITTEN_WORDS_COUNT).split(' ');
