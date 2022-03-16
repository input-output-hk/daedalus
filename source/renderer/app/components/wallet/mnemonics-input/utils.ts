import { times } from 'lodash';

export const createMnemonicsInputInitialValue = (mnemonicsCount: number) =>
  times(mnemonicsCount, () => '');
