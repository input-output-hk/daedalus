import { isEqual } from 'lodash';
import { RTS_FLAGS } from '../config';

export const containsRTSFlags = (flags: string[]) => isEqual(flags, RTS_FLAGS);
