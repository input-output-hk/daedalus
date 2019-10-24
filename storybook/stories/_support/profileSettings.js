// @flow
import {
  DATE_ENGLISH_OPTIONS,
  DATE_JAPANESE_OPTIONS,
  TIME_OPTIONS,
  NUMBER_OPTIONS,
} from '../../../source/renderer/app/config/profileConfig';
import { NUMBER_FORMATS } from '../../../source/common/types/number.types';

export const timeOptions = TIME_OPTIONS.reduce((obj, { label, value }) => {
  obj[label] = value;
  return obj;
}, {});

export const dateOptions = [
  ...DATE_ENGLISH_OPTIONS,
  ...DATE_JAPANESE_OPTIONS,
].reduce((obj, { label, value }) => {
  obj[label] = value;
  return obj;
}, {});

export const numberOptions = NUMBER_OPTIONS.reduce((obj, { value, label }) => {
  obj[label] = NUMBER_FORMATS[value];
  return obj;
}, {});

export const numberDefaultOption = Object.values(NUMBER_FORMATS)[0];
