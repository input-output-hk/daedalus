import { boolean, number, radios, select, text } from '@storybook/addon-knobs';

export const LOADING_KNOB_GROUP = 'Loading';

export const loadingBooleanKnob = (name: string, value: boolean) =>
  boolean(name, value, LOADING_KNOB_GROUP);

export const loadingNumberKnob = (
  name: string,
  value: number,
  options?: Record<string, unknown>
) => number(name, value, options, LOADING_KNOB_GROUP);

export const loadingRadiosKnob = <T extends string>(
  name: string,
  options: Record<string, T>,
  value: T
) => radios(name, options, value, LOADING_KNOB_GROUP) as T;

export const loadingSelectKnob = <T extends string>(
  name: string,
  options: Record<string, T>,
  value: T
) => select(name, options, value, LOADING_KNOB_GROUP) as T;

export const loadingTextKnob = (name: string, value: string) =>
  text(name, value, LOADING_KNOB_GROUP);
