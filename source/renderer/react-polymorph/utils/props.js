// @flow
import filterInvalidDOMProps from 'filter-invalid-dom-props';
import type { ComponentType } from 'react';

// filters out / prevents invalid props from being rendered to the dom
// which would generate an error/warning
export const pickDOMProps = filterInvalidDOMProps;

export const composeFunctions = (...fns: [Function, Function]) => (
  ...args: [any, any]
) => fns.forEach((fn) => fn && fn(...args));

export const numberToPx = (val: string | number) =>
  typeof val === 'number' ? `${val}px` : val;

export const hasProperty = (obj: Object, property: ?string) =>
  Object.prototype.hasOwnProperty.call(obj, property);

export const getDisplayName = (Component: ComponentType<any>) =>
  Component.displayName || Component.name;
