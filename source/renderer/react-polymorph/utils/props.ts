// @ts-nocheck
import filterInvalidDOMProps from 'filter-invalid-dom-props';
import type { ComponentType } from 'react';
// filters out / prevents invalid props from being rendered to the dom
// which would generate an error/warning
export const pickDOMProps = filterInvalidDOMProps;
export const composeFunctions = (
  ...fns: [(...args: Array<any>) => any, (...args: Array<any>) => any]
) => (...args: [any, any]) => fns.forEach((fn) => fn && fn(...args));
export const numberToPx = (val: string | number) =>
  typeof val === 'number' ? `${val}px` : val;
export const hasProperty = (
  obj: Record<string, any>,
  property: string | null | undefined
) => Object.prototype.hasOwnProperty.call(obj, property);
export const getDisplayName = (Component: ComponentType<any>) =>
  Component.displayName || Component.name;
