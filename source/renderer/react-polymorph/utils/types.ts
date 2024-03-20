// @ts-nocheck
import type { ElementRef } from 'react';
export type ReactElementRef<ElementType = HTMLElement> = {
  current: null | ElementRef<ElementType>;
};
export interface InputEvent {
  target: HTMLInputElement;
  inputType: string;
}
