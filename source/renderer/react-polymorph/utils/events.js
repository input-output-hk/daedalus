// @flow
// $FlowFixMe
import type {
  SyntheticMouseEvent,
  SyntheticTouchEvent,
  SyntheticEvent,
  Element,
  ElementRef,
} from 'react';

export const getMousePosition = (event: SyntheticMouseEvent<>) => ({
  x: event.pageX - (window.scrollX || window.pageXOffset),
  y: event.pageY - (window.scrollY || window.pageYOffset),
});

export const getTouchPosition = (event: SyntheticTouchEvent<Element<*>>) => ({
  x: event.touches[0].pageX - (window.scrollX || window.pageXOffset),
  y: event.touches[0].pageY - (window.scrollY || window.pageYOffset),
});

export const pauseEvent = (event: SyntheticEvent<Element<*>>) => {
  event.stopPropagation();
  event.preventDefault();
};

export const addDocumentListeners = (eventMap: {}) => {
  for (const key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      document.addEventListener(key, eventMap[key], false);
    }
  }
};

export const removeDocumentListeners = (eventMap: {}) => {
  for (const key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      document.removeEventListener(key, eventMap[key], false);
    }
  }
};

export const addWindowListeners = (eventMap: {}) => {
  for (const key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      window.addEventListener(key, eventMap[key]);
    }
  }
};

export const removeWindowListeners = (eventMap: {}) => {
  for (const key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      window.removeEventListener(key, eventMap[key]);
    }
  }
};

export const targetIsDescendant = (
  event: SyntheticEvent<HTMLElement>,
  parent: ?ElementRef<*>
) => {
  const clickedNode = event.target;

  // if the node exists,
  // the node is not the given parent,
  // and the node does not contain the parent,
  // then the node is a descendant of the parent
  if (
    clickedNode &&
    parent &&
    parent.contains(clickedNode) &&
    !clickedNode.contains(parent)
  ) {
    return true;
  }

  // otherwise it is not a descendant of the given parent
  return false;
};

export const addEventListenerOnTransitionEnded = (
  element: ElementRef<*>,
  fn: Function
) => {
  const eventName = transitionEventNamesFor(element);
  if (!eventName) return false;
  element.addEventListener(eventName, fn);
  return true;
};

export const removeEventListenerOnTransitionEnded = (
  element: ElementRef<*>,
  fn: Function
) => {
  const eventName = transitionEventNamesFor(element);
  if (!eventName) return false;
  element.removeEventListener(eventName, fn);
  return true;
};

// constants and helper functions /////////

const TRANSITIONS: {
  transition: string,
  OTransition: string,
  MozTransition: string,
  WebkitTransition: string,
} = {
  transition: 'transitionend',
  OTransition: 'oTransitionEnd',
  MozTransition: 'transitionend',
  WebkitTransition: 'webkitTransitionEnd',
};

/* eslint space-before-function-paren:0 */
function transitionEventNamesFor(element: ElementRef<*>) {
  for (const transition in TRANSITIONS) {
    if (
      element &&
      Object.prototype.hasOwnProperty.call(element.style, transition)
    ) {
      return TRANSITIONS[transition];
    }
  }
}
