exports.__esModule = true;
exports.removeEventListenerOnTransitionEnded = exports.addEventListenerOnTransitionEnded = exports.targetIsDescendant = exports.removeWindowListeners = exports.addWindowListeners = exports.removeDocumentListeners = exports.addDocumentListeners = exports.pauseEvent = exports.getTouchPosition = exports.getMousePosition = void 0;
const getMousePosition = function (event) {
  return {
    x: event.pageX - (window.scrollX || window.pageXOffset),
    y: event.pageY - (window.scrollY || window.pageYOffset),
  };
};
exports.getMousePosition = getMousePosition;
const getTouchPosition = function (event) {
  return {
    x: event.touches[0].pageX - (window.scrollX || window.pageXOffset),
    y: event.touches[0].pageY - (window.scrollY || window.pageYOffset),
  };
};
exports.getTouchPosition = getTouchPosition;
const pauseEvent = function (event) {
  event.stopPropagation();
  event.preventDefault();
};
exports.pauseEvent = pauseEvent;
const addDocumentListeners = function (eventMap) {
  for (const key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      document.addEventListener(key, eventMap[key], false);
    }
  }
};
exports.addDocumentListeners = addDocumentListeners;
const removeDocumentListeners = function (eventMap) {
  for (const key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      document.removeEventListener(key, eventMap[key], false);
    }
  }
};
exports.removeDocumentListeners = removeDocumentListeners;
const addWindowListeners = function (eventMap) {
  for (const key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      window.addEventListener(key, eventMap[key]);
    }
  }
};
exports.addWindowListeners = addWindowListeners;
const removeWindowListeners = function (eventMap) {
  for (const key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      window.removeEventListener(key, eventMap[key]);
    }
  }
};
exports.removeWindowListeners = removeWindowListeners;
const targetIsDescendant = function (event, parent) {
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
exports.targetIsDescendant = targetIsDescendant;
const addEventListenerOnTransitionEnded = function (element, fn) {
  const eventName = transitionEventNamesFor(element);
  if (!eventName) return false;
  element.addEventListener(eventName, fn);
  return true;
};
exports.addEventListenerOnTransitionEnded = addEventListenerOnTransitionEnded;
const removeEventListenerOnTransitionEnded = function (element, fn) {
  const eventName = transitionEventNamesFor(element);
  if (!eventName) return false;
  element.removeEventListener(eventName, fn);
  return true;
};
exports.removeEventListenerOnTransitionEnded = removeEventListenerOnTransitionEnded;
// constants and helper functions /////////
const TRANSITIONS = {
  transition: 'transitionend',
  OTransition: 'oTransitionEnd',
  MozTransition: 'transitionend',
  WebkitTransition: 'webkitTransitionEnd',
};
/* eslint space-before-function-paren:0 */
function transitionEventNamesFor(element) {
  for (const transition in TRANSITIONS) {
    if (
      element &&
      Object.prototype.hasOwnProperty.call(element.style, transition)
    ) {
      return TRANSITIONS[transition];
    }
  }
}
