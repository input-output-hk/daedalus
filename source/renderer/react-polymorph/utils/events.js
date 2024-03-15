'use strict';
exports.__esModule = true;
exports.removeEventListenerOnTransitionEnded = exports.addEventListenerOnTransitionEnded = exports.targetIsDescendant = exports.removeWindowListeners = exports.addWindowListeners = exports.removeDocumentListeners = exports.addDocumentListeners = exports.pauseEvent = exports.getTouchPosition = exports.getMousePosition = void 0;
var getMousePosition = function (event) {
  return {
    x: event.pageX - (window.scrollX || window.pageXOffset),
    y: event.pageY - (window.scrollY || window.pageYOffset),
  };
};
exports.getMousePosition = getMousePosition;
var getTouchPosition = function (event) {
  return {
    x: event.touches[0].pageX - (window.scrollX || window.pageXOffset),
    y: event.touches[0].pageY - (window.scrollY || window.pageYOffset),
  };
};
exports.getTouchPosition = getTouchPosition;
var pauseEvent = function (event) {
  event.stopPropagation();
  event.preventDefault();
};
exports.pauseEvent = pauseEvent;
var addDocumentListeners = function (eventMap) {
  for (var key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      document.addEventListener(key, eventMap[key], false);
    }
  }
};
exports.addDocumentListeners = addDocumentListeners;
var removeDocumentListeners = function (eventMap) {
  for (var key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      document.removeEventListener(key, eventMap[key], false);
    }
  }
};
exports.removeDocumentListeners = removeDocumentListeners;
var addWindowListeners = function (eventMap) {
  for (var key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      window.addEventListener(key, eventMap[key]);
    }
  }
};
exports.addWindowListeners = addWindowListeners;
var removeWindowListeners = function (eventMap) {
  for (var key in eventMap) {
    if (Object.prototype.hasOwnProperty.call(eventMap, key)) {
      window.removeEventListener(key, eventMap[key]);
    }
  }
};
exports.removeWindowListeners = removeWindowListeners;
var targetIsDescendant = function (event, parent) {
  var clickedNode = event.target;
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
var addEventListenerOnTransitionEnded = function (element, fn) {
  var eventName = transitionEventNamesFor(element);
  if (!eventName) return false;
  element.addEventListener(eventName, fn);
  return true;
};
exports.addEventListenerOnTransitionEnded = addEventListenerOnTransitionEnded;
var removeEventListenerOnTransitionEnded = function (element, fn) {
  var eventName = transitionEventNamesFor(element);
  if (!eventName) return false;
  element.removeEventListener(eventName, fn);
  return true;
};
exports.removeEventListenerOnTransitionEnded = removeEventListenerOnTransitionEnded;
// constants and helper functions /////////
var TRANSITIONS = {
  transition: 'transitionend',
  OTransition: 'oTransitionEnd',
  MozTransition: 'transitionend',
  WebkitTransition: 'webkitTransitionEnd',
};
/* eslint space-before-function-paren:0 */
function transitionEventNamesFor(element) {
  for (var transition in TRANSITIONS) {
    if (
      element &&
      Object.prototype.hasOwnProperty.call(element.style, transition)
    ) {
      return TRANSITIONS[transition];
    }
  }
}
