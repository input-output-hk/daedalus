'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getFeatureFromContext = exports.useFeature = void 0;
const react_1 = require('react');
/**
 * Manages the lifecycle of a feature based on its provider component.
 *
 * 1. Starts the feature store when the enclosing component mounts
 * 2. Stops the store on unmount
 * @param feature
 */
const useFeature = (feature) => {
  (0, react_1.useLayoutEffect)(() => {
    // Start feature store before first render is done
    feature.start();
  }, []);
  (0, react_1.useEffect)(() => {
    // Stop store on unmount
    return () => {
      feature.stop();
    };
  }, []);
};
exports.useFeature = useFeature;
/**
 * Throws an error if the feature context hasn't been provided
 * before a component tries to use the feature.
 *
 * @param context
 */
function getFeatureFromContext(context) {
  const instance = (0, react_1.useContext)(context);
  if (!instance) {
    throw new Error(`You need to provide a context before using it.`);
  }
  return instance;
}
exports.getFeatureFromContext = getFeatureFromContext;
//# sourceMappingURL=hooks.js.map
