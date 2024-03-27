'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.useTriggerOnRouteLeave = void 0;
const react_1 = require('react');
const useTriggerOnRouteLeave = ({ enabled, isOnRoute, onLeave }) => {
  const [wasOnRoute, setWasOnRoute] = (0, react_1.useState)(false);
  (0, react_1.useEffect)(() => {
    if (!enabled) {
      return () => {};
    }
    if (isOnRoute) {
      setWasOnRoute(true);
    }
    if (wasOnRoute && !isOnRoute) {
      onLeave();
    }
    return () => {
      if (wasOnRoute) {
        onLeave();
      }
    };
  }, [isOnRoute, wasOnRoute]);
};
exports.useTriggerOnRouteLeave = useTriggerOnRouteLeave;
//# sourceMappingURL=hooks.js.map
