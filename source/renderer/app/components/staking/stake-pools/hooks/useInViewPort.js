'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.useInViewPort = void 0;
const react_1 = require('react');
const useInViewPort = () => {
  const [isInViewport, setIsInViewport] = (0, react_1.useState)(true);
  const targetRef = (0, react_1.useRef)(null);
  const observerRef = (0, react_1.useRef)(
    new IntersectionObserver((entries) => {
      entries.forEach((entry) => {
        setIsInViewport(entry.isIntersecting);
      });
    })
  );
  // React will call the ref callback twice. Once when the component mounts and again when it unmounts.
  // https://reactjs.org/docs/hooks-faq.html#how-can-i-measure-a-dom-node
  const setTargetRef = (0, react_1.useCallback)((target) => {
    if (targetRef.current) {
      observerRef.current.unobserve(targetRef.current);
    }
    if (target) {
      observerRef.current.observe(target);
    }
    targetRef.current = target;
  }, []);
  return { isInViewport, setTargetRef };
};
exports.useInViewPort = useInViewPort;
//# sourceMappingURL=useInViewPort.js.map
