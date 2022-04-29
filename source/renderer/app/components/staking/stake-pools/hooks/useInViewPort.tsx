import { useRef, useState, useCallback } from 'react';

export const useInViewPort = () => {
  const [isInViewport, setIsInViewport] = useState(true);
  const targetRef = useRef(null);
  const observerRef = useRef(
    new IntersectionObserver((entries) => {
      entries.forEach((entry) => {
        setIsInViewport(entry.isIntersecting);
      });
    })
  );

  // React will call the ref callback twice. Once when the component mounts and again when it unmounts.
  // https://reactjs.org/docs/hooks-faq.html#how-can-i-measure-a-dom-node
  const setTargetRef = useCallback((target: HTMLElement) => {
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
