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
