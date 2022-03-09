import React, { useRef, useCallback } from 'react';

type Props = {
  onChange: (visible: boolean) => void;
};

export const InView = ({ onChange }: Props) => {
  const elementRef = useRef(null);
  const observerRef = useRef(
    new IntersectionObserver((entries) => {
      entries.forEach((entry) => {
        onChange(entry.isIntersecting);
      });
    })
  );

  const setElementRef = useCallback((element) => {
    if (elementRef.current) {
      observerRef.current.unobserve(elementRef.current);
    }

    if (element) {
      observerRef.current.observe(element);
    }

    elementRef.current = element;
  }, []);

  return <div ref={setElementRef} />;
};
