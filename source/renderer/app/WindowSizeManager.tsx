import { useEffect } from 'react';

type Props = {
  minScreenHeight: number;
  children: Node;
};

function updateMinScreenHeight(minScreenHeight: number) {
  const rootWindowEl = document.getElementById('root');
  if (rootWindowEl) {
    rootWindowEl.style.minHeight = minScreenHeight.toString();
  }
}

export default function WindowSizeManager(props: Props) {
  useEffect(() => {
    updateMinScreenHeight(props.minScreenHeight);
  }, [props.minScreenHeight]);
  return null;
}
