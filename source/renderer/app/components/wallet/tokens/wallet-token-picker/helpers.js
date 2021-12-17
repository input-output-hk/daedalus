// @flow
export type ScrollPosition = 'top' | 'middle' | 'bottom';

export const ScrollPositionEnum: EnumMap<string, ScrollPosition> = {
  TOP: 'top',
  MIDDLE: 'middle',
  BOTTOM: 'bottom',
};

const isScrollAtTop = (element: HTMLElement) => element.scrollTop === 0;
const isScrollAtBottom = (element: HTMLElement) =>
  element.scrollHeight - element.scrollTop === element.clientHeight;

export const getScrollPosition = (element: EventTarget): ScrollPosition => {
  if (!(element instanceof HTMLElement)) {
    return ScrollPositionEnum.TOP;
  }

  if (isScrollAtTop(element)) {
    return ScrollPositionEnum.TOP;
  }

  if (isScrollAtBottom(element)) {
    return ScrollPositionEnum.BOTTOM;
  }

  return ScrollPositionEnum.MIDDLE;
};
