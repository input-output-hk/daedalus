// @flow
import type { ScrollPosition, CheckBoxes } from './types';
import type { AssetToken } from '../../../../api/assets/types';

export const MAX_TOKENS = 30;

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

export const maxTokensArrayToIdMap = (assets: Array<AssetToken>) => ({
  ...assets.slice(0, MAX_TOKENS).reduce(
    (acc, asset) => ({
      ...acc,
      [asset.uniqueId]: true,
    }),
    ({}: CheckBoxes)
  ),
});
