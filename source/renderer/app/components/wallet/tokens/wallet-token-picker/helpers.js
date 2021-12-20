// @flow
import { messages } from './WalletTokenPicker.messages';
import {
  MAX_TOKENS,
  ScrollPositionEnum,
  FilterSelectOptionsEnum,
} from './const';
import type { Intl } from '../../../../types/i18nTypes';
import type { ScrollPosition, CheckBoxes, FilterAssets, Assets } from './types';

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

export const maxTokensArrayToIdMap = (assets: Assets) => ({
  ...assets.slice(0, MAX_TOKENS).reduce(
    (acc, asset) => ({
      ...acc,
      [asset.uniqueId]: true,
    }),
    ({}: CheckBoxes)
  ),
});

export const filterSelectOptions = (intl: Intl) => [
  {
    label: intl.formatMessage(messages.allTokensLabel),
    value: FilterSelectOptionsEnum.ALL,
  },
  {
    label: intl.formatMessage(messages.favoriteTokensLabel),
    value: FilterSelectOptionsEnum.FAVORITES,
  },
];

export const filterAssets = ({
  assets,
  filter,
  tokenFavorites,
}: FilterAssets): Assets => {
  return assets.filter((asset) => {
    return filter === FilterSelectOptionsEnum.FAVORITES
      ? tokenFavorites[asset.uniqueId]
      : true;
  });
};
