// @flow
import xor from 'lodash/xor';
import { messages } from './WalletTokenPicker.messages';
import {
  MAX_TOKENS,
  ScrollPositionEnum,
  FilterSelectOptionsEnum,
} from './const';
import type { Intl } from '../../../../types/i18nTypes';
import type {
  Assets,
  BooleanMap,
  FilterAssets,
  ScrollPosition,
  GetMaxTokensIdsMap,
} from './types';

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

export const getMaxTokensIdsMap = ({
  assetIds,
  previousCheckedIds,
}: GetMaxTokensIdsMap) => {
  const enabledIds = xor(assetIds, previousCheckedIds);
  return enabledIds
    .slice(0, MAX_TOKENS - previousCheckedIds.length)
    .reduce((acc: BooleanMap, element) => ({ ...acc, [element]: true }), {});
};

export const getAssetIds = (assets: Assets) =>
  assets.map<string>(({ uniqueId }) => uniqueId);

export const getCheckedIds = (checkBoxes: BooleanMap) =>
  Object.entries(checkBoxes).reduce(
    (acc: Array<string>, [id, checked]) => (checked ? [...acc, id] : acc),
    []
  );

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
