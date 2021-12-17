// @flow
import type { AssetToken } from '../../../../api/assets/types';

export type ScrollPosition = 'top' | 'middle' | 'bottom';

export type CheckBoxes = { [key: string]: boolean };

export type UseCheckboxes = {
  assets: Array<AssetToken>,
};
