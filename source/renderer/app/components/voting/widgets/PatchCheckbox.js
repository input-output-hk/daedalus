// @flow
import React from 'react';
import type { ComponentType, Element } from 'react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import stylesOverride from './PatchCheckbox.scss';

type Props = {
  checked: boolean,
  className?: string,
  disabled?: boolean,
  label?: string | Element<any>,
  labelLeft?: string | Element<any>,
  labelRight?: string | Element<any>,
  onChange?: Function,
  onBlur?: Function,
  onFocus?: Function,
  skin?: ComponentType<any>,
  theme?: ?Object, // will take precedence over theme in context if passed
  themeId?: string,
  themeOverrides?: Object,
};

export function PatchCheckbox(props: Props) {
  return <Checkbox {...props} themeOverrides={stylesOverride} />;
}
