// @flow
import { omit } from 'lodash';
import React from 'react';
import type { Element } from 'react';
// external libraries
import classnames from 'classnames';

// internal utility functions
import { pickDOMProps } from '../../utils/props';

type Props = {
  checked: boolean,
  className: string,
  disabled: boolean,
  onChange: Function,
  label: string | Element<any>,
  theme: Object,
  themeId: string,
};

export function CheckboxSkin(props: Props) {
  return (
    <div
      role="presentation"
      aria-hidden
      className={classnames([
        props.className,
        props.theme[props.themeId].root,
        props.disabled ? props.theme[props.themeId].disabled : null,
        props.checked ? props.theme[props.themeId].checked : null,
      ])}
      onClick={(event) => {
        if (!props.disabled && props.onChange) {
          props.onChange(!props.checked, event);
        }
      }}
    >
      <input
        {...pickDOMProps(omit(props, 'onChange'))}
        className={props.theme[props.themeId].input}
        type="checkbox"
        readOnly
      />
      <div
        className={classnames([
          props.theme[props.themeId].check,
          props.checked ? props.theme[props.themeId].checked : null,
        ])}
      />
      {props.label && (
        <label className={props.theme[props.themeId].label}>
          {props.label}
        </label>
      )}
    </div>
  );
}
