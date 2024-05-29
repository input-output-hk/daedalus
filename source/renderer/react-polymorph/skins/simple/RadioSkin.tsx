// @ts-nocheck
import React from 'react';
import type { Element } from 'react';
// external libraries
import classnames from 'classnames';
// internal utility functions
import { pickDOMProps } from '../../utils/props';

type Props = {
  className: string;
  disabled: boolean;
  selected: boolean;
  onBlur: (...args: Array<any>) => any;
  onChange: (...args: Array<any>) => any;
  onFocus: (...args: Array<any>) => any;
  label: string | Element<any>;
  theme: Record<string, any>;
  themeId: string;
};
export function RadioSkin(props: Props) {
  const {
    theme,
    themeId,
    className,
    disabled,
    selected,
    onChange,
    label,
  } = props;
  return (
    <div
      role="presentation"
      aria-hidden
      className={classnames([
        className,
        theme[themeId].root,
        disabled ? theme[themeId].disabled : null,
        selected ? theme[themeId].selected : null,
      ])}
      onClick={(event) => {
        if (!disabled && onChange) {
          onChange(!selected, event);
        }
      }}
    >
      <input
        {...pickDOMProps(props)}
        className={theme[themeId].input}
        type="radio"
      />
      <div
        className={classnames([
          theme[themeId].circle,
          selected ? theme[themeId].selected : null,
        ])}
      />
      {label ? <label className={theme[themeId].label}>{label}</label> : null}
    </div>
  );
}
