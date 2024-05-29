// @ts-nocheck
import React from 'react';
import type { Element } from 'react';
// external libraries
import classnames from 'classnames';
// import utility functions
import { pickDOMProps } from '../../utils/props';

type Props = {
  checked: boolean;
  className: string;
  disabled: boolean;
  onChange: (...args: Array<any>) => any;
  labelLeft: string | Element<any>;
  labelRight: string | Element<any>;
  theme: Record<string, any>;
  themeId: string;
};
export function TogglerSkin(props: Props) {
  const { theme, themeId } = props;
  return (
    <div
      role="presentation"
      aria-hidden
      className={classnames([
        props.className,
        theme[themeId].root,
        props.disabled ? theme[themeId].disabled : null,
      ])}
      onClick={(event) => {
        if (!props.disabled && props.onChange) {
          props.onChange(!props.checked, event);
        }
      }}
    >
      <input
        {...pickDOMProps(props)}
        className={theme[themeId].input}
        readOnly
        type="checkbox"
      />
      <div className={theme[themeId].toggler}>
        <span
          className={classnames([
            theme[themeId].label,
            props.checked ? theme[themeId].checked : null,
          ])}
        >
          {props.labelLeft}
        </span>
        <span
          className={classnames([
            theme[themeId].label,
            props.checked ? null : theme[themeId].checked,
          ])}
        >
          {props.labelRight}
        </span>
      </div>
    </div>
  );
}
