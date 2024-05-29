// @ts-nocheck
import React from 'react';
import type { ElementRef, Element } from 'react';
// external libraries
import classnames from 'classnames';
// components
import { FormField } from '../../components/FormField';
// skins
import { FormFieldSkin } from './FormFieldSkin';
// import utility functions
import { pickDOMProps } from '../../utils/props';

type Props = {
  className?: string;
  disabled: boolean;
  error?: string | Element<any>;
  label?: string | Element<any>;
  onBlur?: (...args: Array<any>) => any;
  onChange?: (...args: Array<any>) => any;
  onFocus?: (...args: Array<any>) => any;
  placeholder?: string;
  rows: number;
  textareaRef?: ElementRef<'textarea'>;
  theme: Record<string, any>;
  themeId: string;
  value: string;
};
export function TextAreaSkin(props: Props) {
  const { theme, themeId } = props;
  return (
    <FormField
      className={props.className}
      disabled={props.disabled}
      label={props.label}
      error={props.error}
      formFieldRef={props.textareaRef}
      skin={FormFieldSkin}
      render={(setFormFieldRef) => (
        <textarea
          ref={setFormFieldRef}
          {...pickDOMProps(props)}
          className={classnames([
            theme[themeId].textarea,
            props.disabled ? theme[themeId].disabled : null,
            props.error ? theme[themeId].errored : null,
          ])}
        />
      )}
    />
  );
}
