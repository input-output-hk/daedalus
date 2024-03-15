// @flow
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
  className?: string,
  disabled: boolean,
  error?: string | Element<any>,
  label?: string | Element<any>,
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
  placeholder?: string,
  rows: number,
  textareaRef?: ElementRef<'textarea'>,
  theme: Object,
  themeId: string,
  value: string,
};

export const TextAreaSkin = (props: Props) => {
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
};
