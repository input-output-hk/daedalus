// @flow
import React from 'react';
import type { Ref, Element } from 'react';

// external libraries
import classnames from 'classnames';

// components
import { FormField } from 'react-polymorph/lib/components/FormField';

// internal utility functions
import { pickDOMProps } from 'react-polymorph/lib/utils/props';

// skin
import { FormFieldSkin } from './FormFieldSkinTooltip';

type Props = {
  className?: ?string,
  disabled?: boolean,
  error?: string,
  label?: string | Element<any>,
  inputRef: Ref<'input'>,
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
  onKeyPress?: Function,
  placeholder?: string,
  readOnly?: boolean,
  theme: Object,
  themeId: string,
  value: string,
};

export const InputSkinTooltip = (props: Props) => (
  <FormField
    className={props.className}
    disabled={props.disabled}
    label={props.label}
    error={props.error}
    inputRef={props.inputRef}
    skin={FormFieldSkin}
    theme={props.theme}
    render={() => (
      <input
        ref={props.inputRef}
        {...pickDOMProps(props)}
        className={classnames([
          props.theme[props.themeId].input,
          props.disabled ? props.theme[props.themeId].disabled : null,
          props.error ? props.theme[props.themeId].errored : null,
        ])}
        readOnly={props.readOnly}
      />
    )}
  />
);
