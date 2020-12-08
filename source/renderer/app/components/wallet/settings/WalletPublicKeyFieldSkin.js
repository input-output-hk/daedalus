// @flow
import React from 'react';
import type { ElementRef } from 'react';

// external libraries
import classnames from 'classnames';

// components
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { FormField } from 'react-polymorph/lib/components/FormField';
import type { InputProps } from 'react-polymorph/lib/components/Input';

// skins
import { FormFieldSkin } from 'react-polymorph/lib/skins/simple/FormFieldSkin';

// internal utility functions
import { pickDOMProps } from 'react-polymorph/lib/utils/props';

type Props = InputProps & {
  inputRef: ElementRef<'input'>,
  theme: Object,
  themeId: string,
  tooltip: Node,
  addOn: Node,
};

export default (props: Props) => {
  const renderInput = () => (
    <input
      ref={props.inputRef}
      {...pickDOMProps(props)}
      className={classnames([
        props.theme[props.themeId].input,
        props.disabled ? props.theme[props.themeId].disabled : null,
        props.error || props.showErrorState
          ? props.theme[props.themeId].errored
          : null,
      ])}
      readOnly={props.readOnly}
      onFocus={() => {
        if (props.onFocus) {
          props.onFocus();
        }
        if (props.inputRef && props.inputRef.current) {
          props.inputRef.current.select();
        }
      }}
    />
  );
  const render = () =>
    props.addOn ? (
      <PopOver content={props.tooltip}>
        <div>
          {renderInput()}
          {props.addOn}
        </div>
      </PopOver>
    ) : (
      <span>
        <div>{renderInput()}</div>
      </span>
    );

  return (
    <FormField
      className={props.className}
      disabled={props.disabled}
      label={props.label}
      error={props.error}
      inputRef={props.inputRef}
      skin={FormFieldSkin}
      theme={props.theme}
      render={render}
    />
  );
};
