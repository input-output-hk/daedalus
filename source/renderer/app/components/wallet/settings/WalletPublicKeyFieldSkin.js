// @flow
import React from 'react';
import type { ElementRef } from 'react';

// external libraries
import classnames from 'classnames';
import { isFunction } from 'lodash';

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
};

export default (props: Props) => {
  const renderInput = () => (
    <PopOver content={props.tooltip}>
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
      />
    </PopOver>
  );

  const useSelectionRenderer = (option) => (
    <div className={props.theme[props.themeId].customValueWrapper}>
      {renderInput()}
      <div className={props.theme[props.themeId].customValueBlock}>
        {option && props.selectionRenderer && props.selectionRenderer(option)}
      </div>
    </div>
  );

  const render = () => {
    // check if user has passed render prop "selectionRenderer"
    const hasSelectionRenderer =
      props.selectionRenderer && isFunction(props.selectionRenderer);
    if (hasSelectionRenderer) {
      return useSelectionRenderer(props.selectedOption);
    }
    return renderInput();
  };

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
