// @flow
import React from 'react';
import type { Element, ElementRef } from 'react';

// external libraries
import classnames from 'classnames';

// components
import { Options } from '../../components/Options';

// skins
import { OptionsSkin } from './OptionsSkin';

type Props = {
  activeItem: any,
  className: string,
  isOpen: boolean,
  isOpeningUpward: boolean,
  items: Array<any>,
  label: string | Element<any>,
  noArrow?: boolean,
  onItemSelected?: Function,
  onLabelClick: () => void,
  optionsRef: ElementRef<any>,
  optionsMaxHeight: number,
  optionRenderer?: Function,
  rootRef: ElementRef<*>,
  setMouseOverItems: Function,
  setMouseOverRoot: Function,
  theme: Object,
  themeId: string,
  optionHeight: ?number,
};

export function DropdownSkin(props: Props) {
  const { theme, themeId, setMouseOverItems, setMouseOverRoot } = props;
  const themeApi = theme[themeId];
  return (
    <div
      className={classnames([props.className, themeApi.dropdown])}
      onMouseEnter={() => setMouseOverRoot(true)}
      onMouseLeave={() => setMouseOverRoot(false)}
      ref={props.rootRef}
    >
      <div
        role="presentation"
        aria-hidden
        className={themeApi.label}
        onClick={props.onLabelClick}
      >
        {props.label}
      </div>
      <Options
        isFloating
        isOpen={props.isOpen}
        isOpeningUpward={props.isOpeningUpward}
        noOptionsArrow={props.noArrow}
        noSelectedOptionCheckmark
        onChange={props.onItemSelected}
        options={props.items}
        optionsMaxHeight={props.optionsMaxHeight}
        optionHeight={props.optionHeight}
        optionsRef={props.optionsRef}
        optionRenderer={props.optionRenderer}
        selectedOption={props.activeItem}
        setMouseIsOverOptions={setMouseOverItems}
        skin={OptionsSkin}
        theme={props.theme}
      />
    </div>
  );
}
