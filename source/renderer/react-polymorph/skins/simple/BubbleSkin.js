// @flow
import React from 'react';
import type { Node, ElementRef } from 'react';

// external libraries
import classnames from 'classnames';

// internal utility functions
import { pickDOMProps } from '../../utils/props';
import type { BubblePosition, BubbleProps } from '../../components/Bubble';

type Props = BubbleProps & {
  children?: ?Node,
  position: BubblePosition,
  rootRef: ElementRef<*>,
  theme: Object,
  themeId: string,
};

export const BubbleSkin = (props: Props) => {
  const { arrowRelativeToTip, noArrow, theme, themeId } = props;
  const autoWidthClass = arrowRelativeToTip
    ? theme[themeId].hasAutoWidth
    : null;
  return (
    <div
      ref={props.rootRef}
      {...pickDOMProps(props)}
      className={classnames([
        props.className,
        theme[themeId].root,
        props.isOpeningUpward ? theme[themeId].openUpward : null,
        props.isCentered ? theme[themeId].isCentered : null,
        props.isTransparent ? theme[themeId].transparent : null,
        props.isFloating ? theme[themeId].isFloating : null,
        props.isHidden ? theme[themeId].isHidden : null,
        noArrow ? theme[themeId].noArrow : null,
      ])}
      style={
        props.position && {
          [props.isOpeningUpward ? 'bottom' : 'top']: props.position.positionY,
          left: props.position.positionX,
          width: props.position.width,
        }
      }
    >
      <div
        className={classnames([theme[themeId].bubble, autoWidthClass])}
        data-bubble-container="true"
      >
        {props.children}
        {arrowRelativeToTip && (
          <span
            className={theme[themeId].arrow}
            data-bubble-arrow={noArrow ? undefined : true}
          />
        )}
      </div>
      {!arrowRelativeToTip && (
        <span
          className={theme[themeId].arrow}
          data-bubble-arrow={noArrow ? undefined : true}
        />
      )}
    </div>
  );
};
