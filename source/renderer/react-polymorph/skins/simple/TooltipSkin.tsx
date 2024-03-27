// @ts-nocheck
import React from 'react';
// external libraries
import classnames from 'classnames';
// components
import { Bubble } from '../../components/Bubble';
import type { TooltipProps } from '../../components/Tooltip';
// skins
import { BubbleSkin } from './BubbleSkin';
// internal utility functions
import { pickDOMProps } from '../../utils/props';

type Props = TooltipProps & {
  theme: Record<string, any>;
  themeId: string;
};
export function TooltipSkin(props: Props) {
  const { theme, themeId } = props;
  const isEmpty = props.tip == null || props.tip === '';
  return (
    <span
      {...pickDOMProps(props)}
      className={classnames([
        props.className,
        theme[themeId].root,
        isEmpty ? theme[themeId].isEmpty : null,
        props.isVisible ? theme[themeId].isVisible : null,
        props.isShowingOnHover ? theme[themeId].isShowingOnHover : null,
        props.isCentered ? theme[themeId].isCentered : null,
      ])}
    >
      <Bubble
        className={classnames([
          theme[themeId].bubble,
          props.isAligningRight
            ? theme[themeId].alignRight
            : theme[themeId].alignLeft,
          props.isBounded ? null : theme[themeId].nowrap,
        ])}
        theme={theme}
        isCentered={props.isCentered}
        isOpeningUpward={props.isOpeningUpward}
        skin={BubbleSkin}
        isTransparent={props.isTransparent}
        arrowRelativeToTip={props.arrowRelativeToTip}
      >
        {props.tip}
      </Bubble>
      {props.children}
    </span>
  );
}
