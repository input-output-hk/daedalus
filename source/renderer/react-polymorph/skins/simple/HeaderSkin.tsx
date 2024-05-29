// @ts-nocheck
import React from 'react';
import type { Node } from 'react';
import classnames from 'classnames';

type Props = {
  children?: Node | null | undefined;
  className: string;
  inlineStyles: Record<string, any>;
  theme: Record<string, any>;
};
export function HeaderSkin(props: Props) {
  const { children, className, inlineStyles, theme } = props;
  const themeClasses = Object.values(theme);
  return (
    <header
      className={classnames([className, ...themeClasses])}
      style={inlineStyles}
    >
      {children}
    </header>
  );
}
