// @flow
import React from 'react';
import type { Node } from 'react';
import classnames from 'classnames';

type Props = {
  children?: ?Node,
  className: string,
  inlineStyles: Object,
  theme: Object,
};

export const HeaderSkin = (props: Props) => {
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
};
