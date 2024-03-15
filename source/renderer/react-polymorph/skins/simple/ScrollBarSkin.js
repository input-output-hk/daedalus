// @flow
import React from 'react';

// external libraries
import CustomScrollBar from 'react-scrollbars-custom';

type Props = {
  children: Node,
  className: string,
  style: Object,
  theme: Object,
  themeId: string,
};

export function ScrollBarSkin(props: Props) {
  const { children, style, themeId } = props;
  const theme = props.theme[themeId];

  return (
    <div className={theme.root}>
      <CustomScrollBar style={style} noDefaultStyles>
        {children}
      </CustomScrollBar>
    </div>
  );
}
