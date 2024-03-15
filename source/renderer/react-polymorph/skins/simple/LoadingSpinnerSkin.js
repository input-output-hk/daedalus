// @flow
import React from 'react';

// external libraries
import classnames from 'classnames';

type Props = {
  big: boolean,
  className: string,
  theme: Object,
  themeId: string,
  visible: boolean,
};

export const LoadingSpinnerSkin = (props: Props) => {
  const { big, className, themeId, visible } = props;
  const theme = props.theme[themeId];
  const size = big ? theme.big : theme.small;

  return visible ? (
    <div className={classnames([className, theme.root, size])} />
  ) : null;
};
