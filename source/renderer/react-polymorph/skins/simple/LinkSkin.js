// @flow
import React from 'react';

// external libraries
import classnames from 'classnames';

type Props = {
  label: string,
  isUnderlined: boolean,
  underlineOnHover: boolean,
  hasIconBefore: boolean,
  hasIconAfter: boolean,
  onClick?: Function,
  className: string,
  theme: Object,
  themeId: string,
};

export const LinkSkin = (props: Props) => {
  const {
    label,
    isUnderlined,
    hasIconBefore,
    hasIconAfter,
    onClick,
    className,
    theme,
    themeId,
    underlineOnHover,
  } = props;

  return (
    <span
      role="presentation"
      aria-hidden
      className={classnames([
        className,
        theme[themeId].root,
        hasIconBefore ? theme[themeId].withIconBefore : null,
        hasIconAfter ? theme[themeId].withIconAfter : null,
        isUnderlined && !underlineOnHover ? theme[themeId].underlined : null,
        underlineOnHover ? theme[themeId].underlinedOnHover : null,
      ])}
      onClick={onClick}
    >
      {label}
    </span>
  );
};
