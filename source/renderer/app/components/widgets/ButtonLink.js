// @flow
import React, { Component } from 'react';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';

import styles from './ButtonLink.scss';

type LinkPropsTypes = {
  isUnderlined?: boolean,
  underlineOnHover?: boolean,
  hasIconBefore?: boolean,
  hasIconAfter?: boolean,
  className?: string,
};

type Props = {
  linkProps?: LinkPropsTypes,
  label: string,
};

export default class ButtonLink extends Component<Props> {
  render() {
    const { label } = this.props;
    let { linkProps } = this.props;
    if (!linkProps) linkProps = {};

    const {
      isUnderlined,
      underlineOnHover,
      hasIconBefore,
      hasIconAfter,
      className,
    } = linkProps;

    const linkLabelClasses = classNames([styles.linkLabel, className || null]);

    const linkLabel = (
      <Link
        label={label}
        isUnderlined={isUnderlined || false}
        underlineOnHover={underlineOnHover || false}
        hasIconBefore={hasIconBefore || true}
        hasIconAfter={hasIconAfter || false}
        className={linkLabelClasses}
        skin={LinkSkin}
      />
    );

    return <Button {...this.props} label={linkLabel} />;
  }
}
