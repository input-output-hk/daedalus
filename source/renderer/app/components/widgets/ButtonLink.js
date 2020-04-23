// @flow
import React, { Component } from 'react';
import { get } from 'lodash';
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
    const { label, linkProps } = this.props;
    const linkLabelClasses = classNames([
      styles.linkLabel,
      get(linkProps, 'className', null),
    ]);

    const linkLabel = (
      <Link
        label={label}
        isUnderlined={get(linkProps, 'isUnderlined', false)}
        underlineOnHover={get(linkProps, 'underlineOnHover', false)}
        hasIconBefore={get(linkProps, 'hasIconBefore', true)}
        hasIconAfter={get(linkProps, 'hasIconAfter', false)}
        className={linkLabelClasses}
        skin={LinkSkin}
        themeOverrides={styles}
      />
    );

    return <Button {...this.props} label={linkLabel} />;
  }
}
