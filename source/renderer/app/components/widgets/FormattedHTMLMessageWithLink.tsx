import React, { Component, Fragment } from 'react';
import { intlShape } from 'react-intl';
import type { ReactIntlMessageShape } from '../../i18n/types';
import { ReactIntlMessage } from '../../types/i18nTypes';

type ReactIntlMessageShapeWithLink = ReactIntlMessageShape & {
  values: {
    linkPosition?: string;
    linkLabel: ReactIntlMessage;
    linkURL: ReactIntlMessage;
  };
};
type Props = {
  message: ReactIntlMessageShapeWithLink;
  onExternalLinkClick: (...args: Array<any>) => any;
};
export class FormattedHTMLMessageWithLink extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { message, onExternalLinkClick } = this.props;
    const { linkPosition, linkLabel, linkURL } = message.values;
    const MainMessage = (
      <Fragment key="mainMessage">{intl.formatMessage(message)}</Fragment>
    );
    const url = intl.formatMessage(linkURL);
    const Link = (
      <Fragment key="link">
        <a href={url} onClick={(event) => onExternalLinkClick(url, event)}>
          {intl.formatMessage(linkLabel)}
        </a>
      </Fragment>
    );
    return linkPosition === 'before'
      ? [Link, <React.Fragment key="space">&nbsp;</React.Fragment>, MainMessage]
      : [
          MainMessage,
          <React.Fragment key="space">&nbsp;</React.Fragment>,
          Link,
        ];
  }
}
