// @flow
import React from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import noConnectionIcon from '../../assets/images/top-bar/no-connection.inline.svg';
import styles from './NodeConnectionIcon.scss';

const messages = defineMessages({
  popOver: {
    id: 'topBar.noConnection.popOver',
    defaultMessage: '!!!No internet connection available',
    description: 'Label for the "No Connection" popOver',
  },
});

type Props = {
  hasTadaIcon?: boolean,
  intl: intlShape.isRequired,
};

const NodeConnectionIcon = observer((props: Props) => {
  const { hasTadaIcon, intl } = props;
  const componentClasses = classNames([
    styles.component,
    hasTadaIcon ? styles.hasTadaIcon : null,
  ]);
  return (
    <div className={componentClasses}>
      <PopOver content={intl.formatMessage(messages.popOver)} appendTo="parent">
        <SVGInline className={styles.icon} svg={noConnectionIcon} />
      </PopOver>
    </div>
  );
});

export default injectIntl(NodeConnectionIcon);
