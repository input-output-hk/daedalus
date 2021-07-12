// @flow
import React from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { observer } from 'mobx-react';
import styles from './NoConnectionOverlay.scss';
import daedalusLogo from '../../../assets/images/daedalus-logo-loading-white.inline.svg';

const messages = defineMessages({
  title: {
    id: 'loading.screen.noConnection.title',
    defaultMessage: '!!!No Connection',
    description: 'title for the No Connection dialog.',
  },
  description: {
    id: 'loading.screen.noConnection.description',
    defaultMessage:
      '!!!Daedalus cannot operate without an Internet connection.',
    description: 'description for the No Connection dialog.',
  },
});

type Props = {
  intl: intlShape.isRequired,
};

type ItemCopied = ?string;

const NoConnectionOverlay = observer((props: Props) => {
  const { intl } = props;

  return (
    <div className={styles.component}>
      <SVGInline svg={daedalusLogo} />
      <h3>{intl.formatMessage(messages.title)}</h3>
      <p>{intl.formatMessage(messages.description)}</p>
    </div>
  );
});

export default injectIntl(NoConnectionOverlay);
