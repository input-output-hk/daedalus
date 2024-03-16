// @ts-nocheck
import React from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { defineMessages, injectIntl } from 'react-intl';
import newsFeedIcon from '../../assets/images/top-bar/news-feed-icon.inline.svg';
import styles from './NewsFeedIcon.scss';
import type { Intl } from '../../types/i18nTypes';
import { TOOLTIP_DELAY } from '../../config/timingConfig';

type Props = {
  intl: Intl;
  onNewsFeedIconClick: (...args: Array<any>) => any;
  newsFeedIconClass?: string;
  hasNotification: boolean;
  hasUpdate: boolean;
};

const messages = defineMessages({
  iconTooltip: {
    id: 'news.newsfeed.iconTooltip',
    defaultMessage: '!!!Newsfeed',
    description: 'Newsfeed',
  },
});

function NewsFeedIcon({
  intl,
  onNewsFeedIconClick,
  newsFeedIconClass,
  hasNotification,
  hasUpdate,
}: Props) {
  const buttonClasses = classNames([
    styles.button,
    hasNotification && !hasUpdate && styles.notificationDot,
    hasUpdate && styles.updateDot,
    newsFeedIconClass,
  ]);
  return (
    <div className={styles.component}>
      <PopOver
        appendTo="parent"
        delay={TOOLTIP_DELAY}
        offset={[0, 0]}
        content={
          <span className={styles.tooltip}>
            {intl.formatMessage(messages.iconTooltip)}
          </span>
        }
      >
        <button className={buttonClasses} onClick={onNewsFeedIconClick}>
          <SVGInline className={styles.icon} svg={newsFeedIcon} />
        </button>
      </PopOver>
    </div>
  );
}

export default injectIntl(NewsFeedIcon);
