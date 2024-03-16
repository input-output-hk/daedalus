// @ts-nocheck

import React, { useState } from 'react';
import SVGInline from 'react-svg-inline';
import { injectIntl } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import styles from './StakePoolsSearch.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/list-ic... Remove this comment to see the full error message
import listIcon from '../../../assets/images/list-ic.inline.svg';
import type { Intl } from '../../../types/i18nTypes';
import { messages } from './StakePoolsSearch.messages';

type Props = {
  isListView?: boolean;
  isListViewTooltipVisible?: boolean;
  onClick?: () => void;
  onListViewVisited?: () => void;
  intl: Intl;
  tooltipTarget?: HTMLDivElement;
};

function StakePoolsSearchListViewButtonComponent({
  onClick,
  onListViewVisited,
  isListView,
  isListViewTooltipVisible,
  intl,
  tooltipTarget,
}: Props) {
  const [visible, setVisible] = useState(false);
  const isPopOverVisible = visible || isListViewTooltipVisible;

  return (
    <PopOver
      visible={isPopOverVisible}
      content={intl.formatMessage(messages.listIconTooltip)}
      appendTo={() => tooltipTarget}
    >
      <button
        className={isListView ? styles.selected : null}
        onClick={onClick}
        onMouseEnter={() => setVisible(true)}
        onMouseLeave={() => {
          setVisible(false);
          onListViewVisited();
        }}
      >
        <SVGInline svg={listIcon} />
      </button>
    </PopOver>
  );
}

export const StakePoolsSearchListViewButton = injectIntl(
  StakePoolsSearchListViewButtonComponent
);
