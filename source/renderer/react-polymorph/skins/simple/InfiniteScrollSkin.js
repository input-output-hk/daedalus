// @flow
import React from 'react';
import type { ElementRef, Element } from 'react';

// external libraries
import classnames from 'classnames';

type Props = {
  className: string,
  data: Object | Array<{}>,
  error: boolean | string | Element<*>,
  hasMoreData: boolean,
  isLoading: boolean,
  renderItems: Function,
  scrollContainerRef: ElementRef<*>,
  theme: Object,
  themeId: string,
};

export function InfiniteScrollSkin({
  className,
  data,
  error,
  hasMoreData,
  isLoading,
  renderItems,
  scrollContainerRef,
  theme,
  themeId,
}: Props) {
  return (
    <div
      ref={scrollContainerRef}
      className={classnames([className, theme[themeId].root])}
    >
      {renderItems({
        data,
        error,
        hasMoreData,
        isLoading,
        theme: theme[themeId],
      })}
    </div>
  );
}
