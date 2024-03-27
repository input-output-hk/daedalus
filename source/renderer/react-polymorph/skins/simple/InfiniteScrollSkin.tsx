// @ts-nocheck
import React from 'react';
import type { ElementRef, Element } from 'react';
// external libraries
import classnames from 'classnames';

type Props = {
  className: string;
  data: Record<string, any> | Array<{}>;
  error: boolean | string | Element<any>;
  hasMoreData: boolean;
  isLoading: boolean;
  renderItems: (...args: Array<any>) => any;
  scrollContainerRef: ElementRef<any>;
  theme: Record<string, any>;
  themeId: string;
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
