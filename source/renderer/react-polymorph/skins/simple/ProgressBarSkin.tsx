// @ts-nocheck
import React from 'react';
// external libraries
import classnames from 'classnames';

type Props = {
  className: string;
  label: string;
  progress: number;
  theme: Record<string, any>;
  themeId: string;
};
export function ProgressBarSkin(props: Props) {
  const { className, label, progress, themeId } = props;
  const theme = props.theme[themeId];
  return (
    <div className={classnames([className, theme.track])}>
      <div
        className={theme.progress}
        style={{
          width: `${progress}%`,
        }}
      />
      <div className={theme.label}>
        {progress === 100 && label ? label : null}
      </div>
    </div>
  );
}
