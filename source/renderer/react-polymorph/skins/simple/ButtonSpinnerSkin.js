// @flow
import React from 'react';
import type { Element } from 'react';

// external libraries
import classnames from 'classnames';

// internal components & skins
import { LoadingSpinner } from '../../components/LoadingSpinner';
import { LoadingSpinnerSkin } from './LoadingSpinnerSkin';

// internal utility functions
import { pickDOMProps } from '../../utils/props';

// constants
import { IDENTIFIERS } from '../../components';

type Props = {
  className: string,
  disabled: boolean,
  label: string | Element<any>,
  loading: boolean,
  theme: Object,
  themeId: string,
};

export const ButtonSpinnerSkin = (props: Props) => {
  const { className, disabled, label, loading, themeId } = props;
  const buttonTheme = props.theme[themeId];
  const spinnerTheme = props.theme[IDENTIFIERS.LOADING_SPINNER];

  const renderLoadingSpinner = () => (
    <LoadingSpinner skin={LoadingSpinnerSkin} theme={spinnerTheme} />
  );

  return (
    <button
      {...pickDOMProps(props)}
      className={classnames([
        className,
        buttonTheme.root,
        disabled ? buttonTheme.disabled : null,
      ])}
    >
      {loading ? renderLoadingSpinner() : label}
    </button>
  );
};
