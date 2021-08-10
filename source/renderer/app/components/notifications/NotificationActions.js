// @flow
import React from 'react';
import classnames from 'classnames';
import { map } from 'lodash';
import { Button } from 'react-polymorph/lib/components/Button';
import { Link } from 'react-polymorph/lib/components/Link';
import { observer } from 'mobx-react';
import styles from './NotificationActions.scss';

export type NotificationActionItems = Array<NotificationActionItem>;

export type NotificationActionItem = {
  className?: ?string,
  label: string | Node,
  primary?: boolean,
  disabled?: boolean,
  onClick?: Function,
  onDisabled?: Function,
  isLink?: boolean,
  hasIconAfter?: boolean,
  hasIconBefore?: boolean,
  autoFocus?: boolean,
};

type Props = {
  actions: NotificationActionItems,
};

const NotificationActions = observer(({ actions }: Props) => {
  const componentStyles = classnames([styles.component]);
  return (
    <div className={componentStyles}>
      {map(actions, (action, key) => {
        const buttonClasses = classnames([
          styles.button,
          action.className,
          action.primary ? 'primary' : 'flat',
          action.primary ? styles.primaryButton : styles.secondaryButton,
        ]);
        const autoFocus =
          (action.primary && action.autoFocus) !== false || !!action.autoFocus;
        return !action.isLink ? (
          <Button
            key={key}
            className={buttonClasses}
            label={action.label}
            onClick={action.onClick}
            disabled={action.disabled}
            autoFocus={autoFocus}
          />
        ) : (
          <Link
            key={key}
            className={action.className}
            onClick={action.onClick}
            label={action.label}
            hasIconAfter={action.hasIconAfter}
            hasIconBefore={action.hasIconBefore}
          />
        );
      })}
    </div>
  );
});

export default NotificationActions;
