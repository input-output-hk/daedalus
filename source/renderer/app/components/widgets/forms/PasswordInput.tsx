import classnames from 'classnames';
import React, { Component } from 'react';
import { intlShape } from 'react-intl';
import { PasswordInput as RPPasswordInput } from 'react-polymorph/lib/components/PasswordInput';
import type { PasswordInputProps } from 'react-polymorph/lib/components/PasswordInput';
import globalMessages from '../../../i18n/global-messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './PasswordInput.scss' or its c... Remove this comment to see the full error message
import styles from './PasswordInput.scss';

export type Props = PasswordInputProps;
export class PasswordInput extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { className, ...pwInputProps } = this.props;
    return (
      <div className={classnames([styles.root, className])}>
        <RPPasswordInput
          {...pwInputProps}
          passwordFeedbacks={{
            insecure: intl.formatMessage(
              globalMessages.invalidSpendingPassword
            ),
            weak: intl.formatMessage(globalMessages.weakSpendingPassword),
            strong: intl.formatMessage(globalMessages.strongSpendingPassword),
            noMatch: intl.formatMessage(globalMessages.invalidRepeatPassword),
          }}
        />
      </div>
    );
  }
}
