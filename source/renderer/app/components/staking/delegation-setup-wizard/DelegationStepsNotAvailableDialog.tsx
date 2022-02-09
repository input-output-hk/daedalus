import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationSteps.scss' or its... Remove this comment to see the full error message
import commonStyles from './DelegationSteps.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationStepsNotAvailableD... Remove this comment to see the full error message
import styles from './DelegationStepsNotAvailableDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/attenti... Remove this comment to see the full error message
import attentionImage from '../../../assets/images/attention-dark.inline.svg';

type Props = {
  minDelegationFunds: number;
  onClose: (...args: Array<any>) => any;
};
const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.notAvailable.dialog.title',
    defaultMessage: '!!!Delegation is currently unavailable',
    description:
      'Title "Delegation Setup" on the delegation setup not available dialog.',
  },
  description: {
    id: 'staking.delegationSetup.notAvailable.dialog.description',
    defaultMessage:
      '!!!None of your Shelley wallets currently hold the <span>minimum amount of {minDelegationFunds} ADA</span> required for delegation.',
    description: 'Description on the delegation setup not available dialog.',
  },
  closeButtonLabel: {
    id: 'staking.delegationSetup.notAvailable.dialog.closeButtonLabel',
    defaultMessage: '!!!Close',
    description:
      'Label for close button on the delegation setup not available dialog.',
  },
});
export default class DelegationStepsNotAvailableDialog extends Component<
  Props
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { minDelegationFunds, onClose } = this.props;
    const actions = [
      {
        className: 'closeButton',
        label: intl.formatMessage(messages.closeButtonLabel),
        onClick: onClose,
        primary: true,
      },
    ];
    const dialogClassName = classNames([
      commonStyles.delegationSteps,
      styles.delegationStepsNotAvailableDialogWrapper,
    ]);
    const contentClassName = classNames([commonStyles.content, styles.content]);
    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={dialogClassName}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <div className={contentClassName}>
          <SVGInline svg={attentionImage} className={styles.icon} />
          <p className={styles.description}>
            <FormattedHTMLMessage
              {...messages.description}
              values={{
                minDelegationFunds,
              }}
            />
          </p>
        </div>
      </Dialog>
    );
  }
}
