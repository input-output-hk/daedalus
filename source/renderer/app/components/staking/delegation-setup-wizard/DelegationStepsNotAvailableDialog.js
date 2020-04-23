// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import commonStyles from './DelegationSteps.scss';
import styles from './DelegationStepsNotAvailableDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import attentionImage from '../../../assets/images/attention-dark.inline.svg';

type Props = {
  minDelegationFunds: number,
  onClose: Function,
};

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.notAvailable.dialog.title',
    defaultMessage: '!!!Delegation not available',
    description:
      'Title "Delegation Setup" on the delegation setup not available dialog.',
  },
  description: {
    id: 'staking.delegationSetup.notAvailable.dialog.description',
    defaultMessage:
      '!!!None of your wallets have the <span>minimum amount of {minDelegationFunds} ada</span> required for delegation',
    description: 'Description on the delegation setup not available dialog.',
  },
  closeButtonLabel: {
    id: 'staking.delegationSetup.notAvailable.dialog.closeButtonLabel',
    defaultMessage: '!!!Close',
    description:
      'Label for close button on the delegation setup not available dialog.',
  },
});

export default class DelegationStepsNotAvailableDialog extends Component<Props> {
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
              values={{ minDelegationFunds }}
            />
          </p>
        </div>
      </Dialog>
    );
  }
}
