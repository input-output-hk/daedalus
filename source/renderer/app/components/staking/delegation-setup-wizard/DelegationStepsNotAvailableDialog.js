// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import styles from './DelegationStepsNotAvailableDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import attentionImage from '../../../assets/images/attention-dark.inline.svg';

type Props = {
  onClose: Function,
};

const messages = defineMessages({
  title: {
    id: 'delegation.setup.steps.dialog.title',
    defaultMessage: '!!!Delegation Setup',
    description:
      'Title "Delegation Setup" on the delegation setup not available dialog.',
  },
  subtitle: {
    id: 'delegation.setup.notAvailable.dialog.subtitle',
    defaultMessage: '!!!Delegation not available',
    description: 'Subtitle on the delegation setup not available dialog.',
  },
  description: {
    id: 'delegation.setup.notAvailable.dialog.description',
    defaultMessage:
      '!!!A wallet with at least <span>1 ada</span> is required for delegation setup. Please restore a wallet with ada, or create a new one and fund it with ada in order to access delegation features.',
    description: 'Description on the delegation setup not available dialog.',
  },
  closeButtonLabel: {
    id: 'delegation.setup.notAvailable.dialog.closeButtonLabel',
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
    const { onClose } = this.props;

    const actions = [
      {
        className: 'closeButton',
        label: intl.formatMessage(messages.closeButtonLabel),
        onClick: onClose,
        primary: true,
      },
    ];

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={styles.delegationStepsNotAvailableDialogWrapper}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <div className={styles.content}>
          <SVGInline svg={attentionImage} className={styles.icon} />
          <p className={styles.subtitle}>
            {intl.formatMessage(messages.subtitle)}
          </p>
          <p className={styles.description}>
            <FormattedHTMLMessage {...messages.description} />
          </p>
        </div>
      </Dialog>
    );
  }
}
