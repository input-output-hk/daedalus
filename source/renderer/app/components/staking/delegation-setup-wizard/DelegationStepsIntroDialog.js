// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import commonStyles from './DelegationSteps.scss';
import styles from './DelegationStepsIntroDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import externalLinkIcon from '../../../assets/images/link-ic.inline.svg';

type Props = {
  onClose: Function,
  onContinue: Function,
  onLearnMoreClick: Function,
};

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.intro.step.dialog.title',
    defaultMessage: '!!!Delegation Setup',
    description:
      'Title "Delegation Setup" on the delegation setup "intro" dialog.',
  },
  description: {
    id: 'staking.delegationSetup.intro.step.dialog.description',
    defaultMessage:
      '!!!Follow next sequence of screens to configure delegation for your wallet. During this process, you will need to deposit and pay transaction fees.',
    description: 'Description on the delegation setup "intro" dialog.',
  },
  learnMoreButtonLabel: {
    id: 'staking.delegationSetup.intro.step.dialog.learnMore.buttonLabel',
    defaultMessage: '!!!Learn more',
    description:
      '"Learn more" button label on the delegation setup "intro" dialog.',
  },
  stepsExplanationLabel: {
    id: 'staking.delegationSetup.intro.step.dialog.stepsExplanationLabel',
    defaultMessage: '!!!You will need to complete the following steps:',
    description:
      'Steps explanation label on the delegation setup "intro" dialog.',
  },
  stepsExplanationLabel1: {
    id: 'staking.delegationSetup.intro.step.dialog.stepsExplanation.step1',
    defaultMessage: '!!!Choose a wallet',
    description:
      'Steps explanation list item 1 label on the delegation setup "intro" dialog.',
  },
  stepsExplanationLabel2: {
    id: 'staking.delegationSetup.intro.step.dialog.stepsExplanation.step2',
    defaultMessage: '!!!Choose a stake pool',
    description:
      'Steps explanation list item 2 label on the delegation setup "intro" dialog.',
  },
  stepsExplanationLabel3: {
    id: 'staking.delegationSetup.intro.step.dialog.stepsExplanation.step3',
    defaultMessage: '!!!Confirm delegation',
    description:
      'Steps explanation list item 3 label on the delegation setup "intro" dialog.',
  },
  stepsExplanationLabel4: {
    id: 'staking.delegationSetup.intro.step.dialog.stepsExplanation.step4',
    defaultMessage: '!!!Move all of the ada to a new address',
    description:
      'Steps explanation list item 4 label on the delegation setup "intro" dialog.',
  },
  stepsExplanationOptionalLabel: {
    id:
      'staking.delegationSetup.intro.step.dialog.stepsExplanation.optionalLabel',
    defaultMessage: '!!!(optional)',
    description:
      'Steps explanation list item 4 "Optional" label on the delegation setup "intro" dialog.',
  },
  cancelButtonLabel: {
    id: 'staking.delegationSetup.intro.step.dialog.cancelButtonLabel',
    defaultMessage: '!!!Cancel',
    description:
      'Label for close button on the delegation setup "intro" dialog.',
  },
  continueButtonLabel: {
    id: 'staking.delegationSetup.intro.step.dialog.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the delegation setup "intro" dialog.',
  },
});

export default class DelegationStepsIntroDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, onContinue, onLearnMoreClick } = this.props;

    const actions = [
      {
        className: 'closeButton',
        label: intl.formatMessage(messages.cancelButtonLabel),
        onClick: onClose,
      },
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: onContinue,
        primary: true,
      },
    ];

    const dialogClassName = classNames([
      commonStyles.delegationSteps,
      styles.delegationStepsIntroDialogWrapper,
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
          <p className={styles.description}>
            {intl.formatMessage(messages.description)}
          </p>
          <button className={styles.link} onClick={onLearnMoreClick}>
            {intl.formatMessage(messages.learnMoreButtonLabel)}
            <SVGInline svg={externalLinkIcon} />
          </button>
          <div className={styles.stepsExplanation}>
            <p className={styles.label}>
              {intl.formatMessage(messages.stepsExplanationLabel)}
            </p>
            <ol>
              <li>
                <span>1.</span>{' '}
                {intl.formatMessage(messages.stepsExplanationLabel1)}
              </li>
              <li>
                <span>2.</span>{' '}
                {intl.formatMessage(messages.stepsExplanationLabel2)}
              </li>
              <li>
                <span>3.</span>{' '}
                {intl.formatMessage(messages.stepsExplanationLabel3)}
              </li>
              <li>
                <p>
                  <span>4. </span>
                  {intl.formatMessage(messages.stepsExplanationLabel4)}{' '}
                  <span className={styles.optionalLabel}>
                    {intl.formatMessage(messages.stepsExplanationOptionalLabel)}
                  </span>
                </p>
              </li>
            </ol>
          </div>
        </div>
      </Dialog>
    );
  }
}
