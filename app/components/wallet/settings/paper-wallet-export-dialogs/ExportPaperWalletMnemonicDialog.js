// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import CheckboxWithLongLabel from '../../../widgets/forms/CheckboxWithLongLabel';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import styles from './ExportPaperWalletMnemonicDialog.scss';
import paperWalletImage from '../../../../assets/images/paper-wallet.png';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.export.dialog.mnemonic.headline',
    defaultMessage: '!!!Mnemonic',
    description: 'headline" for paper wallet mnemonic dialog.'
  },
  continueLabel: {
    id: 'paper.wallet.export.dialog.mnemonic.button.continueLabel',
    defaultMessage: '!!!Continue',
    description: 'Label "Continue" for paper wallet mnemonic dialog.'
  },
  confirmPhraseWrittenNotice: {
    id: 'paper.wallet.export.dialog.mnemonic.confirmPhraseWrittenNotice',
    defaultMessage: '!!!I understand all the importance of this phrase and wrote it my paper certificate.',
    description: 'Notice to confirm that paper wallet recovery phrase is written to paper on paper wallet mnemonic dialog.',
  },
});

@observer
export default class ExportPaperWalletMnemonicDialog extends Component {

  props: {
    onContinue: Function,
    onClose: Function,
    onBack: Function,
    onTogglePhraseWrittenNotice: Function,
    isPhraseWrittenNoticeAccepted: boolean,
    recoveryPhrase: string,
  };

  static defaultProps = {
    isPhraseWrittenNoticeAccepted: false,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onContinue,
      onClose,
      onBack,
      onTogglePhraseWrittenNotice,
      isPhraseWrittenNoticeAccepted,
      recoveryPhrase,
    } = this.props;
    const dialogClasses = classnames([
      styles.component,
      'ExportPaperWalletMnemonicDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.continueLabel),
        primary: true,
        disabled: !isPhraseWrittenNoticeAccepted,
        onClick: () => onContinue(),
      }
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        onOverlayClick={onClose}
        active
      >

        <div className={styles.instructions}>
          <p>
            Please, make sure you have carefully writen down your recovery phrase on paper.
            You will need this phrase to unlock your private key. Phrase is case sensitive.
          </p>
        </div>

        <div className={styles.recoveryPhrase}>
          {recoveryPhrase}
        </div>

        <div className={styles.paperWalletImageWrapper}>
          <img src={paperWalletImage} role="presentation" />
        </div>

        <CheckboxWithLongLabel
          label={intl.formatMessage(messages.confirmPhraseWrittenNotice)}
          onChange={onTogglePhraseWrittenNotice}
          checked={isPhraseWrittenNoticeAccepted}
        />


        <DialogBackButton onBack={onBack} />
        <DialogCloseButton onClose={onClose} />

      </Dialog>
    );
  }

}
