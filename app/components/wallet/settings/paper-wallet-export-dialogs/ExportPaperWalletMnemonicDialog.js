// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import Dialog from '../../../widgets/Dialog';
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

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
  onTogglePhraseWrittenNotice: Function,
  isPhraseWrittenNoticeAccepted: boolean,
  recoveryPhrase: string,
};

@observer
export default class ExportPaperWalletMnemonicDialog extends Component<Props> {

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
        onClick: onContinue,
      }
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
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

        <div className={styles.checkbox}>
          <Checkbox
            label={intl.formatMessage(messages.confirmPhraseWrittenNotice)}
            onChange={onTogglePhraseWrittenNotice}
            checked={isPhraseWrittenNoticeAccepted}
            skin={<SimpleCheckboxSkin />}
          />
        </div>

      </Dialog>
    );
  }

}
