import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import { getNetworkExplorerUrl } from '../../../utils/network';
import LocalizableError from '../../../i18n/LocalizableError';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './InstructionsDialog.scss' or ... Remove this comment to see the full error message
import styles from './InstructionsDialog.scss';
import { handleFormErrors } from '../../../utils/ReactToolboxMobxForm';
import {
  PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  PAPER_WALLET_WRITTEN_WORDS_COUNT,
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../../../config/cryptoConfig';
import { DEVELOPMENT } from '../../../../../common/types/environment.types';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.instructions.dialog.headline',
    defaultMessage: '!!!Create a paper wallet certificate',
    description:
      'Headline for the "Paper wallet create certificate instructions dialog".',
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.instructions.dialog.subtitle',
    defaultMessage:
      '!!!Create a paper wallet certificate to store funds offline.',
    description:
      'Subtitle for the "Paper wallet create certificate instructions dialog".',
  },
  subtitle2: {
    id: 'paper.wallet.create.certificate.instructions.dialog.subtitle2',
    defaultMessage:
      '!!!The paper wallet certificate will not be associated with any of your existing wallets. A new, empty wallet will be created.',
    description:
      'subtitle2 for the "Paper wallet create certificate instructions dialog".',
  },
  instructionsListLabel: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.label',
    defaultMessage: '!!!Instructions',
    description:
      'Instructions list label for the "Paper wallet create certificate instructions dialog".',
  },
  instructionsListDefinition1: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition1',
    defaultMessage: `!!!Your printed certificate will include your paper wallet recovery phrase
      of {paperWalletRecoveryPhraseWordCount} words. Note that your paper wallet recovery phrase is
      different to the {walletRecoveryPhraseWordCount}-word recovery phrases used to restore your
      regular Daedalus wallet.`,
    description: 'Wallet certificate create instructions dialog definition 1.',
  },
  instructionsListDefinition2: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition2',
    defaultMessage: `!!!For security reasons, the last {paperWalletWrittenWordsCount} words of your
      paper wallet recovery phrase will not be printed on the paper wallet certificate itself. You
      will need to write them on your certificate by hand in a moment.`,
    description: 'Wallet certificate create instructions dialog definition 2.',
  },
  instructionsListDefinition3: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition3',
    defaultMessage:
      '!!!Use the address on your certificate to send funds to your paper wallet.',
    description: 'Wallet certificate create instructions dialog definition 3.',
  },
  instructionsListDefinition4: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition4',
    defaultMessage: `!!!Your paper wallet will be offline so will not be held in Daedalus.
      To check the balance of the wallet, input the address on the certificate into`,
    description: 'Wallet certificate create instructions dialog definition 4.',
  },
  instructionsListDefinition5: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition5',
    defaultMessage:
      '!!!Store your certificate containing your paper wallet recovery phrase in a safe place.',
    description: 'Wallet certificate create instructions dialog definition 5.',
  },
  printingInstructions: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.printingInstructions',
    defaultMessage: `!!!When you click “Save PDF file for printing” you will be prompted
      to choose a location on your computer where the PDF file will be saved. After that
      open the saved PDF file and print it.`,
    description:
      'Wallet certificate create instructions dialog - printing instructions.',
  },
  cardanoExplorer: {
    id: 'paper.wallet.create.certificate.instructions.dialog.cardanoExplorer',
    defaultMessage: '!!!Cardano Explorer',
    description:
      'Wallet certificate create instructions dialog "Cardano Explorer" label',
  },
  printButtonLabel: {
    id: 'paper.wallet.create.certificate.instructions.dialog.button.printLabel',
    defaultMessage: '!!!Save PDF file for printing',
    description:
      '"Wallet certificate create instructions dialog" print button label.',
  },
});
type Props = {
  inProgress: boolean;
  network: string;
  onClose: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  onPrint: (...args: Array<any>) => any;
  error?: LocalizableError | null | undefined;
};

@observer
class InstructionsDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    network: DEVELOPMENT,
  };

  componentDidUpdate(prevProps: Props) {
    if (!prevProps.error && this.props.error) {
      handleFormErrors('.InstructionsDialog_error', {
        focusElement: true,
      });
    }
  }

  render() {
    const { intl } = this.context;
    const {
      onClose,
      onPrint,
      inProgress,
      onOpenExternalLink,
      network,
      error,
    } = this.props;
    const dialogClasses = classnames([styles.component, 'instructionsDialog']);
    const printButtonClasses = classnames([
      'printButton',
      inProgress ? styles.submitButtonSpinning : null,
    ]);
    const actions = [
      {
        className: printButtonClasses,
        label: intl.formatMessage(messages.printButtonLabel),
        primary: true,
        onClick: onPrint,
      },
    ];

    const openNetworkExplorer = () =>
      onOpenExternalLink(getNetworkExplorerUrl(network));

    const cardanoExplorerLink = (
      <Link
        className={styles.link}
        onClick={openNetworkExplorer}
        label={intl.formatMessage(messages.cardanoExplorer)}
        skin={LinkSkin}
      />
    );
    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.instructionsContentWrapper}>
          <p className={styles.subtitle}>
            {intl.formatMessage(messages.subtitle)}
          </p>
          <p className={styles.subtitle2}>
            {intl.formatMessage(messages.subtitle2)}
          </p>
          <div className={styles.instructionsList}>
            <p className={styles.instructionsListLabel}>
              {intl.formatMessage(messages.instructionsListLabel)}
            </p>

            <ul>
              <li>
                {intl.formatMessage(messages.instructionsListDefinition1, {
                  paperWalletRecoveryPhraseWordCount: PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
                  walletRecoveryPhraseWordCount: WALLET_RECOVERY_PHRASE_WORD_COUNT,
                })}
              </li>
              <li>
                {intl.formatMessage(messages.instructionsListDefinition2, {
                  paperWalletWrittenWordsCount: PAPER_WALLET_WRITTEN_WORDS_COUNT,
                })}
              </li>
              <li>
                {intl.formatMessage(messages.instructionsListDefinition3)}
              </li>
              <li>
                <FormattedMessage
                  {...messages.instructionsListDefinition4}
                  values={{
                    link: cardanoExplorerLink,
                  }}
                />
              </li>
              <li>
                {intl.formatMessage(messages.instructionsListDefinition5)}
              </li>
            </ul>
          </div>

          <p className={styles.printingInstructions}>
            <strong>{intl.formatMessage(messages.printingInstructions)}</strong>
          </p>

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
        </div>
      </Dialog>
    );
  }
}

export default InstructionsDialog;
