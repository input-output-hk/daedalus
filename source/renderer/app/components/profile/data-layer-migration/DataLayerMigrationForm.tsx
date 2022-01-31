import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import LocalizableError from '../../../i18n/LocalizableError';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DataLayerMigrationForm.scss'... Remove this comment to see the full error message
import styles from './DataLayerMigrationForm.scss';

const messages = defineMessages({
  title: {
    id: 'profile.dataLayerMigration.title',
    defaultMessage: '!!!Wallet data migration',
    description: 'Title for the Data Layer Migration screen.',
  },
  content1: {
    id: 'profile.dataLayerMigration.content1',
    defaultMessage:
      '!!!You have installed a version of Daedalus that changes how wallet data is stored and managed. Because of this, all of your wallets need to be restored and synchronized with the complete history of the Cardano blockchain.',
    description: 'Content for the Data Layer Migration screen.',
  },
  content2: {
    id: 'profile.dataLayerMigration.content2',
    defaultMessage:
      '!!!This is an automatic process and does not require any action on your behalf.',
    description: 'Content for the Data Layer Migration screen.',
  },
  content3: {
    id: 'profile.dataLayerMigration.content3',
    defaultMessage:
      '!!!Your transaction history and used addresses will appear in your wallets as they are recovered during the restoration process. Addresses that were not used will not be recovered because they are not recorded on the blockchain. If funds were sent to those addresses you will receive the funds and those addresses will appear in your wallet.',
    description: 'Content for the Data Layer Migration screen.',
  },
  submitLabel: {
    id: 'profile.dataLayerMigration.submitLabel',
    defaultMessage: '!!!Start migration',
    description: 'Submit label for the Data Layer Migration screen.',
  },
});
type Props = {
  onSubmit: (...args: Array<any>) => any;
  error?: LocalizableError | null | undefined;
};

@observer
class DataLayerMigrationForm extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  submit = () => {
    this.props.onSubmit();
  };

  render() {
    const { intl } = this.context;
    const { error } = this.props;
    return (
      <div className={styles.component}>
        <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>

        <p className={styles.content1}>
          {/* intl.formatMessage(messages.content1) */}
          <FormattedHTMLMessage {...messages.content1} />
        </p>
        <p className={styles.content2}>
          {intl.formatMessage(messages.content2)}
        </p>
        <p className={styles.content3}>
          {intl.formatMessage(messages.content3)}
        </p>

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

        <Button
          className={styles.submitButton}
          label={intl.formatMessage(messages.submitLabel)}
          onClick={this.submit}
          skin={ButtonSkin}
        />
      </div>
    );
  }
}

export default DataLayerMigrationForm;
