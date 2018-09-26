// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { defineMessages, intlShape } from 'react-intl';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './DataLayerMigrationForm.scss';

const messages = defineMessages({
  title: {
    id: 'profile.dataLayerMigration.title',
    defaultMessage: '!!!New data layer migration',
    description: 'Title for the Data Layer Migration screen.'
  },
  content1: {
    id: 'profile.dataLayerMigration.content1',
    defaultMessage: '!!!You have installed a version of Daedalus which changes how wallet data is stored and managed. Because of this all of your wallets need to be restored and synchronized with the complete history of Cardano blockchain.',
    description: 'Content for the Data Layer Migration screen.'
  },
  content2: {
    id: 'profile.dataLayerMigration.content2',
    defaultMessage: '!!!This is an automatic process and does not require any actions on your behalf.',
    description: 'Content for the Data Layer Migration screen.'
  },
  content3: {
    id: 'profile.dataLayerMigration.content3',
    defaultMessage: '!!!Your transaction history and used addresses will be appearing in your wallets as they are recovered during the restoration process. Addresses which were not used will not be recovered since there are not recorded on the blockchain. If funds are sent to those addresses you will receive them and those addresses will then appear in your wallet.',
    description: 'Content for the Data Layer Migration screen.'
  },
  submitLabel: {
    id: 'profile.dataLayerMigration.submitLabel',
    defaultMessage: '!!!Migrate Wallets',
    description: 'Submit label for the Data Layer Migration screen.'
  },
});

type Props = {
  onSubmit: Function,
  error?: ?LocalizableError,
};

@observer
export default class DataLayerMigrationForm extends Component<Props> {

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
          {intl.formatMessage(messages.content1)}
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
