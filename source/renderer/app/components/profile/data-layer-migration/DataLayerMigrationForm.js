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
    id: 'profile.dataMigrationLayer.title',
    defaultMessage: '!!!New data layer migration',
    description: 'Title for the Data Layer Migration screen.'
  },
  content: {
    id: 'profile.dataMigrationLayer.content',
    defaultMessage: '!!!We are introducing new data layer Cardano 1.4.7, so we need import wallet again. We are importing wallet, which are not protected with password by ourselves, but password protected wallets should be imported manualy. You will need to check which wallets to import. Make a note that unchecked wallets would be deleted.',
    description: 'Content for the Data Layer Migration screen.'
  },
  submitLabel: {
    id: 'profile.dataMigrationLayer.submitLabel',
    defaultMessage: '!!!Migrate Wallets',
    description: 'Submit label for the Data Layer Migration screen.'
  },
  blogPostLink: {
    id: 'profile.dataMigrationLayer.blogPostLink',
    defaultMessage: '!!!Read blog post',
    description: 'Link text for the blog post in the Data Layer Migration screen.'
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

        <p className={styles.content}>
          {intl.formatMessage(messages.content)}
        </p>

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

        <Button
          className={styles.submitButton}
          label={intl.formatMessage(messages.submitLabel)}
          onClick={this.submit}
          skin={ButtonSkin}
        />

        <button className={styles.blogPost}>
          {intl.formatMessage(messages.blogPostLink)}
        </button>

      </div>
    );
  }

}
