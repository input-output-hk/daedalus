// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { submitOnEnter } from '../../../utils/form';
import styles from './StakePoolsSettings.scss';
import {
  INTERNAL_SMASH_SERVERS,
  SMASH_SERVER_TYPES,
} from '../../../config/stakingConfig';
import type { SmashServerType } from '../../../types/stakingTypes';

const messages = defineMessages({
  smashSelectLabel: {
    id: 'settings.stakePools.smash.select.label',
    defaultMessage: '!!!Off-chain data server (SMASH)',
    description:
      'smashSelectLabel for the "Smash" selection on the Stake Pools settings page.',
  },
  smashSelectCustomServer: {
    id: 'settings.stakePools.smash.select.placeholder',
    defaultMessage: '!!!Custom server',
    description:
      'smashSelectCustomServer option for the "Smash" selection on the Stake Pools settings page.',
  },
  smashURLSelectLabel: {
    id: 'settings.stakePools.smashUrl.select.label',
    defaultMessage: '!!!SMASH server URL',
    description:
      'smashURLSelectLabel for the "Smash Custom Server" selection on the Stake Pools settings page.',
  },
  smashUrlSelectPlaceholder: {
    id: 'settings.stakePools.smashUrl.select.placeholder',
    defaultMessage: '!!!Enter custom server',
    description:
      'smashUrlSelectPlaceholder for the "Smash Custom Server" selection on the Stake Pools settings page.',
  },
});

type Props = {
  smashServerType: string,
  smashServerUrl?: string,
  onSelectSmashServerType: Function,
  onSelectSmashServerUrl: Function,
};

@observer
export default class StakePoolsSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentWillUnmount() {
    const {
      smashServerType,
      smashServerUrl,
      onSelectSmashServerType,
    } = this.props;

    if (smashServerType === SMASH_SERVER_TYPES.CUSTOM && !smashServerUrl) {
      onSelectSmashServerType({ smashServerType: SMASH_SERVER_TYPES.IOHK });
    }
  }

  handleSubmit = () => {
    console.log('handleSubmit');
    // if (this.isConfirmDisabled()) {
    //   return false;
    // }

    // return this.form.submit({
    //   onSuccess: (form) => {
    //     const { onConfirm } = this.props;
    //     const { passphrase } = form.values();
    //     onConfirm(passphrase);
    //   },
    //   onError: () => null,
    // });
  };

  handleSubmitOnEnter = (event: KeyboardEvent) =>
    submitOnEnter(this.handleSubmit, event);

  render() {
    const {
      smashServerType,
      smashServerUrl,
      onSelectSmashServerType,
      onSelectSmashServerUrl,
    } = this.props;
    const { intl } = this.context;

    const smashSelectOptions = [
      ...INTERNAL_SMASH_SERVERS.map(({ name: label, id: value }) => ({
        label,
        value,
      })),
      {
        label: intl.formatMessage(messages.smashSelectCustomServer),
        value: SMASH_SERVER_TYPES.CUSTOM,
      },
    ];

    return (
      <div className={styles.component}>
        <Select
          label={intl.formatMessage(messages.smashSelectLabel)}
          value={smashServerType}
          options={smashSelectOptions}
          onChange={(smashServerType: SmashServerType) => {
            onSelectSmashServerType({ smashServerType });
          }}
          skin={SelectSkin}
          className={styles.select}
          optionHeight={50}
        />
        {smashServerType === SMASH_SERVER_TYPES.CUSTOM && (
          <Input
            value={smashServerUrl || ''}
            label={intl.formatMessage(messages.smashURLSelectLabel)}
            placeholder={intl.formatMessage(messages.smashUrlSelectPlaceholder)}
            skin={InputSkin}
            onKeyPress={this.handleSubmitOnEnter}
          />
        )}
      </div>
    );
  }
}
