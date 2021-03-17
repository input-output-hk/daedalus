// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component, createRef } from 'react';
import type { Element, ElementRef } from 'react';
import { observer } from 'mobx-react';
import { isEqual, pick } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import type { StakePoolFilterOptionsType } from '../../../stores/StakingStore';
import { emptyStakePoolFilterOptions } from '../../../stores/StakingStore';
import TinyCheckbox from '../../widgets/forms/TinyCheckbox';
import TinyButton from '../../widgets/forms/TinyButton';
import globalMessages from '../../../i18n/global-messages';
import styles from './FilterDialog.scss';

const messages = defineMessages({
  resetFilter: {
    id: 'staking.stakePools.filter.resetFilter',
    defaultMessage: '!!!Reset Filter',
    description: 'Reset Filter button label.',
  },
  retiringPools: {
    id: 'staking.stakePools.filter.retiringPools',
    defaultMessage: '!!!Retiring pools',
    description: 'Retiring pools filter type.',
  },
  privatePools: {
    id: 'staking.stakePools.filter.privatePools',
    defaultMessage: '!!!Private pools',
    description: 'Private pools filter type.',
  },
  poolsWithoutOffChainData: {
    id: 'staking.stakePools.filter.poolsWithoutOffChainData',
    defaultMessage: '!!!Pools without off-chain data',
    description: 'Pools without off-chain data filter type.',
  },
  apply: {
    id: 'staking.stakePools.filter.apply',
    defaultMessage: '!!!Apply',
    description: 'Filter button label.',
  },
});

export type FilterDialogProps = {
  populatedFilterOptions: StakePoolFilterOptionsType,
  onFilter: Function,
  triggerElement?: Element<*>,
};

@observer
export default class FilterDialog extends Component<FilterDialogProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form: ReactToolboxMobxForm;
  popoverTippyInstance: ElementRef<*> = createRef();

  constructor(props: FilterDialogProps, context: Object) {
    super(props);

    const {
      populatedFilterOptions: {
        retiringPoolsChecked,
        privatePoolsChecked,
        poolsWithoutOffChainDataChecked,
      },
    } = props;

    const { intl } = context;

    this.form = new ReactToolboxMobxForm({
      fields: {
        retiringPoolsChecked: {
          type: 'checkbox',
          label: intl.formatMessage(messages.retiringPools),
          value: retiringPoolsChecked,
        },
        privatePoolsChecked: {
          type: 'checkbox',
          label: intl.formatMessage(messages.privatePools),
          value: privatePoolsChecked,
        },
        poolsWithoutOffChainDataChecked: {
          type: 'checkbox',
          label: intl.formatMessage(messages.poolsWithoutOffChainData),
          value: poolsWithoutOffChainDataChecked,
        },
      },
    });
  }

  fillFormFields = (filterOptions: StakePoolFilterOptionsType) => {
    const {
      retiringPoolsChecked,
      privatePoolsChecked,
      poolsWithoutOffChainDataChecked,
    } = filterOptions;

    this.form.select('retiringPoolsChecked').set(retiringPoolsChecked);
    this.form.select('privatePoolsChecked').set(privatePoolsChecked);
    this.form
      .select('poolsWithoutOffChainDataChecked')
      .set(poolsWithoutOffChainDataChecked);
  };

  resetForm = () => this.fillFormFields(emptyStakePoolFilterOptions);

  isFormValuesEqualTo = (comparedFilterOptions: StakePoolFilterOptionsType) => {
    const formFieldNames = Object.keys(this.form.fields.toJSON());
    return isEqual(
      this.form.values(),
      pick(comparedFilterOptions, formFieldNames)
    );
  };

  handleSubmit = () => {
    this.form.submit({
      onSuccess: () => {
        const { onFilter } = this.props;
        const formValues = this.form.values();
        onFilter(formValues);
      },
      onError: () => null,
    });
    if (this.popoverTippyInstance.current) {
      this.popoverTippyInstance.current.hide();
    }
  };

  renderFields = () => {
    const { form } = this;
    const retiringPoolsCheckboxField = form.$('retiringPoolsChecked');
    const privatePoolsCheckboxField = form.$('privatePoolsChecked');
    const poolsWithoutOffChainDataCheckboxField = form.$(
      'poolsWithoutOffChainDataChecked'
    );

    return (
      <div className={styles.fields}>
        <TinyCheckbox {...retiringPoolsCheckboxField.bind()} />
        <TinyCheckbox {...privatePoolsCheckboxField.bind()} />
        <TinyCheckbox {...poolsWithoutOffChainDataCheckboxField.bind()} />
      </div>
    );
  };

  renderActionButton = () => {
    const { intl } = this.context;
    const { populatedFilterOptions } = this.props;

    return (
      <div className={styles.action}>
        <TinyButton
          label={intl.formatMessage(messages.apply)}
          loading={false}
          disabled={this.isFormValuesEqualTo(populatedFilterOptions)}
          onClick={this.handleSubmit}
        />
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const { triggerElement } = this.props;

    return (
      <PopOver
        arrow={false}
        interactive
        trigger="click"
        appendTo={document.body}
        onShow={(instance) => {
          this.popoverTippyInstance.current = instance;
        }}
        duration={0}
        offset={[0, 10]}
        maxWidth={640}
        placement="bottom"
        themeVariables={{
          '--rp-pop-over-bg-color':
            'var(--theme-stake-pools-filter-modal-bg-color)',
          '--rp-pop-over-box-shadow': '0 5px 20px 0 rgba(0, 0, 0, 0.25)',
          '--rp-pop-over-border-radius': '4px',
          '--rp-pop-over-border-style': 'solid',
          '--rp-pop-over-padding': 0,
        }}
        popperOptions={{
          modifiers: [
            {
              // This keeps the popover always 20px away from the screen edge
              name: 'preventOverflow',
              options: {
                padding: 20,
              },
            },
          ],
        }}
        content={
          <div className={styles.component}>
            <div className={styles.title}>
              <h4 className={styles.titleText}>
                {intl.formatMessage(globalMessages.filter)}
              </h4>
              <div>
                <button
                  className={styles.titleLink}
                  onClick={this.resetForm}
                  disabled={this.isFormValuesEqualTo(
                    emptyStakePoolFilterOptions
                  )}
                >
                  {intl.formatMessage(messages.resetFilter)}
                </button>
              </div>
            </div>
            <div className={styles.content}>
              {this.renderFields()}
              {this.renderActionButton()}
            </div>
          </div>
        }
      >
        {triggerElement}
      </PopOver>
    );
  }
}
