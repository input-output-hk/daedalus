// @flow
import React, { Component, createRef } from 'react';
import type { ElementRef } from 'react';
import { observer } from 'mobx-react';
import { isEqual, pick } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import type { StakePoolFilterOptionsType } from '../../../stores/StakingStore';
import { defaultStakePoolFilterOptions } from '../../../stores/StakingStore';
import TinyCheckbox from '../../widgets/forms/TinyCheckbox';
import TinyButton from '../../widgets/forms/TinyButton';
import filterIcon from '../../../assets/images/filter-dis-ic.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import styles from './FilterPopOver.scss';

const messages = defineMessages({
  resetFilter: {
    id: 'staking.stakePools.filter.resetFilter',
    defaultMessage: '!!!Reset Filter',
    description: 'Reset Filter button label.',
  },
  publicPoolsWithOffChainData: {
    id: 'staking.stakePools.filter.publicPoolsWithOffChainData',
    defaultMessage: '!!!Public pools with off-chain data',
    description: 'Public pools with off-chain data filter type.',
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

export type FilterPopOverProps = {
  populatedFilterOptions: StakePoolFilterOptionsType,
  onFilter: Function,
  numberOfFilterDimensionsApplied: number,
  isFilterDisabled: boolean,
};

@observer
export default class FilterPopOver extends Component<FilterPopOverProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form: ReactToolboxMobxForm;
  popoverTippyInstance: ElementRef<*> = createRef();

  constructor(props: FilterPopOverProps, context: Object) {
    super(props);

    const {
      populatedFilterOptions: {
        publicPoolsWithOffChainData,
        retiringPools,
        privatePools,
        poolsWithoutOffChainData,
      },
    } = props;

    const { intl } = context;

    this.form = new ReactToolboxMobxForm({
      fields: {
        publicPoolsWithOffChainData: {
          type: 'checkbox',
          label: intl.formatMessage(messages.publicPoolsWithOffChainData),
          value: publicPoolsWithOffChainData,
        },
        retiringPools: {
          type: 'checkbox',
          label: intl.formatMessage(messages.retiringPools),
          value: retiringPools,
        },
        privatePools: {
          type: 'checkbox',
          label: intl.formatMessage(messages.privatePools),
          value: privatePools,
        },
        poolsWithoutOffChainData: {
          type: 'checkbox',
          label: intl.formatMessage(messages.poolsWithoutOffChainData),
          value: poolsWithoutOffChainData,
        },
      },
    });
  }

  fillFormFields = (filterOptions: StakePoolFilterOptionsType) => {
    const {
      publicPoolsWithOffChainData,
      retiringPools,
      privatePools,
      poolsWithoutOffChainData,
    } = filterOptions;

    this.form
      .select('publicPoolsWithOffChainData')
      .set(publicPoolsWithOffChainData);
    this.form.select('retiringPools').set(retiringPools);
    this.form.select('privatePools').set(privatePools);
    this.form.select('poolsWithoutOffChainData').set(poolsWithoutOffChainData);
  };

  resetForm = () => this.fillFormFields(defaultStakePoolFilterOptions);
  restoreForm = () => this.fillFormFields(this.props.populatedFilterOptions);

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
    const publicPoolsWithOffChainDataCheckboxField = form.$(
      'publicPoolsWithOffChainData'
    );
    const retiringPoolsCheckboxField = form.$('retiringPools');
    const privatePoolsCheckboxField = form.$('privatePools');
    const poolsWithoutOffChainDataCheckboxField = form.$(
      'poolsWithoutOffChainData'
    );

    return (
      <div className={styles.fields}>
        <TinyCheckbox {...publicPoolsWithOffChainDataCheckboxField.bind()} />
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
    const { numberOfFilterDimensionsApplied, isFilterDisabled } = this.props;

    return (
      <PopOver
        arrow={false}
        interactive
        trigger="click"
        appendTo={document.body}
        onShow={(instance) => {
          this.restoreForm();
          this.popoverTippyInstance.current = instance;
          setTimeout(() => {
            instance.popper.style.transform = 'translate(-11px, 0)';
          }, 0);
        }}
        duration={0}
        offset={[0, 10]}
        maxWidth={640}
        placement="bottom"
        themeVariables={{
          '--rp-pop-over-bg-color':
            'var(--theme-staking-stake-pools-filter-modal-bg-color)',
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
                {intl.formatMessage(globalMessages.show)}
              </h4>
              <div>
                <button
                  className={styles.titleLink}
                  onClick={this.resetForm}
                  disabled={this.isFormValuesEqualTo(
                    defaultStakePoolFilterOptions
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
        <div className={styles.triggerButton}>
          <TinyButton
            className={classNames(['primary', styles.actionButton])}
            label={
              <>
                <div className={styles.actionLabel}>
                  {intl.formatMessage(globalMessages.filter)}
                  {numberOfFilterDimensionsApplied > 0 && (
                    <span className={styles.numberIndicator}>
                      ({numberOfFilterDimensionsApplied})
                    </span>
                  )}
                </div>
                <SVGInline svg={filterIcon} className={styles.filterIcon} />
              </>
            }
            loading={false}
            disabled={isFilterDisabled}
          />
        </div>
      </PopOver>
    );
  }
}
