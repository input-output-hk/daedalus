// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import TinyButton from '../../widgets/forms/TinyButton';
import filterIcon from '../../../assets/images/filter-dis-ic.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import styles from './FilterButton.scss';

type Props = {
  faded: boolean,
  onClick: Function,
};

@observer
export default class FilterButton extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { faded, onClick } = this.props;
    const buttonLabel = (
      <>
        <div className={styles.actionLabel}>
          {intl.formatMessage(globalMessages.filter)}
        </div>
        <SVGInline svg={filterIcon} className={styles.filterIcon} />
      </>
    );
    const buttonClasses = classNames([
      'primary',
      styles.actionButton,
      faded ? styles.actionButtonFaded : null,
    ]);

    return (
      <div className={styles.component}>
        <TinyButton
          className={buttonClasses}
          label={buttonLabel}
          loading={false}
          onClick={onClick}
        />
      </div>
    );
  }
}
