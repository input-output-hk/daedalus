// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import moment from 'moment';
import styles from './DelegationInfo.scss';

type Props = {
  heading: string,
  info: string,
  timeLeftDesc: string,
  timeLeft: number,
  buttonLabel: string,
};

@observer
export default class DelegationInfo extends Component<Props> {
  render() {
    const { heading, info, timeLeftDesc, timeLeft, buttonLabel } = this.props;
    const duration = moment.duration(timeLeft);
    const days = duration.days();
    const hours = duration.hours();
    const minutes = duration.minutes();
    const timeLeftString = `${days} days ${hours} hours ${minutes} minutes`;

    return (
      <div className={styles.component}>
        <div className={styles.heading}>{heading}</div>
        <div className={styles.info}>{info}</div>
        <div className={styles.timeLeftDesc}>{timeLeftDesc}</div>
        <div className={styles.timeLeft}>{timeLeftString}</div>
        <Button label={buttonLabel} skin={ButtonSkin} />
      </div>
    );
  }
}
