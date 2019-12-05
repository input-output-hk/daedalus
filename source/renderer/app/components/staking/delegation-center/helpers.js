// @flow
import { get } from 'lodash';
import React from 'react';
import SVGInline from 'react-svg-inline';
import styles from './DelegationCenterHeader.scss';
import delimeterIcon from '../../../assets/images/delimeter.inline.svg';
import delimeterSlashIcon from '../../../assets/images/delimeter-slash.inline.svg';

export const with2Decimals = (value: number) => {
  const formattedValue = value.toString().match(/^-?\d+(?:\.\d{0,2})?/);
  const result = get(formattedValue, 0, 0);
  return result;
};

export const generateFieldPanel = (labels: any, values: any, index: number) => {
  const value = values[index];
  const includeSlashDelimeter = index === values.length - 2;
  const includeDotsDelimeter =
    !includeSlashDelimeter && index !== values.length - 1;
  const labelStr = labels[index];
  const valueStr = value.toString();
  let zeroValues = '';
  if (index === 1 && valueStr.length < values[index + 1].toString().length) {
    const zerosToAdd =
      parseInt(values[index + 1].toString().length, 10) -
      parseInt(valueStr.length, 10);
    switch (zerosToAdd) {
      case 1:
        zeroValues = '0';
        break;
      case 2:
        zeroValues = '00';
        break;
      case 3:
        zeroValues = '000';
        break;
      case 4:
        zeroValues = '0000';
        break;
      default:
        break;
    }
  }

  return (
    <div className={styles.fieldPanel}>
      <div className={styles.left}>
        <div className={styles.fieldLabel}>{labelStr}</div>
        <div className={styles.fieldValue}>
          {zeroValues && <span>{zeroValues}</span>}
          {valueStr}
        </div>
      </div>
      {includeDotsDelimeter && (
        <div className={styles.right}>
          <SVGInline svg={delimeterIcon} className={styles.delimeterIcon} />
        </div>
      )}
      {includeSlashDelimeter && (
        <div className={styles.right}>
          <SVGInline
            svg={delimeterSlashIcon}
            className={styles.delimeterSlashIcon}
          />
        </div>
      )}
    </div>
  );
};
