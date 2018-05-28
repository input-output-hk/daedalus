// @flow
import BigNumber from 'bignumber.js';
import { DECIMAL_PLACES_IN_ETC } from '../../config/numbersConfig';

// Amount shown in the Topbar and Sidebar
export const formattedWalletAmount = (
  amount: BigNumber,
  withCurrency: boolean = true,
  long: boolean = false,
) => {
  let formattedAmount = '';

  /* eslint-disable max-len */
  if (long) {
    formattedAmount = amount.toFormat(DECIMAL_PLACES_IN_ETC);
  } else if (amount.isZero()) {
    formattedAmount = '0';
  } else if (amount.lessThan(0.000001)) {
    formattedAmount = '< 0.000001';
  } else if (amount.lessThan(1)) {
    formattedAmount = amount.round(6, BigNumber.ROUND_DOWN);
  } else if (amount.lessThan(1000)) {
    formattedAmount = amount.round(1, BigNumber.ROUND_DOWN);
  } else if (amount.lessThan(1000000)) {
    formattedAmount = `${amount.dividedBy(1000).round(1, BigNumber.ROUND_DOWN)}K`;
  } else if (amount.lessThan(1000000000)) {
    formattedAmount = `${amount.dividedBy(1000000).round(1, BigNumber.ROUND_DOWN)}M`;
  } else if (amount.lessThan(1000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000).round(1, BigNumber.ROUND_DOWN)}B`;
  } else if (amount.lessThan(1000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000).round(1, BigNumber.ROUND_DOWN)}T`;
  } else if (amount.lessThan(1000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000).round(1, BigNumber.ROUND_DOWN)}KT`;
  } else if (amount.lessThan(1000000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000000).round(1, BigNumber.ROUND_DOWN)}MT`;
  } else if (amount.lessThan(1000000000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000000000).round(1, BigNumber.ROUND_DOWN)}BT`;
  } else if (amount.lessThan(1000000000000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000000000000).round(1, BigNumber.ROUND_DOWN)}TT`;
  } else if (amount.lessThan(1000000000000000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000000000000000).round(1, BigNumber.ROUND_DOWN)}KTT`;
  } else if (amount.lessThan(1000000000000000000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000000000000000000).round(1, BigNumber.ROUND_DOWN)}MTT`;
  } else if (amount.lessThan(1000000000000000000000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000000000000000000000).round(1, BigNumber.ROUND_DOWN)}BTT`;
  } else if (amount.lessThan(1000000000000000000000000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000000000000000000000000).round(1, BigNumber.ROUND_DOWN)}TTT`;
  } else if (amount.lessThan(1000000000000000000000000000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000000000000000000000000000).round(1, BigNumber.ROUND_DOWN)}KTTT`;
  } else if (amount.lessThan(1000000000000000000000000000000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000000000000000000000000000000).round(1, BigNumber.ROUND_DOWN)}MTTT`;
  } else if (amount.lessThan(1000000000000000000000000000000000000000000000000)) {
    formattedAmount = `${amount.dividedBy(1000000000000000000000000000000000000000000000).round(1, BigNumber.ROUND_DOWN)}BTTT`;
  } else {
    formattedAmount = `${amount.dividedBy(1000000000000000000000000000000000000000000000000).round(1, BigNumber.ROUND_DOWN)}TTTT`;
  }
  /* eslint-disable max-len */

  if (withCurrency) formattedAmount += ' ETC';

  return formattedAmount.toString();
};

/* eslint-disable no-tabs */
// Symbol	  Name	              Scientific Notation
// K	      Thousand	          1.00E+03
// M	      Million	            1.00E+06
// B	      Billion	            1.00E+09
// T	      Trillion	          1.00E+12
// KT	      Quadrillion	        1.00E+15
// MT	      Quintillion	        1.00E+18
// BT	      Sextillion	        1.00E+21
// TT	      Septillion	        1.00E+24
// KTT	    Octillion	          1.00E+27
// MTT	    Nonillion	          1.00E+30
// BTT	    Decillion	          1.00E+33
// TTT	    Undecillion	        1.00E+36
// KTTT	    Duodecillion	      1.00E+39
// MTTT	    Tredecillion	      1.00E+42
// BTTT	    Quattuordecillion	  1.00E+45
// TTTT	    Quindecillion	      1.00E+48
/* eslint-disable no-tabs */
