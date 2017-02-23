import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  setCertificate: {
    certificate: PropTypes.instanceOf(File).isRequired,
  },
  setPassPhrase: {
    passPhrase: PropTypes.string.isRequired,
  },
  setRedemptionCode: {
    redemptionCode: PropTypes.string.isRequired,
  },
  redeemAda: {
    walletId: PropTypes.string.isRequired,
  },
  adaSuccessfullyRedeemed: {},
  closeAdaRedemptionSuccessOverlay: {},
});
