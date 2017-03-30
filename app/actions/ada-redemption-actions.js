import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  chooseRedemptionType: {
    redemptionType: PropTypes.string.isRequired,
  },
  setCertificate: {
    certificate: PropTypes.instanceOf(File).isRequired,
  },
  removeCertificate: {},
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
