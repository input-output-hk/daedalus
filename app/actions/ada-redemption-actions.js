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
  setEmail: {
    email: PropTypes.string.isRequired,
  },
  setAdaPasscode: {
    adaPasscode: PropTypes.string.isRequired,
  },
  setAdaAmount: {
    adaAmount: PropTypes.string.isRequired,
  },
  redeemAda: {
    walletId: PropTypes.string.isRequired,
  },
  redeemPaperVendedAda: {
    walletId: PropTypes.string.isRequired,
    shieldedRedemptionKey: PropTypes.string.isRequired,
  },
  adaSuccessfullyRedeemed: {},
  closeAdaRedemptionSuccessOverlay: {},
});
