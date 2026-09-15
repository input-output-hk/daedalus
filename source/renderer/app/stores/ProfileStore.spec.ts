/**
 * The one-time decimal places notice: whether a profile is shown it, and
 * whether it comes back.
 *
 * The store is built directly and `setup()` is never called, so nothing
 * registers a reaction or a listener; the one chain under test is driven by
 * hand against a stubbed browser-storage API.
 */
import ProfileStore from './ProfileStore';
import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import { noopAnalyticsTracker } from '../analytics';

const makeStore = ({
  acknowledged,
  termsAccepted,
}: {
  acknowledged: boolean;
  termsAccepted: boolean;
}) => {
  const localStorage = {
    getDecimalPlacesNoticeAcknowledged: jest
      .fn()
      .mockResolvedValue(acknowledged),
    setDecimalPlacesNoticeAcknowledged: jest.fn().mockResolvedValue(undefined),
    getTermsOfUseAcceptance: jest.fn().mockResolvedValue(termsAccepted),
  };
  const api = { localStorage } as unknown as Api;
  const store = new ProfileStore(
    api,
    {} as unknown as ActionsMap,
    noopAnalyticsTracker
  );
  return { store, localStorage };
};

describe('ProfileStore', () => {
  describe('the one-time decimal places notice', () => {
    it('is not shown before the stored flag has been read', () => {
      const { store } = makeStore({ acknowledged: false, termsAccepted: true });
      // A banner that flashes at every start while a storage read is in flight
      // is worse than one that appears a moment late.
      expect(store.isDecimalPlacesNoticeAcknowledged).toBe(true);
    });

    it('is shown to a profile that existed before the update', async () => {
      const { store, localStorage } = makeStore({
        acknowledged: false,
        termsAccepted: true,
      });

      await (store as any)._getDecimalPlacesNoticeAcknowledgement();

      expect(store.isDecimalPlacesNoticeAcknowledged).toBe(false);
      expect(
        localStorage.setDecimalPlacesNoticeAcknowledged
      ).not.toHaveBeenCalled();
    });

    it('is never shown to a profile being created now', async () => {
      // A profile that has not accepted the terms of use is being created at
      // this start, so there is no habit about entering amounts to correct. The
      // flag is written rather than left unset, so the notice does not turn up
      // later once the terms have been accepted.
      const { store, localStorage } = makeStore({
        acknowledged: false,
        termsAccepted: false,
      });

      await (store as any)._getDecimalPlacesNoticeAcknowledgement();

      expect(store.isDecimalPlacesNoticeAcknowledged).toBe(true);
      expect(
        localStorage.setDecimalPlacesNoticeAcknowledged
      ).toHaveBeenCalled();
    });

    it('does not come back for a profile that has been told', async () => {
      const { store, localStorage } = makeStore({
        acknowledged: true,
        termsAccepted: true,
      });

      await (store as any)._getDecimalPlacesNoticeAcknowledgement();

      expect(store.isDecimalPlacesNoticeAcknowledged).toBe(true);
      expect(
        localStorage.setDecimalPlacesNoticeAcknowledged
      ).not.toHaveBeenCalled();
    });

    it('writes the dismissal down, so a restart reads it back', async () => {
      const { store, localStorage } = makeStore({
        acknowledged: false,
        termsAccepted: true,
      });
      await (store as any)._getDecimalPlacesNoticeAcknowledgement();
      expect(store.isDecimalPlacesNoticeAcknowledged).toBe(false);

      await (store as any)._acknowledgeDecimalPlacesNotice();

      expect(store.isDecimalPlacesNoticeAcknowledged).toBe(true);
      expect(
        localStorage.setDecimalPlacesNoticeAcknowledged
      ).toHaveBeenCalledTimes(1);

      // The restart: a second profile reading what the first one wrote.
      const restarted = makeStore({ acknowledged: true, termsAccepted: true });
      await (restarted.store as any)._getDecimalPlacesNoticeAcknowledgement();
      expect(restarted.store.isDecimalPlacesNoticeAcknowledged).toBe(true);
    });
  });
});
