import ClientApi from 'daedalus-client-api';

let messageCallback, errorCallback= null;

export default function registerNotifyCallback(tls, onMessage, onError) {
  messageCallback = onMessage;
  errorCallback = onError;
  return ClientApi.notify(
    tls,
    function handleNotifyMessage(...args) {
      if (messageCallback) {
        try {
          messageCallback(...args);
        } catch(e) {
          // The callback might have been released on page refresh etc.
          messageCallback = null;
        }
      }
    },
    function handleNotifyError(...args) {
      if (errorCallback) {
        try {
          errorCallback(...args);
        } catch(e) {
          // The callback might have been released on page refresh etc.
          errorCallback = null;
        }
      }
    }
  );
}
