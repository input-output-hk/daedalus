// How long the Directory waits for the wallet's DRep list before it shows the
// list as not returned. The wallet cannot cancel a request already sent, so a
// load that outlasts this limit is still applied when its answer arrives.
export const DREP_LIST_TIMEOUT = 30 * 1000;
