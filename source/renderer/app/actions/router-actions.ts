import Action from './lib/Action'; // ======= ROUTER ACTIONS =======

export default class RouterActions {
  goToRoute: Action<{
    route: string;
    params?: Record<string, any> | null | undefined;
  }> = new Action();
}
