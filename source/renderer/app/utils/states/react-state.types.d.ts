type Expand<T> = T extends infer U ? { [K in keyof U]: U[K] } : never;

type TypedStates<T extends Record<string, unknown>> = {
  [K in keyof T]: Expand<{ state: K } & T[K]>;
}[keyof T];

type TypedActions<T extends Record<string, unknown>> = {
  [K in keyof T]: Expand<{ type: K } & T[K]>;
}[keyof T];

type TypedCommands<T extends Record<string, unknown>> = {
  [K in keyof T]: Expand<{ cmd: K } & T[K]>;
}[keyof T];
