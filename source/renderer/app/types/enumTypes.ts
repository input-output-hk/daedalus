type $ElementType<
  T extends { [P in K & any]: any },
  K extends keyof T | number
> = T[K];

export type EnumMap<
  K extends string,
  V,
  O extends Record<string, any> = any
> = O & Record<K, V & $ElementType<O, K>>;
