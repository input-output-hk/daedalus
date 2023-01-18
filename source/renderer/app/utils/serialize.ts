export default (obj: {}) =>
  Object.entries(obj)
    .map(
      ([key, val]: [string, any]) =>
        `${encodeURIComponent(key)}=${encodeURIComponent(val)}`
    )
    .join('&');
