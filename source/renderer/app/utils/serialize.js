export default (obj) =>
  Object.entries(obj)
    .map(([key, val]) => `${encodeURIComponent(key)}=${encodeURIComponent(val)}`).join('&');
