import RouteParser from 'route-parser';

export const matchRoute = (pattern, path) => new RouteParser(pattern).match(path);
