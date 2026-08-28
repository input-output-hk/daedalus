// The detail route embeds a DRep id in the URL; analytics payloads carry the
// current route, so the id segment is replaced with a literal placeholder
// before any URL leaves the renderer.
const DREP_DETAIL_SEGMENT = /^(governance\/dreps\/)[^/?#]+/;

export function maskAnalyticsRoute(route: string): string {
  return route.replace(DREP_DETAIL_SEGMENT, '$1:drepId');
}
