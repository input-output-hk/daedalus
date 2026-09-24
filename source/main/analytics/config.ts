export type AnalyticsConfig = { endpoint: string } | null;

// Main-process configuration only. No endpoint is built in or accepted over IPC.
export function analyticsConfig(
  env: Readonly<Record<string, string | undefined>>,
  packaged: boolean
): AnalyticsConfig {
  if (env.DAEDALUS_ARIADNE_ANALYTICS_ENABLED !== 'true') return null;
  try {
    const url = new URL(env.DAEDALUS_ARIADNE_ANALYTICS_URL || '');
    if (
      url.username ||
      url.password ||
      url.search ||
      url.hash ||
      url.pathname !== '/api/analytics/event'
    )
      return null;
    const developmentLoopback =
      !packaged &&
      env.NODE_ENV === 'development' &&
      env.DAEDALUS_ARIADNE_ALLOW_LOOPBACK_HTTP === 'true' &&
      ['localhost', '127.0.0.1', '[::1]'].includes(url.hostname);
    if (
      url.protocol !== 'https:' &&
      !(url.protocol === 'http:' && developmentLoopback)
    )
      return null;
    return { endpoint: url.href };
  } catch {
    return null;
  }
}
