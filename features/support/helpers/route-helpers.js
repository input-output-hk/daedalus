export const getCurrentAppRoute = async function() {
  const url = (await this.client.url()).value;
  return url.substring(url.indexOf('#/') + 1); // return without the hash
};

export const waitUntilUrlEquals = function(expectedUrl) {
  const context = this;
  return context.client.waitUntil(async function() {
    const url = await getCurrentAppRoute.call(context);
    return url === expectedUrl;
  });
};

export const navigateTo = function(requestedRoute) {
  return this.client.execute(function(route) {
    daedalus.actions.router.goToRoute.trigger({ route });
  }, requestedRoute);
}
