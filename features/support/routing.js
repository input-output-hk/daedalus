export default function () {
  this.Before(function() {
    this.navigateTo = (requestedRoute) => {
      return this.client.execute(function(route) {
        daedalus.state.router.transitionTo(route);
      }, requestedRoute);
    }
  });
}
