export default function () {
  this.Before(function() {
    this.intl = async (translationId, translationValues = {}) => {
      const translation = await this.client.execute(function(id, values) {
        return daedalus.controller.translate({ id }, values);
      }, translationId, translationValues);
      return translation.value;
    };
  });
}
