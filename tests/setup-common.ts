import { After, Before } from "cucumber";
import sinon from "sinon";
import { environment } from "../source/main/environment";

global.environment = environment;
// Add context object to share data between steps
Before(function () {
  this.context = {};
});
After(function () {
  sinon.restore();
});