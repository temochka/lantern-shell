import {Elm} from "DevTools";

const mountTarget = document.getElementById("main");

if (mountTarget) {
  Elm.DevTools.init({node: mountTarget});
} else {
  console.error("The Elm app requires an element with id=\"main\" to mount.");
}
