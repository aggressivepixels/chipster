import { Elm } from "./src/Main.elm";

Elm.Main.init({
  node: document.getElementById("elm"),
  flags: new Date().getMilliseconds()
});
