import { Elm } from "./src/Main.elm";
import games from "./games/*.bin";

Elm.Main.init({
  node: document.getElementById("elm"),
  flags: {
    seed: Date.now(),
    games: Object.entries(games).map(([name, data]) => ({ name, data }))
  }
});
