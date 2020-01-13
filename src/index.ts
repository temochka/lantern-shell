import { Elm } from "LanternShell";
import WebSocketClient from "./LanternJs/WebSocketClient";

const mountTarget = document.getElementById("main");

if (mountTarget) {
  const app = Elm.LanternShell.init({ node: mountTarget });
  const connection = new WebSocketClient(
    `ws://${window.location.host}/_api/ws`,
    message => {
      app.ports.lanternResponsePort.send(message);
    }
  );
  app.ports.lanternRequestPort.subscribe(data => connection.send(data));
} else {
  console.error('The Elm app requires an element with id="main" to mount.');
}
