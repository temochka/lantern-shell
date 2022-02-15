import { Elm } from "LanternShell";
import WebSocketClient from "./LanternJs/WebSocketClient";
import "./LanternJs/WebComponents/CodeEditor";

const mountTarget = document.getElementById("main");

if (mountTarget) {
  const app = Elm.LanternShell.init({
    node: mountTarget,
    flags: { width: window.innerWidth, height: window.innerHeight },
  });
  const wsScheme = location.protocol.match(/^https/) ? "wss" : "ws";
  const connection = new WebSocketClient(
    `${wsScheme}://${window.location.host}/_api/ws`,
    (message) => {
      app.ports.lanternResponsePort.send(message);
    }
  );
  app.ports.lanternRequestPort.subscribe((data) => connection.send(data));
} else {
  console.error('The Elm app requires an element with id="main" to mount.');
}
