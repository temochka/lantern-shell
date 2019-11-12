import { Elm } from "DevTools";

const mountTarget = document.getElementById("main");

if (mountTarget) {
  const connection = new WebSocket("ws://localhost:9000/_api/async");

  connection.onopen = () => {
    const app = Elm.DevTools.init({ node: mountTarget });

    app.ports.lanternRequestPort.subscribe(data => connection.send(data));
    connection.onmessage = event => {
      app.ports.lanternResponsePort.send(event.data);
    };
  };
} else {
  console.error('The Elm app requires an element with id="main" to mount.');
}
