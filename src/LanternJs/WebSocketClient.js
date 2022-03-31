export default class WebSocketClient {
  #endpoint;
  #connection;
  #messageQueue;
  #reconnectInterval;
  #msToReconnect;
  #onMessage;

  constructor(endpoint, onMessage) {
    this.endpoint = endpoint;
    this.messageQueue = [];
    this.reconnectInterval = 10;
    this.msToReconnect = 10;
    this.onMessage = onMessage;

    this.connection = new WebSocket(endpoint);
    this.configureConnection(this.connection);
  }

  send(message) {
    this.messageQueue.push(message);
    this.flush();
  }

  flush() {
    if (this.connection.readyState == WebSocket.CONNECTING) {
      return;
    } else if (this.connection.readyState != WebSocket.OPEN) {
      this.reconnect();
      return;
    }

    while (true) {
      const message = this.messageQueue.shift();
      if (!message) break;

      try {
        this.connection.send(message);
      } catch (INVALID_STATE_ERR) {
        this.messageQueue = [message, ...this.messageQueue];
        break;
      }
    }
  }

  reconnect() {
    if (
      this.connection.readyState === WebSocket.CONNECTING ||
      this.connection.readyState === WebSocket.OPEN
    ) {
      return;
    }

    this.msToReconnect *= 2;
    this.connection = new WebSocket(this.endpoint);
    this.configureConnection(this.connection);
  }

  configureConnection(connection) {
    connection.onopen = () => {
      this.msToReconnect = this.reconnectInterval;
      this.flush();
    };
    connection.onmessage = event => {
      this.onMessage(event.data);
    };
    connection.onerror = event => {
      console.error(
        "WebSocket error: ",
        event,
        " Will try to reconnect in",
        this.msToReconnect,
        "ms"
      );
      window.setTimeout(() => this.reconnect(), this.msToReconnect);
    };
    connection.onclose = event => {
      console.log("WebSocket disconnected.");
    };
  }
}
