export default class WebSocketClient {
  private endpoint: string;
  private connection: WebSocket;
  private messageQueue: Array<string>;
  private reconnectInterval: number;
  private msToReconnect: number;
  private onMessage: (msg: string) => void;

  constructor(endpoint: string, onMessage: (msg: string) => void) {
    this.endpoint = endpoint;
    this.messageQueue = [];
    this.reconnectInterval = 10;
    this.msToReconnect = 10;
    this.onMessage = onMessage;

    this.connection = new WebSocket(endpoint);
    this.configureConnection(this.connection);
  }

  send(message: string): void {
    this.messageQueue.push(message);
    this.flush();
  }

  flush(): void {
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

  private reconnect() {
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

  private configureConnection(connection: WebSocket) {
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
