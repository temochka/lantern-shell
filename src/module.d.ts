interface ElmAppFactory<A> {
  init(options: { node: HTMLElement }): A;
}

interface InPort {
  subscribe(callback: (data: string) => void): void;
}

interface OutPort {
  send(data: string): void;
}

interface DevToolsApp {
  ports: { lanternRequestPort: InPort; lanternResponsePort: OutPort };
}

declare module "DevTools" {
  export const Elm: { DevTools: ElmAppFactory<DevToolsApp> };
}
