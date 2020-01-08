interface ElmAppFactory<A> {
  init(options: { node: HTMLElement }): A;
}

interface InPort {
  subscribe(callback: (data: string) => void): void;
}

interface OutPort {
  send(data: string): void;
}

interface LanternShellApp {
  ports: { lanternRequestPort: InPort; lanternResponsePort: OutPort };
}

declare module "LanternShell" {
  export const Elm: { LanternShell: ElmAppFactory<LanternShellApp> };
}
