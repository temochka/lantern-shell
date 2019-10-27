interface ElmApp {
  init(options: { node: HTMLElement }): void
}

declare module "DevTools" {
  export const Elm: { DevTools : ElmApp };
}
