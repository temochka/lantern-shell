import CodeMirror, { Editor } from "codemirror";
import "codemirror/mode/clojure/clojure";
import "codemirror/mode/sql/sql";
import "codemirror/addon/selection/active-line";
import "codemirror/addon/edit/matchbrackets";
import "codemirror/lib/codemirror.css";
import "codemirror/theme/solarized.css";

customElements.define(
  "code-editor",
  class CodeEditor extends HTMLElement {
    static get observedAttributes() {
      return ["initvalue"];
    }

    _instance?: Editor;
    _value: string | null = null;

    constructor() {
      super();
    }

    get fullValue(): string | null {
      return this._value;
    }

    get initValue(): string | null {
      return this.getAttribute("initvalue");
    }

    set initValue(newValue: string | null) {
      if (newValue !== null && newValue !== this.fullValue) {
        if (!this._instance) return;

        const prevScrollPosition = this._instance.getScrollInfo();
        this._instance.setValue(newValue);
        this._instance.scrollTo(
          prevScrollPosition.left,
          prevScrollPosition.top
        );
      }
    }

    get mode(): string | null {
      return this.getAttribute("mode");
    }

    set mode(value: string | null) {
      if (value === null) value = "sql";
      if (value === this.mode) return;
      if (!this._instance) return;
      this._instance.setOption("mode", value);
    }

    get tabSize(): number | null {
      const str = this.getAttribute("tabSize");
      return str ? parseInt(str) : null;
    }

    set tabSize(value: number | null) {
      if (value === null) value = 2;
      if (value === this.tabSize) return;
      this.tabSize = value;
      if (!this._instance) return;
      this._instance.setOption("tabSize", this.tabSize);
      this._instance.setOption("indentUnit", this.tabSize);
    }

    attributeChangedCallback(
      _name: string,
      _oldValue: string,
      newValue: string
    ) {
      this.initValue = newValue;
    }

    connectedCallback() {
      if (this._instance) return;

      this._instance = CodeMirror(this, {
        lineNumbers: true,
        smartIndent: true,
        styleActiveLine: true,
        indentWithTabs: false,
        matchBrackets: true,
        keyMap: "default",
        tabSize: this.tabSize || 2,
        indentUnit: this.tabSize || 2,
        mode: this.mode || "sql",
        value: this.initValue || "",
        theme: "solarized dark",
        dragDrop: false,
        extraKeys: {
          Tab(cm) {
            let x = "";
            for (let i = cm.getOption("indentUnit")!; i > 0; i--) x += " ";
            cm.replaceSelection(x);
          },
        },
      });
      this._instance.setSize("100%", "100%");

      requestAnimationFrame(() => {
        this._instance?.refresh();
      });

      const runDispatch = () => {
        if (!this._instance) return;
        this._value = this._instance.getDoc().getValue();
        const event = new Event("input");
        this.dispatchEvent(event);
      };

      this._instance.on("changes", runDispatch);
      this._instance.getWrapperElement().style.fontFamily =
        "SF Mono, Menlo, Andale Mono, Monaco, sans-serif";
    }
  }
);
