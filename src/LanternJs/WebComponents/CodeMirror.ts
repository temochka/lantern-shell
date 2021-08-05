import {LitElement, html} from 'lit';
import {customElement, property} from 'lit/decorators.js';

@customElement('code-editor')
export class CodeEditor extends LitElement {
  @property({type: String})
  value: string = "";

  render() {
    return html`<textarea>${this.value}</textarea>`
  }
}
