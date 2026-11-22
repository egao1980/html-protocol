# html-protocol

Lenient **HTML** parse / serialize / query for [cl-stack](https://github.com/egao1980/cl-stack). Not XML Infoset — that is [`xml-protocol`](https://github.com/egao1980/xml-protocol).

| System | Role | OCI |
|--------|------|-----|
| `html-protocol` (`stack-html`) | CLOS nodes, `parse` / `serialize`, tiny CSS `select` | **0.1.1** |
| `html-backend-plump` | Default — [plump](https://shinmera.com/docs/plump/) | **0.1.1** |

**CSS/JS stay opaque text.** `<style>` / `<script>` / `style=` / `href` / `src` are not interpreted. No cascade, no VM. `select` is a tree match (`tag`, `#id`, `.class`, descendant, `>`).

**Serialize escapes by construction.** Every text node is written with `&` `<` `>` escaped, at any depth (document-level text and fragments included); only the text children of `<script>` / `<style>` are written raw. Attribute values escape `&` `"` `<`. Attributes print in the order given to `make-html-element` or parsed; `(setf element-attr)` replaces in place or appends.

```lisp
(asdf:load-system "html-backend-plump")

(let* ((doc (stack-html:parse "<div class='x'><p>hi</p></div>"))
       (p (first (stack-html:select doc "div.x p"))))
  (stack-html:element-text p)           ; "hi"
  (stack-html:serialize doc))
```

## License

MIT — see [LICENSE](LICENSE). Plump is zlib.
