# Hyperlinks can point to any part of a document.
Hyperlinks point to nodes (or, equivalently, to branches).

The target of every hyperlink is a node. If the target is a document root, we can say the link targets the entire document. But it might target some other node deeper in the document.

For example, consider a document called `eggs` like this:
```
  * eggs
  ** health
  *** protein
  *** cholesterol
  ** recipes
  ** mystical
  *** Which came first, the chicken or the egg?
```
The document branches into three parts: `health`, `recipes` and `mystical`. A hyperlink could be to the whole document, or to the `eggs / health` branch, or the `eggs / health / protein` branch. (The last branch mentioned would probably be about the protein in eggs, but the app does not constrain how a user should interpret a path like `eggs / health / protein`.)
