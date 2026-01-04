# Skg

## Purpose
Skg is for thinking, individually and collectively. It is a simple, novel way to write, collect (from other writers), organize and share information. (Any of which can be ignored -- you don't have to share, for instance, to use the app productively.) In a world of abundant information, I hope Skg will let users direct their attention, process information and collaborate more effectively.

Skg begins from a [simple web-of-thought data model](docs/data-model-trees-with-links.md) that [many people already love](docs/who-maps-knowledge.md). Its novelty is to extend that model [to integrate the thoughts of different users](docs/skg-enables-deep-sharing.md).

Technical doubts? See the small [FAQ](docs/FAQ.md).

## If you need more motivation
to get excited about this idea:

- [Preempting some common concerns](docs/preempt.md) about ownership and privacy.
- Some less obvious motivations
  - [Writing is like thinking with power tools.](docs/wise-people-say-write.md)
  - [Traditional data ownership is awkward.](docs/ownership-has-been-awkward.md)
  - The web of thought experience feels good.
    - [It is liberating, fun, peaceful, and psychically integrating.](docs/it-will-be-fun.md)
    - [It is Zen.](docs/knowledge-gardening-is-zen.md)
  - [Private writing is powerful.](docs/private-writing.md)
  - [Integrating your public and private writing only recently became possible](docs/integrate-public-and-private.md)
    - which is important, because [writing is like thinking with power tools](docs/wise-people-say-write.md).
- And of course, [sharing information is powerful.](docs/sharing-is-powerful.md)

## If you're a programmer, please help!
[Skg uses cool technologies!](docs/cool-tech-in-skg.md), including Rust, TypeDB and Emacs. (That said, I'm flexible -- if you want to write a non-Emacs client, or rewrite the server in Haskell, let's go!)

- [Usage: How to run Skg](docs/usage.md)
- Skg's data model and user experience
  - Trees and textlinks: [Some simple but important definitions](docs/vocabulary.md)
  - [TextLinks can point to any part of a document.](docs/hyperlinks.md)
  - ["Lenses": Subscription and overriding in detail](docs/lenses.md)
  - [The data model](docs/data-model-skg-extensions.md).
  - [The sharing model](docs/sharing-model.md).
- [What's done, and what's left to do](docs/progress.md)
- Some reasonable jumping-off points for understanding the code
  - [The TypeDB schema](schema.tql)
  - [The Rust types](rust/types.rs)
  - [The Rust server](rust/serve.rs). (The `main` module is just a wrapper for this module.)
  - [The Emacs client](elisp/skg-client.el)
