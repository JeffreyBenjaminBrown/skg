Skg is for thinking, individually and collectively. It is a simple, novel way to write, collect (from elsewhere), organize and share information. (Any of which can be ignored -- you don't have to share, for instance, to use it productively.) In a world of abundant information, Skg will let users direct their attention, process information and collaborate more effectively.

Skg is [extremely open-source](LICENSE.md). Nobody will kidnap your data.
# What's unique about Skg
Skg is the first app that lets a user:
## Subscribe granularly.
In Skg you do not subscribe as a person to another person. Instead, you subscribe parts of your data to parts of other peoples' data.

No feed will try to show you topics of its inscrutable and nefarious choosing. Rather, when you consciously choose to look at a topic, you will see what the people you subscribe to have shared about that topic. What they have collected and written about on other topics will not interfere with your view of the topic you chose.

In a world of scarce attention overflowing with information, granular subscription should help.
## Share granularly.
You can share not just whole documents, but selected parts of documents. (Technically, each document is a tree, and each spot in the tree can be shared separately.)

[Private](private-writing.md) and [public](public-writing.md) writing are both important, in different ways. Granular sharing lets you integrate your private and public thoughts, without exposing anything you want to withhold. For instance, you might have a document on love that you want to share, but might like a private section of it called "things I learned about intimacy the hard way". Skg will let you do that. Nobody will see the private branch, but to you it will appear alongside your other thoughts on love.

This kind of context switching is recursive: Just as a private branch can be part of public content, some branch of that private branch can again be public.

When you want to view your work in "public mode" (maybe to share your screen) private sections will be hidden.
## Manipulate others' data as if it were your own, while staying informed of their changes.
If another user shares data with you, you can navigate, edit and reshape it just like it was yours. When you modify it, your data transparently gains a `lens` onto their document. The lens is like a clone. It comes into existence with all the original's text and branches, plus a subscription to the original. If you want to see the original, you can. But your lens onto their data is yours, so you can rearrange its contents, edit them, and scatter them across your other files. Until you remove the subscription, anything the other author adds to the original note will be visible from your lens as new "unintegrated subscribed content".

There's a detailed example [here](docs/lenses.md).
## Learn from how others study your ideas -- how they integrate your notes into theirs.
Suppose someone has a lens onto one of your notes. They can make their lens public. You can configure your system to be aware of their public data. If both of those things happen, then when you visit your note, you will see their subscription. If you want to investigate, you can find their lens (which can also include private data visible only to them). This will let you see how your conception of the topic differs from theirs, what else they consider relevant, etc.

For instance, suppose you had a node titled "irresponsibility", a branch of which was called "living like there's no tomorrow". Someone might clone your "irresponsibility" and locate "living like there's no tomorrow" under a separate title "presence", which in turn they had filed under "peace". If their subscription to your "irresponsibility" is visible to you, and you investigate, you will find a novel, positive spin on a phenomenon, "living like there's no tomorrow", which you had previously framed entirely negatively.

In this way, when someone studies your writing, they can help you understand the topic you wrote about, without requiring any extra effort on their part.
## Multidimensional trees
Kind of technical but [really cool](docs/multidimensional-trees.md).
# If you're still unconvinced
there are more [motivations](MOTIVATIONS.md).
# If you're a programmer
Please help.[Skg uses cool technologies!](docs/cool-tech-in-skg.md), including Rust, TypeDB and Emacs. (That said, I'm flexible -- if you want to write a non-Emacs client, or rewrite the server in Haskell, let's go!)

- [[Usage: How to run Skg](docs/usage.md)
- Skg's data model and user experience
  - Trees and hyperlinks: [Some simple but important definitions](docs/vocabulary.md)
  - [Hyperlinks can point to any part of a document.](docs/hyperlinks.md)
  - ["Lenses": Subscription and overriding in detail](docs/lenses.md)
  - [The data model](docs/data-model.md).
  - [The sharing model](docs/sharing-model.md).
- [What's done, and what's left to do](docs/progress.md)
- Some reasonable jumping-off points for understanding the code
  - [The TypeDB schema](schema.tql)
  - [The Rust types](rust/types.rs)
  - [The Rust server](rust/serve.rs). (The `main` module is just a wrapper for this module.)
  - [The Emacs client](elisp/client.el)
