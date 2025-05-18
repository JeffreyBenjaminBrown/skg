Skg is for thinking, individually and collectively. It is a simple, novel way to write, collect (from other writers), organize and share information. (Any of which can be ignored -- you don't have to share, for instance, to use it productively.) In a world of abundant information, I hope Skg will let users direct their attention, process information and collaborate more effectively.
# What's unique about Skg
Skg would be the first app to let a user:
## Subscribe granularly.
In Skg you do not subscribe as a person to another person. Instead, you subscribe parts of your data to parts of other peoples' data.

No feed algorithm will guess what to show you. Rather, when you consciously choose to look at a topic, you will see what the people you subscribe to have shared about that topic. For everyone you subscribe to, you can separately choose which topics to subcribe to.

In a world of scarce attention overflowing with information, granular subscription should help.
## Share granularly.
You can share not just whole documents, but selected parts of documents. (Technically, each document is a tree, and each spot in the tree can be shared separately.)

[Private](docs/private-writing.md) and [public](docs/public-writing.md) writing are both important, in different ways. Granular sharing lets you integrate your private and public thoughts, without exposing anything you want to withhold.

For instance, you might have a note on "motivation", with safely publishable sections about hope, commitment, confidence, realism, etc. And you might want to give it a private branch called "embarrassing ambitions", with branches like "Speak on behalf of the trees" or "Immortality through identity loss".

Skg will let you make "embarrassing ambitions" a private branch of your public "motivation" document. Nobody else will see your private branch, but to you it will appear alongside your other thoughts on motivation.

This kind of public/private context switching in the tree is recursive: Just as a private branch can be part of public content, some branch inside that private branch can again be public. For instance, your private "immortality through identity loss" might in turn contain the public branches "publishing" and "biological reproduction". (Since those two public branches are only connected to "motivations" through intervening private nodes, they would be invisible from "motivations", but could still be accessible from other contexts.)
## Manipulate others' data as if it were your own, while staying informed of their changes.
If another user shares data with you, you can navigate, edit and reshape it just like it was yours. When you "modify" external data (that is, data you do not control), your data quietly gains a `lens` onto the modified document. The lens is like a clone. It comes into existence with all the original's text and branches, plus a subscription to the original. If you want to see the original, you still can. But your lens onto it is yours, so you can rearrange its contents, edit them, and scatter them across your other files. Until you remove the subscription, anything the other author adds to the original note will be visible from your lens as new "unintegrated subscribed content". (The original data that you cloned is already considered integrated.)

There's a detailed example [here](docs/lenses.md).
## Learn from how others study your ideas -- how they integrate your notes into theirs.
Suppose someone has a lens onto one of your notes. They can make their lens public. You can configure your system to be aware of their public data. If both of those things happen, then when you visit your note, you will see their subscription. If you want to investigate, you can find their lens (which can also include private data visible only to them). This will let you see how your conception of the topic differs from theirs, what else they consider relevant, etc.

For instance, suppose you had a node titled "irresponsibility", a branch of which was called "living like there's no tomorrow". Someone might clone your "irresponsibility" and locate "living like there's no tomorrow" under a separate title "presence", which in turn they had filed under "peace". If their subscription to your "irresponsibility" is visible to you, and you investigate, you will find a novel, positive spin on a phenomenon, "living like there's no tomorrow", which you had previously framed entirely negatively.

Or they might, without even modifying it, put links to your document on "absurdity" into a number of theirs:
- awe
- drama
- imagination
- the limits of logic
- things maybe only expressible through art
- Emotional valence is not always obvious.

In this way, when someone studies your writing, they can help you understand the topic you wrote about, without requiring any extra effort on their part.
## Multidimensional trees
Kind of technical but [really cool](docs/multidimensional-trees.md).
# Thank you for reading that! Please tell me what you think.
# If you're not sure it would be valuable
I've collected more arguments that it would, [here](MOTIVATIONS.md).
# If you're a programmer, please help!
[Skg uses cool technologies!](docs/cool-tech-in-skg.md), including Rust, TypeDB and Emacs. (That said, I'm flexible -- if you want to write a non-Emacs client, or rewrite the server in Haskell, let's go!)

- [Usage: How to run Skg](docs/usage.md)
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
