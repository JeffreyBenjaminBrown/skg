Skg is for thinking, individually and collectively. It is a simple, novel way to write, collect (from elsewhere), organize and share information. (Any of which can be ignored -- you don't have to share, for instance, to use it productively.) In a world of abundant information, Skg will let users direct their attention, process information and collaborate more effectively.

Skg is [[extremely open-source](LICENSE.md)]. Nobody will kidnap your data.
# What's unique about Skg
Skg is the first app that lets a user:
## Subscribe granularly.
In Skg you do not subscribe as a person to another person. Instead, you subscribe parts of your data to parts of other peoples' data. When you view your data on a topic, you will be able to see what the people you subscribe to have shared about that topic, ignoring their thoughts on other topics.

In a world of scarce attention overflowing with information, granular subscription should help.
## Share granularly.
You can share not just whole documents, but selected parts of documents. (Technically, each document is a tree, and each spot in the tree can be shared separately.)

[[Private](private-writing.md)] and [[public](public-writing.md)] writing are both important, in different ways. Granular sharing lets you integrate your private and public thoughts, without exposing anything you want to withhold. If you have, for instance, private and public thoughts on love, you can work on them both in the same space, without exposing any of the private thoughts.

When you want to view your work in "public mode" (maybe to share your screen) the private parts will be hidden.
## Manipulate others' data as if it were your own, while staying informed of their changes.
If another user shares data with you, you can navigate, edit and reshape it just like it was yours. When you modify it, your data transparently gains a `lens` onto their document. The lens is like a clone, and begins its existence with a subscription to the original. You don't really change the other author's data, and if you want to see the original, you can. But your lens onto their data is yours, so you can rearrange its contents, edit them, and scatter them across your other files. Until you remove the subscription, anything the other author adds to the original note will be visible from your lens as new "unintegrated subscribed content".
## Learn from how others study your ideas -- how they integrate your notes into theirs.
Suppose someone has a lens onto one of your notes. They can make their lens public. You can configure your system to be aware of their public data. If both of those things happen, then when you visit your note, you will see their subscription. If you want to investigate, you can find their lens (which can also include private data visible only to them). This will let you see how your conception of the topic differs from theirs, what else they consider relevant, etc.

In this way, when someone studies your writing, they can help you understand the topic you wrote about, without requiring any extra effort on their part.
# If you're still unconvinced
- [[Preempting some common concerns](docs/preempt.md)] about ownership and privacy.
- Some obvious motivations
  - [[Public writing is powerful](docs/public-writing.md)]
  - [[Sharing information is powerful.](docs/sharing-is-powerful.md)]
- Some less obvious motivations
  - [[Private writing is powerful.](docs/private-writing.md)]
  - [[Traditional data ownership is awkward.](docs/ownership-has-been-awkward.md)]
  - [[Integrating your public and private thinking only recently became possible.](docs/integrate-public-and-private.md)]
  - [[Knowledge gardening is Zen.](docs/knowledge-gardening-is-zen.md)]
# If you're a programmer
I'd love some help. [[Skg uses cool technologies!](docs/cool-tech-in-skg.md)]

- [[Usage: How to run Skg](docs/usage.md)
- Skg's data model and user experience
  - Trees and hyperlinks: [[Some simple but important definitions](docs/vocabulary.md)]
  - [[Hyperlinks can point to any part of a document.](docs/hyperlinks.md)]
  - [["Lenses": Subscription and overriding in detail](docs/lenses.md)]
  - [[The data model](docs/data-model.md)].
- [[What's done, and what's left to do](docs/progress.md)]
- Some likely jumping-off points for understanding the code
  - [[The TypeDB schema](schema.tql)]
  - [[The Rust types](rust/types.rs)]
  - [[The Rust server](rust/serve.rs)]
  - [[The Emacs client](elisp/client.el)]
