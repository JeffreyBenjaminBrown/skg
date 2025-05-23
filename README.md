# Who cares?
Skg is for thinking, individually and collectively. It is a simple, novel way to write, collect (from other writers), organize and share information. (Any of which can be ignored -- you don't have to share, for instance, to use the app productively.) In a world of abundant information, I hope Skg will let users direct their attention, process information and collaborate more effectively.

Skg extends the data model explored by knowledge management apps like Notion, TheBrain, OneNote, LogSeq, Roam Research, org-roam, Tana, Obsidian and others. (You may have encountered knowledge management [under some other name](docs/synonyms-for-concept-mapping.md).)

You can skip all the hyperlinks and just read this README to see what the app does.
# What's unique about Skg
Skg would be the first app to let a user:
## Learn from how others use your ideas.
When someone integrates your (public) notes into their (public) notes, they reveal potentially new context to you.

For instance, suppose you had a node titled "irresponsibility", a branch of which was called "living like there's no tomorrow". Someone might link to your "living like there's no tomorrow" from their note "peace, presesnce and mindfulness". Their new link to your note could reveal a positive spin on a phenomenon, "living like there's no tomorrow", which you had previously framed entirely in the negative.

In this way another user, by trying to understand your idea, can help you understand it yourself.
## Subscribe granularly.
In Skg you do not subscribe as a person to another person. Instead, you subscribe parts of your data to parts of other peoples' data.

No feed algorithm will guess what to show you. Rather, when you consciously choose to look at a topic, you will see what the people you subscribe to have shared about that topic. For everyone you subscribe to, you can separately choose which topics to subcribe to.

In a world of scarce attention overflowing with information, granular subscription should help.
## Share granularly.
Granular sharing lets you integrate your private and public thinking while maintaining your privacy. That's because you can share not just whole documents, but selected parts of documents, while keeping the rest private. (Technically, each document is a tree, and each spot in the tree can be shared separately.)

There are good reasons to keep data private: to protect another's privacy, to continue processing an idea that does not yet deserve to be shared, to maintain a competitive edge, simple embarrassment, and probably others. But if you share your writing, others can help you understand it -- even by simply studying it themselves, as described above. Thus, when you make an entire document private just to hide a small part of it, you and others are both worse off.

As an example, suppose your notes on neurotransmitters include an idea for a drug, which you want to keep secret. Skg will let you view your secret drug idea in its natural context: your neurotransmitter notes. Sharing your neurotransmitter notes will let you learn from the people who use them, without revealing your secret drug idea.
## Manipulate others' data as if it were your own, while staying informed of their changes.
If another user shares data with you, you can navigate, edit and reshape it just like it was yours. When you "modify" external data (that is, data you do not control), your data quietly gains a `lens` onto the modified document. The lens is like a clone, which comes into existence with all the original's text and branches, plus a subscription to the original. If you want to see the original, you still can. But your lens is yours, so you can rearrange its contents, edit them, and scatter them across your other files. Until you remove the subscription, anything the other author adds to the original note will be visible from your lens as new "unintegrated subscribed content". (The original data that you cloned is considered already integrated.)

There's a detailed example [here](docs/lenses.md).
## Multidimensional trees
This is a little technical but [really cool](docs/multidimensional-trees.md).
# Thanks for reading that!
Please tell me what you think. My email address is jeff.brown.the@gmail.com
# If you're not sure it would be valuable
I've collected more arguments that it would be, [here](MOTIVATIONS.md).
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
  - [The Emacs client](elisp/main.el)
