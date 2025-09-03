I format code unusually. If you are a human I won't force you to adhere to these conventions, but if you are an AI, it won't cost you much time, so please do.

* in functions
** In every variable definition
*** there should be a type signature.
*** After the equals sign should follow a newline.
** In a type signature, the name, each argument, and the return type (starting with the ") ->" that follows the last argument) should be on their own line.
* comments
** If a comment applies to a block of code, put it inside the block.
   Sometimes I use unnecessary braces, just so I can stick a comment inside them, making it clear that the comment applies only to the encolsed code.
** Try to make it visibly obvious what code a comment applies to.
   For instance, if the comment applies to a variable definition, put it after the equals sign. This way I know it only applies to that definition, not to following lines of code.
** Preserve existing comments.
   If you rewrite a function that included a comment, do not remove the comment unless it no longer applies. (TODO items, for instance, might be rendered obsolete and deserve deletion.)
* whitespace
** Trailing brackets should be on the last line of whatever they close, rather than the next line(s). (I do like a quiet diff, but more important than that is for the code itself to be readable.)
** Put whitespace around all punctuation
   by which I mean all of the following, and anything similar:
   < > ( ) [ ] { } . ; + - * / :: ? &
   (unless the / is part of a path -- that would be silly).
** Group adjacent punctuation marks.
   In a sequence of brackets, commas and semicolons, group neighboring pairs of brackets, adjoining neighboring commas, semicolons and question marks if they appear. So, for instance:
   (()) -> (( ))
   ()) -> () )
   (((x))) -> (( (x) ))
   )),),)?; -> )), ),) ?;
** Avoid empty lines in function definitions.
   Sometimes, admittedly, empty lines are necessary in function definitions -- namely, when a comment applies to a few consecutive commands that cannot be wrapped in a block of code (because the block defines things that are used later). In that case, precede the commands with the comment, and follow them with a newline so it's clear where the comment stops applying.
   But if possible, use one of the above tricks to group the comment with the code it applies to, rendering the trailing empty line unnecessary.
* Reformat nothing for its own sake.
  All the above are how you should format new or modified code. Do not change existing code, however, to meet these formatting specifications.
