# Emacs Kanji Quiz

This package provides commands which process a buffer of Japanese
vocabulary terms in a simple format, and present those terms to the
user in a simple flashcard-like format.

![Demo Animation](../assets/kanji-quiz-demo.gif?raw=true)

# Quiz Input Format

Each vocabulary term is described in a "paragraph," in the usual Emacs
sense; that is, each is separated from the next by two or more newline
characters.

The first line in the paragraph is the Japanese term: a word or
phrase, of which any number of characters are kanji (that is,
characters which match the Emacs regular expression `\cC`).

If any of the characters in the term are kanji, then the second line
of the paragraph must be a list of hiragana readings for each
successive kanji character, in order.  The readings are separated by
any space character (those matching the regular expression `\s-`, or
the Unicode character IDEOGRAPHIC SPACE, U+3000).  A reading may be
followed by a positive whole number in parentheses, which indicates
that the reading spans that number of kanji.  If a reading is not
followed by such text, then it is taken to span a single kanji.

The remaining lines of each paragraph are the English definition of
the term.  The definition may extend over any number of lines.

If the sum of the spans of the provided readings does not match the
number of kanji in the term, an error will be signaled when the terms
are parsed.

## Examples

A term without any kanji:

> ほんま  
> truth; reality

Some terms with kanji:

> 騒がしい  
> さわ  
> noisy

The reading さわ (sawa) will appear centered above the character 騒.

> 切り落とす  
> き お  
> to cut off; to lop off

The readings き (sa) and お (o) will appear centered above the characters
切 and 落 respectively.

> 完璧  
> かん ぺき  
> perfect; complete; flawless

The readings かん (kan) and ぺき (peki) will appear centered above the
characters 完 and 璧 respectively.

> 今日  
> きょう(2)  
> today

The reading きょう (kyō) spans both of the kanji in the word 今日, and
will be shown centered above the pair of characters.

# Starting a quiz

Two commands are provided to start a quiz.

* `kanji-quiz-start-english-first`  
  The quiz first presents the English definition of each term, then
  the Japanese term, then the kanji readings (if any).
* `kanji-quiz-start-kanji-first`  
  The quiz first presents the Japanese term, then the kanji readings
  (if any), then the English definition.

In both cases, the quiz terms are taken from the current buffer when
the command is invoked.

* If the region is active, the terms are taken from the region.
* Otherwise, if a prefix argument `N` is supplied, then the `N` terms
  starting at point are taken.
* Otherwise, all terms from point until the end of the buffer are
  taken.
  
The order of the terms is randomized when the quiz begins.  When the
user advances beyond the last term, the order of the terms not yet
ejected is re-randomized, and those terms are presented again.

# Quiz commands

Three keystroke commands are provided while a quiz is active:

* `n`: `kanji-quiz-advance`  
  Advances the quiz one step.  That is:
  * If the quiz is English-first, advance from the English definition,
    to the Japanese term, to the hiragana readings (if any), to the
    next term.
  * If the quiz is kanji-first, advance from the Japanese term, to the
    hiragana readings (if any), to the English definition, to the next
    term.
* `x`: `kanji-quiz-eject-term`  
  Advance to the next term.  The current term will not be repeated
  during the current quiz.  If the current term is the only term
  remaining in the quiz, an error message is presented instead.
* `q`: `bury-buffer`  
  This is the usual Emacs command that sends the quiz buffer to the
  bottom of the buffer list.

An in-progress quiz cannot be restarted from the beginning as such.
To do so, re-issue a quiz-beginning command
(`kanji-quiz-start-kanji-first` or `kanji-quiz-start-english-first`)
and give it the same set of terms.
