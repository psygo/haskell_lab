# Go in the Terminal with Haskell

Go in the terminal, with Haskell.

This project (will) offers:

- A CLI for working with SGF files.
- An interactive way of playing Go in the terminal.

## Compilation

Even if this package doesn't get to be available through the typical package managers, you can always build it from source.

Examine the compilation/run scripts in the `tools` folder for more information.

## References

- SGF
    - [Smart Game Format (SGF) - Wikipedia][wikipedia_sgf]
    - [Definition of the Smart Game Format | British Go Association][british_sgf]
    - [Official SGF Specification][red_bean_sgf]
- Haskell JSON Parsing
    - [Aeson Package](https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson.html#v:FromJSONKeyTextParser)
    - [Parsing JSON with Aeson - School of Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json)
    - [A cheatsheet to JSON handling with Aeson | William Yao](https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html)
    - [Examining a JSON file with the aeson package - Haskell Data Analysis Cookbook](https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783286331/1/ch01lvl1sec13/examining-a-json-file-with-the-aeson-package)
- UI
	- [Wikipedia - Curses](https://en.wikipedia.org/wiki/Curses_(programming_library))
	- [Snake in Haskell with Ncurses - Code Review Stack Exchange](https://codereview.stackexchange.com/questions/189397/snake-in-haskell-with-ncurses)
	- [HSCurses](https://hackage.haskell.org/package/hscurses-1.4.2.0/docs/UI-HSCurses-Curses.html)
- Others
    - [Unicode Table][unicode_table]

[british_sgf]: http://www.britgo.org/tech/sgfspec.html
[red_bean_sgf]: https://www.red-bean.com/sgf/
[unicode_table]: https://unicode-table.com/en/#25CB
[wikipedia_sgf]: https://en.wikipedia.org/wiki/Smart_Game_Format
