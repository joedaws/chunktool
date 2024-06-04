# chunktool

A command line interface (cli) for performing tedious tasks you may not want to do manually.

# Building

There will at some future point be a distributable build of this tool but for now
one has to build it to use it.

- Install [`ghcup`](https://www.haskell.org/ghcup/install/)
- clone this repo to your favorite projects directory
- `cd your-awesome-project-dir/chunktool`
- `stack install`

If the installation goes as planned inspect the message for the location of the 
executable `chunktool`. For me it is `~/.local/bin` which I happened to have already
added to my `$PATH`. Either append the install location to `PATH` or use it fully
qualified path.

# Usage

## Chunk String for X (formerly twitter)

When an idea that takes more than 280 characters to render comes into your head,
you may choose to share it with the world by posting on X. The `threadify` function
of `chunktool` takes your long string and converts it to a sequence of chunks
each of which is under the 280 character limit and each of which has 
an index like `(3/10)`.

Take your idea and put it into a file or type it straight into your terminal.
Then invoke `chunktool threadify` like

``` sh
chunktool threadify "$(cat hello.txt)"
```

> Note: If you get a command ntoe found error see comments in Building Section of this README

For example, the string

``` txt
hello there! how are you? This is an example of a tweet (or whatever they are called now) that
is longer than two hundred and eighty characters in length. Such a string would not be able to
be a single tweet (maybe they are called posts now), but would have to be split up into chunks.
It's pretty easy to split up strings by hand as well as number them by hand, but for me it still
takes more concentration than feels worth the cost. Why not let our computer friends (who love strings by the way!)
take care of it for us? I think it sounds reasonable.
```

Will be rendered as

``` txt
hello there! how are you? This is an example of a tweet (or whatever they are called now)
that is longer than two hundred and eighty characters in length. Such a string would not be able t
o be a single tweet (maybe they are called posts now), but would have to be split up 1/3

into chunks. It's pretty easy to split up strings by hand as well as number them by hand,
but for me it still takes more concentration than feels worth the cost. Why not let our computer friends
(who love strings by the way!) take care of it for us? I think it sounds 2/3

reasonable. 3/3
```

which you can copy and past into twitter!

**Assumptions**

- all words in the input string are less than 250 characters in length (250 < 280 to leave buffer room)
