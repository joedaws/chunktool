# chunktool

A command line interface (cli) for breaking up strings into chunks.

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

> Note: If you get a command not found error see comments in Building Section of this README

# Usage

The platforms =x= and =bsky= are built for short posts. If your idea is longer than 
the character limit of the platform you want to post on you want want to chunk
the string into numbered functions. `chunktool` takes your long string and converts 
it to a sequence of chunks each of which is under the character limit 
of your desired platform. Each resulting post will include an index like `3/10`.

Take your idea and put it into a file or type it straight into your terminal.
Then invoke `chunktool {platform-name}` like

``` sh
chunktool bsky "How now brown cow?"
```

You can also forward text from a file like

``` sh
chunktool x "$(cat docs/hello.txt)"
```

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

which you can copy and past into your desired platform.

**Assumptions**

- all words in the input string are less than 250 characters in length (250 < 280 to leave buffer room)
