[![MELPA](https://melpa.org/packages/pepita-badge.svg)](https://melpa.org/#/pepita)

# pepita
_"Pepita": "nugget" in Spanish._

Run a Splunk search from Emacs.

Splunk is a tool that ingests plain data (for example, logs) and makes it searchable. It exposes a REST API, which made this package possible.
See https://www.splunk.com for more information.

## Table of contents

<!--ts-->

   * [Installation and configuration](#installation-and-configuration)

   * [Manual](#manual)
     * [Interactive functions](#interactive-functions)
     * [Results buffer](#results-buffer)
   * [Roadmap](#roadmap)
<!--te-->

## Installation and configuration

Place pepita.el in your load-path. Or (preferred) install from MELPA.

The next step would be tocall `customize-group` for pepita. Not a lot to see so far:

1. **REQUIRED**: Add the Splunk URL _(Notice the trailing /)_:

```elisp
    (setq 'pepita-splunk-url "https://splunk.something.com:8089/services/")
```

2. You can set `pepita-splunk-username` if you don't want to enter your user name on each session

# Manual

The first request to Splunk you will be prompted user/pass and then your credentials will be
cached in memory as long as Emacs is open. I couldn't get session auth to work 
yet (so we don't need to keep credentials around anymore) but it's a feature in the roadmap.

## Interactive functions

* `pepita-new-search`: Prompts for a query text and time range. If called with prefix arg, 
provides the parameters from the last search as starting point.

* `pepita-search-at-point`: Just like the previous function, but use the region, or current
line if region is not active, as query text. If called with prefix args, prepend the last 
search text to the new input.

I keep an org file with some common queries, in those scenarios the second function is really handy.
It's also useful to refine a search from results (highlight the text you want to add to the query and
call with prefix arg).

## Search parameters

_Query text_: this is exacly what you would type in the search box in Splunk

_From_: A time specification, or blank.

_To_: A time specification, or blank.


Splunk is really flexible with the format for the last two. For the full details see https://docs.splunk.com/Documentation/Splunk/7.2.4/SearchReference/SearchTimeModifiers, but the following examples can get you started: 

* -5d => five days ago
* -30m => last thirty minutes
* 2019-01-01T14:00:00 => Jan 1st 2019 at 2 PM _ISO 8601 format_
* From: -3h To: -10m => events from 3 hours ago up to 10 minutes ago

## Results buffer

The search runs in the background, and the results are displayed in a new buffer, in CSV format.
From that buffer you can use:
* j - to export to JSON
* h - to export to HTML
* ? - to see the parameters used in the query
* g - to re-run the query in the same results buffer. Use prefix arg to re-run the query in a new buffer.

## Queries in progress

The command `pepita-queries-running` will open a buffer with the list of queries waiting for results. Press `g` to refresh the list.
This is useful if you kick off several complex, long running queries.
