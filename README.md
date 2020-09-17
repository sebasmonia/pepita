[![MELPA](https://melpa.org/packages/pepita-badge.svg)](https://melpa.org/#/pepita)

# pepita
_"Pepita": "nugget" in Spanish._

Run a Splunk search from Emacs.

Splunk is a tool that ingests plain data (for example, logs) and makes it searchable. It exposes a REST API, which made this package possible.
See https://www.splunk.com for more information.

**You can buy me a [cup of ko-fi](https://ko-fi.com/A0A527CN7)! There's also a [PayPal option](https://www.paypal.me/sebasmonia).**

## Table of contents

<!--ts-->

   * [Installation and configuration](#installation-and-configuration)
   * [Usage](#usage)
     * [Interactive functions](#interactive-functions)
     * [Parametrized queries](#parametrized-queries)
     * [Splunk search parameters](#splunk-search-parameters)
     * [Results buffer](#results-buffer)
     * [Queries in progress and completed](#queries-in-progress-and-completed)
     * [My workflow](#my-workflow)

<!--te-->

## Installation and configuration

Place pepita.el in your load-path. Or (preferred) install from MELPA.

The next step would be tocall `customize-group` for pepita. Not a lot to see so far:

1. **REQUIRED**: Add the Splunk URL _(Notice the trailing /)_:

```elisp
    (setq 'pepita-splunk-url "https://splunk.something.com:8089/services/")
```

2. You can set `pepita-splunk-username` if you don't want to enter your user name on each session

# Usage

The first request to Splunk you will be prompted user/pass and then your credentials will be
cached in memory as long as Emacs is open. I couldn't get session auth to work 
yet (so that we don't need to keep credentials around anymore) but it's a feature in the roadmap.

## Interactive functions

In my config, I've bound these to `<f12>` and `S-<f12>` respectively:

* `pepita-new-search`: Prompts for a query text and time range. If called with prefix arg, 
provides the parameters from the last search as starting point.

* `pepita-search-at-point`: Just like the previous function, but use the region, or current
line if region is not active, as query text. If called with prefix args, prepend the last 
search text to the new input.

I keep an org file with some common queries, in those scenarios the second function is really handy.
It's also useful to refine a search from results (highlight the text you want to add to the query and
call with prefix arg).
See the next section for even more usefulness from `search-at-point`.

## Parametrized queries

You can store parameters in a query text, and you will be prompted for replacements when using `pepita-search-at-point`.  
This can be super handy if you are building a library of common queries. Let's say you have the following line in a text or org file:

`index=some_application_name TheWorstExceptionEver "%%SomeText%%"  Hostname=%%The host%%`

You set the point anywhere on it, and use your binding for `pepita-search-at-point`.  
You will be prompted "Value for parameter [SomeText]: " and then "Value for parameter [The host]: ". Finally you will see the usual query prompt, with all values replaced.

Note that the parameters name don't matter. For each match to the regex `%%.*?%%` you will be prompted for a replacement that will be inserted literally in the query.  
Ex. `A query %%NoName%% %%NoName%% %%NoName%%` means you will be prompted for the parameter "NoName" three times in a row.

## Splunk search parameters

_Query text_: this is exactly what you would type in the search box in Splunk

_From_: A time specification, or blank.

_To_: A time specification, or blank.

Splunk is really flexible with the format for the last two. For the full details see https://docs.splunk.com/Documentation/Splunk/7.2.4/SearchReference/SearchTimeModifiers, but the following examples can get you started: 

* -5d => five days ago
* -30m => last thirty minutes
* 2019-01-01T14:00:00 => Jan 1st 2019 at 2 PM _ISO 8601 format_
* From: -3h To: -10m => events from 3 hours ago up to 10 minutes ago

## Results buffer

The search runs in the background, and the results are displayed in a new buffer, in CSV format. You can save the buffer and open it in Excel, for example, or load the data in SQLite.  
You can use `C-h m` (describe-mode) to see the commands available. For convenience they are listed below:

* ? - to see the parameters used in the query in the echo area.
* j - to export to JSON. You can select which fields to export (separate by comma), or leave blank to export all of them.
* h - to export to HTML. Field selection works the same. Generates a local web page and launches your default browser. It uses https://datatables.net and you can hide columns dinamically.
* o - to export the results as an Org table. These are easily sorted and you can remove columns within Emacs, but don't support multi line cells. Use your best judgement :)
* t - calls `toggle-truncate-lines`. Depending on your search results truncating can be more or less convenient.
* r - to rename the results buffer to something more meaningful. It suggests the "Splunk:" prefix.
* g - to re-run the query in the same results buffer. Use prefix arg to edit the query before running it.
* G - same as `g`, but will send the results to a new buffer. Useful to compare two runs of the same search, or the same query but adjusting the time span.
* q - to quit, as in most Emacs read-only buffers.

## Queries in progress and completed

The command `pepita-queries-running` will open a buffer with the list of queries waiting for results. Press `g` to refresh the list.
This is useful to keep track of complex, long running queries, without searching your buffer list for all "Splunk results" buffers.

Use `pepita-queries-history` to open a buffer with the list of queries that completed during the current session. RET will re-run
the query under point and `g` will refresh the list. If you are looking at a list of results exported and want to tweak the search but
don't have the original results buffer open to use `?`, then this mode might save the day.

## My workflow

Most of the keybindings came from my own usage (you are welcome to suggest more, and also share your own workflow to refine the functionality).  
I keep an org file with queries that I run with `pepita-search-at-point`, a few of them are parametrized with %%. If I'm looking for error messages or API calls I use `g` to keep refreshing the results in the last 10 minutes/hour etc.  
Sometimes once I get data, I want to filter certain fields. For example, I want to check out how many hosts ran a certain process, using `C-u G` I can add " | table _time host | dedup host" and get a separate results buffer.  
Keep in mind using `g` or `G` with a prefix arg will allow adjusting the query and also the timespan, sometimes when hunting for something you don't modify the query but keep adding hours or days until you hit the jackpot.


Finally, until further optimization, exporting to JSON or HTML doesn't support more than 400~500 items (more if you pre-filter the fields). However, I've been able to successfully retrieve 2GB+ of results, saving the results buffer (since it is CSV) and open the files in Excel. I suppose you could also import the CSV in a database, if needed.


