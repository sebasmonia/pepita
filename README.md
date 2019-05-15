[![MELPA](https://melpa.org/packages/pepita-badge.svg)](https://melpa.org/#/pepita)

# pepita
_"Pepita": "nugget" in Spanish._

Run a Splunk search from Emacs.

Splunk is a tool that ingests plain data (for example, logs) and makes it searchable. It exposes a REST API, which made this package possible.
See https://www.splunk.com for more information.

## Table of contents

<!--ts-->

   * [Installation and configuration](#installation-and-configuration)
   * [Usage](#usage)
     * [Interactive functions](#interactive-functions)
     * [Parametrized queries](#parametrized-queries)
     * [Splunk search parameters](#splunk-search-parameters)
     * [Results buffer](#results-buffer)
     * [Queries in progress](#queries-in-progress)
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

You can store parameters in a query text, and you will be prompted for replaments when using `pepita-search-at-point`.  
This can be super handy if you are building a library of common queries. Let's say you have the following line in a text or org file:

`index=some_application_name TheWorstExceptionEver "%%SomeText%%"  Hostname=%%The host%%`

You set the point anywhere on it, and use your binding for `pepita-search-at-point`.  
You will be prompted "Value for parameter [SomeText]: " and then "Value for parameter [The host]: ". Finally you will see the usual query prompt, with all values replaced.

Note that the parameters name don't matter. For each match to the regex `%%.*?%%` you will be prompted for a replacement that will be inserted literally in the query.
Ex. `A query %%NoName%% %%NoName%% %%NoName%%` means you will be prompted for the parameter "NoName" three times in a row.

## Splunk search parameters

_Query text_: this is exacly what you would type in the search box in Splunk

_From_: A time specification, or blank.

_To_: A time specification, or blank.

Splunk is really flexible with the format for the last two. For the full details see https://docs.splunk.com/Documentation/Splunk/7.2.4/SearchReference/SearchTimeModifiers, but the following examples can get you started: 

* -5d => five days ago
* -30m => last thirty minutes
* 2019-01-01T14:00:00 => Jan 1st 2019 at 2 PM _ISO 8601 format_
* From: -3h To: -10m => events from 3 hours ago up to 10 minutes ago

## Results buffer

The search runs in the background, and the results are displayed in a new buffer, in CSV format. You can save the buffer and open it in Excel, for example, or load the data in SQLite.  
When viewing the results the following commands are available:
* ? - to see the parameters used in the query in the echo area.
* j - to export to JSON. You can select which fields to export (separate by comma), or leave blank to export all of them.
* h - to export to HTML. Field selection works the same. Generates a local web page and launches your default browser. It uses https://datatables.net and you can hide columns dinamically.
* g - to re-run the query in the same results buffer. Use prefix arg to edit the query before running it. If you are constantly checking for a certain message in the last, say 5 minutes, this is great.
* G - same as `g`, but will send the results to a new buffer. Useful to compare two runs of the same search, or the same query but adjusting the time span.

## Queries in progress

The command `pepita-queries-running` will open a buffer with the list of queries waiting for results. Press `g` to refresh the list.
This is useful if you kick off several complex, long running queries.
