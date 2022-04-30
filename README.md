# Quoter

## Purpose

A relatively simple program to read quotes in json format from a disk,
select a random quote, then print it to standard out.
Useful for Kmail and other things.

## Prerequisites

* an Ada compiler that supports Ada 2022, such as recent editions of [GNAT](https://www.adacore.com/download)
* [GnatColl](https://github.com/AdaCore/gnatcoll), to parse json
* a collection of quotes in the json format specified [below](#example)

## Building and installing

    gprbuild -P quoter.gpr
...should build an executable named `main`.
At some point I may rename this in the source and configuration,
but for now I just rename the output file to `ada_signature`,
then move it somewhere into my path (such as `~/bin`).

## Running

Supposing that the signatures are stored in `~/signatures.json`,

    ada_signature ~/signatures.json

## Format

The program expects a JSON array of JSON objects whose fields can be:

* `quote`: the desired quotation
* `author`: the person who wrote or spoke the quotation
* `text`: the text or speech where the quotation can be found
* `speaker`: useful for when the author puts the quote in the mouth of a speaker,
  as one finds in fiction

### Example

    [
        {
            "quote": "I feel that I have done nothing well. But I have done what I could.",
            "author": "St. Dorothy Day",
            "text": "The Long Loneliness"
        },
        {
            "quote": "However white, however beautiful [Anna's] fair arms, however attractive her full bosom, her flushed face against that dark hair, [Vronsky will] find still better ones, as my disgusting, pathetic and dear husband seeks and finds them.",
            "speaker": "Darya Alexandrovna",
            "text": "Anna Karenina, Part VI, Chapter XXIII",
            "author": "Leo Tolstoy"
        }
    ]

## License

What I have written, I place in the public domain.
