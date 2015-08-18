# logtoy -- A Toy Logging Service

This was a coding sample that I did for a company that no longer exists.

## Task
Write a logging web service with 2 routes.

## Log

The "/log" route accepts a JSON object `{"level": string, "msg": string, "name": string}`. It persists the received data to a human-readable log file named after the name JSON Key. Logging levels should use this data type:

    data LogLevel = Debug | Info | Warn | Error 

The log should have this format:

    [Info] Hello Logger!

## Read

The "/read/a_log_name" route reads back log messages that were written to the file named in the url

It accepts

a query parameter "level" which will return only log messages logged at or above that level.
a query parameter "lines" which is the maximum number of lines that should be returned

## Goals

### Correctness

The web service should meet the specification with minimal defects.

The specification is intentionally vague on implementation details. If you have questions about what behavior is correct, feel free to either ask about it or choose something reasonable.

### Readability and code cleanliness

Make the code nice for your reviewers. We already have the spec, so do not worry about re-documenting the API.

### Performance

This is a non-goal. But we will ask follow-up questions about how performance could be improved.
