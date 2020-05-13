
## TLDR

Don't use `;` in descriptions, it starts a comment.

```
./hledger-time-report.hs --start 01/11 --end 01/18 > report.csv
./meetings.sh 01/01 01/11
# check monthly summary:
hledger print -f timekeeping.journal -b 2020/02/7 -e 2020/03/25 | hledger -f- b Report
```

## Quick command reference

### Projects balance

```
hledger -f timekeeping.journal b          # <- this does both
hledger -f timekeeping.journal b project
hledger -f timekeeping.journal b unbilled
```

#### Filter by active accounts

```
hledger -f timekeeping.journal b $ACTIVE_CAC_ENTRIES
hledger -f timekeeping.journal b $ACTIVE_CAC_PROJECTS
```

### Sort entries by date (juse use `print`)

```
hledger -f timekeeping.journal print
```

### Date range

Retrieve entries in a particular range, starting at `-b`
and ending **before** `-e`:

```
hledger -f timekeeping.journal print -b 2020/01/07 -e 2020/01/08
```

### List collaborative meetings

```
hledger print -b 2020/01/06 -e 2020/01/08 | hledger -f timekeeping.journal print tag:collab-meeting
```

or just do  `meetings.sh 01/06 01/08`.

### Output to CSV file

These methods also remove any comments after the description.

`stdout`: 

```
hledger register -f timekeeping.journal Misc -O csv
```

Directly to a file:

```
hledger register -f timekeeping.journal Misc -o foo.csv
```

### Pull entries for a particular report as CSV

This combines queries and CSV output:

hledger print -b 2020/01/06 -e 2020/01/08 | hledger register -f- Misc -O csv

## Billing

We send out a bill about once per week:

1. Create a summary description using the date filter
2. Bill for `x = floor(current balance)`:

```
Last/Day/Of/Report
  Billed                  x
  Project                -x
  
```

The current balance for a project can be obtained like:

```
hledger -f timekeeping.journal b -N --format "%(total)" Metajelo
```


## Week-to-week carry-over

**Note** this shouldn't be needed using the billing method outlined above.

Ideally generate these entries automatically as a script:

Given an amount `x` held-over (due to not being an integer),

we first give that time back to the project to make it an integer:

```
Last/Day/Of/Previous/Report  ; NONINT: hold-returned:
  Project                -x
  Time                    x
  
```

Then we give time back to that project

```
First/Day/Of/Next/Report  ; NONINT: hold-taken:
  Project                 x
  Time                   -x
  
```

After looking at the `Transaction` type, it is probably easier to generate these
without using the hledger API.

## Rewrite account names

[link](https://hledger.org/rewrite-account-names.html)

## Importing values

To avoid balancing a transaction, enclose the account in `()`. However,
this is probably not a good idea, better to keep things in balance;
instead, use two accounts `Time`, for the journal's current time spent,
and `PriorTime`, for time imported from a previous journal.

### End-of-year closing


[See](https://youtu.be/H_CdGzLbc7A?t=3311) `hledger close`, or synonymously:
`hledger-equity.hs` (`hledger equity`). But note: https://github.com/simonmichael/hledger/issues/1165