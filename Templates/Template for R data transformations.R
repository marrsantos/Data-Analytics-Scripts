#
# Template for some data transformations
# Author: Marcelo Santos
#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
# Working with dates 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
# Formats
#
# %a / Abbreviated weekday name in the current locale on this platform. (Also matches full name on input: in some locales there are no abbreviations of names.)
# %A / Full weekday name in the current locale. (Also matches abbreviated name on input.)
# %b / Abbreviated month name in the current locale on this platform. (Also matches full name on input: in some locales there are no abbreviations of names.)
# %B / Full month name in the current locale. (Also matches abbreviated name on input.)
# %c / Date and time. Locale-specific on output, "%a %b %e %H:%M:%S %Y" on input.
# %C / Century (00–99): the integer part of the year divided by 100.
# %d / Day of the month as decimal number (01–31).
# %D / Date format such as %m/%d/%y: the C99 standard says it should be that exact format (but not all OSes comply).
# %e / Day of the month as decimal number (1–31), with a leading space for a single-digit number.
# %F / Equivalent to %Y-%m-%d (the ISO 8601 date format).
# %g / The last two digits of the week-based year (see %V). (Accepted but ignored on input.)
# %G / The week-based year (see %V) as a decimal number. (Accepted but ignored on input.)
# %h / Equivalent to %b.
# %H / Hours as decimal number (00–23). As a special exception strings such as 24:00:00 are accepted for input, since ISO 8601 allows these.
# %I / Hours as decimal number (01–12).
# %j / Day of year as decimal number (001–366).
# %m / Month as decimal number (01–12).
# %M / Minute as decimal number (00–59).
# %n / Newline on output, arbitrary whitespace on input.
# %p / AM/PM indicator in the locale. Used in conjunction with %I and not with %H. An empty string in some locales (for example on some OSes, non-English European locales including Russia). The behaviour is undefined if used for input in such a locale.
#      Some platforms accept %P for output, which uses a lower-case version (%p may also use lower case): others will output P.
# %r / For output, the 12-hour clock time (using the locale's AM or PM): only defined in some locales, and on some OSes misleading in locales which do not define an AM/PM indicator. For input, equivalent to %I:%M:%S %p.
# %R / Equivalent to %H:%M.
# %S / Second as integer (00–61), allowing for up to two leap-seconds (but POSIX-compliant implementations will ignore leap seconds).
# %t / Tab on output, arbitrary whitespace on input.
# %T / Equivalent to %H:%M:%S.
# %u / Weekday as a decimal number (1–7, Monday is 1).
# %U / Week of the year as decimal number (00–53) using Sunday as the first day 1 of the week (and typically with the first Sunday of the year as day 1 of week 1). The US convention.
# %V / Week of the year as decimal number (01–53) as defined in ISO 8601. If the week (starting on Monday) containing 1 January has four or more days in the new year, then it is considered week 1. Otherwise, it is the last week of the previous year, and the next week is week 1. (Accepted but ignored on input.)
# %w / Weekday as decimal number (0–6, Sunday is 0).
# %W / Week of the year as decimal number (00–53) using Monday as the first day of week (and typically with the first Monday of the year as day 1 of week 1). The UK convention.
# %x / Date. Locale-specific on output, "%y/%m/%d" on input.
# %X / Time. Locale-specific on output, "%H:%M:%S" on input.
# %y / Year without century (00–99). On input, values 00 to 68 are prefixed by 20 and 69 to 99 by 19 – that is the behaviour specified by the 2004 and 2008 POSIX standards, but they do also say ‘it is expected that in a future version the default century inferred from a 2-digit year will change’.
# %Y / Year with century. Note that whereas there was no zero in the original Gregorian calendar, ISO 8601:2004 defines it to be valid (interpreted as 1BC): see https://en.wikipedia.org/wiki/0_(year). Note that the standards also say that years before 1582 in its calendar should only be used with agreement of the parties involved.
#      For input, only years 0:9999 are accepted.
# %z / Signed offset in hours and minutes from UTC, so -0800 is 8 hours behind UTC. Values up to +1400 are accepted. (Standard only for output.)
# %Z / (Output only.) Time zone abbreviation as a character string (empty if not available). This may not be reliable when a time zone has changed abbreviations over the years.
#
# Strptime (Date-time Conversion Functions to and from Character)
#
# It will create a POSIXlt class object from a string
#
# time zone -02 refers to Brazilian daylight saving period
#
(a<-strptime("01/01/2011", "%d/%m/%Y"))
# $zone=null will reset gmt offset
a$zone<-NULL
a
# next example, observe that the am/pm conversion 
# is working and 5:18 was changed to 17:18
x <- c("06-01-19, 5:12am", "06-07-20, 5:15am",
       "06-07-21, 5:18pm")
strptime(x, "%y-%m-%d, %I:%M%p")
# format needs to refer to the original string. 
# Otherwise Na will be generated. Changing to /
strptime(x, "%y/%m/%d, %I:%M%p")
# H for 0-24 format and I for 0-12
strptime("19-11-2016 22:13", "%d-%m-%Y %H:%M")
strptime("01/01/2007 12:00", "%d/%m/%Y %H:%M")
# %B Full month name in the current locale
strptime('Abril 26, 2001', '%B %d, %Y')
strptime('22JUN01','%d%b%y')
# parsing milliseconds. digits.secs=3 include milliseconds. 
op <- options(digits.secs=3)
strptime('2013-08-28 01:02:03.124','%Y-%m-%d %H:%M:%OS')
options(op)
strptime('2013-08-28 01:02:03.124','%Y-%m-%d %H:%M:%S')
# Excel date numbers - date given as number of days since 1900-01-01
strptime(as.Date(35981, origin = "1899-12-30"), "%Y-%m-%d")
# ISO format
strptime("2012-09-11T21:23:20Z", '%Y-%m-%dT%H:%M:%S')
#
# POSIXlt (local time)
#
# POSIXlt (calendar time)
# sec 0–61: seconds.
# min 0–59: minutes.
# hour 0–23: hours.
# mday 1–31: day of the month
# mon 0–11: months after the first of the year.
# year years since 1900.
# wday 0–6 day of the week, starting on Sunday.
# yday 0–365: day of the year.
# isdst Daylight Saving Time flag. Positive if in force, zero if not, negative if unknown.
# zone (Optional.) The abbreviation for the time zone in force at that time: "" if unknown (but "" might also be used for UTC).
# gmtoff (Optional.) The offset in seconds from GMT: positive values are East of the meridian. Usually NA if unknown, but 0 could mean unknown.
#
# POSIXct ()
#
# ISOdate
#
# ISOdatetime
#
# Date
#
#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
# left and Right join (dyplr)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
