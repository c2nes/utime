package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

const Year3000Epoch = 32503680000

func abs(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}

func comp(a, b func(string) string) func(string) string {
	return func(s string) string {
		return a(b(s))
	}
}

func parse(s string) (time.Time, error) {
	// Trim special characters to make it easier to copy/paste values
	s = strings.Trim(s, "`~!@#$%^&*()-=_+[]\\{}|;':\",./<>?")
	s = strings.TrimSpace(s)
	s = strings.ToUpper(s)

	// Try as epoch timestamp first
	sec, err := strconv.ParseInt(s, 0, 64)
	if err == nil {
		scale := int64(1)
		for abs(sec/scale) > Year3000Epoch {
			scale *= 1000
		}
		nsec := (1000000000 / scale) * (sec % scale)
		sec = sec / scale
		return time.Unix(sec, nsec), nil
	}

	rfc3339Variants := []func(string) string{
		// RFC3339 w/ space instead of 'T' as date/time separator.
		func(s string) string {
			return strings.Replace(s, "T", " ", 1)
		},
		// RFC3339 w/ alternate timezone indicator
		func(s string) string {
			return strings.Replace(s, "Z07:00", " MST", 1)
		},
		// RFC3339 w/o timezone indicator (local time)
		func(s string) string {
			return strings.Replace(s, "Z07:00", "", 1)
		},
	}

	// Add composed variations
	rfc3339Variants = append(
		rfc3339Variants,
		comp(rfc3339Variants[0], rfc3339Variants[1]),
		comp(rfc3339Variants[0], rfc3339Variants[2]),
	)

	// Fallback to trying various layouts
	layouts := []string{
		time.RFC3339Nano,
		time.RFC3339,
	}

	for _, variant := range rfc3339Variants {
		layouts = append(
			layouts,
			variant(time.RFC3339Nano),
			variant(time.RFC3339),
		)
	}

	layouts = append(
		layouts,
		time.ANSIC,
		time.RFC822,
		time.RFC822Z,
		time.RFC850,
		time.RFC1123,
		time.RFC1123Z,
		time.UnixDate,
		time.RubyDate,
		time.Kitchen,
		// Date only
		"2006-01-02",
	)

	for _, layout := range layouts {
		t, err := time.ParseInLocation(layout, s, time.Local)
		if err == nil {
			return t, nil
		}
	}

	return time.Time{}, errors.New("unrecognized timestamp")
}

func main() {
	var args []string
	var text []string
	for _, arg := range os.Args[1:] {
		if len(text) > 0 || !strings.HasPrefix(arg, "-") {
			text = append(text, arg)
		} else {
			args = append(args, arg)
		}
	}
	flag.CommandLine.Parse(args)

	var input time.Time

	if len(os.Args) > 1 {
		s := strings.Join(os.Args[1:], " ")

		var err error
		input, err = parse(s)

		if err != nil {
			fmt.Fprintln(os.Stderr, err.Error())
			os.Exit(1)
		}
	} else {
		input = time.Now()
	}

	fmt.Println(input.Local().Format(time.RFC3339Nano))
	fmt.Println(input.UTC().Format(time.RFC3339Nano))
	fmt.Println(input.Unix())
	fmt.Println(input.UnixNano() / 1000000)
	fmt.Println(input.UnixNano())
}
