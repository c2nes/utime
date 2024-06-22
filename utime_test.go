package main

import (
	"fmt"
	"testing"
	"time"
)

func TestParse(t *testing.T) {
	*preferDDMM = true
	layouts := []string{
		time.Layout,
		time.ANSIC,
		time.UnixDate,
		time.RubyDate,
		time.RFC822,
		time.RFC822Z,
		time.RFC850,
		time.RFC1123,
		time.RFC1123Z,
		time.RFC3339,
		time.RFC3339Nano,
		time.Kitchen,
		time.Stamp,
		time.StampMilli,
		time.StampMicro,
		time.StampNano,
		time.DateTime,
		time.DateOnly,
		time.TimeOnly,
	}

	now := time.Now()
	for _, layout := range layouts {
		s := now.Format(layout)
		expected, err := time.Parse(layout, s)
		if err != nil {
			panic(err)
		}
		zero, _ := time.Parse("", "")
		actual, err := parse(zero, s)
		if err != nil {
			t.Errorf("failed to parse %q: %v", s, err)
			continue
		}
		if expected.Second() != actual.Time.Second() {
			t.Errorf("S: %v (expected) != %v (actual); %q", expected, actual, s)
		}
		if expected.Nanosecond() != actual.Time.Nanosecond() {
			t.Errorf("N: %v (expected) != %v (actual); %q", expected.Nanosecond(), actual.Time.Nanosecond(), s)
		}
	}
}

func test(t *testing.T, subject string, expected string) {
	expectedTime, err := time.Parse(time.RFC3339Nano, expected)
	if err != nil {
		panic(err)
	}
	zero := time.Date(2000, 1, 1, 0, 0, 0, 0, time.UTC)
	actual, err := parse(zero, subject)
	if err != nil {
		t.Errorf("failed to parse %q: %v", subject, err)
		return
	}
	if !actual.Time.Equal(expectedTime) {
		t.Errorf("%s (expected) != %s (actual); subject: %q", expectedTime, actual.Time, subject)
	}
}

func testErr(t *testing.T, subject string, expected error) {
	zero, _ := time.Parse("", "")
	actual, err := parse(zero, subject)
	if err == nil {
		t.Errorf("expected error %q, actually parsed as %q; subject: %q", expected, actual, subject)
		return
	}
	if err != expected {
		t.Errorf("expected error %q, actually %q; subject: %q", expected, err, subject)
		return
	}
}

func TestParse2(t *testing.T) {
	*preferDDMM = true

	test(t, "10am on feb 3 2024", "2024-02-03T10:00:00Z")
	test(t, "10am on. feb 3 2024", "2024-02-03T10:00:00Z")
	test(t, "feb 3 2024 at 10am", "2024-02-03T10:00:00Z")
	test(t, "10am +5m", "2000-01-01T10:05:00Z")
	test(t, "10am -5m", "2000-01-01T09:55:00Z")
	test(t, "10am +05m", "2000-01-01T10:05:00Z")
	test(t, "10am -05m", "2000-01-01T09:55:00Z")
	test(t, "2024/02/12-07:43:12", "2024-02-12T07:43:12Z")
	test(t, "2024/02/12-07:43:12 in EST", "2024-02-12T07:43:12Z")
	test(t, "2024/02/01 10am america/new_york", "2024-02-01T10:00:00-05:00")
	test(t, "2024/02/01 10am America/New_York", "2024-02-01T10:00:00-05:00")
	test(t, "2024/02/01 10am EST", "2024-02-01T10:00:00-05:00")
	test(t, "2024/02/01 10am dubai", "2024-02-01T10:00:00+04:00")
	// Ensure timezones and "at" are applied in order
	test(t, "2024/02/01 EST at noon in utc", "2024-02-01T17:00:00+00:00")
	test(t, "2024/02/01 EST in utc at noon", "2024-02-01T12:00:00+00:00")
	testErr(t, "1 hour after 12:00pm at 2pm", errTimeAssertionFailure)
	test(t, "1 hour after 1pm at 2pm", "2000-01-01T14:00:00Z")
	test(t, "tomorrow at 2pm", "2000-01-02T14:00:00Z")
	test(t, "1 hour after noon utc tomorrow", "2000-01-02T13:00:00Z")
	test(t, "1 hour after 2pm at 10:00:00-0500", "2000-01-01T15:00:00Z")
	test(t, "now", "2000-01-01T00:00:00Z")
	test(t, "today", "2000-01-01T00:00:00Z")
	// - "next" and "last" look for a match outside the current week/month/year
	// - "this" looks for a matching within the current week/month/year
	// - a day, day of week, or month will use the closest match
	test(t, "next friday", "2000-01-07T00:00:00Z")
	test(t, "last friday", "1999-12-24T00:00:00Z")
	test(t, "friday", "1999-12-31T00:00:00Z")
	test(t, "next monday", "2000-01-03T00:00:00Z")
	test(t, "last monday", "1999-12-20T00:00:00Z")
	test(t, "monday", "2000-01-03T00:00:00Z")
	test(t, "next sunday", "2000-01-09T00:00:00Z")
	test(t, "last sunday", "1999-12-26T00:00:00Z")
	test(t, "sunday", "2000-01-02T00:00:00Z")
	test(t, "next saturday", "2000-01-08T00:00:00Z")
	test(t, "last saturday", "1999-12-25T00:00:00Z")
	test(t, "saturday", "2000-01-01T00:00:00Z")
	test(t, "next february", "2001-02-01T00:00:00Z")
	test(t, "last february", "1999-02-01T00:00:00Z")
	test(t, "february", "2000-02-01T00:00:00Z")
	test(t, "next september", "2001-09-01T00:00:00Z")
	test(t, "last september", "1999-09-01T00:00:00Z")
	test(t, "september", "1999-09-01T00:00:00Z")
	test(t, "january", "2000-01-01T00:00:00Z")
	test(t, "last january", "1999-01-01T00:00:00Z")
	test(t, "next january", "2001-01-01T00:00:00Z")
	test(t, "next feb 29", "2004-02-29T00:00:00Z")
	test(t, "last feb 29", "1996-02-29T00:00:00Z")
	test(t, "feb 29", "2000-02-29T00:00:00Z")
	test(t, "+3 tue", "2000-01-18T00:00:00Z")
	test(t, "+0 tue", "2000-01-04T00:00:00Z")
	test(t, "+0 sat", "2000-01-01T00:00:00Z")
	test(t, "+10 sat", "2000-03-04T00:00:00Z")
	test(t, "+5 feb 15", "2004-02-15T00:00:00Z")
	test(t, "+10 feb 15", "2009-02-15T00:00:00Z")
	test(t, "+5 feb 29", "2016-02-29T00:00:00Z")
	test(t, "-2 feb 29", "1992-02-29T00:00:00Z")
	// Date test (preferDDMM is enabled above)
	test(t, "2003/02/01", "2003-02-01T00:00:00Z")
	test(t, "01/02/2003", "2003-02-01T00:00:00Z")
	test(t, "1/02/2003", "2003-02-01T00:00:00Z")
	test(t, "01/2/2003", "2003-02-01T00:00:00Z")
	test(t, "1/2/2003", "2003-02-01T00:00:00Z")
}

func TestTokenize(t *testing.T) {
	type testCase struct {
		s string
		e []string
	}

	cases := []testCase{
		{"+5m", []string{"+5", "m"}},
		{"10am +5m", []string{"10", "am", "+5", "m"}},
		{"abc. def+ ghi-", []string{"abc", "def", "ghi"}},
	}

	for _, c := range cases {
		actual := fmt.Sprintf("%#v", tokenize(c.s))
		expected := fmt.Sprintf("%#v", c.e)
		if actual != expected {
			t.Errorf("%s (actual) != %s (expected)", actual, expected)
		}
	}
}
